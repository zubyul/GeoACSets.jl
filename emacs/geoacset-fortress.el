;;; geoacset-fortress.el --- Dwarf Fortress-style location game from Beeper messages -*- lexical-binding: t -*-

;; A roguelike map of real locations mentioned across Beeper chats.
;; Each location is a "settlement" with activity, inhabitants, and events
;; pulled from actual message history. Navigate, zoom, inspect.
;;
;; Rendering: CP437-style glyphs, color-coded by GF(3) trit.
;; Data: locations hardcoded from Beeper archive extraction.

;;; Code:

(require 'cl-lib)
(require 'geoacset-tile)

;; ─────────────────────────────────────────────────────────
;; Location database (from Beeper message extraction)
;; ─────────────────────────────────────────────────────────

(defvar gf-locations
  '(;; name          lat      lng      glyph  pop  type         last-event
    ("Vivarium"      45.5267  -122.6818 ?Ω    12  "hackerspace" "BCI night shift")
    ("Warehouse"     45.5230  -122.6850 ?▓     8  "workshop"    "glocked in around")
    ("Frontier Tower" 37.7805  -122.3920 ?▲    949 "coliving"    "Deep Prasad examined")
    ("222 Dore St"   37.7590  -122.4120 ?◆     4  "studio"      "rest of daylight")
    ("Ocean Beach"   37.7594  -122.5107 ?≈     20 "gathering"   "sunset session")
    ("Bookstore PDX" 45.5230  -122.6810 ?♦     3  "meetup"      "meet at bookstore")
    ("NW 11th Ave"   45.5267  -122.6818 ?⌂     5  "residence"   "downstairs code 721912")
    ("Ohio State"    39.9985  -83.0145  ?♣     1  "university"  "Greek dialect speaker")
    ("France"        48.8566  2.3522    ?♠     1  "abroad"      "Erin applies from France")
    ("Ontario"       43.6532  -79.3832  ?♥     1  "subsidiary"  "Plurigrid Canada inc")
    ("Bay Area"      37.7749  -122.4194 ?★     50 "protest"     "historically large protest")
    ("Washington"    38.9072  -77.0369  ?⊕     2  "archive"     "Ladino manuscripts"))
  "Locations extracted from Beeper messages. Each is a settlement.")

(defvar gf-player-pos 0
  "Index into gf-locations for current player position.")

(defvar gf-view-mode 'world
  "Current view: 'world (map) or 'local (settlement detail).")

(defvar gf-messages-cache nil
  "Recent messages for current location.")

(defvar gf-tick 0
  "Game tick counter for animation.")

;; ─────────────────────────────────────────────────────────
;; CP437 / Dwarf Fortress rendering
;; ─────────────────────────────────────────────────────────

(defface gf-terrain    '((t :foreground "#555555")) "Terrain.")
(defface gf-water      '((t :foreground "#4488cc")) "Water.")
(defface gf-mountain   '((t :foreground "#887744")) "Mountains.")
(defface gf-forest     '((t :foreground "#228833")) "Forest.")
(defface gf-settlement '((t :foreground "#ffcc00" :weight bold)) "Settlement.")
(defface gf-player     '((t :foreground "#ff3333" :background "#331111" :weight bold)) "Player.")
(defface gf-trit-minus '((t :foreground "#e94560")) "GF(3) -1.")
(defface gf-trit-zero  '((t :foreground "#aaaaaa")) "GF(3) 0.")
(defface gf-trit-plus  '((t :foreground "#00ff88")) "GF(3) +1.")
(defface gf-event      '((t :foreground "#cc88ff")) "Event text.")
(defface gf-header     '((t :foreground "#ffffff" :background "#222244" :weight bold)) "Header bar.")
(defface gf-dim        '((t :foreground "#444444")) "Dim elements.")
(defface gf-pop-high   '((t :foreground "#ff8800" :weight bold)) "High population.")
(defface gf-pop-low    '((t :foreground "#666600")) "Low population.")

(defconst gf-terrain-chars
  '((?. . gf-terrain)      ; plains
    (?, . gf-terrain)      ; grassland
    (?~ . gf-water)        ; water
    (?≈ . gf-water)        ; ocean
    (?^ . gf-mountain)     ; mountain
    (?▲ . gf-mountain)     ; peak
    (?♣ . gf-forest)       ; forest
    (?♠ . gf-forest)       ; dense forest
    (?# . gf-terrain))     ; wall
  "Terrain character → face mapping.")

(defun gf--trit-face (loc)
  "GF(3) face for location LOC based on OLC trit."
  (let* ((lat (nth 1 loc)) (lng (nth 2 loc))
         (code (geoacset-tile--encode lat lng 8))
         (trit (geoacset-tile--gf3-trit code)))
    (pcase trit (-1 'gf-trit-minus) (0 'gf-trit-zero) (1 'gf-trit-plus))))

(defun gf--pop-face (pop)
  "Face based on population POP."
  (if (> pop 20) 'gf-pop-high 'gf-pop-low))

(defun gf--pseudo-terrain (row col seed)
  "Generate pseudo-terrain char at ROW COL with SEED."
  (let* ((h (logxor (+ (* row 7919) (* col 104729) seed) (lsh seed -3)))
         (v (mod (abs h) 100)))
    (cond
     ((< v 5)  ?≈)    ; ocean
     ((< v 12) ?~)    ; river
     ((< v 25) ?♣)    ; forest
     ((< v 35) ?,)    ; grass
     ((< v 45) ?^)    ; hill
     ((< v 50) ?♠)    ; dense forest
     (t        ?.))))  ; plains

;; ─────────────────────────────────────────────────────────
;; World map renderer
;; ─────────────────────────────────────────────────────────

(defun gf--render-world ()
  "Render the Dwarf Fortress-style world map."
  (let* ((inhibit-read-only t)
         (w (window-width))
         (h (- (window-height) 4))  ; reserve header + footer
         (map-w (min w 80))
         (map-h (min h 35))
         ;; World bounds (fit all locations)
         (lats (mapcar (lambda (l) (nth 1 l)) gf-locations))
         (lngs (mapcar (lambda (l) (nth 2 l)) gf-locations))
         (min-lat (- (apply #'min lats) 2))
         (max-lat (+ (apply #'max lats) 2))
         (min-lng (- (apply #'min lngs) 2))
         (max-lng (+ (apply #'max lngs) 2))
         (lat-range (- max-lat min-lat))
         (lng-range (- max-lng min-lng))
         ;; Map grid
         (grid (make-vector (* map-h map-w) ?.))
         (face-grid (make-vector (* map-h map-w) 'gf-terrain))
         (player-loc (nth gf-player-pos gf-locations)))
    ;; Fill terrain
    (dotimes (r map-h)
      (dotimes (c map-w)
        (let ((ch (gf--pseudo-terrain r c 42)))
          (aset grid (+ (* r map-w) c) ch)
          (aset face-grid (+ (* r map-w) c)
                (or (cdr (assq ch gf-terrain-chars)) 'gf-terrain)))))
    ;; Place settlements
    (cl-loop for loc in gf-locations
             for i from 0
             do (let* ((lat (nth 1 loc))
                       (lng (nth 2 loc))
                       (r (round (* (/ (- max-lat lat) lat-range) (1- map-h))))
                       (c (round (* (/ (- lng min-lng) lng-range) (1- map-w))))
                       (r (max 0 (min (1- map-h) r)))
                       (c (max 0 (min (1- map-w) c)))
                       (idx (+ (* r map-w) c))
                       (glyph (nth 3 loc)))
                  (aset grid idx glyph)
                  (aset face-grid idx
                        (if (= i gf-player-pos) 'gf-player 'gf-settlement))))
    ;; Render header
    (erase-buffer)
    (insert (propertize
             (format " GEOACSET FORTRESS  tick:%04d  %s  OLC:%s  trit:%+d "
                     gf-tick
                     (car player-loc)
                     (geoacset-tile--encode (nth 1 player-loc) (nth 2 player-loc) 8)
                     (geoacset-tile--gf3-trit
                      (geoacset-tile--encode (nth 1 player-loc) (nth 2 player-loc) 8)))
             'face 'gf-header)
            "\n")
    ;; Render map
    (dotimes (r map-h)
      (dotimes (c map-w)
        (let* ((idx (+ (* r map-w) c))
               (ch (aref grid idx))
               (face (aref face-grid idx)))
          (insert (propertize (string ch) 'face face))))
      (insert "\n"))
    ;; Render settlement list (sidebar info)
    (insert (propertize "─── SETTLEMENTS " 'face 'gf-header)
            (propertize (make-string (max 0 (- map-w 16)) ?─) 'face 'gf-header)
            "\n")
    (cl-loop for loc in gf-locations
             for i from 0
             do (let* ((name (nth 0 loc))
                       (pop (nth 4 loc))
                       (kind (nth 5 loc))
                       (event (nth 6 loc))
                       (selected (= i gf-player-pos))
                       (trit-f (gf--trit-face loc)))
                  (insert
                   (propertize (if selected "►" " ") 'face (if selected 'gf-player 'gf-dim))
                   (propertize (format " %-18s" name) 'face (if selected 'gf-player trit-f))
                   (propertize (format " pop:%-4d" pop) 'face (gf--pop-face pop))
                   (propertize (format " %-11s" kind) 'face 'gf-dim)
                   (propertize (format " %s" event) 'face 'gf-event)
                   "\n")))
    ;; Footer
    (insert (propertize
             " [j/k]select [RET]enter [z]zoom [q]quit [t]tick [b]beeper-pull "
             'face 'gf-header))
    (goto-char (point-min))))

;; ─────────────────────────────────────────────────────────
;; Local view renderer (settlement detail)
;; ─────────────────────────────────────────────────────────

(defun gf--render-local ()
  "Render detailed settlement view (Dwarf Fortress embark-style)."
  (let* ((inhibit-read-only t)
         (loc (nth gf-player-pos gf-locations))
         (name (nth 0 loc))
         (lat (nth 1 loc))
         (lng (nth 2 loc))
         (glyph (nth 3 loc))
         (pop (nth 4 loc))
         (kind (nth 5 loc))
         (event (nth 6 loc))
         (olc (geoacset-tile--encode lat lng 10))
         (trit (geoacset-tile--gf3-trit olc))
         (trit-f (gf--trit-face loc))
         ;; Local map: 20x40 of the settlement
         (map-h 18) (map-w 50))
    (erase-buffer)
    ;; Header
    (insert (propertize
             (format " %s %s  pop:%d  %s  OLC:%s  trit:%+d "
                     (string glyph) name pop kind olc trit)
             'face 'gf-header)
            "\n\n")
    ;; Settlement ASCII art based on type
    (pcase kind
      ("hackerspace"
       (insert (propertize "  ┌─────────────────────────────────────────────┐\n" 'face trit-f))
       (insert (propertize "  │  ╔══╗  ╔══╗  ╔══╗        ┌──┐  ┌──┐       │\n" 'face trit-f))
       (insert (propertize "  │  ║≡≡║  ║≡≡║  ║≡≡║  BCI   │☼☼│  │☼☼│  EEG  │\n" 'face trit-f))
       (insert (propertize "  │  ╚══╝  ╚══╝  ╚══╝  LAB   └──┘  └──┘       │\n" 'face trit-f))
       (insert (propertize "  │                                             │\n" 'face trit-f))
       (insert (propertize "  │  ┌────────────┐  ┌─────────────────────┐   │\n" 'face trit-f))
       (insert (propertize "  │  │ SOLDERING  │  │    MAIN FLOOR       │   │\n" 'face trit-f))
       (insert (propertize "  │  │   BENCH    │  │  ☺ ☺   ☺   ☺  ☺    │   │\n" 'face trit-f))
       (insert (propertize "  │  │  ⌐¬ ⌐¬    │  │    desks + screens  │   │\n" 'face trit-f))
       (insert (propertize "  │  └────────────┘  │  ☺       ☺         │   │\n" 'face trit-f))
       (insert (propertize "  │                  └─────────────────────┘   │\n" 'face trit-f))
       (insert (propertize "  │  ┌──────┐                    ┌──────────┐  │\n" 'face trit-f))
       (insert (propertize "  │  │PANTRY│  ═══ HALLWAY ═══   │ BATHROOM │  │\n" 'face trit-f))
       (insert (propertize "  │  └──────┘                    └──────────┘  │\n" 'face trit-f))
       (insert (propertize "  └═════════════════╡DOOR╞════════════════════┘\n" 'face trit-f))
       (insert (propertize "           535 NW 11th Ave, Portland\n" 'face 'gf-dim)))
      ("coliving"
       (insert (propertize "              ▲▲▲ FRONTIER TOWER ▲▲▲\n" 'face trit-f))
       (insert (propertize "             ╔══════════════════════╗\n" 'face trit-f))
       (insert (propertize "             ║ ┌──┐┌──┐┌──┐┌──┐   ║ 12F\n" 'face trit-f))
       (insert (propertize "             ║ │☺ ││☺ ││☺ ││☺ │   ║ 11F\n" 'face trit-f))
       (insert (propertize "             ║ └──┘└──┘└──┘└──┘   ║ 10F\n" 'face trit-f))
       (insert (propertize "             ║ ┌──┐┌──┐┌──┐┌──┐   ║ 9F\n" 'face trit-f))
       (insert (propertize "             ║ │☺ ││☺ ││  ││☺ │   ║ 8F LONGEVITY\n" 'face trit-f))
       (insert (propertize "             ║ └──┘└──┘└──┘└──┘   ║ 7F\n" 'face trit-f))
       (insert (propertize "             ║ ┌──┐┌──┐┌──┐┌──┐   ║ 6F\n" 'face trit-f))
       (insert (propertize "             ║ │♣ ││☺ ││☺ ││☼ │   ║ 5F NEUROTECH\n" 'face trit-f))
       (insert (propertize "             ║ └──┘└──┘└──┘└──┘   ║ 4F\n" 'face trit-f))
       (insert (propertize "             ║ ═══ COMMONS ═══    ║ 3F\n" 'face trit-f))
       (insert (propertize "             ║  ☺ ☺ ☺  GYM  ☺ ☺  ║ 2F\n" 'face trit-f))
       (insert (propertize "             ╚══════╡LOBBY╞═══════╝ 1F\n" 'face trit-f))
       (insert (propertize "                  SF, pop 949\n" 'face 'gf-dim)))
      ("workshop"
       (insert (propertize "  ┌═══════════════════════════════════════┐\n" 'face trit-f))
       (insert (propertize "  ║  THE WAREHOUSE                       ║\n" 'face trit-f))
       (insert (propertize "  ║                                       ║\n" 'face trit-f))
       (insert (propertize "  ║  ┌─────┐  ┌─────┐  ┌──────────────┐  ║\n" 'face trit-f))
       (insert (propertize "  ║  │TOOLS│  │PARTS│  │  MAIN SPACE   │  ║\n" 'face trit-f))
       (insert (propertize "  ║  │⌐¬⌐¬ │  │▓▓▓▓│  │               │  ║\n" 'face trit-f))
       (insert (propertize "  ║  └─────┘  └─────┘  │  ☺ ☺    ☺    │  ║\n" 'face trit-f))
       (insert (propertize "  ║                     │   HARDWARE    │  ║\n" 'face trit-f))
       (insert (propertize "  ║  DGX SPARK CLUSTER  │               │  ║\n" 'face trit-f))
       (insert (propertize "  ║  [9641][4a97][94e2] └──────────────┘  ║\n" 'face trit-f))
       (insert (propertize "  ╚═══════════════╡LOADING DOCK╞══════════╝\n" 'face trit-f))
       (insert (propertize "              Portland, OR\n" 'face 'gf-dim)))
      (_
       ;; Generic settlement
       (insert (propertize (format "  ┌─────────────────────┐\n") 'face trit-f))
       (dotimes (_ 5)
         (insert (propertize (format "  │  %s  %s  %s  %s  %s  │\n"
                                     (if (> (random 3) 0) "☺" " ")
                                     (if (> (random 4) 0) "." "♦")
                                     (if (> (random 3) 0) " " "☺")
                                     (if (> (random 5) 0) "." "▓")
                                     (if (> (random 4) 0) " " "☺"))
                             'face trit-f)))
       (insert (propertize "  └═══════╡DOOR╞════════┘\n" 'face trit-f))))
    ;; Event log
    (insert "\n")
    (insert (propertize " ─── RECENT EVENTS ─── \n" 'face 'gf-header))
    (insert (propertize (format " » %s\n" event) 'face 'gf-event))
    (insert (propertize (format " » OLC hierarchy: %s → %s → %s\n"
                                (geoacset-tile--encode lat lng 4)
                                (geoacset-tile--encode lat lng 6)
                                olc)
                        'face 'gf-dim))
    (insert (propertize (format " » %.4f°N %.4f°W  trit=%+d\n" lat (abs lng) trit)
                        'face 'gf-dim))
    ;; Footer
    (insert "\n")
    (insert (propertize
             " [ESC]world map [z]zoom OLC tiles [q]quit "
             'face 'gf-header))
    (goto-char (point-min))))

;; ─────────────────────────────────────────────────────────
;; Game loop and commands
;; ─────────────────────────────────────────────────────────

(defun gf-render ()
  "Render current view."
  (with-current-buffer (get-buffer-create "*geoacset-fortress*")
    (pcase gf-view-mode
      ('world (gf--render-world))
      ('local (gf--render-local)))))

(defun gf-next ()
  "Select next settlement."
  (interactive)
  (setq gf-player-pos (mod (1+ gf-player-pos) (length gf-locations)))
  (gf-render))

(defun gf-prev ()
  "Select previous settlement."
  (interactive)
  (setq gf-player-pos (mod (1- gf-player-pos) (length gf-locations)))
  (gf-render))

(defun gf-enter ()
  "Enter local view of current settlement."
  (interactive)
  (setq gf-view-mode 'local)
  (gf-render))

(defun gf-world ()
  "Return to world map."
  (interactive)
  (setq gf-view-mode 'world)
  (gf-render))

(defun gf-zoom-olc ()
  "Open OLC tile view centered on current settlement."
  (interactive)
  (let* ((loc (nth gf-player-pos gf-locations))
         (lat (nth 1 loc))
         (lng (nth 2 loc)))
    (geoacset-tile-here lat lng)))

(defun gf-tick ()
  "Advance game tick (animate settlements)."
  (interactive)
  (setq gf-tick (1+ gf-tick))
  ;; Randomly shift a population
  (let* ((idx (random (length gf-locations)))
         (loc (nth idx gf-locations))
         (pop (nth 4 loc))
         (delta (- (random 5) 2)))
    (setf (nth 4 (nth idx gf-locations)) (max 1 (+ pop delta))))
  (gf-render))

(defun gf-quit ()
  "Quit fortress mode."
  (interactive)
  (kill-buffer "*geoacset-fortress*"))

(defvar gf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'gf-next)
    (define-key map (kbd "k") #'gf-prev)
    (define-key map (kbd "n") #'gf-next)
    (define-key map (kbd "p") #'gf-prev)
    (define-key map (kbd "<down>") #'gf-next)
    (define-key map (kbd "<up>") #'gf-prev)
    (define-key map (kbd "RET") #'gf-enter)
    (define-key map (kbd "ESC") #'gf-world)
    (define-key map (kbd "z") #'gf-zoom-olc)
    (define-key map (kbd "t") #'gf-tick)
    (define-key map (kbd "q") #'gf-quit)
    map)
  "Keymap for geoacset-fortress.")

(define-derived-mode gf-mode special-mode "GeoFortress"
  "Major mode for GeoACSets Fortress game."
  (setq buffer-read-only nil
        truncate-lines t)
  (use-local-map gf-mode-map))

;;;###autoload
(defun geoacset-fortress ()
  "Launch GeoACSets Fortress — Dwarf Fortress-style location game from Beeper."
  (interactive)
  (switch-to-buffer (get-buffer-create "*geoacset-fortress*"))
  (gf-mode)
  (setq gf-player-pos 0
        gf-view-mode 'world
        gf-tick 0)
  (gf-render)
  (message "Welcome to GeoACSets Fortress. [j/k] navigate, [RET] enter, [z] zoom OLC."))

(provide 'geoacset-fortress)
;;; geoacset-fortress.el ends here
