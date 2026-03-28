;;; geoacset-tile.el --- Emacs frame tiling via OLC Plus Codes + GeoACSets -*- lexical-binding: t -*-

;; Tile Emacs windows as a geographic brick diagram.
;; Each window maps to an OLC tile at a chosen precision level.
;; Zooming changes precision: P2 (coarsest) → P4 → P6 → P8 → P10 → P11 (finest).
;;
;; The OLC grid is a 20×20 grid at each level, but the visible portion
;; is determined by the "viewport" OLC code. Zooming in refines the code
;; by 2 characters, splitting the current tile into a 20×20 sub-grid
;; of which we display the windowed portion as a brick layout.
;;
;; Brick layout: odd rows offset by half a tile width (like actual bricks).
;; This is the natural OLC layout — latitude bands are uniform but
;; longitude bands interleave.
;;
;; Architecture:
;;   Emacs (this file)
;;     ↕ julia-vterm or async process
;;   GeoACSets.jl + OLC encode/decode
;;     ↕ morphism traversal
;;   SchOLCTileHierarchy (categorical spatial index)

;;; Code:

(require 'cl-lib)

;; ─────────────────────────────────────────────────────────
;; State
;; ─────────────────────────────────────────────────────────

(defvar geoacset-tile-center-code "849VQHFJ+X6"
  "Current OLC center code (San Francisco default).")

(defvar geoacset-tile-zoom 6
  "Current zoom level. One of: 2 4 6 8 10 11.
Corresponds to OLC precision (code length before +).")

(defvar geoacset-tile-cols 5
  "Number of tile columns in the brick layout.")

(defvar geoacset-tile-rows 4
  "Number of tile rows in the brick layout.")

(defvar geoacset-tile-buffers (make-hash-table :test 'equal)
  "Map from OLC code → buffer for each visible tile.")

(defvar geoacset-tile-julia-process nil
  "Julia subprocess for GeoACSets queries.")

(defvar geoacset-tile--last-frame-config nil
  "Saved window configuration before tiling.")

;; ─────────────────────────────────────────────────────────
;; OLC arithmetic (pure elisp, no FFI needed for grid math)
;; ─────────────────────────────────────────────────────────

(defconst geoacset-tile--olc-alphabet "23456789CFGHJMPQRVWX"
  "OLC encoding alphabet (20 characters).")

(defun geoacset-tile--char-val (c)
  "Return numeric value 0-19 of OLC character C."
  (cl-position c geoacset-tile--olc-alphabet))

(defun geoacset-tile--val-char (v)
  "Return OLC character for numeric value V (0-19)."
  (aref geoacset-tile--olc-alphabet (mod v 20)))

(defun geoacset-tile--precision-step (zoom)
  "Lat/lng step size in degrees for ZOOM level."
  (pcase zoom
    (2  20.0)
    (4  1.0)
    (6  0.05)
    (8  0.0025)
    (10 0.000125)
    (11 0.00003125)))

(defun geoacset-tile--code-to-latlon (code)
  "Decode OLC CODE to (lat . lng) of center. Simplified decoder."
  (let* ((clean (replace-regexp-in-string "[+0]" "" code))
         (lat 0.0) (lng 0.0)
         (lat-res 400.0) (lng-res 400.0))
    ;; Pairs: positions 0,1 = lat,lng at 20x resolution
    (cl-loop for i from 0 below (min (length clean) 10) by 2
             do (setq lat-res (/ lat-res 20.0)
                      lng-res (/ lng-res 20.0))
             (when (< i (length clean))
               (setq lat (+ lat (* (geoacset-tile--char-val (aref clean i)) lat-res))))
             (when (< (1+ i) (length clean))
               (setq lng (+ lng (* (geoacset-tile--char-val (aref clean (1+ i))) lng-res)))))
    ;; Offset to center of tile and shift from 0-based to -90/+180
    (cons (- (+ lat (/ lat-res 2.0)) 90.0)
          (- (+ lng (/ lng-res 2.0)) 180.0))))

(defun geoacset-tile--neighbor-code (code dlat dlng)
  "Return OLC code offset by DLAT rows and DLNG columns from CODE.
Uses the tile step size at the code's precision level."
  (let* ((center (geoacset-tile--code-to-latlon code))
         (step (geoacset-tile--precision-step geoacset-tile-zoom))
         (new-lat (+ (car center) (* dlat step)))
         (new-lng (+ (cdr center) (* dlng step))))
    ;; Clamp
    (setq new-lat (max -90.0 (min 90.0 new-lat)))
    (setq new-lng (max -180.0 (min 180.0 new-lng)))
    (geoacset-tile--encode new-lat new-lng geoacset-tile-zoom)))

(defun geoacset-tile--encode (lat lng precision)
  "Encode LAT LNG to OLC code at PRECISION (2,4,6,8,10,11)."
  (let* ((lat (+ lat 90.0))   ; shift to 0-180
         (lng (+ lng 180.0))   ; shift to 0-360
         (code "")
         (lat-res 400.0) (lng-res 400.0)
         (pairs (/ (min precision 10) 2)))
    (dotimes (_ pairs)
      (setq lat-res (/ lat-res 20.0)
            lng-res (/ lng-res 20.0))
      (let ((lat-digit (min 19 (floor (/ lat lat-res))))
            (lng-digit (min 19 (floor (/ lng lng-res)))))
        (setq lat (- lat (* lat-digit lat-res))
              lng (- lng (* lng-digit lng-res)))
        (setq code (concat code
                           (string (geoacset-tile--val-char lat-digit))
                           (string (geoacset-tile--val-char lng-digit))))))
    ;; Insert + after 8 chars
    (if (>= (length code) 8)
        (concat (substring code 0 8) "+" (substring code 8))
      (concat code (make-string (- 8 (length code)) ?0) "+"))))

(defun geoacset-tile--gf3-trit (code)
  "Compute GF(3) trit for OLC CODE. Returns -1, 0, or 1."
  (let ((sum 0))
    (dotimes (i (length code))
      (let ((c (aref code i)))
        (unless (or (= c ?+) (= c ?0))
          (let ((v (geoacset-tile--char-val c)))
            (when v (setq sum (+ sum v)))))))
    (1- (mod sum 3))))

;; ─────────────────────────────────────────────────────────
;; Trit → face coloring
;; ─────────────────────────────────────────────────────────

(defun geoacset-tile--trit-face (trit)
  "Return face for GF(3) TRIT value."
  (pcase trit
    (-1 'geoacset-tile-minus-face)
    (0  'geoacset-tile-ergodic-face)
    (1  'geoacset-tile-plus-face)))

(defface geoacset-tile-minus-face
  '((t :background "#1a1a2e" :foreground "#e94560"))
  "Face for MINUS (-1) trit tiles.")

(defface geoacset-tile-ergodic-face
  '((t :background "#16213e" :foreground "#e0e0e0"))
  "Face for ERGODIC (0) trit tiles.")

(defface geoacset-tile-plus-face
  '((t :background "#0f3460" :foreground "#00ff88"))
  "Face for PLUS (+1) trit tiles.")

(defface geoacset-tile-cursor-face
  '((t :background "#533483" :foreground "#ffffff" :weight bold))
  "Face for the currently selected tile.")

;; ─────────────────────────────────────────────────────────
;; Tile buffer creation
;; ─────────────────────────────────────────────────────────

(defun geoacset-tile--make-buffer (code row col)
  "Create or reuse buffer for tile at OLC CODE, grid position ROW COL."
  (let* ((name (format "*tile:%s*" code))
         (buf (or (gethash code geoacset-tile-buffers)
                  (get-buffer-create name)))
         (trit (geoacset-tile--gf3-trit code))
         (center (geoacset-tile--code-to-latlon code))
         (step (geoacset-tile--precision-step geoacset-tile-zoom))
         (is-center (string= code geoacset-tile-center-code))
         (brick-offset (if (cl-oddp row) "╱" " ")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "%s%s\n" brick-offset code)
                 'face (if is-center 'geoacset-tile-cursor-face
                          (geoacset-tile--trit-face trit))))
        (insert (propertize
                 (format " %+d  %.4f,%.4f\n" trit (car center) (cdr center))
                 'face (geoacset-tile--trit-face trit)))
        (insert (propertize
                 (format " step=%.6f°\n" step)
                 'face (geoacset-tile--trit-face trit)))
        ;; Brick pattern indicator
        (insert (propertize
                 (if (cl-oddp row)
                     " ╱╲╱╲╱╲╱╲╱╲\n"
                   " ╲╱╲╱╲╱╲╱╲╱\n")
                 'face (geoacset-tile--trit-face trit)))
        ;; Fill remaining space with trit-colored background
        (let ((fill-lines 20))
          (dotimes (_ fill-lines)
            (insert (propertize
                     (format " %s\n"
                             (make-string 18
                                          (pcase trit (-1 ?-) (0 ?.) (1 ?+))))
                     'face (geoacset-tile--trit-face trit)))))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (puthash code buf geoacset-tile-buffers)
    buf))

;; ─────────────────────────────────────────────────────────
;; Layout engine: brick tiling
;; ─────────────────────────────────────────────────────────

(defun geoacset-tile--compute-grid ()
  "Compute grid of OLC codes around center, with brick offset on odd rows.
Returns list of (row col olc-code)."
  (let ((half-rows (/ geoacset-tile-rows 2))
        (half-cols (/ geoacset-tile-cols 2))
        grid)
    (dotimes (r geoacset-tile-rows)
      (let ((dlat (- half-rows r)))  ; top = positive lat
        (dotimes (c geoacset-tile-cols)
          (let* ((brick-offset (if (cl-oddp r) 0.5 0.0))
                 (dlng (+ (- c half-cols) brick-offset))
                 (code (geoacset-tile--neighbor-code
                        geoacset-tile-center-code dlat dlng)))
            (push (list r c code) grid)))))
    (nreverse grid)))

(defun geoacset-tile-layout ()
  "Tile the current frame with OLC brick grid. Fast as possible."
  (interactive)
  ;; Save config for restore
  (setq geoacset-tile--last-frame-config (current-window-configuration))
  ;; Clear old tile buffers
  (maphash (lambda (_k buf) (when (buffer-live-p buf) (kill-buffer buf)))
           geoacset-tile-buffers)
  (clrhash geoacset-tile-buffers)
  ;; Delete all windows
  (delete-other-windows)
  ;; Compute grid
  (let* ((grid (geoacset-tile--compute-grid))
         (rows geoacset-tile-rows)
         (cols geoacset-tile-cols)
         ;; Create the window grid: split vertically first, then horizontally
         (windows '()))
    ;; Vertical splits for rows
    (dotimes (r (1- rows))
      (split-window-vertically))
    (balance-windows)
    ;; For each row, split horizontally
    (let ((row-windows '())
          (w (frame-first-window)))
      ;; Collect one window per row
      (dotimes (_ rows)
        (push w row-windows)
        (setq w (next-window w 'no-minibuf)))
      (setq row-windows (nreverse row-windows))
      ;; Split each row window horizontally
      (dolist (rw row-windows)
        (select-window rw)
        (dotimes (_ (1- cols))
          (split-window-horizontally))
        (balance-windows-area)))
    ;; Now assign buffers to windows in order
    (let ((w (frame-first-window)))
      (dolist (tile grid)
        (cl-destructuring-bind (row col code) tile
          (let ((buf (geoacset-tile--make-buffer code row col)))
            (set-window-buffer w buf)
            (set-window-dedicated-p w t)
            (push (list w code row col) windows)
            (setq w (next-window w 'no-minibuf))))))
    ;; Select center tile
    (let ((center-entry (cl-find-if
                         (lambda (e) (string= (nth 1 e) geoacset-tile-center-code))
                         windows)))
      (when center-entry (select-window (car center-entry))))
    (message "GeoACSets tile: %s zoom=%d (%dx%d) trit=%+d"
             geoacset-tile-center-code
             geoacset-tile-zoom
             cols rows
             (geoacset-tile--gf3-trit geoacset-tile-center-code))))

;; ─────────────────────────────────────────────────────────
;; Navigation: zoom, pan, jump
;; ─────────────────────────────────────────────────────────

(defun geoacset-tile-zoom-in ()
  "Zoom in: increase precision by one OLC level."
  (interactive)
  (let ((next (pcase geoacset-tile-zoom
                (2 4) (4 6) (6 8) (8 10) (10 11) (11 11))))
    (setq geoacset-tile-zoom next)
    ;; Re-encode center at new precision
    (let ((c (geoacset-tile--code-to-latlon geoacset-tile-center-code)))
      (setq geoacset-tile-center-code
            (geoacset-tile--encode (car c) (cdr c) geoacset-tile-zoom)))
    (geoacset-tile-layout)))

(defun geoacset-tile-zoom-out ()
  "Zoom out: decrease precision by one OLC level."
  (interactive)
  (let ((prev (pcase geoacset-tile-zoom
                (11 10) (10 8) (8 6) (6 4) (4 2) (2 2))))
    (setq geoacset-tile-zoom prev)
    (let ((c (geoacset-tile--code-to-latlon geoacset-tile-center-code)))
      (setq geoacset-tile-center-code
            (geoacset-tile--encode (car c) (cdr c) geoacset-tile-zoom)))
    (geoacset-tile-layout)))

(defun geoacset-tile-pan (dlat dlng)
  "Pan by DLAT rows and DLNG columns."
  (setq geoacset-tile-center-code
        (geoacset-tile--neighbor-code geoacset-tile-center-code dlat dlng))
  (geoacset-tile-layout))

(defun geoacset-tile-pan-north () (interactive) (geoacset-tile-pan 1 0))
(defun geoacset-tile-pan-south () (interactive) (geoacset-tile-pan -1 0))
(defun geoacset-tile-pan-east ()  (interactive) (geoacset-tile-pan 0 1))
(defun geoacset-tile-pan-west ()  (interactive) (geoacset-tile-pan 0 -1))

(defun geoacset-tile-jump (code)
  "Jump to OLC CODE. Teleportation test: instant reframe."
  (interactive "sOLC code: ")
  (setq geoacset-tile-center-code code)
  ;; Infer zoom from code length
  (let ((clean-len (length (replace-regexp-in-string "[+0]" "" code))))
    (setq geoacset-tile-zoom
          (pcase clean-len
            (2 2) (4 4) (6 6) (8 8) (10 10) (_ 11))))
  (geoacset-tile-layout))

(defun geoacset-tile-select-window-tile ()
  "Jump center to the tile under the current window."
  (interactive)
  (let ((buf (window-buffer (selected-window))))
    (when buf
      (let ((code (cl-loop for k being the hash-keys of geoacset-tile-buffers
                           using (hash-values v)
                           when (eq v buf) return k)))
        (when code
          (setq geoacset-tile-center-code code)
          (geoacset-tile-layout))))))

(defun geoacset-tile-restore ()
  "Restore window configuration from before tiling."
  (interactive)
  (when geoacset-tile--last-frame-config
    (set-window-configuration geoacset-tile--last-frame-config)
    (setq geoacset-tile--last-frame-config nil)))

;; ─────────────────────────────────────────────────────────
;; Keymap
;; ─────────────────────────────────────────────────────────

(defvar geoacset-tile-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'geoacset-tile-zoom-in)
    (define-key map (kbd "-") #'geoacset-tile-zoom-out)
    (define-key map (kbd "=") #'geoacset-tile-zoom-in)
    (define-key map (kbd "<up>") #'geoacset-tile-pan-north)
    (define-key map (kbd "<down>") #'geoacset-tile-pan-south)
    (define-key map (kbd "<right>") #'geoacset-tile-pan-east)
    (define-key map (kbd "<left>") #'geoacset-tile-pan-west)
    (define-key map (kbd "k") #'geoacset-tile-pan-north)
    (define-key map (kbd "j") #'geoacset-tile-pan-south)
    (define-key map (kbd "l") #'geoacset-tile-pan-east)
    (define-key map (kbd "h") #'geoacset-tile-pan-west)
    (define-key map (kbd "g") #'geoacset-tile-jump)
    (define-key map (kbd "RET") #'geoacset-tile-select-window-tile)
    (define-key map (kbd "q") #'geoacset-tile-restore)
    (define-key map (kbd "r") #'geoacset-tile-layout)
    map)
  "Keymap for geoacset-tile navigation.")

;; ─────────────────────────────────────────────────────────
;; Minor mode
;; ─────────────────────────────────────────────────────────

(define-minor-mode geoacset-tile-mode
  "Minor mode for navigating OLC tile grid.
\\{geoacset-tile-map}"
  :lighter " OLC"
  :keymap geoacset-tile-map
  :global t)

;; ─────────────────────────────────────────────────────────
;; Julia bridge (async GeoACSets queries)
;; ─────────────────────────────────────────────────────────

(defun geoacset-tile-julia-query (code callback)
  "Query GeoACSets.jl for tile hierarchy at CODE. Call CALLBACK with result."
  (let ((cmd (format
              "using GeoACSets; t=OLCTileHierarchy(); println(region_of_tile(t,%s))"
              (prin1-to-string code))))
    (if (and geoacset-tile-julia-process
             (process-live-p geoacset-tile-julia-process))
        (process-send-string geoacset-tile-julia-process (concat cmd "\n"))
      ;; Degrade gracefully: pure-elisp OLC math is sufficient for tiling
      (funcall callback nil))))

;; ─────────────────────────────────────────────────────────
;; Entry points
;; ─────────────────────────────────────────────────────────

;;;###autoload
(defun geoacset-tile-here (lat lng)
  "Tile the frame centered on LAT LNG at current zoom."
  (interactive "nLatitude: \nnLongitude: ")
  (setq geoacset-tile-center-code
        (geoacset-tile--encode lat lng geoacset-tile-zoom))
  (geoacset-tile-mode 1)
  (geoacset-tile-layout))

;;;###autoload
(defun geoacset-tile-sf ()
  "Quick start: tile centered on San Francisco."
  (interactive)
  (geoacset-tile-here 37.7749 -122.4194))

;;;###autoload
(defun geoacset-tile-pdx ()
  "Quick start: tile centered on Portland."
  (interactive)
  (geoacset-tile-here 45.5152 -122.6784))

;;;###autoload
(defun geoacset-tile-vivarium ()
  "Quick start: tile centered on Vivarium (535 NW 11th Ave, Portland)."
  (interactive)
  (geoacset-tile-here 45.5267 -122.6818))

(provide 'geoacset-tile)
;;; geoacset-tile.el ends here
