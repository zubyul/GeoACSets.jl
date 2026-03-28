#!/usr/bin/env bb

;; fortress-ctl.bb — Babashka fortress controller
;;
;; Replaces fortress-ctl.sh with native Clojure:
;;   - emacsclient control
;;   - Beeper Desktop API (direct HTTP, no curl)
;;   - OLC encode/decode
;;   - BCI sim data generation
;;   - DuckDB archive queries

(require '[babashka.http-client :as http]
         '[babashka.process :as p]
         '[babashka.fs :as fs]
         '[cheshire.core :as json]
         '[clojure.string :as str])

;; ─────────────────────────────────────────────────
;; Config
;; ─────────────────────────────────────────────────

(def elisp-dir (str (fs/parent (fs/absolutize *file*))))
(def tile-el   (str elisp-dir "/geoacset-tile.el"))
(def fort-el   (str elisp-dir "/geoacset-fortress.el"))
(def bci-el    (str elisp-dir "/geoacset-crdt-bci.el"))

(def beeper-api "http://localhost:23373")

;; ─────────────────────────────────────────────────
;; Emacsclient
;; ─────────────────────────────────────────────────

(defn ec [elisp]
  (let [r (p/shell {:out :string :err :string :continue true}
                    "emacsclient" "--eval" elisp)]
    (when (zero? (:exit r))
      (str/trim (:out r)))))

(defn ec! [elisp]
  (or (ec elisp)
      (do (println "emacsclient failed — is daemon running?")
          (System/exit 1))))

(defn daemon-running? []
  (some? (ec "(emacs-version)")))

(defn ensure-daemon! []
  (when-not (daemon-running?)
    (println "Starting emacs daemon...")
    (p/shell "emacs" "--daemon"
             "-l" tile-el "-l" fort-el "-l" bci-el)
    (Thread/sleep 2000)))

(defn ensure-fortress! []
  (when (not= "t" (ec "(buffer-live-p (get-buffer \"*geoacset-fortress*\"))"))
    (ec "(geoacset-fortress)")))

;; ─────────────────────────────────────────────────
;; Beeper API (direct HTTP, no curl/python)
;; ─────────────────────────────────────────────────

(defn beeper-token []
  (or (System/getenv "BEEPER_ACCESS_TOKEN")
      (-> (p/shell {:out :string}
                    "fnox" "get" "BEEPER_ACCESS_TOKEN"
                    "--age-key-file" (str (fs/home) "/.age/key.txt"))
          :out str/trim)))

(defn beeper-get [url]
  (try
    (let [r (p/shell {:out :string :err :string :continue true}
                     "curl" "-sS" "--max-time" "10"
                     "-H" (str "Authorization: Bearer " (beeper-token))
                     (str beeper-api url))]
      (when (zero? (:exit r))
        (json/parse-string (:out r) true)))
    (catch Exception e
      (println "Beeper API error:" (.getMessage e))
      nil)))

(defn beeper-post [path body]
  (try
    (let [r (p/shell {:out :string :err :string :continue true}
                     "curl" "-sS" "--max-time" "10" "-X" "POST"
                     "-H" (str "Authorization: Bearer " (beeper-token))
                     "-H" "Content-Type: application/json"
                     "-d" (json/generate-string body)
                     (str beeper-api path))]
      (when (zero? (:exit r))
        (json/parse-string (:out r) true)))
    (catch Exception e
      (println "Beeper send error:" (.getMessage e))
      nil)))

;; ─────────────────────────────────────────────────
;; OLC encode/decode (pure Clojure)
;; ─────────────────────────────────────────────────

(def olc-alphabet "23456789CFGHJMPQRVWX")

(defn olc-encode [lat lng precision]
  (let [lat (+ lat 90.0)
        lng (+ lng 180.0)
        pairs (/ (min precision 10) 2)]
    (loop [i 0
           lat lat lng lng
           lat-res (/ 400.0 20.0) lng-res (/ 400.0 20.0)
           code ""]
      (if (>= i pairs)
        (let [padded (str code (apply str (repeat (max 0 (- 8 (count code))) "0")) "+")]
          (if (> (count code) 8)
            (str (subs code 0 8) "+" (subs code 8))
            padded))
        (let [lat-d (min 19 (int (/ lat lat-res)))
              lng-d (min 19 (int (/ lng lng-res)))]
          (recur (inc i)
                 (- lat (* lat-d lat-res))
                 (- lng (* lng-d lng-res))
                 (/ lat-res 20.0)
                 (/ lng-res 20.0)
                 (str code
                      (nth olc-alphabet lat-d)
                      (nth olc-alphabet lng-d))))))))

(defn olc-trit [code]
  (let [chars (remove #{\+ \0} code)
        sum (reduce + (map #(.indexOf olc-alphabet (str %)) chars))]
    (- (mod sum 3) 1)))

;; ─────────────────────────────────────────────────
;; Location extraction from Beeper messages
;; ─────────────────────────────────────────────────

(def loc-patterns
  {"vivarium"       "Vivarium"
   "warehouse"      "Warehouse"
   "frontier tower" "Frontier Tower"
   "frontier"       "Frontier Tower"
   "building"       "Frontier Tower"
   "dore"           "222 Dore St"
   "ocean beach"    "Ocean Beach"
   "ohio"           "Ohio State"
   "france"         "France"
   "canada"         "Ontario"
   "portland"       "Vivarium"
   "bay area"       "Bay Area"
   "san francisco"  "Bay Area"
   "washington"     "Washington"
   "bookstore"      "Bookstore PDX"})

(defn extract-location-events [messages]
  (let [seen (atom {})]
    (doseq [m messages
            :let [text (str/lower-case (or (:text m) ""))
                  sender (or (:senderName m) "?")]]
      (doseq [[pattern settlement] loc-patterns]
        (when (and (str/includes? text pattern)
                   (not (get @seen settlement)))
          (swap! seen assoc settlement
                 (str sender ": " (subs text 0 (min 55 (count text))))))))
    @seen))

;; ─────────────────────────────────────────────────
;; Commands
;; ─────────────────────────────────────────────────

(defmulti cmd (fn [c & _] (keyword c)))

(defmethod cmd :launch [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! "(with-current-buffer \"*geoacset-fortress*\"
                   (format \"Fortress at: %s tick:%d\"
                           (car (nth gf-player-pos gf-locations)) gf-tick))")))

(defmethod cmd :status [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! "(with-current-buffer \"*geoacset-fortress*\"
                   (mapconcat
                    (lambda (loc)
                      (format \"%-18s pop:%-4d %-11s %s\"
                              (nth 0 loc) (nth 4 loc) (nth 5 loc) (nth 6 loc)))
                    gf-locations \"\\n\"))")))

(defmethod cmd :goto [_ name & _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! (format "(with-current-buffer \"*geoacset-fortress*\"
                   (let ((idx (cl-position-if
                               (lambda (l) (string-match-p \"%s\" (car l)))
                               gf-locations)))
                     (if idx
                         (progn (setq gf-player-pos idx gf-view-mode 'world)
                                (gf-render)
                                (format \"→ %%s\" (car (nth idx gf-locations))))
                       \"not found\")))" name))))

(defmethod cmd :enter [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! "(with-current-buffer \"*geoacset-fortress*\"
                   (setq gf-view-mode 'local) (gf-render)
                   (buffer-substring-no-properties (point-min) (min (point-max) 1200)))")))

(defmethod cmd :world [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! "(with-current-buffer \"*geoacset-fortress*\"
                   (setq gf-view-mode 'world) (gf-render)
                   (buffer-substring-no-properties (point-min) (min (point-max) 2000)))")))

(defmethod cmd :zoom [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! "(with-current-buffer \"*geoacset-fortress*\"
                   (let* ((loc (nth gf-player-pos gf-locations))
                          (lat (nth 1 loc)) (lng (nth 2 loc))
                          (code (geoacset-tile--encode lat lng 6)))
                     (setq geoacset-tile-center-code code geoacset-tile-zoom 6)
                     (mapconcat
                      (lambda (tile)
                        (let* ((r (nth 0 tile)) (c (nth 1 tile)) (code (nth 2 tile))
                               (trit (geoacset-tile--gf3-trit code))
                               (ll (geoacset-tile--code-to-latlon code)))
                          (format \"%s[%d,%d] %s trit:%+d %.3f,%.3f\"
                                  (if (cl-oddp r) \"╱\" \" \") r c code trit (car ll) (cdr ll))))
                      (geoacset-tile--compute-grid) \"\\n\")))")))

(defmethod cmd :tick [_ & [n]]
  (ensure-daemon!)
  (ensure-fortress!)
  (let [n (or (some-> n parse-long) 1)]
    (dotimes [_ n]
      (ec "(with-current-buffer \"*geoacset-fortress*\" (gf-tick))"))
    (println (ec! "(format \"tick:%d\" gf-tick)"))))

(defmethod cmd :pull [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println "Pulling from Beeper...")
  (let [ies-url "/v1/chats/%21lksb6cgAyplpiGk3dgdr%3Abeeper.local/messages?limit=200"
        data (beeper-get ies-url)
        events (extract-location-events (or (:items data) []))]
    (if (empty? events)
      (println "No location events found")
      (let [elisp-pairs (str "("
                             (str/join " "
                               (map (fn [[k v]]
                                      (format "(\"%s\" . \"%s\")"
                                              k (str/replace v "\"" "")))
                                    events))
                             ")")]
        (println (ec! (format "(with-current-buffer \"*geoacset-fortress*\"
                        (let ((updates '%s))
                          (dolist (upd updates)
                            (let ((loc (cl-find-if (lambda (l) (string= (car l) (car upd))) gf-locations)))
                              (when loc (setf (nth 6 loc) (cdr upd)))))
                          (setq gf-tick (1+ gf-tick))
                          (gf-render)
                          (format \"Pulled %%d events at tick %%d\" (length updates) gf-tick)))"
                              elisp-pairs)))))))

(defmethod cmd :send [_ chat-name & words]
  (let [text (str/join " " words)]
    (println "Searching for chat:" chat-name)
    (let [results (beeper-get (str "/v1/chats/search?query=" (java.net.URLEncoder/encode chat-name "UTF-8")))
          chat (first (:items results))]
      (if-not chat
        (println "Chat not found")
        (let [chat-id (:id chat)
              enc-id (java.net.URLEncoder/encode chat-id "UTF-8")]
          (println (format "Sending to %s (%s)..." (:title chat) (:type chat)))
          (let [resp (beeper-post (str "/v1/chats/" enc-id "/messages") {:text text})]
            (println (if resp
                       (format "Sent: %s" (or (:id resp) (:pendingMessageID resp) "ok"))
                       "Send failed"))))))))

(defmethod cmd :olc [_ lat-s lng-s & [prec-s]]
  (let [lat (parse-double lat-s)
        lng (parse-double lng-s)
        prec (or (some-> prec-s parse-long) 10)
        code (olc-encode lat lng prec)
        trit (olc-trit code)]
    (println (format "%s  trit:%+d  (%.4f, %.4f) precision=%d" code trit lat lng prec))))

(defmethod cmd :bci-sim [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! "(progn (geoacset-bci-factory) (gcb-bci-sim-start) \"BCI sim started\")")))

(defmethod cmd :bci-stop [& _]
  (ensure-daemon!)
  (println (ec! "(gcb-bci-sim-stop)")))

(defmethod cmd :bci-status [& _]
  (ensure-daemon!)
  (println (ec! "(format \"source:%s α:%.1f β:%.1f θ:%.1f focus:%.2f spark:%s\"
                  (plist-get gcb-bci-state :source)
                  (plist-get gcb-bci-state :alpha)
                  (plist-get gcb-bci-state :beta)
                  (plist-get gcb-bci-state :theta)
                  (plist-get gcb-bci-state :focus)
                  (gcb-bci-sparkline))")))

(defmethod cmd :crdt [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (println (ec! "(progn (geoacset-bci-factory) (gcb-crdt-start) \"CRDT started\")")))

(defmethod cmd :open [& _]
  (ensure-daemon!)
  (ensure-fortress!)
  (p/exec "emacsclient" "-t" "--eval" "(switch-to-buffer \"*geoacset-fortress*\")"))

(defmethod cmd :eval [_ & exprs]
  (println (ec! (str/join " " exprs))))

(defmethod cmd :default [c & _]
  (println "fortress-ctl.bb — babashka fortress controller")
  (println)
  (println "Navigation:")
  (println "  launch              Start daemon + fortress")
  (println "  status              Show all settlements")
  (println "  goto <name>         Navigate to settlement")
  (println "  enter               Local view (ASCII floorplan)")
  (println "  world               World map")
  (println "  zoom                OLC tile grid")
  (println "  tick [n]            Advance game ticks")
  (println "  open                Open in terminal emacs")
  (println)
  (println "Beeper:")
  (println "  pull                Live message → settlement events")
  (println "  send <chat> <text>  Send message via Beeper")
  (println)
  (println "BCI:")
  (println "  bci-sim             Start simulated BCI stream")
  (println "  bci-stop            Stop BCI stream")
  (println "  bci-status          Show BCI band powers + sparkline")
  (println)
  (println "CRDT:")
  (println "  crdt                Start CRDT sharing")
  (println)
  (println "Util:")
  (println "  olc <lat> <lng> [p] Encode lat/lng to OLC")
  (println "  eval <elisp>        Raw emacsclient eval"))

;; ─────────────────────────────────────────────────
;; Main
;; ─────────────────────────────────────────────────

(apply cmd (or (seq *command-line-args*) ["help"]))
