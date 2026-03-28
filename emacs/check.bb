#!/usr/bin/env bb
;; check.bb — verify that all the pieces work together
;;
;; The list of things that should work:
;;
;; .dd  (dedushka)  — keystroke injection payloads (Tribe 2: hardware)
;; .bb  (babashka)  — clojure scripts (Tribe 3: making things)
;; .gbb (gay bb)    — GF(3)-colored babashka (Tribe 1+3: math + making)
;; .el  (elisp)     — emacs modules (Tribe 1: the living system)
;; .sh  (shell)     — legacy glue (should migrate to .gbb)
;;
;; This script checks every link in the chain.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[clojure.string :as str])

(def checks (atom []))

(defn check! [name test-fn]
  (let [result (try (test-fn) (catch Exception e {:ok false :msg (.getMessage e)}))]
    (swap! checks conj (merge {:name name} result))))

(defn ok [msg] {:ok true :msg msg})
(defn fail [msg] {:ok false :msg msg})

(defn file-exists? [path]
  (if (fs/exists? path)
    (ok (str (fs/size path) " bytes"))
    (fail "MISSING")))

(defn cmd-ok? [& args]
  (let [r (apply p/shell {:out :string :err :string :continue true} args)]
    (if (zero? (:exit r))
      (ok (str/trim (subs (:out r) 0 (min 60 (count (:out r))))))
      (fail (str "exit " (:exit r))))))

;; ═══════════════════════════════════════════════
;; .dd — DuckyScript payloads (Tribe 2: hardware)
;; ═══════════════════════════════════════════════

(println "\n=== .dd (dedushka) — keystroke injection ===")

(doseq [f ["payload_unified.dd" "payload_linux.dd" "payload_macos.dd" "payload_windows.dd"]]
  (check! (str "dd:" f)
    #(file-exists? (str (fs/home) "/worlds/f/fleet-bootstrap/" f))))

;; ═══════════════════════════════════════════════
;; .bb — Babashka scripts (Tribe 3: making)
;; ═══════════════════════════════════════════════

(println "=== .bb (babashka) — clojure scripts ===")

(check! "bb:version"
  #(cmd-ok? "bb" "--version"))

(doseq [f ["bci-catalog.bb" "bci-channel-activity.bb" "gay-fnox-open-game.bb"]]
  (check! (str "bb:" f)
    #(file-exists? (str (fs/home) "/worlds/" f))))

(check! "bb:fortress-ctl"
  #(file-exists? (str (fs/home) "/worlds/g/GeoACSets.jl/emacs/fortress-ctl.bb")))

;; ═══════════════════════════════════════════════
;; .gbb — Gay Babashka (Tribe 1+3: math + making)
;; ═══════════════════════════════════════════════

(println "=== .gbb (gay babashka) — GF(3)-colored ===")

(check! "gbb:beeper"
  #(file-exists? (str (fs/home) "/.agents/skills/beeper/scripts/beeper.gbb")))

(check! "gbb:beeper-runs"
  #(cmd-ok? "bb" (str (fs/home) "/.agents/skills/beeper/scripts/beeper.gbb") "help"))

;; ═══════════════════════════════════════════════
;; .el — Emacs modules (Tribe 1: the living system)
;; ═══════════════════════════════════════════════

(println "=== .el (elisp) — emacs modules ===")

(let [emacs-dir (str (fs/home) "/worlds/g/GeoACSets.jl/emacs/")]
  (doseq [f ["geoacset-tile.el" "geoacset-fortress.el" "geoacset-crdt-bci.el"]]
    (check! (str "el:" f)
      #(file-exists? (str emacs-dir f)))))

(check! "el:emacs-available"
  #(cmd-ok? "emacs" "--version"))

(check! "el:tile-batch-test"
  #(let [r (p/shell {:out :string :err :string :continue true}
            "emacs" "--batch"
            "-l" (str (fs/home) "/worlds/g/GeoACSets.jl/emacs/geoacset-tile.el")
            "--eval" "(message \"%s\" (geoacset-tile--encode 45.5267 -122.6818 8))")]
     (if (str/includes? (str (:err r) (:out r)) "84QV")
       (ok "OLC encode works")
       (fail "OLC encode broken"))))

(check! "el:emacs-daemon"
  #(let [r (p/shell {:out :string :err :string :continue true}
            "emacsclient" "--eval" "(emacs-version)")]
     (if (zero? (:exit r))
       (ok (str/trim (:out r)))
       (fail "daemon not running"))))

;; ═══════════════════════════════════════════════
;; .sh — Shell scripts (legacy, should → .gbb)
;; ═══════════════════════════════════════════════

(println "=== .sh (legacy shell) — migration candidates ===")

(let [beeper-dir (str (fs/home) "/.agents/skills/beeper/scripts/")]
  (doseq [f ["beeper_route.sh" "beeper_bisim.sh" "beeper_paginate.sh"
             "beeper_whatsapp_archive.sh" "beeper_verify_send.sh"
             "beeper_upload_guard.sh" "beeper_send_file.sh"]]
    (check! (str "sh:" f " (→gbb)")
      #(file-exists? (str beeper-dir f)))))

;; ═══════════════════════════════════════════════
;; DuckDB — materialized tables
;; ═══════════════════════════════════════════════

(println "=== duckdb — materialized tables ===")

(check! "duckdb:binary"
  #(cmd-ok? "duckdb" "--version"))

(doseq [table ["dm_landscape" "contacts" "barton_sent"]]
  (check! (str "duckdb:" table)
    #(let [r (p/shell {:out :string :err :string :continue true}
              "duckdb" (str (fs/home) "/i.duckdb") "-noheader" "-csv"
              "-c" (format "SELECT COUNT(*) FROM %s;" table))]
       (if (zero? (:exit r))
         (ok (str (str/trim (:out r)) " rows"))
         (fail "table missing")))))

;; ═══════════════════════════════════════════════
;; Beeper Desktop API
;; ═══════════════════════════════════════════════

(println "=== beeper — desktop API ===")

(check! "beeper:api"
  #(let [r (p/shell {:out :string :err :string :continue true}
            "curl" "-sS" "--max-time" "3" "http://localhost:23373/v1/chats/search"
            "-G" "--data-urlencode" "query=ies"
            "-H" (str "Authorization: Bearer "
                      (str/trim (:out (p/shell {:out :string}
                        "fnox" "get" "BEEPER_ACCESS_TOKEN"
                        "--age-key-file" (str (fs/home) "/.age/key.txt"))))))]
     (if (and (zero? (:exit r)) (str/includes? (:out r) "items"))
       (ok "API responding")
       (fail "API unreachable"))))

;; ═══════════════════════════════════════════════
;; SQLite sources
;; ═══════════════════════════════════════════════

(println "=== sqlite — source databases ===")

(check! "sqlite:beeper-account.db"
  #(file-exists? (str (fs/home) "/Library/Application Support/BeeperTexts/account.db")))

(check! "sqlite:imessage-chat.db"
  #(file-exists? (str (fs/home) "/Library/Messages/chat.db")))

;; ═══════════════════════════════════════════════
;; Cross-system links (the actual "should work together" list)
;; ═══════════════════════════════════════════════

(println "=== links — things that connect ===")

(check! "link:gbb→duckdb (beeper.gbb staleness)"
  #(cmd-ok? "bb" (str (fs/home) "/.agents/skills/beeper/scripts/beeper.gbb") "staleness"))

(check! "link:gbb→beeper-api (beeper.gbb find-chat)"
  #(cmd-ok? "bb" (str (fs/home) "/.agents/skills/beeper/scripts/beeper.gbb") "route" "find-chat" "ies"))

(check! "link:bb→emacs (fortress-ctl status)"
  #(let [r (p/shell {:out :string :err :string :continue true}
            "bb" (str (fs/home) "/worlds/g/GeoACSets.jl/emacs/fortress-ctl.bb") "status")]
     (if (and (zero? (:exit r)) (str/includes? (:out r) "Vivarium"))
       (ok "fortress alive in daemon")
       (fail "fortress not running"))))

(check! "link:el→olc→gf3 (trit computation)"
  #(let [r (p/shell {:out :string :err :string :continue true}
            "bb" (str (fs/home) "/worlds/g/GeoACSets.jl/emacs/fortress-ctl.bb") "olc" "45.5267" "-122.6818" "8")]
     (if (and (zero? (:exit r)) (str/includes? (:out r) "84QV"))
       (ok "OLC+GF(3) working")
       (fail "OLC broken"))))

;; ═══════════════════════════════════════════════
;; Report
;; ═══════════════════════════════════════════════

(println "\n═══════════════════════════════════════════════")
(println "CHECKLIST RESULTS")
(println "═══════════════════════════════════════════════\n")

(let [results @checks
      pass (filter :ok results)
      fail (remove :ok results)]
  (doseq [r results]
    (println (format "%s %-42s %s"
               (if (:ok r) "✓" "✗")
               (:name r)
               (:msg r))))
  (println)
  (println (format "%d/%d pass, %d fail"
             (count pass) (count results) (count fail)))
  (when (seq fail)
    (println "\nFAILED:")
    (doseq [r fail]
      (println (format "  ✗ %s: %s" (:name r) (:msg r)))))
  (System/exit (if (empty? fail) 0 1)))
