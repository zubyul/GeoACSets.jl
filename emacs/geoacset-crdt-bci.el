;;; geoacset-crdt-bci.el --- CRDT + BCI factory bridge for GeoACSets Fortress -*- lexical-binding: t -*-

;; Bridges CRDT collaborative editing and BCI neural signal streams
;; into the GeoACSets Fortress game. BCI data flows as settlement events;
;; CRDT enables multi-user fortress control.
;;
;; Architecture:
;;   BCI Hardware (Cyton/nRF5340)
;;     → OpenBCI / brainflow
;;     → neurokit2 feature extraction
;;     → NATS broadcast (vivarium topic)
;;     → this bridge (NATS subscribe → fortress events)
;;
;;   CRDT:
;;     crdt.el server on port 6531
;;     → share *geoacset-fortress* buffer
;;     → multiple users see same game state
;;     → GF(3) conflict resolution on simultaneous moves

;;; Code:

(require 'cl-lib)

;; ─────────────────────────────────────────────────────────
;; BCI Signal State
;; ─────────────────────────────────────────────────────────

(defvar gcb-bci-state
  '(:alpha 0.0 :beta 0.0 :theta 0.0 :delta 0.0 :gamma 0.0
    :focus 0.0 :artifact nil :channels 8 :sample-rate 250
    :last-update nil :source "none")
  "Current BCI state — band powers and metadata.")

(defvar gcb-bci-history nil
  "Ring buffer of recent BCI readings for sparkline display.")

(defvar gcb-bci-max-history 60
  "Max readings to keep in history.")

(defvar gcb-nats-process nil
  "NATS subscriber process for BCI data.")

;; ─────────────────────────────────────────────────────────
;; CRDT State
;; ─────────────────────────────────────────────────────────

(defvar gcb-crdt-active nil
  "Non-nil when CRDT sharing is active.")

(defvar gcb-crdt-peers nil
  "List of connected CRDT peer names.")

(defvar gcb-crdt-port 6531
  "CRDT server port.")

;; ─────────────────────────────────────────────────────────
;; BCI → Fortress Bridge
;; ─────────────────────────────────────────────────────────

(defun gcb-bci-update (band-powers)
  "Update BCI state from BAND-POWERS plist and inject into fortress.
BAND-POWERS: (:alpha N :beta N :theta N :delta N :gamma N)"
  (setq gcb-bci-state
        (plist-put (plist-put (plist-put (plist-put (plist-put
          gcb-bci-state
          :alpha (or (plist-get band-powers :alpha) 0))
          :beta (or (plist-get band-powers :beta) 0))
          :theta (or (plist-get band-powers :theta) 0))
          :delta (or (plist-get band-powers :delta) 0))
          :gamma (or (plist-get band-powers :gamma) 0)))
  (plist-put gcb-bci-state :last-update (current-time))
  ;; Compute focus metric (beta/theta ratio)
  (let* ((beta (plist-get gcb-bci-state :beta))
         (theta (max 0.01 (plist-get gcb-bci-state :theta)))
         (focus (/ beta theta)))
    (plist-put gcb-bci-state :focus focus))
  ;; Push to history
  (push (copy-sequence gcb-bci-state) gcb-bci-history)
  (when (> (length gcb-bci-history) gcb-bci-max-history)
    (setq gcb-bci-history (butlast gcb-bci-history)))
  ;; Inject into fortress as Vivarium event
  (when (and (boundp 'gf-locations) gf-locations)
    (let ((viv (cl-find-if (lambda (l) (string= (car l) "Vivarium")) gf-locations)))
      (when viv
        (setf (nth 6 viv)
              (format "BCI: α=%.1f β=%.1f focus=%.2f %s"
                      (plist-get gcb-bci-state :alpha)
                      (plist-get gcb-bci-state :beta)
                      (plist-get gcb-bci-state :focus)
                      (gcb-bci-sparkline)))))))

(defun gcb-bci-sparkline ()
  "Generate a sparkline string from BCI focus history."
  (let* ((chars "▁▂▃▄▅▆▇█")
         (values (mapcar (lambda (s) (plist-get s :focus))
                         (seq-take gcb-bci-history 20)))
         (mx (max 1.0 (apply #'max (or values '(1)))))
         (mn (apply #'min (or values '(0)))))
    (mapconcat
     (lambda (v)
       (let ((idx (min 7 (floor (* (/ (- v mn) (max 0.01 (- mx mn))) 7.99)))))
         (string (aref chars idx))))
     (reverse values) "")))

;; ─────────────────────────────────────────────────────────
;; BCI Simulated Source (for testing without hardware)
;; ─────────────────────────────────────────────────────────

(defvar gcb-bci-sim-timer nil
  "Timer for simulated BCI data.")

(defun gcb-bci-sim-start ()
  "Start simulated BCI data stream (no hardware needed)."
  (interactive)
  (gcb-bci-sim-stop)
  (plist-put gcb-bci-state :source "simulated")
  (setq gcb-bci-sim-timer
        (run-with-timer 0 0.5
         (lambda ()
           (gcb-bci-update
            (list :alpha (+ 8.0 (* 4.0 (sin (* 0.1 (float-time)))))
                  :beta  (+ 15.0 (* 5.0 (sin (* 0.3 (float-time)))))
                  :theta (+ 5.0 (* 2.0 (sin (* 0.05 (float-time)))))
                  :delta (+ 2.0 (* 1.0 (sin (* 0.02 (float-time)))))
                  :gamma (+ 30.0 (* 10.0 (sin (* 0.5 (float-time)))))))
           ;; Auto-refresh fortress if visible
           (when (and (boundp 'gf-view-mode)
                      (get-buffer "*geoacset-fortress*")
                      (eq gf-view-mode 'local)
                      (= gf-player-pos 0))  ; at Vivarium
             (with-current-buffer "*geoacset-fortress*"
               (gf-render))))))
  (message "BCI simulation started (0.5s interval)"))

(defun gcb-bci-sim-stop ()
  "Stop simulated BCI data."
  (interactive)
  (when gcb-bci-sim-timer
    (cancel-timer gcb-bci-sim-timer)
    (setq gcb-bci-sim-timer nil))
  (plist-put gcb-bci-state :source "none")
  (message "BCI simulation stopped"))

;; ─────────────────────────────────────────────────────────
;; NATS Subscriber (real BCI data from vivarium topic)
;; ─────────────────────────────────────────────────────────

(defun gcb-nats-subscribe ()
  "Subscribe to NATS vivarium.bci topic for live BCI data."
  (interactive)
  (when gcb-nats-process
    (delete-process gcb-nats-process))
  (if (not (executable-find "nats"))
      (message "nats CLI not found — install with: brew install nats-io/nats-tools/nats")
    (plist-put gcb-bci-state :source "nats")
    (setq gcb-nats-process
          (start-process "nats-bci" "*nats-bci*"
                         "nats" "sub" "vivarium.bci" "--raw"))
    (set-process-filter gcb-nats-process #'gcb--nats-filter)
    (message "Subscribed to NATS vivarium.bci")))

(defun gcb--nats-filter (proc output)
  "Process NATS BCI messages. Expects JSON: {alpha:N, beta:N, ...}."
  (condition-case nil
      (let ((data (json-parse-string output :object-type 'plist)))
        (gcb-bci-update data))
    (error nil)))

(defun gcb-nats-unsubscribe ()
  "Unsubscribe from NATS BCI topic."
  (interactive)
  (when gcb-nats-process
    (delete-process gcb-nats-process)
    (setq gcb-nats-process nil))
  (plist-put gcb-bci-state :source "none")
  (message "Unsubscribed from NATS"))

;; ─────────────────────────────────────────────────────────
;; CRDT Fortress Sharing
;; ─────────────────────────────────────────────────────────

(defun gcb-crdt-start ()
  "Start CRDT server and share the fortress buffer."
  (interactive)
  (require 'crdt nil t)
  (if (not (featurep 'crdt))
      (message "crdt.el not installed — M-x package-install RET crdt")
    ;; Start session
    (with-current-buffer (get-buffer-create "*geoacset-fortress*")
      (crdt--share-buffer (current-buffer))
      (setq gcb-crdt-active t)
      (message "CRDT sharing fortress on port %d" gcb-crdt-port))))

(defun gcb-crdt-stop ()
  "Stop CRDT sharing."
  (interactive)
  (setq gcb-crdt-active nil)
  (message "CRDT sharing stopped"))

;; ─────────────────────────────────────────────────────────
;; BCI Overlay for Fortress Local View
;; ─────────────────────────────────────────────────────────

(defun gcb-bci-overlay-string ()
  "Generate BCI status string for fortress header."
  (if (string= (plist-get gcb-bci-state :source) "none")
      ""
    (format " BCI[%s] α:%.0f β:%.0f θ:%.0f foc:%.1f %s"
            (plist-get gcb-bci-state :source)
            (plist-get gcb-bci-state :alpha)
            (plist-get gcb-bci-state :beta)
            (plist-get gcb-bci-state :theta)
            (plist-get gcb-bci-state :focus)
            (gcb-bci-sparkline))))

;; ─────────────────────────────────────────────────────────
;; Integration: patch fortress header to show BCI + CRDT
;; ─────────────────────────────────────────────────────────

(defun gcb-patch-fortress-header ()
  "Advise gf--render-world to include BCI/CRDT status."
  (advice-add 'gf--render-world :after
    (lambda ()
      (with-current-buffer "*geoacset-fortress*"
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (end-of-line)
          (insert (propertize (gcb-bci-overlay-string)
                              'face 'gf-event))
          (when gcb-crdt-active
            (insert (propertize
                     (format " CRDT:%d" gcb-crdt-port)
                     'face 'gf-header)))))))
  (advice-add 'gf--render-local :after
    (lambda ()
      (with-current-buffer "*geoacset-fortress*"
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (end-of-line)
          (insert (propertize (gcb-bci-overlay-string)
                              'face 'gf-event)))))))

;; ─────────────────────────────────────────────────────────
;; Keybindings (extend fortress keymap)
;; ─────────────────────────────────────────────────────────

(defun gcb-setup-keys ()
  "Add BCI/CRDT keys to fortress keymap."
  (when (boundp 'gf-mode-map)
    (define-key gf-mode-map (kbd "B") #'gcb-bci-sim-start)
    (define-key gf-mode-map (kbd "N") #'gcb-nats-subscribe)
    (define-key gf-mode-map (kbd "C") #'gcb-crdt-start)
    (define-key gf-mode-map (kbd "S") #'gcb-bci-sim-stop)))

;; ─────────────────────────────────────────────────────────
;; Entry point
;; ─────────────────────────────────────────────────────────

;;;###autoload
(defun geoacset-bci-factory ()
  "Initialize BCI factory mode in fortress."
  (interactive)
  (require 'geoacset-fortress)
  (gcb-setup-keys)
  (gcb-patch-fortress-header)
  (message "BCI Factory mode active. [B]sim [N]nats [C]crdt [S]stop"))

(provide 'geoacset-crdt-bci)
;;; geoacset-crdt-bci.el ends here
