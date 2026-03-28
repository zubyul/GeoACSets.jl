#!/usr/bin/env bash
set -euo pipefail

# Control GeoACSets Fortress via emacs server.
# Usage:
#   fortress-ctl.sh launch              — start daemon + fortress
#   fortress-ctl.sh goto <name>         — navigate to settlement
#   fortress-ctl.sh enter               — local view
#   fortress-ctl.sh world               — world map
#   fortress-ctl.sh zoom                — OLC tile grid for current location
#   fortress-ctl.sh tick [n]            — advance n ticks
#   fortress-ctl.sh pull                — live beeper message pull
#   fortress-ctl.sh status              — show all settlements
#   fortress-ctl.sh open                — open frame in terminal
#   fortress-ctl.sh eval <elisp>        — raw eval

ELISP_DIR="$(cd "$(dirname "$0")" && pwd)"
TILE="$ELISP_DIR/geoacset-tile.el"
FORT="$ELISP_DIR/geoacset-fortress.el"
BCI="$ELISP_DIR/geoacset-crdt-bci.el"

ec() { emacsclient --eval "$1" 2>/dev/null; }

ensure_daemon() {
  if ! ec '(emacs-version)' >/dev/null 2>&1; then
    echo "Starting emacs daemon..."
    emacs --daemon -l "$TILE" -l "$FORT" -l "$BCI" 2>/dev/null
  fi
}

ensure_fortress() {
  local has_buf
  has_buf=$(ec '(buffer-live-p (get-buffer "*geoacset-fortress*"))' 2>/dev/null)
  if [[ "$has_buf" != "t" ]]; then
    ec '(geoacset-fortress)' >/dev/null
  fi
}

case "${1:-status}" in
  launch)
    ensure_daemon
    ensure_fortress
    echo "Fortress running in emacs daemon."
    ec '(with-current-buffer "*geoacset-fortress*"
          (format "At: %s  tick:%d  view:%s"
                  (car (nth gf-player-pos gf-locations))
                  gf-tick gf-view-mode))'
    ;;

  goto)
    NAME="${2:?Usage: fortress-ctl.sh goto <name>}"
    ensure_daemon && ensure_fortress
    ec "(with-current-buffer \"*geoacset-fortress*\"
          (let ((idx (cl-position-if
                      (lambda (l) (string-match-p \"$NAME\" (car l)))
                      gf-locations)))
            (if idx
                (progn (setq gf-player-pos idx gf-view-mode 'world)
                       (gf-render)
                       (format \"Moved to: %s\" (car (nth idx gf-locations))))
              \"Settlement not found\")))"
    ;;

  enter)
    ensure_daemon && ensure_fortress
    ec '(with-current-buffer "*geoacset-fortress*"
          (setq gf-view-mode (quote local))
          (gf-render)
          (buffer-substring-no-properties (point-min) (min (point-max) 1200)))'
    ;;

  world)
    ensure_daemon && ensure_fortress
    ec '(with-current-buffer "*geoacset-fortress*"
          (setq gf-view-mode (quote world))
          (gf-render)
          (buffer-substring-no-properties (point-min) (min (point-max) 2000)))'
    ;;

  zoom)
    ensure_daemon && ensure_fortress
    ec '(with-current-buffer "*geoacset-fortress*"
          (let* ((loc (nth gf-player-pos gf-locations))
                 (lat (nth 1 loc)) (lng (nth 2 loc))
                 (code (geoacset-tile--encode lat lng 6)))
            (setq geoacset-tile-center-code code geoacset-tile-zoom 6)
            (let ((grid (geoacset-tile--compute-grid)))
              (mapconcat
               (lambda (tile)
                 (let* ((r (nth 0 tile)) (c (nth 1 tile)) (code (nth 2 tile))
                        (trit (geoacset-tile--gf3-trit code))
                        (ll (geoacset-tile--code-to-latlon code))
                        (brick (if (cl-oddp r) "╱" " ")))
                   (format "%s[%d,%d] %s trit:%+d %.3f,%.3f"
                           brick r c code trit (car ll) (cdr ll))))
               grid "\n"))))'
    ;;

  tick)
    N="${2:-1}"
    ensure_daemon && ensure_fortress
    for (( i=0; i<N; i++ )); do
      ec '(with-current-buffer "*geoacset-fortress*" (gf-tick))'
    done
    ec '(with-current-buffer "*geoacset-fortress*"
          (format "tick:%d" gf-tick))'
    ;;

  pull)
    ensure_daemon && ensure_fortress
    TOKEN=$(fnox get BEEPER_ACCESS_TOKEN --age-key-file ~/.age/key.txt 2>/dev/null || echo "")
    if [[ -z "$TOKEN" ]]; then
      echo "No beeper token"; exit 1
    fi
    EVENTS=$(curl -sS --max-time 10 \
      "http://localhost:23373/v1/chats/%21lksb6cgAyplpiGk3dgdr%3Abeeper.local/messages?limit=50" \
      -H "Authorization: Bearer $TOKEN" 2>/dev/null | python3 -c "
import sys, json, re
loc_map = {
    'vivarium': 'Vivarium', 'warehouse': 'Warehouse',
    'frontier tower': 'Frontier Tower', 'frontier': 'Frontier Tower',
    'building': 'Frontier Tower', 'dore': '222 Dore St',
    'ocean beach': 'Ocean Beach', 'ohio': 'Ohio State',
    'france': 'France', 'canada': 'Ontario', 'portland': 'Vivarium',
    'bay area': 'Bay Area', 'san francisco': 'Bay Area',
    'washington': 'Washington', 'bookstore': 'Bookstore PDX',
}
d = json.load(sys.stdin)
seen = {}
for m in d.get('items', []):
    text = (m.get('text','') or '').lower()
    sender = m.get('senderName','?')
    for pat, sett in loc_map.items():
        if pat in text and sett not in seen:
            clean = text[:55].replace('\"','').replace('\\\\','')
            seen[sett] = f'{sender}: {clean}'
pairs = ' '.join(f'(\"{k}\" . \"{v}\")' for k,v in seen.items())
print(f'({pairs})')
" 2>/dev/null)

    ec "(with-current-buffer \"*geoacset-fortress*\"
          (let ((updates '$EVENTS))
            (dolist (upd updates)
              (let ((loc (cl-find-if (lambda (l) (string= (car l) (car upd))) gf-locations)))
                (when loc (setf (nth 6 loc) (cdr upd)))))
            (setq gf-tick (1+ gf-tick))
            (gf-render)
            (format \"Pulled %d events at tick %d\" (length updates) gf-tick)))"
    ;;

  status)
    ensure_daemon && ensure_fortress
    ec '(with-current-buffer "*geoacset-fortress*"
          (mapconcat
           (lambda (loc)
             (format "%-18s pop:%-4d %-11s %s"
                     (nth 0 loc) (nth 4 loc) (nth 5 loc) (nth 6 loc)))
           gf-locations "\n"))'
    ;;

  open)
    ensure_daemon && ensure_fortress
    emacsclient -t --eval '(switch-to-buffer "*geoacset-fortress*")'
    ;;

  bci-sim)
    ensure_daemon && ensure_fortress
    ec '(progn (geoacset-bci-factory) (gcb-bci-sim-start) "BCI sim started")'
    ;;

  bci-stop)
    ensure_daemon
    ec '(gcb-bci-sim-stop)'
    ;;

  bci-nats)
    ensure_daemon && ensure_fortress
    ec '(progn (geoacset-bci-factory) (gcb-nats-subscribe) "NATS BCI subscribed")'
    ;;

  bci-status)
    ensure_daemon
    ec '(format "source:%s alpha:%.1f beta:%.1f focus:%.2f sparkline:%s"
         (plist-get gcb-bci-state :source)
         (plist-get gcb-bci-state :alpha)
         (plist-get gcb-bci-state :beta)
         (plist-get gcb-bci-state :focus)
         (gcb-bci-sparkline))'
    ;;

  crdt-start)
    ensure_daemon && ensure_fortress
    ec '(progn (geoacset-bci-factory) (gcb-crdt-start) "CRDT started")'
    ;;

  crdt-stop)
    ensure_daemon
    ec '(gcb-crdt-stop)'
    ;;

  eval)
    shift
    ec "$*"
    ;;

  *)
    echo "Usage: fortress-ctl.sh {launch|goto|enter|world|zoom|tick|pull|status|open}"
    echo "       fortress-ctl.sh {bci-sim|bci-stop|bci-nats|bci-status}"
    echo "       fortress-ctl.sh {crdt-start|crdt-stop|eval}"
    ;;
esac
