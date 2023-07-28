;;; evil-keypad.el --- Evil keypad mode -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Keypad state is a special state to simulate C-x and C-c key sequences.
;; There are three commands:
;;
;; evil-keypad-start
;; Enter keypad state, and simulate this key with Control modifier.
;;
;; evil-keypad-self-insert
;; This command is bound to every single key in keypad state.
;; The rules,
;; - If current key is SPC, the next will be considered without modifier.
;; - If current key is m, the next will be considered with Meta modifier.
;; - Other keys, or SPC and m after a prefix, means append a key input, by default, with Control modifier.
;;
;; evil-keypad-undo
;; Remove the last input, if there's no input in the sequence, exit the keypad state.

;;; Code:

(require 'subr-x)
(require 'evil)
(defvar evil--keypad-keys nil)
(defvar evil--keypad-previous-state nil)
(defvar evil--keypad-allow-quick-dispatch nil)

(defvar evil--prefix-arg nil)
(defvar evil--use-literal nil)
(defvar evil--use-meta nil)
(defvar evil--use-both nil)

(defvar evil--keypad-this-command nil
  "Command name for current keypad execution.")

(defvar evil--keypad-keymap-description-activated nil
  "Whether KEYPAD keymap description is already activated.")

(defvar evil--keypad-help nil
  "If keypad in help mode.")

(defvar evil--keypad-base-keymap nil
  "The keymap used to lookup keys in KEYPAD state.

Nil means to lookup in top-level.")

(defcustom evil-keypad-message t
  "Whether to log keypad messages in minibuffer."
  :group 'evil
  :type 'boolean)

(defcustom evil-keypad-self-insert-undefined t
  "Whether to self-insert a key in keypad mode if it is undefined"
  :group 'evil
  :type 'boolean)

(defun evil--keypad-format-upcase (k)
  "Return S-k for upcase k."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun evil--keypad-format-key-1 (key)
  "Return a display format for input KEY."
  (cl-case (car key)
    (meta (format "M-%s" (cdr key)))
    (control (format "C-%s" (evil--keypad-format-upcase (cdr key))))
    (both (format "C-M-%s" (evil--keypad-format-upcase (cdr key))))
    (literal (cdr key))))

(defun evil--keypad-format-prefix ()
  "Return a display format for current prefix."
  (cond
   ((equal '(4) evil--prefix-arg)
    "C-u ")
   (evil--prefix-arg
    (format "%s " evil--prefix-arg))
   (t "")))

(defun evil--is-self-insertp (cmd)
  (and (symbolp cmd)
       (string-match-p "\\`.*self-insert.*\\'"
                       (symbol-name cmd))))

(defun evil--keypad-lookup-key (keys)
  (let* ((overriding-local-map evil--keypad-base-keymap)
         (keybind (key-binding keys)))
    (unless (and (evil--is-self-insertp keybind)
                 (not evil-keypad-self-insert-undefined))
      keybind)))

(defun evil--keypad-format-keys (&optional prompt)
  "Return a display format for current input keys."
  (let ((result ""))
    (setq result
          (thread-first
              (mapcar #'evil--keypad-format-key-1 evil--keypad-keys)
            (reverse)
            (string-join " ")))
    (cond
     (evil--use-both
      (setq result
            (if (string-empty-p result)
                "C-M-"
              (concat result " C-M-"))))
     (evil--use-meta
      (setq result
            (if (string-empty-p result)
                "M-"
              (concat result " M-"))))
     (evil--use-literal
      (setq result (concat result " ○")))

     (prompt
      (setq result (concat result " C-"))))
    result))

(defun evil--keypad-has-sub-meta-keymap-p ()
  (and (not evil--use-literal)
       (not evil--use-both)
       (not evil--use-meta)
       (or (not evil--keypad-keys)
           (let* ((key-str (evil--keypad-format-keys nil))
                  (keymap (evil--keypad-lookup-key (kbd key-str))))
             (and (keymapp keymap)
                  (lookup-key keymap ""))))))

(defun evil--exit-keypad-state ()
  "Exit keypad state."
  ;; (evil-keypad-mode -1)
  (evil-change-to-previous-state)
  ;; (when (and (eq 'beacon evil--keypad-previous-state)
  ;;            evil--current-state)
  ;;   (evil--beacon-apply-command evil--keypad-this-command))
  ;; (when evil--keypad-previous-state
  ;;   (evil--switch-state evil--keypad-previous-state))
  )

(defun evil--keypad-quit ()
  "Quit keypad state."
  (setq evil--keypad-keys nil
        evil--use-literal nil
        evil--use-meta nil
        evil--use-both nil
        evil--keypad-help nil)
  (setq overriding-local-map nil)
  (evil--exit-keypad-state))

(defun evil-keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when evil-keypad-message
    (message "KEYPAD exit"))
  (evil--keypad-quit))

(defun evil-keypad-get-title (def)
  "Return a symbol as title or DEF.

Returning DEF will result in a generated title."
  (if-let ((cmd (and (symbolp def)
                     (commandp def)
                     (get def 'evil-dispatch))))
      (evil--keypad-lookup-key (kbd cmd))
    def))

(defvar evil-keypad-get-title-function 'evil-keypad-get-title
  "The function used to get the title of a keymap or command.")

(defun evil--get-event-key (e)
  (if (and (integerp (event-basic-type e))
           (member 'shift (event-modifiers e)))
      (upcase (event-basic-type e))
    (event-basic-type e)))

(defun evil--make-keymap-for-describe (keymap control)
  (let ((km (make-keymap)))
    (suppress-keymap km t)
    (when (keymapp keymap)
      (map-keymap
       (lambda (key def)
         (unless (member (event-basic-type key) '(127))
           (when (if control (member 'control (event-modifiers key))
                   (not (member 'control (event-modifiers key))))
             (define-key km (vector (evil--get-event-key key))
                         (funcall evil-keypad-get-title-function def)))))
       keymap))
    km))

(defcustom evil-keypad-leader-dispatch nil
  "The fallback dispatching in KEYPAD when there's no translation.

The value can be either a string or a keymap:
A keymap stands for a base keymap used for further translation.
A string stands for finding the keymap at a specified key binding.
Nil stands for C-c."
  :group 'evil
  :type '(choice (string :tag "Keys")
                 (variable :tag "Keymap")
                 (const nil)))

(defcustom evil-keypad-meta-prefix ?m
  "The prefix represent M- in KEYPAD state."
  :group 'evil
  :type 'character)

(defcustom evil-keypad-ctrl-meta-prefix ?M
  "The prefix represent C-M- in KEYPAD state."
  :group 'evil
  :type 'character)

(defcustom evil-keypad-literal-prefix 32
  "The prefix represent no modifier in KEYPAD state."
  :group 'evil
  :type 'character)

(defcustom evil-keypad-start-keys
  '((?c . ?c)
    (?h . ?h)
    (?x . ?x))
  "Alist of keys to begin keypad translation. When a key char is pressed,
it's corresponding value is appended to C- and the user is
prompted to finish the command."
  :group 'evil
  :type '(alist :key-type (character :tag "From")
                :value-type (character :tag "To")))

(defun evil--event-key (e)
  (let ((c (event-basic-type e)))
    (if (and (char-or-string-p c)
             (member 'shift (event-modifiers e)))
        (upcase c)
      c)))

(defun evil--keypad-get-keymap-for-describe ()
  (let* ((input (thread-first
                  (mapcar #'evil--keypad-format-key-1 evil--keypad-keys)
                  (reverse)
                  (string-join " ")))
         (meta-both-keymap (evil--keypad-lookup-key
                            (read-kbd-macro
                             (if (string-blank-p input)
                                 "ESC"
                               (concat input " ESC"))))))
    (cond
     (evil--use-meta
      (when meta-both-keymap
        (evil--make-keymap-for-describe meta-both-keymap nil)))
     (evil--use-both
      (when meta-both-keymap
        (evil--make-keymap-for-describe meta-both-keymap t)))
     (evil--use-literal
      (when-let ((keymap (evil--keypad-lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (evil--make-keymap-for-describe keymap nil))))

     ;; For leader popup
     ;; evil-keypad-leader-dispatch can be string, keymap or nil
     ;; - string, dynamically find the keymap
     ;; - keymap, just use it
     ;; Leader keymap may contain evil-dispatch commands
     ;; translated names based on the commands they refer to
     ((null evil--keypad-keys)
      (when-let ((keymap (if (stringp evil-keypad-leader-dispatch)
                             (evil--keypad-lookup-key (read-kbd-macro evil-keypad-leader-dispatch))
                           (or evil-keypad-leader-dispatch
                               mode-specific-map))))
        (let ((km (make-keymap)))
          (suppress-keymap km t)
          (map-keymap
           (lambda (key def)
             (when (and (not (member 'control (event-modifiers key)))
                        (not (member key (list evil-keypad-meta-prefix
                                               evil-keypad-ctrl-meta-prefix
                                               evil-keypad-literal-prefix)))
                        (not (alist-get key evil-keypad-start-keys)))
               (let ((keys (vector (evil--get-event-key key))))
                 (unless (lookup-key km keys)
                   (define-key km keys (funcall evil-keypad-get-title-function def))))))
           keymap)
          km)))

     (t
      (when-let ((keymap (evil--keypad-lookup-key (read-kbd-macro input))))
        (when (keymapp keymap)
          (let* ((km (make-keymap))
                 (has-sub-meta (evil--keypad-has-sub-meta-keymap-p))
                 (ignores (if has-sub-meta
                              (list evil-keypad-meta-prefix
                                    evil-keypad-ctrl-meta-prefix
                                    evil-keypad-literal-prefix
                                    127)
                            (list evil-keypad-literal-prefix 127))))
            (suppress-keymap km t)
            (map-keymap
             (lambda (key def)
               (unless (member 'control (event-modifiers key))
                 (unless (member key ignores)
                   (define-key km (vector (evil--get-event-key key)) (funcall evil-keypad-get-title-function def)))))
             keymap)
            (map-keymap
             (lambda (key def)
               (when (member 'control (event-modifiers key))
                 (unless (member (evil--event-key key) ignores)
                   (when def
                     (define-key km (vector (evil--get-event-key key)) (funcall evil-keypad-get-title-function def))))))
             keymap)
            km)))))))

(defun evil-describe-keymap (keymap)
  (when (and keymap (not defining-kbd-macro) (not evil--keypad-help))
    (let* ((rst))
      (map-keymap
       (lambda (key def)
         (let ((k (if (consp key)
                      (format "%s .. %s"
                              (key-description (list (car key)))
                              (key-description (list (cdr key))))
                    (key-description (list key)))))
           (let (key-str def-str)
             (cond
              ((and (commandp def) (symbolp def))
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (symbol-name def) 'face 'font-lock-function-name-face)))
              ((symbolp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize (concat "+" (symbol-name def)) 'face 'font-lock-keyword-face)))
              ((functionp def)
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "?closure" 'face 'font-lock-function-name-face)))
              (t
               (setq key-str (propertize k 'face 'font-lock-constant-face)
                     def-str (propertize "+prefix" 'face 'font-lock-keyword-face))))
             (push (cons key-str def-str) rst))))
       keymap)
      (setq rst (reverse rst))
      (let ((msg (evil--describe-keymap-format rst)))
        (let ((message-log-max)
              (max-mini-window-height 1.0))
          (save-window-excursion
            (with-temp-message
                (format "%s\nKEYPAD: %s%s"
                        msg
                        (let ((pre (evil--keypad-format-prefix)))
                          (if (string-blank-p pre)
                              ""
                            (propertize pre 'face 'font-lock-comment-face)))
                        (propertize (evil--keypad-format-keys nil) 'face 'font-lock-string-face))
              (sit-for 1000000 t))))))))

(defvar evil-keypad-describe-keymap-function 'evil-describe-keymap
  "The function used to describe (KEYMAP) during keypad execution.

To integrate WhichKey-like features with keypad.
Currently, keypad is not working well with which-key,
so Evil ships a default `evil-describe-keymap'.
Use (setq evil-keypad-describe-keymap-function nil) to disable popup.")

(defcustom evil-keypad-describe-delay
  0.5
  "The delay in seconds before popup keybinding descriptions appear."
  :group 'evil
  :type 'number)

(defun evil--keypad-display-message ()
  (let (overriding-local-map)
    (when evil-keypad-describe-keymap-function
      (when (or
             evil--keypad-keymap-description-activated

             (setq evil--keypad-keymap-description-activated
                   (sit-for evil-keypad-describe-delay t)))
        (let ((keymap (evil--keypad-get-keymap-for-describe)))
          (funcall evil-keypad-describe-keymap-function keymap))))))

(defun evil--transpose-lists (lists)
  (when lists
    (let* ((n (seq-max (mapcar #'length lists)))
           (rst (apply #'list (make-list n ()))))
      (mapc (lambda (l)
              (seq-map-indexed
               (lambda (it idx)
                 (cl-replace rst
                             (list (cons it (nth idx rst)))
                             :start1 idx
                             :end1 (1+ idx)))
               l))
            lists)
      (mapcar #'reverse rst))))

(defun evil--sum (sequence)
  (seq-reduce #'+ sequence 0))

(defun evil--string-pad (s len pad &optional start)
  (if (<= len (length s))
      s
    (if start
	(concat (make-string (- len (length s)) pad) s)
      (concat s (make-string (- len (length s)) pad)))))

(defun evil--string-join (sep s)
  (string-join s sep))

(defface evil-keypad-cannot-display
  '((((class color) (background dark))
     (:height 0.7 :foreground "grey90"))
    (((class color) (background light))
     (:height 0.7 :foreground "grey10")))
  "Face for Evil keypad message when cannot display popup."
  :group 'evil)

(defun evil--describe-keymap-format (pairs &optional width)
  (let* ((fw (or width (frame-width)))
         (cnt (length pairs))
         (best-col-w nil)
         (best-rows nil))
    (cl-loop for col from 5 downto 2  do
             (let* ((row (1+ (/ cnt col)))
                    (v-parts (seq-partition pairs row))
                    (rows (evil--transpose-lists v-parts))
                    (col-w (thread-last
                             v-parts
                             (mapcar
                              (lambda (col)
                                (cons (seq-max (or (mapcar (lambda (it) (length (car it))) col) '(0)))
                                      (seq-max (or (mapcar (lambda (it) (length (cdr it))) col) '(0))))))))
                    ;; col-w looks like:
                    ;; ((3 . 2) (4 . 3))
                    (w (thread-last
                         col-w
                         ;; 4 is for the width of arrow(3) between key and command
                         ;; and the end tab or newline(1)
                         (mapcar (lambda (it) (+ (car it) (cdr it) 4)))
                         (evil--sum))))
               (when (<= w fw)
                 (setq best-col-w col-w
                       best-rows rows)
                 (cl-return nil))))
    (if best-rows
        (thread-last
          best-rows
          (mapcar
           (lambda (row)
             (thread-last
               row
               (seq-map-indexed
                (lambda (it idx)
                  (let* ((key-str (car it))
                         (def-str (cdr it))
                         (l-r (nth idx best-col-w))
                         (l (car l-r))
                         (r (cdr l-r))
                         (key (evil--string-pad key-str l 32 t))
                         (def (evil--string-pad def-str r 32)))
                    (format "%s%s%s"
                            key
                            (propertize " → " 'face 'font-lock-comment-face)
                            def))))
               (evil--string-join " "))))
          (evil--string-join "\n"))
      (propertize "Frame is too narrow for KEYPAD popup" 'face 'evil-keypad-cannot-display))))

(defun evil-keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond
   (evil--use-both
    (setq evil--use-both nil))
   (evil--use-literal
    (setq evil--use-literal nil))
   (evil--use-meta
    (setq evil--use-meta nil))
   (t
    (pop evil--keypad-keys)))
  (if evil--keypad-keys
      (progn
        (evil--keypad-display-message))
    (when evil-keypad-message
      (message "KEYPAD exit"))
    (evil--keypad-quit)))

(defun evil--keypad-show-message ()
  (let ((message-log-max))
    (message "KEYPAD%s: %s%s"
             (if evil--keypad-help " describe key" "")
             (let ((pre (evil--keypad-format-prefix)))
               (if (string-blank-p pre)
                   ""
                 (propertize pre 'face 'font-lock-comment-face)))
             (propertize (evil--keypad-format-keys nil) 'face 'font-lock-string-face))))

(defun evil--keypad-try-execute ()
  "Try execute command.

If there is a command available on the current key binding,
try replacing the last modifier and try again."
  (unless (or evil--use-literal
              evil--use-meta
              evil--use-both)
    (let* ((key-str (evil--keypad-format-keys nil))
           (cmd (evil--keypad-lookup-key (read-kbd-macro key-str))))
      (cond
       ((commandp cmd t)
        (setq current-prefix-arg evil--prefix-arg
              evil--prefix-arg nil)
        (if evil--keypad-help
            (progn
              (evil--keypad-quit)
              (describe-function cmd))
          (let ((evil--keypad-this-command cmd))
            (evil--keypad-quit)
            (setq real-this-command cmd
                  this-command cmd)
            (call-interactively cmd))))
       ((keymapp cmd)
        (when evil-keypad-message (evil--keypad-show-message))
        (evil--keypad-display-message))
       ((equal 'control (caar evil--keypad-keys))
        (setcar evil--keypad-keys (cons 'literal (cdar evil--keypad-keys)))
        (evil--keypad-try-execute))
       (t
        (setq evil--prefix-arg nil)
        (message "%s is undefined" (evil--keypad-format-keys nil))
        (evil--keypad-quit))))))

(defun evil--parse-input-event (e)
  (cond
   ((equal e 32)
    "SPC")
   ((characterp e)
    (string e))
   ((equal 'tab e)
    "TAB")
   ((equal 'return e)
    "RET")
   ((equal 'backspace e)
    "DEL")
   ((equal 'escape e)
    "ESC")
   ((symbolp e)
    (format "<%s>" e))
   (t nil)))

(defun evil--parse-string-to-keypad-keys (str)
  (let ((strs (split-string str " ")))
    (thread-last
      strs
      (mapcar
       (lambda (str)
         (cond
          ((string-prefix-p "C-M-" str)
           (cons 'both (substring str 4)))
          ((string-prefix-p "C-" str)
           (cons 'control (substring str 2)))
          ((string-prefix-p "M-" str)
           (cons 'meta (substring str 2)))
          (t
           (cons 'literal str)))))
      (reverse))))

(defvar evil-keypad-state-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'evil-keypad-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'evil-keypad-self-insert)
        (setq i (1+ i)))
      (define-key map (kbd "DEL") 'evil-keypad-undo)
      (define-key map (kbd "<backspace>") 'evil-keypad-undo)
      (define-key map (kbd "<escape>") 'evil-keypad-quit)
      (define-key map [remap keyboard-quit] 'evil-keypad-quit)
      (define-key map (kbd "<deletechar>") 'evil-keypad-self-insert)
      (define-key map (kbd "<tab>") 'evil-keypad-self-insert)
      (define-key map (kbd "TAB") 'evil-keypad-self-insert)
      (define-key map (kbd "<return>") 'evil-keypad-self-insert)
      (define-key map (kbd "<up>") 'evil-keypad-self-insert)
      (define-key map (kbd "<down>") 'evil-keypad-self-insert)
      (define-key map (kbd "<left>") 'evil-keypad-self-insert)
      (define-key map (kbd "<right>") 'evil-keypad-self-insert)
      (define-key map (kbd "<home>") 'evil-keypad-self-insert)
      (define-key map (kbd "<end>") 'evil-keypad-self-insert)
      (define-key map (kbd "<next>") 'evil-keypad-self-insert)
      (define-key map (kbd "<prior>") 'evil-keypad-self-insert)
      (define-key map (kbd "<insert>") 'evil-keypad-self-insert)
      (define-key map (kbd "RET") 'evil-keypad-self-insert))
    map)
  "Keymap for Evil keypad state.")

(defun evil--get-leader-keymap ()
  (cond
   ((keymapp evil-keypad-leader-dispatch)
    evil-keypad-leader-dispatch)

   ((null evil-keypad-leader-dispatch)
    evil-keypad-state-keymap)))

(defun evil-keypad-self-insert ()
  "Default command when keypad state is enabled."
  (interactive)
  (setq this-command last-command)
  (when-let ((e (evil--event-key last-input-event))
             (key (evil--parse-input-event e)))
    (let ((has-sub-meta (evil--keypad-has-sub-meta-keymap-p)))
      (cond
       (evil--use-literal
        (push (cons 'literal key)
              evil--keypad-keys)
        (setq evil--use-literal nil))
       (evil--use-both
        (push (cons 'both key) evil--keypad-keys)
        (setq evil--use-both nil))
       (evil--use-meta
        (push (cons 'meta key) evil--keypad-keys)
        (setq evil--use-meta nil))
       ((and (equal e evil-keypad-meta-prefix)
             (not evil--use-meta)
             has-sub-meta)
        (setq evil--use-meta t))
       ((and (equal e evil-keypad-ctrl-meta-prefix)
             (not evil--use-both)
             has-sub-meta)
        (setq evil--use-both t))
       ((and (equal e evil-keypad-literal-prefix)
             (not evil--use-literal)
             evil--keypad-keys)
        (setq evil--use-literal t))
       (evil--keypad-keys
        (push (cons 'control key) evil--keypad-keys))
       ((alist-get e evil-keypad-start-keys)
        (push (cons 'control (evil--parse-input-event
                              (alist-get e evil-keypad-start-keys)))
              evil--keypad-keys))
       (evil--keypad-allow-quick-dispatch
        (if-let ((keymap (evil--get-leader-keymap)))
            (setq evil--keypad-base-keymap keymap)
          (setq evil--keypad-keys (evil--parse-string-to-keypad-keys evil-keypad-leader-dispatch)))
        (push (cons 'literal key) evil--keypad-keys))
       (t
        (push (cons 'control key) evil--keypad-keys))))

    ;; Try execute if the input is valid.
    (if (or evil--use-literal
            evil--use-meta
            evil--use-both)
        (progn
          (when evil-keypad-message (evil--keypad-show-message))
          (evil--keypad-display-message))
      (evil--keypad-try-execute))))

(defun evil-keypad ()
  "Enter keypad state."
  (interactive)
  (setq this-command last-command)
  (setq overriding-local-map evil-keypad-state-keymap
        overriding-terminal-local-map nil)
  (evil--keypad-display-message))

(defun evil-keypad-start ()
  "Enter keypad state with current input as initial key sequences."
  (interactive)
  (setq this-command last-command)
  (setq overriding-local-map evil-keypad-state-keymap
        overriding-terminal-local-map nil
        evil--keypad-allow-quick-dispatch nil)
  (call-interactively 'evil-keypad-self-insert))

(defun evil-keypad-describe-key ()
  "Describe key via KEYPAD input."
  (interactive)
  (setq this-command last-command)
  (setq overriding-local-map evil-keypad-state-keymap
        evil--keypad-help t)
  (evil--keypad-show-message)
  (evil--keypad-display-message))

(evil-define-state keypad
  "Keypad state."
  :tag " <K> "
  :message "-- Keypad state --"
  :entry-hook (evil-keypad)
  :intercept-esc nil
  (when (eq evil-state 'keypad)
    (setq evil--prefix-arg current-prefix-arg
	      evil--keypad-keymap-description-activated nil
	      evil--keypad-allow-quick-dispatch t
          evil--keypad-base-keymap nil
          evil--keypad-keys nil
          evil--use-literal nil
          evil--use-meta nil
          evil--use-both nil)))

;; Which-key integration
(defvar evil--which-key-setup nil)

(defun evil--which-key-describe-keymap ()
  (if which-key-mode
      (setq evil-keypad-describe-keymap-function
	        (lambda (keymap)
	          (which-key--create-buffer-and-show nil keymap nil (concat "Evil: " (evil--keypad-format-keys)))))
    (setq evil-keypad-describe-keymap-function 'evil-describe-keymap)))

(defun evil--setup-which-key (enable)
  (setq evil--which-key-setup enable)
  (if enable
      (progn
        (add-hook 'which-key-mode-hook 'evil--which-key-describe-keymap))
    (remove-hook 'which-key-mode-hook 'evil--which-key-describe-keymap)))

(provide 'evil-keypad)

;;; evil-keypad.el ends here
