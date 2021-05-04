;;; composable.el --- composable editing -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Simon Friis Vindum

;; Author: Simon Friis Vindum <simon@vindum.io>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Composable editing for Emacs

;; composable.el is composable text editing for Emacs.  It improves the
;; basic editing power of Emacs by making commands combineable.

;; It's inspired by vim but implemented in a way that reuses existing
;; Emacs infrastructure.  This makes it simple and compatible with
;; existing Emacs functionality and concepts.  composable.el only brings
;; together existing features in a slightly different way.

;; Composable editing is a simple abstraction that makes it possible to
;; combine _actions_ with _objects_.  The key insight in composable.el is
;; that Emacs already provides all the primitives to implement composable
;; editing.  An action is an Emacs command that operates on the region.
;; Thus `kill-region` and `comment-region` are actions.  An object is
;; specified by a command that moves point and optionally sets the mark
;; as well.  Examples are `move-end-of-line` and `mark-paragraph`.

;; So actions and objects are just names for things already present in
;; Emacs.  The primary feature that composable.el introduces is a
;; _composable command_.  A composable command has an associated action.
;; Invoking it works like this:

;; 1. If the region is active the associated action is invoked directly.
;; 2. Otherwise nothing happens, but the editor is now listening for an
;;    object.  This activates a set of bindings that makes it convenient
;;    to input objects.  For instance pressing `l` makes the action
;;    operate on the current line.
;; 3. After the object has been entered the action is invoked on the
;;    specified object.


;;; Code:

(require 'composable-mark)

;;* Customization
(defgroup composable nil
  "Composable editing."
  :prefix "composable-"
  :group 'tools)

(defcustom composable-which-keys t
  "Show bindings available when entering composable if which-key is installed."
  :type 'boolean)

(defcustom composable-repeat t
  "Repeat the last excuted action by repressing the last key."
  :type 'boolean)

(defcustom composable-repeat-copy-save-last t
  "Keep only the last copied text in the `kill-ring'."
  :type 'boolean)

(defcustom composable-object-cursor (and (display-graphic-p)
                                         #'composable-half-cursor)
  "Use a custom face for the cursor when in object mode.
This can be either a function or any value accepted by
`cursor-type'."
  :type 'function)

(defcustom composable-twice-mark #'composable-mark-line
  "Thing to mark when a composable command is called twice successively."
  :type 'function)

(defcustom composable-mode-line-color "cyan"
  "Color for mode-line background when composable is active."
  :type 'color)

(defcustom composable-mode-debug-level 1
  "Print verbose information when composable modes toggle."
  :type 'integer)

(defface composable-highlight
  '((t (:inherit secondary-selection :extend nil)))
  "Faced used to highlight the saved region.")

(defvar composable--overlay nil)
(defvar composable--saved-mode-line-color nil)
(defvar composable--command nil)
(defvar composable--count 0)                 ;; Count the repeated times
(defvar composable--prefix-arg nil)
(defvar composable--start-marker (make-marker))
(defvar composable--command-prefix nil)
(defvar composable--saved-cursor nil)
(defvar composable--expand nil)
(defvar composable--which-key-timer nil)
(defvar composable--char-input nil)

(defcustom composable-fn-pair-alist
  '((forward-word . backward-word)
    (move-end-of-line . back-to-indentation)
    (next-line . previous-line)
    (forward-paragraph . backward-paragraph)
    (forward-sentence . backward-sentence))
  "Alist with pairs of functions."
  :type '(alist :key-type symbol :value-type symbol))

(defsubst composable-mode-debug-message (format-string &rest args)
  "Print messages only when `composable-mode-debug' is `non-nil'.

The arguments FORMAT-STRING and ARGS are the same than in the
`message' function."
  (if (> composable-mode-debug-level 0)
      (let ((inhibit-message (< composable-mode-debug-level 2)))
        (apply #'message format-string args))))

(defun composable-create-composable (command)
  "Take a function and return it in a composable wrapper.
The returned function will ask for an object, mark the region it
specifies and call COMMAND on the region."
  `(defun ,(intern (concat "composable-" (symbol-name `,command))) (arg)
     ,(format "Composable wrapper for `%s'" (symbol-name command))
     (interactive "P")
     (cond ((or (region-active-p) ;; With region
		(bound-and-true-p multiple-cursors-mode))
	    (setq composable--count 0)
	    (call-interactively #',command))
           (composable-object-mode ;; Repeated
	    (setq this-command composable-twice-mark)
	    (funcall composable-twice-mark arg))
           (t                      ;; First call no region
	    (setq composable--command-prefix arg
                  composable--command #',command)
	    (composable-object-mode)))))

(defmacro composable-def (commands-list)
  "Define composable function from a list COMMANDS.
The list should contain functions operating on regions.
For each function named foo a function name composable-foo is created."
  `(progn ,@(mapcar #'composable-create-composable commands-list)
	  (easy-mmode-defmap composable-mode-map
	    '(,@(mapcar (lambda (command)
			  `([remap ,command] . ,(intern (concat "composable-" (symbol-name command)))))
			commands-list)
	      ([remap kill-line] . composable-delete-region))
	    "Keymap for composable-mode commands after entering.")))

(composable-def (kill-region
		 kill-ring-save
		 indent-region
		 comment-dwim
		 upcase-region
		 downcase-region
		 delete-region))

(defun composable-half-cursor ()
  "Change cursor to a half-height box."
  (setq cursor-type
	(cons 'hbar (/ (window-pixel-height) (* (window-height) 2)))))

(defun composable--call-excursion (command)
  "Call COMMAND if set then go to POINT-MARK marker."
  (when (commandp command)
    (let ((current-prefix-arg composable--command-prefix))
      (call-interactively command)
      (if (= composable--count 1)
          (push-mark (point) t)
        (set-mark (point)))
      (goto-char composable--start-marker))))

(defun composable--repeater (command object direction)
  "Preserve point at POINT-MARKER when doing COMMAND.
Executes on OBJECT in LAST-PREFIX direction."
  (lambda ()
    (interactive)
    (unless composable--expand
      (goto-char (mark t)))
    (activate-mark)
    (setq composable--count (1+ composable--count))
    (let ((current-prefix-arg direction))
      (call-interactively object))
    (composable--call-excursion command)))

(defconst composable--arguments
  '(universal-argument
    digit-argument
    negative-argument
    composable-begin-argument
    composable-end-argument))

(defun composable--object-exit ()
  "Actions to perform every time composable exits."

  (composable-mode-debug-message "Exit composable-object-mode")
  (set-marker composable--start-marker nil)

  (when composable--saved-cursor
   (setq cursor-type composable--saved-cursor))
  (when composable--saved-mode-line-color
    (set-face-attribute 'mode-line nil :background composable--saved-mode-line-color))
  (when composable--overlay
    (delete-overlay composable--overlay))

  (advice-remove 'keyboard-quit #'composable-object-mode-disable)
  (setq composable--expand nil))  ;; By default the commands don't expand

(defun composable--singleton-map (key def)
  "Create a map with a single KEY with definition DEF."
  (let ((map (make-sparse-keymap)))
    (define-key map key def)
    ;; When using composable char can repeat the char to repeat the
    ;; command
    (if (characterp composable--char-input)
        (define-key map (string composable--char-input) def))
    map))

(defun composable--activate-repeat (object)
  "Activate repeat map on OBJECT preserving point at POINT-MARKER."
  (interactive)
  (set-transient-map
   (composable--singleton-map
    (vector last-command-event)
    (composable--repeater composable--command object
                          (composable--direction last-prefix-arg)))
   t
   #'composable--object-exit))

(defun composable--handle-prefix (command)
  "Handle prefix arg where the COMMAND is paired in PAIRS."
  (let ((pair (or (alist-get command composable-fn-pair-alist)
                  (car (rassq command composable-fn-pair-alist)))))
    (cond (pair
           (push-mark)
           (call-interactively pair))
          (mark-active
           (if (eq composable--prefix-arg 'composable-begin)
               (progn
                 (set-mark (min (mark t) composable--start-marker))
                 (goto-char (min (point) composable--start-marker)))
             (set-mark (max (mark t) composable--start-marker))
             (goto-char (max (point) composable--start-marker)))))))

(defun composable--post-command-hook-handler ()
  "Called after each command when composable-object-mode is on."
  (cond
   ((= composable--count 0)
    (setq composable--count 1))
   ((and (not (member this-command composable--arguments)) ;; detect prefix < 25.1
         (not (eq last-command this-command))) ;; in 25.1 prefix args don't change `this-command'
    (when composable--prefix-arg
      (composable--handle-prefix this-command))
    (when composable-repeat
      (composable--activate-repeat this-command))
    (composable--call-excursion composable--command)
    (composable-object-mode -1))))

(defun composable-begin-argument ()
  "Set prefix argument to end."
  (interactive)
  (setq composable--prefix-arg 'composable-begin))

(defun composable-end-argument ()
  "Set prefix argument to end."
  (interactive)
  (setq composable--prefix-arg 'composable-end))

(defun composable-goto-char (arg)
  "Goto-char command for composable."
  (interactive "p")
  (unless composable--char-input
    (setq composable--char-input (read-char "char: " t)))
  (search-forward (char-to-string composable--char-input) nil nil arg))

(defun copy-region-as-kill-advise (beg end &optional _)
  "Extra advise for copy-region-as-kill to enable the overlay.

This also prevents messing the clipboard."
  (interactive (list (mark) (point)))
  (when (and (marker-position composable--start-marker)
	     (> composable--count 0))
    (move-overlay composable--overlay
                  (min beg composable--start-marker end)
                  (max beg composable--start-marker end))

    (when (and (> composable--count 1)
               composable-repeat-copy-save-last)
      (setq last-command #'kill-region))))

(easy-mmode-defmap composable-object-mode-map
  `(,@(mapcar (lambda (num)
		(cons (format "%s" num) 'digit-argument))
	      (number-sequence 0 9))
    ("-" . negative-argument)
    ("," . composable-begin-argument)
    ("." . composable-end-argument)
    ("a" . move-beginning-of-line)
    ("c" . composable-goto-char)
    ("e" . move-end-of-line)
    ("f" . forward-word)
    ("b" . backward-word)
    ("u" . mark-whole-buffer)
    ("n" . next-line)
    ("p" . previous-line)
    ("l" . composable-mark-line)
    ("{" . backward-paragraph)
    ("}" . forward-paragraph)
    ("s" . mark-sexp)
    ("m" . back-to-indentation)
    ("w" . composable-mark-word)
    ("y" . composable-mark-symbol)
    ("h" . composable-mark-paragraph)
    ("j" . composable-mark-join)
    ("o" . composable-mark-up-list)
    ("g" . keyboard-quit))
  "Keymap for composable-object-mode commands after entering.")

(define-minor-mode composable-object-mode
  "Composable mode."
  :lighter (if (> composable-mode-debug-level 1)
               " Composable object" "")
  :keymap composable-object-mode-map
  (if composable-object-mode
      (progn
	(when (and composable-mode-line-color  ;; Mode-line
		   (color-supported-p composable-mode-line-color))
	  (setq composable--saved-mode-line-color (face-attribute 'mode-line :background))
	  (set-face-attribute 'mode-line nil :background composable-mode-line-color))

	(when composable-object-cursor       ;; "Change cursor cursor to C"
	  (setq composable--saved-cursor cursor-type
		cursor-type (or (and (functionp composable-object-cursor)
				     (funcall composable-object-cursor))
				composable-object-cursor)))

	(setq composable--start-marker (point-marker)
              composable--count 0
              composable--char-input nil)

	(push-mark nil t)

	;; which-key
	(when (and composable-which-keys
		   (bound-and-true-p which-key-mode))
	  (setq composable--which-key-timer
		(run-with-idle-timer which-key-idle-delay nil
				     #'which-key-show-keymap 'composable-object-mode-map t)))

	(add-hook 'post-command-hook #'composable--post-command-hook-handler)
	(advice-add 'keyboard-quit :before #'composable-object-mode-disable)
	(composable-mode-debug-message "Start composable-object-mode (command: %s)" this-command))

    ;; else
    (remove-hook 'post-command-hook #'composable--post-command-hook-handler)
    (setq composable--prefix-arg nil
          composable--command nil)

    (when (bound-and-true-p which-key-mode)
      (cancel-timer composable--which-key-timer))

    (when (or (called-interactively-p 'any)
              (not composable-repeat))
      (composable--object-exit)
      (deactivate-mark))))

(defun composable-object-mode-disable ()
  (interactive)
  (when composable-object-mode ;; This check is extremely important
    (funcall-interactively #'composable-object-mode -1)))

;;;###autoload
(define-minor-mode composable-mode
  "Toggle Composable mode."
  :lighter " Composable mode"
  :global 1
  :keymap composable-mode-map
  (if composable-mode
      (progn
        (setq composable--overlay (make-overlay 0 0))

        (overlay-put composable--overlay 'priority 999)
        (overlay-put composable--overlay 'face 'composable-highlight)
        ;; associate the overlay with no specific buffer. Otherwise it
        ;; may be not visible when set for the first time and appear
        ;; visible when not expected.
        (delete-overlay composable--overlay)
	(advice-add 'copy-region-as-kill :before #'copy-region-as-kill-advise))
    (setq composable--overlay nil)
    (advice-remove 'copy-region-as-kill #'copy-region-as-kill-advise)))

(defun composable--deactivate-mark-hook-handler ()
  "Leave object mode when the mark is disabled."
  (composable-object-mode-disable))

(defun composable--set-mark-command-advice (arg)
  "Advice for `set-mark-command'.
Activates composable-object-mode unless ARG is non-nil."
  (unless (or composable-object-mode
              arg
              (bound-and-true-p multiple-cursors-mode))
    (setq composable--expand t)
    (composable-object-mode 1)))

;;;###autoload
(define-minor-mode composable-mark-mode
  "Toggle composable mark mode."
  :global 1
  (if composable-mark-mode
      (progn
        (add-hook 'deactivate-mark-hook #'composable--deactivate-mark-hook-handler)
        (advice-add 'set-mark-command :before #'composable--set-mark-command-advice))
    (remove-hook 'deactivate-mark-hook #'composable--deactivate-mark-hook-handler)
    (advice-remove 'set-mark-command #'composable--set-mark-command-advice)))

(provide 'composable)

;;; composable.el ends here
