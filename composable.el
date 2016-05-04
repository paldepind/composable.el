;;; composable.el --- composable editing -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Simon Friis Vindum

;; Author: Simon Friis Vindum <simon@vindum.io>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))

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

(defcustom composable-repeat t
  "Repeat the last excuted action by repressing the last key."
  :type 'boolean)

(defcustom composable-object-cursor 'composable-half-cursor
  "Use a custom face for the cursor when in object mode.
This can be either a function or any value accepted by
`cursor-type'.")

(defcustom composable-twice-mark 'composable-mark-line
  "Thing to mark when a composable command is called twice successively.")

(defface easy-kill-selection '((t (:inherit secondary-selection)))
  "Faced used to highlight kill candidate.")

(defvar composable--command nil)
(defvar composable--skip-first)
(defvar composable--prefix-arg nil)
(defvar composable--start-point)
(defvar composable--fn-pairs (make-hash-table :test 'equal))
(defvar composable--command-prefix nil)
(defvar composable--saved-cursor nil)

(defun composable-create-composable (command)
  "Take a function and return it in a composable wrapper.
The returned function will ask for an object, mark the region it
specifies and call COMMAND on the region."
  (lambda (arg)
    (interactive "P")
    (cond (mark-active
           (call-interactively command))
          (composable-object-mode
           (setq this-command composable-twice-mark)
           (funcall composable-twice-mark arg))
          (t
           (setq composable--command-prefix arg)
           (setq composable--command command)
           (composable-object-mode)))))

(defun composable-def (commands)
  "Define composable function from a list COMMANDS.
The list should contain functions operating on regions.
For each function named foo a function name composable-foo is created."
  (dolist (c commands)
    (fset (intern (concat "composable-" (symbol-name c)))
          (composable-create-composable c))))

(composable-def
 '(kill-region kill-ring-save indent-region comment-or-uncomment-region
   smart-comment-region upcase-region downcase-region))

(defun composable-half-cursor ()
  "Change cursor to a half-height box."
  (let ((height (/ (window-pixel-height) (* (window-height) 2))))
    (setq cursor-type (cons 'hbar height))))

(defun composable--set-cursor (c)
  "Set cursor to C"
  (when c (setq cursor-type (if (functionp c) (funcall c) c))))

(defun composable--singleton-map (key def)
  "Create a map with a single KEY with definition DEF."
  (let ((map (make-sparse-keymap)))
    (define-key map key def)
    map))

(defun composable--call-excursion (command point-mark)
  "Call COMMAND if set then go to POINT-MARK marker."
  (when (commandp command)
    (let ((current-prefix-arg composable--command-prefix))
      (call-interactively command))
    (goto-char (marker-position point-mark))))

(defun composable--repeater (point-marker command object direction)
  "Preserve point at POINT-MARKER when doing COMMAND on OBJECT in DIRECTION."
  (lambda ()
    (interactive)
    (goto-char (marker-position point-marker))
    ;; Activate mark, some mark functions expands region when mark is active
    (set-mark (mark))
    (let ((current-prefix-arg direction))
      (call-interactively object))
    (set-marker point-marker (point))
    (composable--call-excursion command composable--start-point)))

(defun composable--direction (arg)
  "Direction of ARG."
  (let ((n (prefix-numeric-value arg)))
    (if n (/ n (abs n)) 1)))

(defun composable--contain-marking ()
  "Remove marking before or after point based on prefix argument."
  (let ((fn (if (eq composable--prefix-arg 'composable-begin) 'min 'max))
        (pos (marker-position composable--start-point)))
    (set-mark (funcall fn (mark) pos))
    (goto-char (funcall fn (point) pos))))

(defvar composable--arguments
  '(universal-argument digit-argument negative-argument
   composable-begin-argument composable-end-argument))

(defun composable--activate-repeat (object point-marker)
  "Activate repeat map on OBJECT preserving point at POINT-MARKER."
  (interactive)
  (set-transient-map
   (composable--singleton-map
    (vector last-command-event)
    (composable--repeater point-marker composable--command object (composable--direction last-prefix-arg)))
   t
   (lambda ()
     (set-marker point-marker nil)
     (set-marker composable--start-point nil))))

(defun composable--handle-prefix (command pairs)
  "Handle prefix arg where the COMMAND is paired in PAIRS."
  (let ((pair (gethash command pairs)))
    (cond (pair (set-mark (point)) (call-interactively pair))
          (mark-active (composable--contain-marking)))))

(defun composable--post-command-hook-handler ()
  "Called after each command when composable-object-mode is on."
  (cond
   (composable--skip-first
    (setq composable--skip-first nil))
   ((and (not (member this-command composable--arguments)) ;; detect prefix < 25.1
         (not (eq last-command this-command))) ;; in 25.1 prefix args don't change `this-command'
    (when composable--prefix-arg (composable--handle-prefix this-command composable--fn-pairs))
    (when composable-repeat (composable--activate-repeat this-command (point-marker)))
    (composable--call-excursion composable--command composable--start-point)
    (composable-object-mode -1))))

(defun composable-add-pair (fn1 fn2)
  "Take two commands FN1 and FN2 and add them as pairs."
  (puthash fn2 fn1 composable--fn-pairs)
  (puthash fn1 fn2 composable--fn-pairs))

(defun composable-add-pairs (pairs)
  "Add a list of PAIRS."
  (dolist (p pairs)
    (composable-add-pair (car p) (cadr p))))

(composable-add-pairs
 '((forward-word backward-word)
   (move-end-of-line back-to-indentation)
   (next-line previous-line)
   (forward-paragraph backward-paragraph)
   (forward-sentence backward-sentence)))

(defun composable-begin-argument ()
  "Set prefix argument to end."
  (interactive)
  (setq composable--prefix-arg 'composable-begin))

(defun composable-end-argument ()
  "Set prefix argument to end."
  (interactive)
  (setq composable--prefix-arg 'composable-end))

(defface composable-highlight '((t (:inherit secondary-selection)))
  "Faced used to highlight the saved region.")

(defvar composable--overlay nil)

(defun composable--delete-highlight ()
  "Delete overlay."
  (delete-overlay composable--overlay)
  (remove-hook 'pre-command-hook 'composable--delete-highlight))

(fset 'composable-save-region
  (composable-create-composable
   (lambda (beg end)
     (interactive "r")
     (let ((o (make-overlay beg end)))
       (copy-region-as-kill beg end)
       (setq composable--overlay o)
       (overlay-put o 'priority 999)
       (overlay-put o 'face 'composable-highlight)
       (add-hook 'pre-command-hook 'composable--delete-highlight)))))

(define-minor-mode composable-object-mode
  "Composable mode."
  :lighter "Object "
  :keymap
  '(((kbd "1") . digit-argument)
    ((kbd "2") . digit-argument)
    ((kbd "3") . digit-argument)
    ((kbd "4") . digit-argument)
    ((kbd "5") . digit-argument)
    ((kbd "6") . digit-argument)
    ((kbd "7") . digit-argument)
    ((kbd "8") . digit-argument)
    ((kbd "9") . digit-argument)
    ((kbd "-") . negative-argument)
    ((kbd ".") . composable-end-argument)
    ((kbd ",") . composable-begin-argument)
    ((kbd "a") . move-beginning-of-line)
    ((kbd "e") . move-end-of-line)
    ((kbd "f") . forward-word)
    ((kbd "b") . backward-word)
    ((kbd "u") . mark-whole-buffer)
    ((kbd "n") . next-line)
    ((kbd "p") . previous-line)
    ((kbd "l") . composable-mark-line)
    ((kbd "{") . backward-paragraph)
    ((kbd "}") . forward-paragraph)
    ((kbd "s") . mark-sexp)
    ((kbd "w") . composable-mark-word)
    ((kbd "y") . composable-mark-symbol)
    ((kbd "h") . mark-paragraph)
    ((kbd "m") . back-to-indentation)
    ((kbd "j") . composable-mark-join)
    ((kbd "g") . composable-object-mode)
    ((kbd "C-g") . composable-object-mode))
  (if composable-object-mode
      (progn
        (setq composable--saved-cursor cursor-type)
        (composable--set-cursor composable-object-cursor)
        (if (not mark-active) (push-mark nil t))
        (setq composable--start-point (point-marker))
        (setq composable--skip-first t)
        (add-hook 'post-command-hook 'composable--post-command-hook-handler))
    (setq cursor-type composable--saved-cursor)
    (remove-hook 'post-command-hook 'composable--post-command-hook-handler)
    (setq composable--prefix-arg nil)
    (setq composable--command nil)))

;;;###autoload
(define-minor-mode composable-mode
  "Toggle Composable mode."
  :lighter " Composable"
  :global 1
  :keymap
  `((,(kbd "C-w") . composable-kill-region)
    (,(kbd "M-w") . composable-save-region)
    (,(kbd "M-;") . composable-comment-or-uncomment-region)
    (,(kbd "C-x C-u") . composable-upcase-region)
    (,(kbd "C-x C-l") . composable-downcase-region)
    (,(kbd "C-M-\\") . composable-indent-region)))

(defun composable--deactivate-mark-hook-handler ()
  "Leave object mode when the mark is disabled."
  (when composable-object-mode (composable-object-mode -1)))

(defun composable--set-mark-command-advice (arg)
  "Advice for `set-mark-command'.
Activates composable-object-mode unless ARG is non-nil."
  (unless (or composable-object-mode arg)
    (composable-object-mode)))

;;;###autoload
(define-minor-mode composable-mark-mode
  "Toggle composable mark mode."
  :global 1
  (if composable-mark-mode
      (progn
        (add-hook 'deactivate-mark-hook 'composable--deactivate-mark-hook-handler)
        (advice-add 'set-mark-command :before 'composable--set-mark-command-advice))
    (remove-hook 'deactivate-mark-hook 'composable--deactivate-mark-hook-handler)
    (advice-remove 'set-mark-command 'composable--set-mark-command-advice)))

(provide 'composable)

;;; composable.el ends here
