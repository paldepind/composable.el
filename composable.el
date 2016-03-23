;;; composable.el --- composable editing -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Simon Friis Vindum

;; Author: Simon Friis Vindum <simon@vindum.io>
;; Keywords: lisp
;; Version: 0.0.1

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


;;; Code:

(defvar composable--binders (make-hash-table :test 'equal))

(defun composable-add-motions (&rest bs)
  "Add motions.
BS must be pairs of a keybinding and a function."
  (dolist (b bs)
    (puthash (kbd (car b)) (cadr b) composable--binders)))

(defun composable--def-arg (arg)
  "Return 1 if ARG is 0 otherwise ARG."
  (if (zerop arg) 1 arg))

(defun composable--is-string-num-p (s)
  "Return t if S begins with a number."
  (string-match-p "^[0-9]+" s))

(defun composable--execute-action (command arg key)
  "Execute the action COMMAND with ARG in the region KEY."
  (save-excursion
    (push-mark nil nil t)
    (let ((current-prefix-arg (composable--def-arg arg)))
      (call-interactively (gethash key composable--binders)))
    (call-interactively command)))

(defun composable--read-keys (command arg)
  "Read keys and then incoke COMMAND with ARG."
  (let ((key (read-key-sequence nil)))
    (if (composable--is-string-num-p key)
        (composable--read-keys command (+ (* arg 10) (string-to-number key)))
      (composable--execute-action command arg key))))

(defun composable-create-composable (command)
  "Take a function and return it in a composable wrapper.
The returned function will ask for a motion, mark the region it
specifies and call COMMAND on the region."
  (lambda ()
    (interactive)
    (if mark-active
        (call-interactively command)
      (composable--read-keys command 0))))

(defun composable-def (commands)
  "Define composable function from a list COMMANDS.
The list should contain functions operating on regions.
For each function named foo a function name composable-foo is created."
  (dolist (c commands)
    (fset (intern (concat "composable-" (symbol-name c)))
          (composable-create-composable c))))

(defun composable-mark-line (arg)
  "Mark ARG lines."
  (interactive "p")
  (beginning-of-line)
  (push-mark nil nil t)
  (dotimes (_ arg) (forward-line)))

(composable-def
 '(kill-region kill-ring-save smart-comment-region))

(composable-add-motions
 '("e" move-end-of-line)
 '("a" move-beginning-of-line)
 '("'" avy-goto-char-in-line)
 '("f" forward-word)
 '("b" backward-word)
 '("n" next-line)
 '("p" previous-line)
 '("SPC" composable-mark-line)
 '("{" backward-paragraph)
 '("}" forward-paragraph))

(global-set-key (kbd "C-w") 'composable-kill-region)
(global-set-key (kbd "M-w") 'composable-kill-ring-save)
(global-set-key (kbd "M-;") 'composable-smart-comment-region)

(provide 'composable)

;;; composable.el ends here
