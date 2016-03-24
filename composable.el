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

(defvar composable--command)
(defvar composable--skip-first)

(defun composable-create-composable (command)
  "Take a function and return it in a composable wrapper.
The returned function will ask for a motion, mark the region it
specifies and call COMMAND on the region."
  (lambda ()
    (interactive)
    (setq composable--command command)
    (composable-range-mode)))

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

(defun composable--post-command-hook-handler ()
  "Called after each command when composable-rangemode is on."
  (cond (composable--skip-first
         (push-mark nil nil t)
         (setq composable--skip-first nil))
        ((/= (point) (mark))
         (call-interactively composable--command)
         (goto-char (mark))
         (composable-range-mode -1)
         (remove-hook 'post-command-hook 'composable--mode-hook))))


(global-set-key (kbd "C-w") 'composable-kill-region)
(global-set-key (kbd "M-w") 'composable-kill-ring-save)
(global-set-key (kbd "M-;") 'composable-smart-comment-region)

(define-minor-mode composable-range-mode
  "Composable mode."
  :lighter "Range "
  :keymap
  '(((kbd "e") . move-end-of-line)
    ((kbd "1") . digit-argument)
    ((kbd "2") . digit-argument)
    ((kbd "3") . digit-argument)
    ((kbd "4") . digit-argument)
    ((kbd "5") . digit-argument)
    ((kbd "6") . digit-argument)
    ((kbd "7") . digit-argument)
    ((kbd "8") . digit-argument)
    ((kbd "9") . digit-argument)
    ((kbd "0") . digit-argument)
    ((kbd "a") . move-beginning-of-line)
    ((kbd "'") . avy-goto-char-in-line)
    ((kbd "f") . forward-word)
    ((kbd "b") . backward-word)
    ((kbd "n") . next-line)
    ((kbd "p") . previous-line)
    ((kbd "l") . composable-mark-line)
    ((kbd "{") . backward-paragraph)
    ((kbd "}") . forward-paragraph))
  :after-hook
  (when (not mark-active)
    (setq composable--skip-first t))
  (add-hook 'post-command-hook 'composable--post-command-hook-handler))

(provide 'composable)

;;; composable.el ends here
