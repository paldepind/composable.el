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

(setq binders (make-hash-table :test 'equal))

(defun add-motions (&rest bs)
  (dolist (b bs)
    (puthash (kbd (car b)) (cadr b) binders)))

(defun composable--def-arg (arg) (if (zerop arg) 1 arg))

(defun composable--is-string-num-p (s)
  (string-match-p "^[0-9]+" s))

(defun composable--read-keys (command arg)
  (let ((key (read-key-sequence nil)))
    (if (composable--is-string-num-p key)
        (let* ((num (string-to-int key))
               (new-arg (+ (* arg 10) num)))
          (composable--read-keys command new-arg))
      (save-excursion
        (push-mark nil nil t)
        (let ((current-prefix-arg (composable--def-arg arg)))
          (call-interactively (gethash key binders)))
        (call-interactively command)))))

(defun create-action (command)
  (lambda ()
    (interactive)
    (if mark-active
        (call-interactively command)
      (composable--read-keys command 0))))

(defun add-actions (&rest as)
  (dolist (a as)
    (global-set-key (kbd (car a)) (create-action (cadr a)))))

(defun composable-mark-line (arg)
  "Marks ARG lines."
  (interactive "p")
  (beginning-of-line)
  (push-mark nil nil t)
  (dotimes (i arg) (forward-line)))

(add-motions
 '("e" move-end-of-line)
 '("a" move-beginning-of-line)
 '("'" avy-goto-char-in-line)
 '("f" forward-word)
 '("b" backward-word)
 '("n" next-line)
 '("p" previous-line)
 '("SPC" composable-mark-line))

(add-actions
 '("C-w" kill-region)
 '("M-w" kill-ring-save)
 '("M-;" smart-comment-region))

(provide 'composable)
(provide 'composable.el)

;;; composable.el ends here
