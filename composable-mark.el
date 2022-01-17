;;; composable.mark.el --- composable editing -*- lexical-binding: t; -*-

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

;; Marking commands useful with composable.


;;; Code:

(defvar composable--border-point)
(defvar composable--count)

(defun composable--direction (arg)
  "Direction of ARG."
  (let ((n (prefix-numeric-value arg))) (/ n (abs n))))

(defun composable-mark-join (arg)
  "Mark the whitespace separating lines.
Between the line above if ARG is negative otherwise below."
  (interactive "p")
  (forward-line arg)
  (cl-flet ((move (dir)
		  (if (< 0 dir)
                      (skip-chars-forward "[:space:]\n")
                    (skip-chars-backward "[:space:]\n"))))
    (when (< arg 0) (end-of-line))
    (move arg)
    (push-mark nil nil t)
    (move (- arg))))

(defmacro composable--mark-with-forward (forward arg)
  "Mark a region based on a FORWARD movement and ARG.
The movement must mark backwards with negative arguments."
  `(let* ((amount (prefix-numeric-value ,arg))
          (dir (/ amount (abs amount))))
     (when (= composable--count 1)
       (funcall ,forward dir)
       (funcall ,forward (- dir))
       (setq composable--border-point (point-marker))
       (set-mark (point))
       (if (< 0 amount)
	   (goto-char (min (mark t) (point)))
	 (goto-char (max (mark t) (point)))))
     (funcall ,forward amount)))

(defun composable-mark-line (arg)
  "Mark ARG lines.
Supports negative argument and repeating."
  (interactive "P")
  (composable--mark-with-forward #'forward-line arg))

(defun composable-mark-word (arg)
  "Mark ARG words.
Supports negative arguments and repeating."
  (interactive "P")
  (composable--mark-with-forward #'forward-word arg))

(defun composable-mark-symbol (arg)
  "Mark ARG symbols.
Supports negative arguments and repeating."
  (interactive "P")
  (composable--mark-with-forward #'forward-symbol arg))

(defun composable-mark-paragraph (arg)
  "Mark ARG symbols.
Supports negative arguments and repeating."
  (interactive "P")
  (composable--mark-with-forward #'forward-paragraph arg))

(defun composable--up-list (arg)
  "Up-list ARG times with better quotes support."
  (let ((syntax-ppss (syntax-ppss)))
    (if (nth 3 syntax-ppss)
	(goto-char (nth 8 syntax-ppss))
      (up-list arg))))

(defun composable-mark-up-list (arg)
  "Mark ARG upper lists.
Supports negative arguments and repeating."
  (interactive "P")
  (composable--mark-up #'forward-sexp #'composable--up-list arg))

(defun composable--mark-up (forward up arg)
  "Mark a region based on a FORWARD and UP movement and ARG.
The movement must mark backwards with negative arguments."
  (let* ((amount (if arg
                     (prefix-numeric-value arg)
                   (if (< (mark t) (point)) -1 1))))
    (push-mark
     (progn
       (when (region-active-p)
         (goto-char (mark t)))
       (funcall up (- amount))
       (point))
     nil t)
    (funcall forward amount)))

(provide 'composable-mark)

;;; composable-mark.el ends here
