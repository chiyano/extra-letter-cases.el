;;; extra-letter-cases.el --- Extra letter case

;; Copyright (C) 2014  Chiyano

;; Author: Chiyano <chiyanop at gmail dot com>
;; Keywords: convenience, wp

;; This file is not part of GNU Emacs.

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

;; 

;;; Code:

(defun camelize (string)
  "Camelize string."
  (save-match-data
    (let ((case-fold-search nil))
      (if (string-match "[A-Z-_]" string)
	  (let ((beginning (match-beginning 0))
		(c (match-string 0 string)))
	    (if (string-match "[-_]" c)
		(concat (substring string 0 beginning)
			(let ((a (camelize
				  (substring string (1+ beginning) (length string)))))
			  (concat (upcase (substring a 0 1))
				  (substring a 1 (length a)))))
	      (concat (substring string 0 beginning) c
		      (camelize (substring string (1+ beginning) (length string))))))
	string))))

(defun separate-string (string sep &optional no1st)
  "Make string to be separated with `sep'."
  (save-match-data
    (let ((case-fold-search nil))
      (if (string-match "[A-Z-_]" string)
	  (let ((beginning (match-beginning 0))
		(c (match-string 0 string)))
	    (if (string-match "[-_]" c)
		(concat (substring string 0 beginning)
			(let ((a (separate-string
				  (substring string (1+ beginning) (length string))
				  sep t)))
			  (concat (and (string-match "^[^_-]" a) sep)
				  (downcase (substring a 0 1))
				  (substring a 1 (length a)))))
	      (concat (substring string 0 beginning)
		      (and (or no1st (not (eq 0 beginning))) sep)
		      (downcase c)
		      (separate-string (substring string (1+ beginning) (length string)) sep t))))
	string))))

(defun snakify (string)
  "Snakify string."
  (separate-string string "_"))

(defun spinalize (string &optional no1st)
  "Spinalize string."
  (separate-string string "-"))

(defun letter-case (fun)
  (let ((start (point)))
    (forward-word)
    (let ((string (buffer-substring start (point))))
      (backward-delete-char (- (point) start))
      (insert (funcall fun string)))))

;;;###autoload
(defun camelize-word ()
  "Camelize word."
  (interactive)
  (letter-case 'camelize))

;;;###autoload
(defun snakify-word ()
  "Snakify word."
  (interactive)
  (letter-case 'snakify))

;;;###autoload
(defun spinalize-word ()
  (interactive)
  (letter-case 'spinalize))

;;;###autoload
(define-minor-mode extra-letter-cases-minor-mode
  "Toggle extra-letter-cases minor mode."
  ;; The initial value.
  :init-value t
  ;; The indicator for the mode line.
  :lighter " excases"
  ;; The minor mode bindings.
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") 'camelize-word)
    (define-key map (kbd "M-[") 'snakify-word)
    (define-key map (kbd "M-{") 'spinalize-word)
    map))

(provide 'extra-letter-cases)
;;; extra-letter-cases.el ends here
