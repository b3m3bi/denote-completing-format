;;; denote-completing-format.el --- Format Denote's file completing-read table -*- lexical-binding: t -*-

;; Copyright (C) 2023  Luis García

;; Author: Luis García <sharlsberg37@gmail.com>
;; URL: https://github.com/emacs-citar/citar.git
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; denote-completing-format offers
;; `denote-completing-format-open-or-create' comand, an alternative
;; to `denote-open-or-create' command, that aligns and fontifies
;; the file completion candidates to facilitate search and selection.

;;; Code:

(require 'denote)

(defgroup denote-completing-format ()
  "Format completion candidates for searching denote files."
  :group 'denote
  :prefix "denote-completing-format-")

(defcustom denote-completing-format-subdir-width 30
  "Width of subdir container in formatted completing table."
  :group 'denote-completing-format
  :type 'number)

(defcustom denote-completing-format-id-width 15
  "Width of id container in formatted completing table."
  :group 'denote-completing-format
  :type 'number)

(defcustom denote-completing-format-title-width 80
  "Width of title container in formatted completing table."
  :group 'denote-completing-format
  :type 'number)

(defcustom denote-completing-format-extension-width 5
  "Width of file extension container in formatted completing table."
  :group 'denote-completing-format
  :type 'number)

(defcustom denote-completing-format-keywords-width 40
  "Width of keywords container in formatted completing table."
  :group 'denote-completing-format
  :type 'number)

(defface denote-completing-format-faces-subdirectory '((t :inherit denote-faces-extension))
  "Face for subdirectory of file name."
  :group 'denote-completing-format)

(defface denote-completing-format-faces-date '((t :inherit denote-faces-date))
  "Face for date of file name."
  :group 'denote-completing-format)

(defface denote-completing-format-faces-time '((t :inherit denote-completing-format-faces-date))
  "Face for time of file name."
  :group 'denote-completing-format)

(defface denote-completing-format-faces-title '((t :inherit denote-faces-title))
  "Faces for file name."
  :group 'denote-completing-format)

(defface denote-completing-format-faces-extension '((t :inherit denote-faces-extension))
  "Face for file extension."
  :group 'denote-completing-format)

(defface denote-completing-format-faces-keywords '((t :inherit denote-faces-keywords))
  "Face for keywords."
  :group 'denote-completing-format)

(defface denote-completing-format-faces-delimiter '((t :inherit denote-faces-delimiter))
  "Face for delimimiters."
  :group 'denote-completing-format)

(defcustom denote-completing-format-deslugiffy-title nil
  "If non-nil deslugiffy file title."
  :group 'denote-completing-format
  :type 'boolean)

(defcustom denote-completing-format-create-function #'denote
  "Funcion used for reating notes."
  :group 'denote-completing-format
  :type '(choice (const denote)
		 (const denote-subdirectory)
		 (const denote-type)
		 (const denote-date)))

;; helper function to obtain subdirs from a path
(defun denote-completing-format-extract-subdir-from-path (path)
  "Return subdirs below of `denote-directory' of PATH."
  (let* ((subdir-regexp (concat (denote-directory) "\\(.*\\)" denote-id-regexp)))
    (when (string-match subdir-regexp path)
      ;; add "/" to facilitate filtering by subdir on search
      (concat "/" (match-string 1 path)))))

;; Modified version of `denote-retrieve-filename-title'.
;; This function doesn't retrieve `file-name-base' and makes
;; desluggifing optional
(defun denote-completing-format-retrieve-filename-title (file &optional desluggify)
  "Return title of Denote FILE or empty string if ther is no title.
If DESLUGGIFY is non-nil apply `denote-desluggify'."
  (if-let* (((file-exists-p file))
            ((denote-file-has-identifier-p file))
            ((string-match denote-title-regexp file))
            (title (match-string 1 file)))
      (if desluggify
	  (denote-desluggify title)
	title)
    ""))

;; This slow and inefficient function is the the
;; responsible of formatting the candidates
(defun denote-completing-format-build-formated-hash-table (all-files)
  (let ((htable (make-hash-table :test 'equal)))
    (mapcar
     (lambda(file)
       (if (and (denote-file-has-identifier-p file)
		(not (backup-file-name-p file)))
	   (puthash (concat
		     (propertize
		      (truncate-string-to-width
		       (denote-completing-format-extract-subdir-from-path file)
		       denote-completing-format-subdir-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-subdirectory)
		     (propertize
		      (truncate-string-to-width
		       (denote-extract-id-from-string file)
		       denote-completing-format-id-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-date)
		     (propertize "--" 'face 'denote-completing-format-faces-delimiter)
		     (propertize
		      (truncate-string-to-width
		       (denote-completing-format-retrieve-filename-title file denote-completing-format-deslugiffy-title)
		       denote-completing-format-title-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-title)
		     (propertize "__" 'face 'denote-faces-delimiter)
		     (propertize
		      (truncate-string-to-width
		       (string-join (denote-extract-keywords-from-path file) "_")
		       denote-completing-format-keywords-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-keywords)
		     (propertize
		      (truncate-string-to-width
		       (concat (file-name-extension file t))
		       denote-completing-format-extension-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-extension))
		    file
		    htable)))
     all-files)
    htable))

;; Modified version of `denote-open-or-create'
(defun denote-completing-format-open-or-create (target)
  "Visit TARGET file in variable `denote-directory'.
If file does not exist, invoke `denote' to create a file."
  (interactive (list (denote-completing-format-file-prompt)))
  (if (file-exists-p target)
      (find-file target)
    (call-interactively denote-completing-format-create-function)))

;; Modified version of `denote-file-prompt' 
(defun denote-completing-format-file-prompt ()
  "Prompt for file with identifier in variable `denote-directory'.
With optional INITIAL-TEXT, use it to prepopulate the minibuffer."
  (let* ((project-find-functions #'denote-project-find)
	 (project (project-current nil (denote-directory)))
	 (dirs (list (project-root project)))
	 (all-files (project-files project dirs))
 	 (completion-ignore-case read-file-name-completion-ignore-case)
	 (htable (denote-completing-format-build-formated-hash-table all-files))
	 (chosen (completing-read  "Select note: " htable nil nil nil 'denote--title-history)))
    (if-let*
	((chosen-value (gethash chosen htable)))
	chosen-value
      chosen)))

;; Modified version of `denote-link-or-create'
(defun denote-completing-format-link-or-create (target &optional id-only)
  "Use `denote-link' on TARGET file, creating it if necessary"
  (interactive (list (denote-completing-format-file-prompt) current-prefix-arg))
  (if (file-exists-p target)
      (denote-link target id-only)
    (call-interactively #'denote-link-after-creating)))

(provide 'denote-completing-format)
;;; denote-completing-format.el ends here
