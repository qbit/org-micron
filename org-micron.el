;;; org-micron.el --- Micron Backend for Org Export Engine -*- lexical-binding: t; -*-

;; Author: Aaron Bieber <aaron@bolddaemon.com>
;; Keywords: org, text, micron
;; URL: https://codeberg.org/qbit/ox-micron

;; Copyright (C) 2025 Aaron Bieber <aaron@bolddaemon.com>
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a simple Micron backend for Org exporter.

;;; Code:
(require 'ox)
(require 'ox-publish)

(defun org-micron-bold (_bold contents _info)
  "Transcode BOLD object into Micron format."
  (format "`!%s`!" contents))

(defun org-micron-underline (_underline contents _info)
  "Transcode UNDERLINE object into Micron format."
  (format "`_%s`_" contents))

(defun org-micron-italic (_italic contents _info)
  "Transcode ITALIC object into Micron format."
  (format "`*%s`*" contents))

(defun org-micron-section (_section contents _info)
  "Transcode SECTION object into Micron format."
  (concat contents "\n"))

(defun org-micron-paragraph (_paragraph contents _info)
  "Transcode PARAGRAPH object into Micron format."
  (concat contents "\n\n"))

(defun org-micron-headline (headline contents info)
  "Transcode HEADLINE objects into Micron format."
  (let* ((level (org-export-get-relative-level headline info))
	 (title (org-export-data (org-element-property :raw-value headline) info))
	 (delim (make-string level ?>)))
    (concat delim " " title "\n\n"
	    (or contents ""))))

(defun org-micron-table (_table contents _info)
  "Translate TABLE object into Micron format."
  (format "`=\n%s\n`=" contents))

(defun org-micron-table-row (_table-row contents _info)
  "Transcode TABLE ROW object into Micron format."
  (concat contents "\n"))

(defun org-micron-table-cell (_table-cell contents _info)
  "Transcode TABLE CELL object into Micron format."
  (concat " | " contents))

(defun org-micron-horizontal-rule (_hr _contents _info)
  "Translate HORIZONTAL-RULE objects into Micron format."
  "-\n\n"
  )

(defun org-micron-code (src-block _contents info)
  "Translate SRC-BLOCK objects into Micron format."
  (concat "`=\n" (org-remove-indentation
		  (org-export-format-code-default src-block info)) "`=\n"))

(defun org-micron-link (link desc info)
  "Translate LINK object into Micron format."
  (let* ((link-org-files-as-micron
	  (lambda (raw-path)
	    (if (string= ".org" (downcase (file-name-extension raw-path ".")))
		(concat (file-name-sans-extension raw-path) ".mu") raw-path)))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 (path (cond ((string-equal type "file")
		      (funcall link-org-files-as-micron raw-path))
		     ((member type '("custom-id" "id" "fuzzy"))
		      raw-path)
		     (t (concat type ":" raw-path)))))
    (cond
     ((org-export-custom-protocol-maybe link desc 'micron info))
     (t (if (not desc) (format "`[%s]" path)
	  (format "`[%s`%s]" desc path))))))

(org-export-define-backend 'micron
  '((bold            . org-micron-bold)
    (headline        . org-micron-headline)
    (horizontal-rule . org-micron-horizontal-rule)
    (italic          . org-micron-italic)
    (link            . org-micron-link)
    (paragraph       . org-micron-paragraph)
    (section         . org-micron-section)
    (src-block       . org-micron-code)
    (table           . org-micron-table)
    (table-cell      . org-micron-table-cell)
    (table-row       . org-micron-table-row)
    (underline       . org-micron-underline)))

(defun org-micron-publish-to-micron (plist filename pub-dir)
  "Publish an org file to Micron."
  (org-publish-org-to 'micron filename ".mu" plist pub-dir))

(defun org-micron-export-to-micron
    (&optional async subtreep visible-only body-only)
  "Export current buffer to a Micron file."
  (interactive)
  (let ((outfile (org-export-output-file-name ".mu" subtreep)))
    (org-export-to-file
	'micron outfile async subtreep visible-only body-only)))

(defun org-micron-export-as-micron
    (&optional async subtreep visible-only body-only)
  "Export current buffer to a Micron buffer."
  (interactive)
  (org-export-to-buffer
      'micron "*Org Micron Export*" async subtreep visible-only body-only))

(provide 'org-micron)
