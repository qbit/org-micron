;;; ox-micron.el --- Micron Backend for Org Export Engine -*- lexical-binding: t; -*-

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
(defun org-micron-bold (bold contents info)
  "Transcode BOLD object into Micron format."
  (format "`!%s`!" contents))

(defun org-micron-underline (underline contents info)
  "Transcode UNDERLINE object into Micron format."
  (format "`_%s`_" contents))

(defun org-micron-italic (italic contents info)
  "Transcode ITALIC object into Micron format."
  (format "`*%s`*" contents))

(defun org-micron-section (section contents info)
  "Transcode SECTION object into Micron format."
  (concat contents "\n"))

(defun org-micron-paragraph (paragraph contents info)
  "Transcode PARAGRAPH object into Micron format."
  (concat contents "\n\n"))

(defun org-micron-headline (headline contents info)
  "Transcode HEADLINE objects into Micron format."
  (let* ((level (org-export-get-relative-level headline info))
	 (title (org-export-data (org-element-property :raw-value headline) info))
	 (delim (make-string level ?>)))
    (concat delim " " title "\n\n"
	    (or contents ""))))

(defun org-micron-horizontal-rule (hr contents info)
  "Translate HORIZONTAL-RULE objects into Micron format."
  "-\n\n"
  )

(defun org-micron-code (src-block contents info)
  "Translate SRC-BLOCK objects into Micron format."
  (concat "`=\n" (org-remove-indentation
		  (org-export-format-code-default src-block info)) "`="))

(org-export-define-backend 'micron
  '((bold            . org-micron-bold)
    (headline        . org-micron-headline)
    (horizontal-rule . org-micron-horizontal-rule)
    (italic          . org-micron-italic)
    (paragraph       . org-micron-paragraph)
    (section         . org-micron-section)
    (src-block       . org-micron-code)
    (underline       . org-micron-underline)))

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

(provide 'ox-micron)
