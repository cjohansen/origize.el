;;; origize.el --- Insert spaces and newlines to center content in visible
;;;                part of frame.

;; Copyright (C) 2012 Christian Johansen

;; Author: Christian Johansen <christian@cjohansen.no>
;; Keywords: Presentation center vertically horizontally

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

;;; Code:

(defun origize/strip-whitespace ()
  (interactive)

  ;; remove trailing white space
  (cleanup-buffer)

  ;; remove white space at start of lines
  (goto-char (point-min))
  (while (re-search-forward "^\s+" nil t)
    (replace-match "" nil nil))

  ;; remove white space at start of buffer
  (goto-char (point-min))
  (insert "\n")
  (delete-blank-lines)
  (delete-char 1))

(defun origize/center-horizontally ()
  (interactive)
  (let ((fill-column (round (* (window-width) 0.8))))
    (center-region (point-min) (point-max))))

(defun origize/content-height ()
  (save-excursion
    (goto-char (point-max))
    (- (line-number-at-pos) 1)))

(defun origize/center-vertically ()
  (interactive)
  (let* ((page-center (/ (window-height) 2))
         (content-center (/ (origize/content-height) 2))
         (content-top (- page-center content-center 1)))
    (while (< (point) content-top)
      (insert "\n"))))

(defun origize/recenter-buffer ()
  (interactive)
  (origize/strip-whitespace)
  (origize/center-horizontally)
  (origize/center-vertically))

(defun origize/zoom-in ()
  (interactive)
  (zoom-frm-in)
  (origize/recenter-buffer))

(defun origize/zoom-out ()
  (interactive)
  (zoom-frm-out)
  (origize/recenter-buffer))

(provide 'origize)
