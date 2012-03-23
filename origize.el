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

(defun origize/center-line ()
  "Center line horizontally by inserting spaces at the beginning
of the line"
  (interactive)
  (origize/clear-left-indent)
  (let ((left-indent (/ (- (window-width) (origize/char-count)) 2))
        (start (point)))
    (if (> left-indent 0)
        (while (< (- (point) start) left-indent)
          (insert " "))))
  (end-of-line)
  (if (eobp) (insert "
")))

(defun origize/clear-left-indent ()
  (back-to-indentation)
  (while (not (bolp))
    (backward-delete-char 1)))

(defun origize/char-count ()
  (save-excursion
    (back-to-indentation)
    (let ((indentation (point)))
      (search-forward-regexp "$" (origize/eolpos) t)
      (while (and (not (bolp)) (looking-at "[\s]*$"))
        (backward-char))
      (1+ (- (point) indentation)))))

(defun origize/eolpos ()
  (save-excursion
    (end-of-line)
    (point)))

(defun origize/safe-next-line ()
  (end-of-line)
  (if (not (eobp))
      (progn (beginning-of-line) (next-line))))

(defun origize/num-leading-blank-lines ()
  (save-excursion
    (beginning-of-buffer)
    (let ((started)
          (num-lines 0))
      (while (not started)
        (beginning-of-line)
        (if (search-forward-regexp "^\s*$" (origize/eolpos) t)
            (setq num-lines (1+ num-lines))
          (setq started t))
        (end-of-line)
        (if (eobp) (setq started t))
        (beginning-of-line)
        (next-line))
      num-lines)))

(defun origize/pad-vertically ()
  "Insert newlines at the beginning of the buffer until the
content is vertically centered"
  (let ((missing (- (window-height) (origize/num-leading-blank-lines)))
        (i 0))
    (beginning-of-buffer)
    (while (< i missing)
      (insert "
")
      (setq i (1+ i)))))

(defun origize/center-buffer-vertically ()
  (origize/pad-vertically)
  (beginning-of-buffer)
  (goto-line (1+ (origize/num-leading-blank-lines)))
  (recenter-top-bottom))

(defun origize/recenter-buffer ()
  "Indent content with spaces until content is positioned in
horizontally centered"
  (interactive)
  (beginning-of-buffer)
  (while (not (eobp))
    (origize/center-line)
    (origize/safe-next-line))
  (origize/center-buffer-vertically))

(defun origize/zoom-frm-in ()
  (interactive)
  (zoom-frm-in)
  (origize/recenter-buffer))

(defun origize/zoom-frm-out ()
  (interactive)
  (zoom-frm-out)
  (origize/recenter-buffer))
