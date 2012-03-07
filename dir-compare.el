;;; diff-ediff.el --- navigate diff files with Ediff

;; Copyright (C) 2012 Matt McClure

;; Author: Matt McClure
;; Keywords: unix, tools

;; dir-compare-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; dir-compare-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with dir-compare-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add to your Emacs startup file:
;;
;;    (load "/path/to/dir-compare.el")

;;; Code:

(define-derived-mode dir-compare-mode
  fundamental-mode "Dir-Cmp"
  "Major mode for comparing and syncing two directories.
\\{dir-compare-mode-map}"
  nil)

(define-key dir-compare-mode-map "\C-m" 'dir-compare-do-ediff)

(defvar rsync-output-buffer " *dir-compare-rsync")

(defvar comparison-view-buffer "*Dir-Compare*")

(defun compare-dirs (dir1 dir2)
  (interactive "Dleft directory: \nDright directory: ")
  (get-buffer-create rsync-output-buffer)
  (set-buffer rsync-output-buffer)
  (erase-buffer)
  (let ((normalized-dir1 (normalize-dir-string dir1))
        (normalized-dir2 (normalize-dir-string dir2)))
    (call-process-shell-command
     (format "rsync -nirlptgoD --delete '%s' '%s'" normalized-dir1 normalized-dir2)
     nil rsync-output-buffer)
    (update-comparison-view normalized-dir1 normalized-dir2)))

(defun normalize-dir-string (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun update-comparison-view (&optional dir1 dir2)
  (set-buffer rsync-output-buffer)
  (get-buffer-create comparison-view-buffer)
  (let ((rsync-output (buffer-string)))
    (switch-to-buffer comparison-view-buffer)
    (set 'buffer-read-only nil)
    (erase-buffer)
    (insert rsync-output)
    (dir-compare-mode)
    (set 'buffer-read-only t)
    (if dir1 (set (make-local-variable 'left-dir) dir1))
    (if dir2 (set (make-local-variable 'right-dir) dir2))))

(defun dir-compare-do-ediff ()
  (interactive)
  (let* ((file-A (concat left-dir (file-on-current-line)))
         (file-B (concat right-dir (file-on-current-line)))
         (buf-A (or (get-file-buffer file-A) (find-file-noselect file-A)))
         (buf-B (or (get-file-buffer file-B) (find-file-noselect file-B))))
    (ediff-buffers buf-A buf-B)))

(defun file-on-current-line ()
  (buffer-substring-no-properties (+ (line-beginning-position) 10) (line-end-position)))

(provide 'dir-compare-mode)
