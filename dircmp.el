;;; diff-ediff.el --- navigate diff files with Ediff

;; Copyright (C) 2012 Matt McClure

;; Author: Matt McClure
;; Keywords: unix, tools

;; dircmp-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; dircmp-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with dircmp-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add to your Emacs startup file:
;;
;;    (load "/path/to/dircmp.el")

;;; Code:

(define-derived-mode dircmp-mode
  fundamental-mode "DirCmp"
  "Major mode for comparing and syncing two directories.
\\{dircmp-mode-map}"
  nil)

(define-key dircmp-mode-map "\C-m" 'dircmp-do-ediff)

(defvar rsync-output-buffer " *dircmp-rsync*")

(defvar comparison-view-buffer "*DirCmp*")

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

(defun update-comparison-view (dir1 dir2)
  (set-buffer rsync-output-buffer)
  (get-buffer-create comparison-view-buffer)
  (let ((rsync-output (buffer-string)))
    (switch-to-buffer comparison-view-buffer)
    (set 'buffer-read-only nil)
    (erase-buffer)
    (insert (format "Directory comparison: %s | %s\n\n" dir1 dir2))
    (format-rsync-output rsync-output)
    (switch-to-buffer comparison-view-buffer)
    (dircmp-mode)
    (set 'buffer-read-only t)
    (if dir1 (set (make-local-variable 'left-dir) dir1))
    (if dir2 (set (make-local-variable 'right-dir) dir2))))

(defun dircmp-do-ediff ()
  (interactive)
  (let* ((file-A (concat left-dir (file-on-current-view-line)))
         (file-B (concat right-dir (file-on-current-view-line)))
         (buf-A (or (get-file-buffer file-A) (find-file-noselect file-A)))
         (buf-B (or (get-file-buffer file-B) (find-file-noselect file-B))))
    (ediff-buffers buf-A buf-B)))

(defun file-on-current-raw-line ()
  (buffer-substring-no-properties (+ (line-beginning-position) 10) (line-end-position)))

(defun comparison-on-current-raw-line ()
  (buffer-substring-no-properties (line-beginning-position) (+ (line-beginning-position) 9)))

(defun file-on-current-view-line ()
  (buffer-substring-no-properties (+ (line-beginning-position) 20) (line-end-position)))

(defun format-rsync-output (rsync-output)
  (progn
    (switch-to-buffer rsync-output-buffer)
    (goto-char (point-min))
    (while (> (- (line-end-position) (line-beginning-position)) 10)
      (let ((raw-comparison (comparison-on-current-raw-line))
            (file (file-on-current-raw-line)))
        (switch-to-buffer comparison-view-buffer)
        (insert (format "%19s %s\n" (format-comparison raw-comparison) file))
        (switch-to-buffer rsync-output-buffer)
        (forward-line)))))

(defun format-comparison (raw-comparison)
  (cond ((string-match "^\*deleting" raw-comparison)
         "right only")
        ((string-equal ">f+++++++" raw-comparison)
         "left only")
        ((string-equal "c" (substring raw-comparison 0 1))
         "left only")
        ((string-equal "s" (substring raw-comparison 3 4))
         "content differs")
        ((string-equal "." (substring raw-comparison 0 1))
         "equal")
        (t raw-comparison)
        ))

(provide 'dircmp-mode)
