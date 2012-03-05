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

; (define-key dir-compare-mode-map "\C-c\C-e" 'dir-compare-do-ediff)

(defvar rsync-output-buffer " *dir-compare-rsync")

(defvar comparison-view-buffer "*Dir-Compare*")

(defun compare-dirs (dir1 dir2)
  (interactive "Dleft directory: \nDright directory: ")
  (shell-command
   (format "rsync -nirlptgoD --delete '%s' '%s'" (normalize-dir-string dir1) (normalize-dir-string dir2))
   rsync-output-buffer)
  (update-comparison-view))

(defun normalize-dir-string (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun update-comparison-view ()
  (set-buffer rsync-output-buffer)
  (let ((rsync-output (buffer-string)))
    (switch-to-buffer comparison-view-buffer)
    (erase-buffer)
    (insert rsync-output)))

(provide 'dir-compare-mode)
