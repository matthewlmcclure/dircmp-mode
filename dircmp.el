;;; dircmp-mode.el --- Compare and sync directories.

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
  (setq goal-column 20))

(define-key dircmp-mode-map "\C-m" 'dircmp-do-ediff)
(define-key dircmp-mode-map ">" 'dircmp-do-sync-left-to-right)
(define-key dircmp-mode-map "<" 'dircmp-do-sync-right-to-left)
(define-key dircmp-mode-map "g" 'recompare-dirs)
(define-key dircmp-mode-map "n" 'next-line)
(define-key dircmp-mode-map "p" 'previous-line)

(defvar rsync-output-buffer " *dircmp-rsync-output*")
(defvar diff-output-buffer " *dircmp-diff-output*")
(defvar comparison-view-buffer "*DirCmp*")
(defvar dircmp-compare-recursive t)
(defvar dircmp-compare-links t)
(defvar dircmp-compare-permissions t)
(defvar dircmp-compare-times t)
(defvar dircmp-compare-group t)
(defvar dircmp-compare-owner t)
(defvar dircmp-compare-devices-and-specials t)
(defvar dircmp-compare-content nil)
(defvar dircmp-ignore-whitespace-differences nil)

;;; Idea: Optionally use custom comparators by file type. E.g., DOM comparison for XML.

(defun compare-dirs (dir1 dir2)
  (interactive "DLeft directory: \nDRight directory: ")
  (recompare-dirs dir1 dir2))

(defun recompare-dirs (&optional dir1 dir2)
  (interactive)
  (get-buffer-create rsync-output-buffer)
  (set-buffer rsync-output-buffer)
  (erase-buffer)
  (let ((normalized-dir1 (if dir1 (normalize-dir-string dir1) left-dir))
        (normalized-dir2 (if dir2 (normalize-dir-string dir2) right-dir)))
    (set (make-local-variable 'left-dir) normalized-dir1)
    (set (make-local-variable 'right-dir) normalized-dir2)
    (compare-with-rsync left-dir right-dir)
    (refine-comparison-with-diff)
    (update-comparison-view left-dir right-dir)))

(defun compare-with-rsync (dir1 dir2)
  (call-process-shell-command
   (format "rsync -nirlptgoD --delete '%s' '%s'" dir1 dir2)
   nil rsync-output-buffer))

(defun refine-comparison-with-diff ()
  (if dircmp-ignore-whitespace-differences
      (save-excursion
        (get-buffer-create diff-output-buffer)
        (set-buffer diff-output-buffer)
        (erase-buffer)
        (set-buffer rsync-output-buffer)
        (goto-char (point-min))
        (let ((lines (count-lines (point-min) (point-max))))
          (while (<= (line-number-at-pos) lines)
            (if (or (string-equal "c" (substring (comparison-on-current-rsync-line) 2 3))
                    (string-equal "s" (substring (comparison-on-current-rsync-line) 3 4)))
                (progn
                  (set-buffer diff-output-buffer)
                  (erase-buffer)
                  (call-process-shell-command
                   (format "diff -q -s -w '%s' '%s'" (left-on-current-rsync-line) (right-on-current-rsync-line))
                   nil diff-output-buffer)
                  (if (re-search-backward " are identical\n" nil t)
                      (progn
                        (set-buffer rsync-output-buffer)
                        (goto-char (+ (line-beginning-position) 2))
                        (delete-char 2)
                        (insert "..")))))
            (set-buffer rsync-output-buffer)
            (forward-line))))))

(defun normalize-dir-string (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun update-comparison-view (dir1 dir2)
  (set-buffer rsync-output-buffer)
  (get-buffer-create comparison-view-buffer)
  (let ((rsync-output (buffer-string)))
    (switch-to-buffer comparison-view-buffer)
    (let ((line (line-number-at-pos)))
      (set 'buffer-read-only nil)
      (erase-buffer)
      (insert (format "Directory comparison:\n\n Left: %s\nRight: %s\n\n" dir1 dir2))
      (format-rsync-output rsync-output)
      (switch-to-buffer comparison-view-buffer)
      (dircmp-mode)
      (set 'buffer-read-only t)
      (goto-char (point-min)) (forward-line (- line 1)))))

(defun dircmp-do-ediff ()
  (interactive)
  (let* ((file-A (left-on-current-view-line))
         (file-B (right-on-current-view-line))
         (buf-A (or (get-file-buffer file-A) (find-file-noselect file-A)))
         (buf-B (or (get-file-buffer file-B) (find-file-noselect file-B))))
    (ediff-buffers buf-A buf-B)))

(defun dircmp-do-sync-left-to-right ()
  (interactive)
  (let ((command (format "rsync -idlptgoD --delete '%s' '%s'"
                         (directory-file-name (left-on-current-view-line))
                         (file-name-directory (directory-file-name (right-on-current-view-line))))))
    (call-process-shell-command command))
  (recompare-dirs))

(defun dircmp-do-sync-right-to-left ()
  (interactive)
  (let ((command (format "rsync -idlptgoD --delete '%s' '%s'"
                         (directory-file-name (right-on-current-view-line))
                         (file-name-directory (directory-file-name (left-on-current-view-line))))))
    (call-process-shell-command command))
  (recompare-dirs))

(defun file-on-current-rsync-line ()
  (save-excursion
    (switch-to-buffer rsync-output-buffer)
    (buffer-substring-no-properties (+ (line-beginning-position) 10) (line-end-position))))

(defun comparison-on-current-rsync-line ()
  (save-excursion
    (switch-to-buffer rsync-output-buffer)
    (buffer-substring-no-properties (line-beginning-position) (+ (line-beginning-position) 9))))

(defun file-on-current-view-line ()
  (save-excursion
    (switch-to-buffer comparison-view-buffer)
    (buffer-substring-no-properties (+ (line-beginning-position) 20) (line-end-position))))

(defun left-on-current-rsync-line ()
  (save-excursion
    (switch-to-buffer rsync-output-buffer)
    (concat left-dir (file-on-current-rsync-line))))

(defun right-on-current-rsync-line ()
  (save-excursion
    (switch-to-buffer rsync-output-buffer)
    (concat right-dir (file-on-current-rsync-line))))

(defun left-on-current-view-line ()
  (save-excursion
    (switch-to-buffer rsync-output-buffer)
    (concat left-dir (file-on-current-view-line))))

(defun right-on-current-view-line ()
  (save-excursion
    (switch-to-buffer rsync-output-buffer)
    (concat right-dir (file-on-current-view-line))))

(defun format-rsync-output (rsync-output)
  (save-excursion
    (switch-to-buffer rsync-output-buffer)
    (goto-char (point-min))
    (while (> (- (line-end-position) (line-beginning-position)) 10)
      (let ((rsync-comparison (comparison-on-current-rsync-line))
            (file (file-on-current-rsync-line)))
        (switch-to-buffer comparison-view-buffer)
        (insert (format "%19s %s\n" (format-comparison rsync-comparison) file))
        (switch-to-buffer rsync-output-buffer)
        (forward-line)))))

(defun format-comparison (rsync-comparison)
  (cond ((string-match "^\*deleting" rsync-comparison)
         "right only")
        ((string-equal ">f+++++++" rsync-comparison)
         "left only")
        ((string-equal "c" (substring rsync-comparison 0 1))
         "left only")
        ((string-equal "c" (substring rsync-comparison 2 3))
         "content differs")
        ((string-equal "s" (substring rsync-comparison 3 4))
         "size differs")
        ((string-equal "t" (substring rsync-comparison 4 5))
         "timestamps differ")
        ((string-equal "p" (substring rsync-comparison 5 6))
         "permissions differ")
        ((string-equal "o" (substring rsync-comparison 6 7))
         "owner differs")
        ((string-equal "g" (substring rsync-comparison 7 8))
         "group differs")
        ((string-equal "." (substring rsync-comparison 0 1))
         "equivalent")
        (t rsync-comparison)
        ))

(provide 'dircmp-mode)
