;;; task.el --- Additional commands for project.el

;; Copyright (C) 2020 Roni Kallio

;; Author: Roni Kallio <roni@kallio.app>

;; Task is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Task is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Task.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Task extends `project.el' with some much needed commands.

;;; Code:

(require 'project)

(defun task-vterm ()
  "Start vterm in the current project's root directory.
If a buffer already exists for running a vterm in the project's
root, switch to it.  Otherwise, create a new vterm buffer.  With
\\[universal-argument] prefix arg, create a new vterm buffer even
if one already exists."
  (interactive)
  (if (require 'vterm nil t)
      (let* ((default-directory (project-root (project-current t)))
             (default-project-vterm-name
               (concat "*" (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory default-directory)))
                       "-vterm*"))
             (vterm-buffer (get-buffer default-project-vterm-name)))
        (if (and vterm-buffer (not current-prefix-arg))
            (pop-to-buffer vterm-buffer)
          (vterm (generate-new-buffer-name default-project-vterm-name))))
    (error "Package 'vterm' is not available")))

(defun task-term ()
  "Start a `term' session in the current project's root directory.
If a buffer already exists for running `term' in the
project's root, switch to it.  Otherwise, create a new shell
buffer.  With \\[universal-argument] prefix arg, create a new
`term' buffer even if one already exists."
  (interactive)
  (require 'term)
  (require 'shell)
  (let* ((default-directory (project-root (project-current t)))
         (default-program (or explicit-shell-file-name
                              (getenv "ESHELL")
                              (getenv "SHELL")
                              "/bin/sh"))
         (default-project-term-name
           (format "*%s-term*"
                   (file-name-nondirectory
                    (directory-file-name
                     (file-name-directory default-directory)))))
         (existing-buffer (get-buffer default-project-term-name))
         (term-buffer
          (if (or current-prefix-arg (not existing-buffer))
              (let* ((buffer-name
                      (generate-new-buffer-name default-project-term-name))
                     (program (read-from-minibuffer "Run program: "
                                                    default-program))
                     (buffer (term-ansi-make-term buffer-name program)))
                (set-buffer buffer)
                (term-mode)
                (term-char-mode)
                buffer)
            existing-buffer)))
    (pop-to-buffer term-buffer)))

(defun task-multi-occur (&optional nlines)
  "Do a `multi-occur' in the project buffers.
With a prefix argument, show NLINES of context above and below."
  (interactive "P")
  (require 'seq)
  (let* ((predicate '(lambda (buffer)
                       (string-match "^ " (buffer-name buffer))))
         ;; Ignore invisible files
         (project-buffers
          (seq-remove predicate (project--buffer-list (project-current t)))))
    (multi-occur project-buffers
                 (car (occur-read-primary-args))
                 (prefix-numeric-value nlines))))

(defun task-grep (&optional regexp)
  "Perform grep on project files."
  (interactive "i")
  (require 'grep)
  (let* ((proj (project-current t))
         (root (project-root proj))
         (file-relative-name (lambda (file)
                               (file-relative-name file root)))
         (relative-paths (mapcar 'file-relative-name (project-files proj)))
         (search-regexp (or regexp (project--read-regexp))))
    (grep-compute-defaults)
    (lgrep search-regexp (mapconcat 'identity relative-paths " ") root)))

(provide 'task)
;;; task.el ends here
