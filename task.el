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

(eval-when-compile
  (require 'subr-x)
  (defvar explicit-shell-file-name))

(declare-function project-root "project" (project))
(declare-function vterm "ext:vterm" (&optional buffer-name))
(declare-function term-ansi-make-term "term")
(declare-function term-mode "term")
(declare-function term-char-mode "term")
;; using internal functions is probably brittle
(declare-function project--buffer-list "project")

;;;###autoload
(defun task-vterm (ignore-existing-buffer)
  "Start `vterm' in the current project's root directory.
If a `vterm' buffer already exists for the project, switch to it.
When IGNORE-EXISTING-BUFFER is non-nil, create a new `vterm' buffer,
even if one already exists.  When calling interactively,
\\[universal-argument] sets IGNORE-EXISTING-BUFFER."
  (interactive "P")
  (if (require 'vterm nil t)
      ;; Intentionally shadow `default-directory' for `vterm'
      (let* ((default-directory (project-root (project-current t)))
             (default-buffer-name
               (format "*%s-vterm*"
                       (file-name-nondirectory
                        (directory-file-name default-directory))))
             (pop-to-previous (not ignore-existing-buffer)))
        (if-let ((pop-to-previous)
                 (previous-buffer (get-buffer default-buffer-name)))
            ;; TODO should user be able to choose the used buffer switch method?
            (pop-to-buffer previous-buffer)
          ;; FIXME following buffers are named *<project>-vterm*<N> instead of
          ;; *<project>-vterm<N>*
          (vterm (generate-new-buffer-name default-buffer-name))))
    (error "Package `vterm' is not available")))

;;;###autoload
(defun task-term (ignore-existing-buffer)
  "Start `term' in the current project's root directory.
If a `term' buffer already exists for the project, switch to it.
When IGNORE-EXISTING-BUFFER is non-nil, create a new `term' buffer,
even if one already exists.  When calling interactively,
\\[universal-argument] sets IGNORE-EXISTING-BUFFER."
  (interactive "P")
  ;; Intentionally shadow `default-directory'
  (let* ((default-directory (project-root (project-current t)))
         (default-program (or explicit-shell-file-name
                           (getenv "ESHELL")
                           (getenv "SHELL")
                           "/bin/sh"))
         (default-buffer-name
           (format "*%s-term*"
                   (file-name-nondirectory
                    (directory-file-name default-directory))))
         (pop-to-previous (not ignore-existing-buffer)))
    (if-let ((pop-to-previous)
             (previous-buffer (get-buffer default-buffer-name)))
        ;; TODO should user be able to choose the used buffer switch method?
        (pop-to-buffer previous-buffer)
      ;; FIXME following buffers are named *<project>-term*<N> instead of
      ;; *<project>-term<N>*
      (let* ((buffer-name (generate-new-buffer-name default-buffer-name))
             (program (read-from-minibuffer "Run program: "
                                            default-program))
             (term-buffer (term-ansi-make-term buffer-name program)))
        (with-current-buffer term-buffer
          (term-mode)
          (term-char-mode))
        ;; TODO should user be able to choose the used buffer switch method?
        (pop-to-buffer term-buffer)))))

;;;###autoload
(defun task-multi-occur (&optional nlines)
  "Do a `multi-occur' in project buffers.
With a prefix argument, show NLINES of context above and below."
  (interactive "p")
  (let* ((buffer-ephemeral-p (lambda (buffer)
                               (string-prefix-p " " (buffer-name buffer))))
         (project-buffers
          (seq-remove buffer-ephemeral-p
                      (project--buffer-list (project-current t)))))
    ;; TODO get the opened occur buffer and rename it to *<project>-Occur*
    (multi-occur project-buffers (car (occur-read-primary-args)) nlines)))

(defun task-grep (&optional regexp)
  "Run grep with REGEXP on project files."
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

(defun task-ripgrep (&optional regexp)
  "Run ripgrep with REGEXP on project files."
  (interactive "i\nP")
  (if (require 'ripgrep nil t)
      (let* ((proj (project-current t))
             (root (project-root proj))
             (fun (lambda (path)
                    (concat "--glob !" path)))
             (args (mapcar fun (project-ignores proj root)))
             (search-regexp (or regexp (project--read-regexp))))
        (ripgrep-regexp search-regexp root args))
    (error "Package 'ripgrep' is not available")))

(provide 'task)
;;; task.el ends here
