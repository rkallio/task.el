;;; task.el --- Additional commands for project.el -*- lexical-binding: t -*-

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
  (defvar explicit-shell-file-name)
  (defvar ag-ignore-list))

(declare-function project-root "project" (project))
(declare-function vterm "ext:vterm" (&optional buffer-name))
(declare-function term-ansi-make-term "term")
(declare-function term-mode "term")
(declare-function term-char-mode "term")
;; using internal functions is probably brittle
(declare-function project--buffer-list "project")
(declare-function ripgrep-regexp "ext:ripgrep")
(declare-function ag-regexp "ext:ag")

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

;;;###autoload
(defun task-grep (regexp)
  "Run `grep' with REGEXP on project files.
When ran interactively, query for REGEXP."
  (interactive (list (project--read-regexp)))
  (let* ((project (project-current t))
         (project-root (project-root project))
         (file-path-relative-to-project-root (lambda (file)
                               (file-relative-name file project-root)))
         (relative-paths (mapcar file-path-relative-to-project-root
                                 (project-files project))))
    (grep-compute-defaults)
    ;; TODO rewrite grep buffer name
    (lgrep regexp (string-join relative-paths " ") project-root)))

;;;###autoload
(defun task-ripgrep (regexp)
  "Run ripgrep with REGEXP on project files.
When ran interactively, query for REGEXP."
  (interactive (list (project--read-regexp)))
  (if (require 'ripgrep nil t)
      (let* ((project (project-current t))
             (project-root (project-root project))
             (glob-prefix (lambda (path)
                            (concat "--glob !" path)))
             (arguments (mapcar glob-prefix
                                (project-ignores project project-root))))
        ;; TODO rewrite ripgrep buffer name
        (ripgrep-regexp regexp project-root arguments))
    (error "Package `ripgrep' is not available")))

;;;###autoload
(defun task-ag (regexp)
  "Run `ag' with REGEXP on project files.
When ran interactively, query for REGEXP."
  (interactive (list (project--read-regexp)))
  (if (require 'ag nil t)
      (let* ((project (project-current t))
             (project-root (project-root project))
             (ag-ignore-list (append ag-ignore-list
                                     (project-ignores project project-root))))
        ;; TODO rewrite ag buffer name
        (ag-regexp regexp project-root))
    (error "Package `ag' is not available")))

(provide 'task)
;;; task.el ends here
