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
    (error "package 'vterm' is not available")))

(provide 'task)
;;; task.el ends here
