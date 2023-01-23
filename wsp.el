;;; wsp.el --- Functions for a workspace-centric workflow.  -*- lexical-binding: t -*-
;;
;; Copyright © 2020 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;; URL: https://github.com/petergardfjall/emacs-wsp
;; Keywords: workspace, project
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))
;; Version: 0.1.0
;; Homepage: https://github.com/petergardfjall/emacs-wsp
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; A collection of functions and interactive commands that coordinate the use of
;; `project.el` and `desktop-save-mode` to support a workspace-centric workflow.
;;
;; The user can manage and switch between a collection of workspaces, each of
;; which tracks a set of projects (directory roots).  The user can manage and
;; switch between projects within the workspace.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'desktop)

(defvar wsp-workspaces-dir (concat (file-name-as-directory user-emacs-directory) "wsp")
  "Directory where workspace state is saved.")

(defvar wsp-desktop-filename  "desktop"
  "The file name to use for `desktop-save-mode` state.")

(defvar wsp--current-workspace nil
  "Tracks the name of the workspace currently open.")


;;
;; workspaces
;;

(defun wsp-workspace-open (name &optional first-project-dir)
  "Open (or switch to) workspace NAME.
When creating a new workspace, the first project directory to
include in workspace may optionally be passed as
FIRST-PROJECT-DIR.  If it's not passed, the user will be
prompted."
  (interactive
   (list (completing-read "Select workspace: " (wsp-workspace-list) nil nil)))
  ;; if a workspace is already open, close it
  (when (wsp-workspace-current)
    (wsp-workspace-close))

  (unless (wsp-workspace-exists name)
    (wsp--workspace-create name first-project-dir))
  (wsp--workspace-load name)

  (require 'project)
  (add-hook 'project-find-functions #'wsp--try-project-list))


(defun wsp--workspace-create (name &optional first-project-dir)
  "Create a workspace named NAME.
The first project directory to include in workspace can
optionally be passed as FIRST-PROJECT-DIR.  If it's not passed,
the user will be prompted."
  (unless (string-match "^[_.0-9A-Za-z\\-]+$" name)
    (error "A workspace name may only contain a-z, A-Z, 0-9, underscore (_), dash (-), and dot (.)"))

  (wsp--workspace-init name)
  (message "creating new workspace '%s' ..." name)
  (make-directory (wsp--workspace-dir name) t)

  ;; read initial project directory
  (let* ((proj-path (or first-project-dir
			 (wsp--read-dir-name "Add first workspace project directory:"))))
    ;; add first project to project.el (will also save state)
    (project-remember-project (wsp--project proj-path))))



(defun wsp--workspace-init (name)
  "Reset and prime libraries for being used with workspace named NAME."
  (wsp--desktop-init name)
  (wsp--project-init name))


(defun wsp-workspace-close ()
  "Close the current workspace (if one is open)."
  (interactive)
  (wsp--ensure-workspace-open)

  ;; save current desktop
  (desktop-save desktop-dirname t)
  ;; disable desktop-save mode
  (desktop-save-mode 0)

  ;; close all open project buffers
  (dolist (project-name (wsp-project-list))
    (wsp-project-close project-name))

  ;; set no current project
  (setq project--list 'unset)
  (setq project-list-file 'unset)

  ;; set no current workspace
  (setq wsp--current-workspace nil)

  (remove-hook 'project-find-functions #'wsp--try-project-list))


(defun wsp-workspace-current ()
  "Return the name of the currently open workspace or nil if none is open."
  (interactive)
  wsp--current-workspace)


(defun wsp-workspace-delete (name)
  "Delete the workspace named NAME.
If this happens to be the current workspace, it is first closed."
  (interactive
   (list (completing-read "Select workspace to delete: "
			  (wsp-workspace-list) nil t)))
  ;; if deleting the current workspace, first close it.
  (when (string= (wsp-workspace-current) name)
    (wsp-workspace-close))
  ;; ... then perform delete
  (when (wsp-workspace-exists name)
    (wsp--pulse)
    (message "deleting workspace %s ..." name)
    (delete-directory (wsp--workspace-dir name) t)))


(defun wsp-workspace-exists (name)
  "Determine if a saved workspace named NAME exists."
  (member name (wsp-workspace-list)))


(defun wsp-workspace-list ()
  "List the collection of known (saved) workspaces."
  (wsp--ensure-workspaces-dir)
  (mapcar #'wsp--basename (wsp--list-dirs wsp-workspaces-dir)))


(defun wsp--ensure-workspace-open ()
  "Fails with an error if no workspace is currently open."
  (if (not (wsp-workspace-current))
      (error "No workspace opened")
    t))


(defun wsp--workspace-load (name)
  "Load workspace named NAME."
  (message "loading workspace %s ..." name)

  ;; set up all libraries for use with workspace NAME
  (wsp--workspace-init name)

  (wsp--project-activate name)
  (wsp--desktop-activate name)

  (setq wsp--current-workspace name)
  (message "workspace '%s' loaded." name))


(defun wsp--desktop-init (name)
  "Disable and prepare `desktop-save-mode` for use in workspace NAME."
  (desktop-save-mode 0)
  ;; save settings
  (let ((save-path (wsp--desktop-save-path name)))
    ;; state dir
    (setq desktop-dirname (file-name-directory save-path))
    ;; state file
    (setq desktop-base-file-name (file-name-nondirectory save-path))
    ;; directory where desktop state is to be loaded from.
    (setq desktop-path (list desktop-dirname)))

  ;; load the desktop if locked?
  (setq desktop-load-locked-desktop 'ask)
  ;; always save on exit (without prompting)
  (setq desktop-save t))


(defun wsp--project-init (name)
  "Prepare `project.el` for use in workspace NAME."
  ;; make sure project list gets reloaded from new location.
  (setq project--list 'unset)
  (setq project-list-file (wsp--projects-file name)))


(defun wsp--desktop-load-hook ()
  "To be called when a desktop is successsfully loaded."
  (message "desktop loaded: %s" desktop-path))


(defun wsp--desktop-activate (name)
  "Activate/load `desktop-save-mode` in workspace NAME."
  (desktop-save-mode +1)

  ;; on successsful desktop-read
  (add-hook 'desktop-after-read-hook #'wsp--desktop-load-hook)
  ;; on failed desktop-read: disable desktop-save-mode, since we want to
  ;; disable automatic saving of desktop on exit in this case.
  (add-hook 'desktop-not-loaded-hook
	    (lambda ()
	      (display-warning :warning "couldn't load desktop. disabling desktop-save-mode ...")
	      (desktop-save-mode 0)))

  ;; try to load saved desktop if one exists
  (when (file-exists-p (wsp--desktop-save-path name))
    (desktop-read)))


(defun wsp--project-activate (name)
  "Activate/load `project.el` in workspace NAME.
This loads values into `project--list' from `project-list-file'."
  (project--read-project-list))


(defun wsp--desktop-save-path (name)
  "State file destination for `desktop-save-mode` in workspace NAME."
  (concat (wsp--workspace-dir name) wsp-desktop-filename))

(defun wsp--projects-file (name)
  "State file destination for `project.el` in workspace NAME."
  (concat (wsp--workspace-dir name) "projects"))

(defun wsp--workspace-dir (name)
  "Workspace state directory for workspace NAME."
  (file-name-as-directory (concat (file-name-as-directory wsp-workspaces-dir) name)))



;;
;; projects
;;

(defun wsp-project-list ()
  "Return the name of each project added to the current workspace."
  (interactive)

  (if (not (wsp-workspace-current))
      nil ;; no workspace set => empty list
    (let* ((project-roots (mapcar #'car project--list))
	   ;; get rid of trailing slashes
	   (project-dirs (mapcar 'directory-file-name project-roots))
	   (project-names (mapcar 'file-name-nondirectory project-dirs)))
      (sort project-names 'string-lessp))))


(defun wsp-project-list-open ()
  "Return the name of each project in the current workspace with open buffers."
  (cl-remove-if-not #'wsp--project-is-open (wsp-project-list)))

(defun wsp-project-exists (name)
  "Determine if the current workspace contains a project named NAME."
    (if (not (wsp-workspace-current))
	nil ;; no workspace set => empty list
      (member name (wsp-project-list))))


(defun wsp-project-current ()
  "Return the wsp project open in the current buffer or nil."
  (interactive)
  (wsp--ensure-workspace-open)

  ;; TODO
  (let ((proj-path (cl-find-if (lambda (proj)
				 (string-prefix-p proj (buffer-file-name (window-buffer))))
			       (wsp--project-list-abspath))))
    (if proj-path
	(message "%s" (wsp--basename proj-path))
      nil)))



(defun wsp-project-add (dir-path)
  "Add a project with directory tree rooted at DIR-PATH."
  (interactive (list (and (wsp--ensure-workspace-open) (wsp--read-dir-name "Project directory to add:"))))

  (let ((project-name (wsp--basename dir-path)))
    (when (wsp-project-exists project-name)
      (error "Project already exists in workspace"))
    (project-remember-project (wsp--project dir-path))
    (wsp--pulse)
    (message "project added: %s" dir-path)))



;; TODO rename: wsp-project-close-buffers?
(defun wsp-project-close (project-name)
  "Close all project buffers for PROJECT-NAME."
  (interactive
   (progn
     (unless (wsp-project-list-open)
       (error "Nothing to close (no projects with open buffers)"))
     (list (completing-read "Select project to close: " (wsp-project-list-open) nil t))))

  (dolist (buffer (wsp--project-buffers project-name))
    (kill-buffer buffer)))


(defun wsp-project-close-current ()
  "Close the currently open project."
  (interactive)
  (if (wsp-project-current)
      (wsp-project-close (wsp-project-current))
    (error "Not visiting a wsp project buffer")))


(defun wsp-project-close-other ()
  "Close all open projects except for the currently open project."
  (interactive)
  (if (wsp-project-current)
      (dolist (other-proj (cl-remove-if (lambda (proj-name) (string= proj-name (wsp-project-current)))
					(wsp-project-list)))
	(wsp-project-close other-proj))
    (error "Not visiting a wsp project buffer")))


(defun wsp-project-delete (project-name)
  "Delete PROJECT-NAME from current workspace."
  (interactive
   (list (completing-read "Select project to delete: " (wsp-project-list)  nil t)))
  (when (= (length (wsp-project-list)) 1)
    (error "Cannot delete last project in workspace"))

  ;; close all project buffers
  (wsp-project-close project-name)
  (let* ((project-path (wsp--project-path project-name))
	 (abs-path (expand-file-name project-path)))
    (project-forget-project project-path)
    (wsp--pulse)
    (message "project removed: %s (%s)" project-name project-path)))


(defun wsp-project-switch (project-name)
  "Switch to a buffer in project PROJECT-NAME.
Switches to the most recently open buffer in the project if one
has been opened or otherwise prompts to select a file from the
project."
  (interactive
   (list (completing-read "Switch to project: " (wsp-project-list)  nil t)))
  (let ((proj-dir (wsp--project-path project-name))
	(proj-buffers (wsp--project-buffers project-name)))
    (if proj-buffers
	;; If a project buffer is already opened, switch to that.
	(switch-to-buffer (car proj-buffers))
      ;; Otherwise: select a buffer in the other project to open via project.el
      (project-switch-project proj-dir))))

(defun wsp--basename (path)
  "Return the base name of a PATH.
For a PATH of either `/a/b/c/` or `/a/b/c`, the result is `c`."
  (file-name-nondirectory (directory-file-name path)))

(defun wsp--list-dirs (path)
  "List all directories under PATH."
  (cl-remove-if 'wsp--isdot-p
	     (cl-remove-if-not 'wsp--isdir-p (directory-files path t))))

(defun wsp--isdot-p (path)
  "Determine if PATH is a `.` or `..` file entry."
  (or (string= (file-name-base path) ".") (string= (file-name-base path) "..")))

(defun wsp--isdir-p (path)
  "Determine if PATH is a directory."
  (file-attribute-type (file-attributes path)))


(defun wsp--project-path (project-name)
  "Return the project path corresponding to the wsp PROJECT-NAME.
Note that the path may not be expanded (for example, it could be
`~/.emacs.d`)."
  (cl-find-if (lambda (proj) (string= (wsp--basename proj) project-name))
	      (project-known-project-roots)))

(defun wsp--project-list-abspath ()
  "List all project.el known project roots with absolute paths."
  (let* ((project-roots (mapcar #'car project--list))
	 (abs-paths (mapcar #'expand-file-name project-roots)))
    abs-paths))


(defun wsp--project-is-open (project-name)
  "Indicates if the given project PROJECT-NAME is open (has open buffers)."
  (wsp--project-buffers project-name))

(defun wsp--project-buffers (project-name)
  "List all open file buffers that belong to project PROJECT-NAME."
  ;; get all project buffers except special buffers
  (cl-remove-if-not
   (lambda (buffer) (wsp--file-visiting-buffer buffer))
   (let ((project-path (wsp--project-path project-name)))
     (project-buffers (wsp--project project-path)))))


(defun wsp--ensure-workspaces-dir ()
  "Ensures that the workspaces directory has been created."
  (make-directory wsp-workspaces-dir t))


(defun wsp--project (project-path)
  "Turn a PROJECT-PATH into a project.el API-compatible project."
  (unless (file-directory-p project-path)
    (error "Cannot turn '%s' into a project: not a directory" project-path))
  (cons 'transient project-path))
  ;; (project-current nil (wsp--closest-dir project-path)))

(defun wsp--closest-dir (path)
  "Return the closest parent directory for PATH.
If PATH is a directory, PATH is returned. Otherwise its parent is returned."
  (let ((path (wsp--normalized-path path)))
    (if (file-directory-p path)
        path
      (file-name-directory path))))

(defun wsp--normalized-path (path)
  "Return a normalized PATH that is expanded and trimmed of trailing slash."
  (string-trim-right (expand-file-name path) "/"))


(defun wsp--file-visiting-buffer (buffer)
  "Indicate if the given BUFFER is visiting a file.
This can be used to distinguish regular file buffers from special
ones like for example '*scratch*'."
  (buffer-file-name buffer))

(defun wsp--read-dir-name (prompt)
  "Reads a directory name in the mini-buffer using the given PROMPT."
  ;; Cleans the path and then abbreviates it (for example substituing ~ for the
  ;; home directory.
  (abbreviate-file-name (expand-file-name (read-directory-name (format prompt)))))

(defun wsp--pulse ()
  "Produces a flashing pulse at point.
Used to as a visual cue that a command modified the workspace."
  (pulse-momentary-highlight-one-line (point) 'cursor))


(defun wsp--try-project-list (dir)
  "Find a project containing DIR from the `project-list-file'.

It finds the longest prefix match to correctly capture nested projects."
  (let ((longest-match nil))
    (dolist (it (project-known-project-roots))
      (let ((it (abbreviate-file-name it))
            (dir (abbreviate-file-name dir)))
        (when (and (string-prefix-p it dir)
                   (file-directory-p it)
                   (> (length it) (length longest-match)))
          (setq longest-match it))))
    (when longest-match
      (cons 'transient longest-match))))


(provide 'wsp)

;;; wsp.el ends here
