;;; mip-mode.el --- virtual projects for emacs.

;; Copyright (C) 2012 Eeli Reilin

;; Author: Eeli Reilin <eeli.reilin@gmail.com>
;; Keywords: workspaces workspace project projects mip-mode

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

;;; Commentary:

;; mip-mode is a minor mode which enables users to quickly jump between
;; virtual projects and their files.  MIP stands for Multiple Interactive
;; Projects.
;;
;; mip-mode defines a workspace as a "root directory which contains
;; project subdirectories".

;; To enable mip globally, add the following to your .emacs file:
;;
;; (global-mip-mode t)

;; Before mip-mode can be used, we must define a list of workspaces;
;; Workspaces are just directories which contain project subdirectories.

;; For example, I usually have a dedicated location for all of my
;; projects in my home directory:
;;
;; ~/workspace
;; ~/workspace/project-1
;; ~/workspace/project-2

;; To tell workspace directories to mip-mode, we must add the following
;; to our .emacs file:
;;
;; (setq mip-workspaces '("~/workspace"))
;; or
;; (add-to-list 'mip-workspaces "~/workspace")
;;
;; You can have multiple workspaces as long as their projects don't
;; conflict.

;; You can also ignore specific projects:
;;
;; (setq mip-ignored-projects '("project-2"))
;; or
;; (add-to-list 'mip-ignored-projects "project-2")
;;
;; You can give a specific project name or a regular expression to match
;; multiple projects.

;; You can tell mip to close all project specific buffers:
;;
;; (setq mip-kill-project-buffers-on-close t)
;;
;; mip checks each buffer if it's file resides in the project's
;; directory, and if it does, kills it.

;; mip can even kill the magit-status buffer:
;;
;; (setq mip-kill-magit-status-buffer-on-close t)

;;; How to use:

;; C-c wg: go to project (mip-goto-project)
;; C-c wk: close project (mip-close-open-project)
;; C-c wr: refresh project (mip-refresh-open-project)
;; C-c wc: current project (mip-get-open-project)
;; C-c wf: find file in current project (mip-find-file-in-open-project)

;;; Code:

(require 'ido)
(require 'dash)

(defconst mip-mode-version "0.1.0")

(defvar mip--open-project nil
  "Name of the currently open project.")

(defvar mip--open-project-path nil
  "Path to the currently open project.")

(defvar mip--open-project-files-hash nil
  "Hash table containing filenames as keys and full paths as values for the currently open project.")

(defvar mip--mode-line-string nil
  "What will be displayed on the mode line.")

(defvar mip-mode-map (make-sparse-keymap)
  "Keymap for mip-mode.")

;;; Customizable variables:

(defgroup mip nil
  "mip-mode; virtual projects for Emacs."
  :prefix "mip-"
  :version "24.1.1"
  :link '(emacs-commentary-link "mip-mode")
  :link '(url-link "https://github.com/ereilin/mip-mode"))

(defcustom mip-ignored-projects '()
  "List of projects to ignore.

Project names can be written as whole or as regular expressions
to match multiple projects."
  :type '(repeat string)
  :group 'mip)

(defcustom mip-workspaces '()
  "List of paths to workspaces.

A workspace is a directory which can contain multiple projects as
subdirectories.  If workspaces contain duplicate projects, then
the functionality of mip-mode becomes undefined."
  :type '(repeat directory)
  :group 'mip)

(defcustom mip-kill-project-buffers-on-close nil
  "Kill all project's buffers on close.

A buffer is considered to belong to a project, if the file it
presents is inside the project's directory.

If there are any unsaved buffers, confirmation is needed."
  :type 'boolean
  :group 'mip)

(defcustom mip-kill-magit-status-buffer-on-close nil
  "Kill magit's status buffer on close.

Magit creates a buffer named '*magit: projectname*, when
magit-status is called.

When value is non-nil, the status buffer will be killed when a
project is closed. Otherwise nothing is done to the status
buffer."
  :type 'boolean
  :group 'mip)

(defcustom mip-show-on-mode-line t
  "Show currently open project on the mode line.

The project name is appended to the global-mode-string, and
should be the rightmost visible string on the mode line.

Project is represented according to mip-mode-line-format."
  :type 'boolean
  :group 'mip)

(defcustom mip-scan-dot-git nil
  "Should the '.git'-directory be scanned."
  :type 'boolean
  :group 'mip)

(defcustom mip-mode-line-format " [%s]"
  "Template for displaying open project on mode line.

A string is printed verbatim in the mode line except for %-constructs:
  %s -- print project name.")

(defcustom mip-open-project-after-find-file nil
  "Open project if a found file belongs to it.

This can be a little distracting if you're working on multiple
projects at once.  Use with caution."
  :type 'boolean
  :group 'mip)

(defcustom mip-open-project-hook nil
  "List of functions to be called after a project is opened."
  :type 'hook
  :group 'mip
  :options '(magit-status))

(defcustom mip-close-project-hook nil
  "List of functions to be called after a project is closed."
  :type 'hook
  :group 'mip)

(defcustom mip-refresh-project-hook nil
  "List of functions to be called after a project is refreshed."
  :type 'hook
  :group 'mip)


(defun mip-directory-empty-p (path)
  "Return t if directory in PATH is empty, nil otherwise."
  (equal (length (directory-files path)) 2))


(defun mip-path-is-dot-git-p (path)
  "Return t if PATH is '.git', nil otherwise."
  (string-equal (file-name-nondirectory path) ".git"))


(defun mip-path-is-project-root (path)
  "Return t if PATH is a project root, nil otherwise."
  (string-equal path (concat mip--open-project-path "/")))


(defun mip-should-ignore-path-p (path)
  "Return t if PATH should be ignored, nil otherwise."
  (let ((ignore nil))
    (-each mip-ignored-files (lambda (rule)
                               (setq ignore (string-match rule path))))
    ignore))


(defun mip-should-skip-path-p (path)
  "Return t if PATH should be skipped while scanning, nil otherwise."
  (and (not (string-equal path mip--open-project-path))
       (and (mip-path-is-dot-git-p path) (not mip-scan-dot-git))
       (not (mip-directory-empty-p path))))


(defun mip-scan-workspace (workspace &optional ignored)
  "Return a list projects in WORKSPACE.

If IGNORED is a list, projects that match the regexes are ignored."
  (let ((projects ())
        (paths (directory-files workspace t nil t)))
    (dolist (path paths projects)
      (let ((file (file-name-nondirectory path)))
        (if (and (file-directory-p path) (not (string-prefix-p "." file)))
            (if (listp ignored)
                (dolist (regexp ignored nil)
                  (if (not (string-match regexp file))
                      (add-to-list 'projects file)
                    (message "ignoring %s" file)))
              (add-to-list 'projects file)))))))


(defun mip-scan-workspaces (workspaces)
  "Return a list of projects in WORKSPACES."
  (let ((projects ())
        (ignored mip-ignored-projects))
    (-each workspaces (lambda (workspace)
             (if (file-directory-p workspace)
                 (setq projects (append projects (mip-scan-workspace workspace ignored)))
               (warn "workspace %s is not a directory" workspace))))
    projects))


(defun mip-find-project-directory (project)
  "Return path to the project's directory."
  (let ((project-directory nil))
    (-each-while mip-workspaces
                 (lambda (workspace)
                   (not project-directory))
                 (lambda (workspace)
                   (let ((path (concat (file-name-as-directory workspace) project)))
                     (if (file-exists-p path)
                         (setq project-directory path)))))
    project-directory))


(defun mip-find-file-in-open-project-starting-with (str)
  "Returns the unique identifier for the found file, or nil if
not found."
  (let ((match nil))
    (block break
      (maphash (lambda (key value)
                 (if (string-prefix-p str key)
                     (progn
                       (setq match key)
                       (return-from break)))) mip--open-project-files-hash))
    match))


(defun mip-open-project-files-hash-put (key value)
  "Puts a value into the hash ensuring that the key is unique."
  (let* ((current-value (or (gethash key mip--open-project-files-hash)
                            (gethash (mip-find-file-in-open-project-starting-with key) mip--open-project-files-hash)))
         (new-old-key nil)
         (path (abbreviate-file-name value))
         (root (file-name-directory path)))
    (if (and current-value
             (not (mip-path-is-project-root root)))
        (progn
          (remhash key mip--open-project-files-hash)
          (setq new-old-key (concat key (format "<%s>" (substring (file-name-directory (abbreviate-file-name current-value))
                                                                  (+ 1 (length mip--open-project-path))))))
          (setq key (concat key (format "<%s>" (substring root (+ 1 (length mip--open-project-path))))))))
    (if new-old-key
        (puthash new-old-key current-value mip--open-project-files-hash))
    (puthash key value mip--open-project-files-hash)))


(defun mip-scan-path (path &optional recursive)
  "Add every file in PATH to mip--open-project-files-hash.

If RECURSIVE is non-nil, this function is called recursively for
all subdirectories of PATH."
  (if (not (mip-should-skip-path-p path))
      (let* ((current-files (directory-files path t nil t))
             (subdirectories (if recursive (-filter (lambda (file-path)
                                                      (file-directory-p file-path))
                                                    current-files)))
             (files (-difference current-files subdirectories)))
          (-each files
                 (lambda (file-path)
                   (mip-open-project-files-hash-put (file-name-nondirectory file-path)
                                                    file-path)))
          (-each subdirectories
                 (lambda (directory-path)
                   (let ((dirname (file-name-nondirectory directory-path)))
                     (if (and (not (string-equal dirname "."))
                              (not (string-equal dirname "..")))
                         (mip-scan-path directory-path t))))))))


(defun mip-project-files (filter-ignored)
  "Return a list of files belonging to the open project.

If FILTER is non-nil, ignored files will be filtered out."
  (let ((files '()))
    (maphash (lambda (filename filepath)
               (if (not (and filter-ignored (mip-should-ignore-path-p filepath)))
                   (setq files (cons filename files))))
             mip--open-project-files-hash)
    files))


(defun mip-project-of-file (file)
  "Return a name of the project of FILE.

Return nil if FILE doesn't belong to any project."
  (let ((projects (mip-scan-workspaces mip-workspaces))
        (belongs nil))
    (-each projects (lambda (project)
             (let ((project-path (mip-find-project-tory project)))
               (if (string-prefix-p (abbreviate-file-name project-path) (abbreviate-file-name file))
                   (setq belongs project)))))
    belongs))


(defun mip-file-belongs-to-project (file project)
  "Return t if FILE belongs to PROJECT, nil otherwise."
  (string-prefix-p (abbreviate-file-name (mip-find-project-directory project)) (abbreviate-file-name file)))


(defun mip-kill-project-buffers (project)
  "Kill all buffers belonging to PROJECT."
  (let ((interrupted nil))
    (-each buffer-list
         (lambda (buffer)
           (let ((filename (buffer-file-name buffer)))
             (if filename
                 (if (mip-file-belongs-to-project filename project)
                     (if (not (kill-buffer buffer))
                         (setq interrupted t)))))))
           interrupted))

(defun mip-maybe-append-mode-line-string (string)
  "Appends STRING to global-mode-string if it isn't there already."
  (let ((found nil))
    (dolist (string global-mode-string t)
      (if (eq 'string 'mip--mode-line-string)
          (setq found t)))
    (unless found
      )))

(defun mip-open-project (project)
  "Open project PROJECT if it's not already open.

Close previously open project if any."
  (if (not (string-equal mip--open-project project))
      (if (if mip--open-project
              (mip-close-project mip--open-project)
            t)
          (progn
            (setq mip--open-project project
                  mip--open-project-path (mip-find-project-directory project)
                  mip--open-project-files-hash (make-hash-table :test 'equal))
            (mip-scan-path mip--open-project-path t)
            (cd mip--open-project-path)
            (run-hooks 'mip-open-project-hook)
            (if mip-show-on-mode-line
                (progn
                  (setq mip--mode-line-string (format mip-mode-line-format mip--open-project))
                  (add-to-list 'global-mode-string mip--mode-line-string t)))
            (message "project %s opened" project)))
    (message "project %s already open" project)))


(defun mip-close-project (project)
  "Close PROJECT if it's open."
  (if (and (string-equal mip--open-project project) (if mip-kill-project-buffers-on-close
                                                       (not (mip-kill-project-buffers project))
                                                      t))
      (progn
        (run-hooks 'mip-close-project-hook)
        (delq mip--mode-line-string global-mode-string)
        (setq mip--open-project nil
              mip--open-project-path nil
              mip--open-project-files-hash nil
              mip--mode-line-string nil)
        (message "project %s closed" project))))


(defun mip-maybe-open-project ()
  (if mip-open-project-after-find-file
      (let ((project (mip-project-of-file (buffer-file-name))))
        (if (and (not (string-equal project mip--open-project)) project)
            (mip-open-project project)))))
(add-hook 'find-file-hook 'mip-maybe-open-project)


;;; Interactive functions:

(defun mip-goto-project ()
  "Prompt user for project.

Return and open the chosen project."
  (interactive)
  (let ((project (ido-completing-read "Project: " (mip-scan-workspaces mip-workspaces))))
    (mip-open-project project)
    project))


(defun mip-close-open-project ()
  "Close the currently open project if any."
  (interactive)
  (if (and (boundp 'mip--open-project) (not (null mip--open-project)))
      (if mip--open-project
            (mip-close-project mip--open-project))
    (message "no project open to kill")))


(defun mip-get-open-project ()
  "Print out the name of the currently open project."
  (interactive)
  (if mip--open-project
      (message "open project: %s" mip--open-project)
    (message "no open project")))


(defun mip-refresh-open-project ()
  "Refresh mip--open-project-files-hash with the latest files."
  (interactive)
  (if mip--open-project
      (progn
        (setq mip--open-project-files-hash nil
              mip--open-project-files-hash (make-hash-table :test 'equal))
        (mip-scan-path mip--open-project-path t)
        (run-hooks 'mip-refresh-project-hook)
        (message "project %s refreshed" mip--open-project))
    (message "no project open to refresh")))


(defun mip-find-file-in-open-project (arg)
  "Prompt for file in the currently open project.

If there's no open project, one will be opened before finding any
file.

When called with a prefix argument, all files in the project will
be shown."
  (interactive "P")
  (let ((project (if mip--open-project
                     mip--open-project
                   (mip-goto-project)))) ;; Goto a project first if not in one already
       (let ((file (ido-completing-read (concat "Find file in " mip--open-project ": ") (mip-project-files (not arg)))))
         (let ((path (gethash file mip--open-project-files-hash)))
           (find-file (if path
                          path
                        (concat mip--open-project-path "/" file)))))))


(define-key mip-mode-map (kbd "C-c pg") 'mip-goto-project)
(define-key mip-mode-map (kbd "C-c pf") 'mip-find-file-in-open-project)
(define-key mip-mode-map (kbd "C-c pk") 'mip-close-open-project)
(define-key mip-mode-map (kbd "C-c pr") 'mip-refresh-open-project)
(define-key mip-mode-map (kbd "C-c pc") 'mip-get-open-project)

(define-minor-mode mip-mode
  "mip mode"
  nil
  " mip"
  mip-mode-map)

(define-globalized-minor-mode global-mip-mode mip-mode (lambda () (mip-mode t)))

(provide 'mip-mode)
;;; mip-mode.el ends here
