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

(defconst mip-mode-version "0.1.0")

(defvar mip--open-project nil
  "Name of the currently open project.")

(defvar mip--open-project-path nil
  "Path to the currently open project.")

(defvar mip--open-project-files-hash nil
  "Hash table containing filenames as keys and full paths as values for the currently open project.")

(defvar mip--modeline-string nil
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

Project is represented as '[#project-name]."
  :type 'boolean
  :group 'mip)

(defcustom mip-open-project-after-find-file nil
  "Open project if a found file belongs to it.

This can be a little distracting if you're working on multiple
projects at once.  Use with caution."
  :type 'boolean
  :group 'mip)

(defcustom mip-uniquify-file-method 'path
  "How duplicate filenames should be uniquified.

number means that a number is appended to the filename.
Example: file, file<2>, file<3>

path means that the relative path of the file is appended to the
filename.
Example: file, file<include>, file<include/foo>"
  :type '(choice (symbol :tag "path" :value 'path)
                 (symbol :tag "number" :value 'number))
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
    (dolist (workspace workspaces projects)
      (if (file-directory-p workspace)
          (setq projects (append projects (mip-scan-workspace workspace ignored)))
        (error "workspace %s is not a directory" workspace)))))


(defun mip-find-project-directory (project)
  "Return path to the project's directory."
  (let ((workspaces mip-workspaces)
        (directory nil))
    (dolist (workspace workspaces directory)
      (let ((path (concat (file-name-as-directory workspace) project)))
        (if (file-exists-p path)
            (setq directory path))))))


(defun mip-open-project-files-hash-put (key value &optional number)
  "Puts a value into the hash ensuring that the key is unique.

Behavior depends on the variable mip-uniquify-file-method."
  (if (null mip-uniquify-file-method)
      (setq mip-uniquify-file-method 'path))
  (let ((current-value (gethash key mip--open-project-files-hash)))
    (if current-value
        (if (eq mip-uniquify-file-method 'number)
            (progn
              (setq number 1
                    key (concat key (format "<%d>" (+ 1 number))))
              (mip-open-project-files-hash-put key value (+ 1 number)))
          (progn
            (let ((path (abbreviate-file-name (file-name-directory value)))
                  (root mip--open-project-path))

              (setq key (concat key (format "<%s>" (substring path (+ (length root) 1) -1))))
              (mip-open-project-files-hash-put key value))))
      (puthash key value mip--open-project-files-hash))))


(defun mip-scan-path (path &optional recursive)
  "Add every file in PATH to mip--open-project-files-hash.

If RECURSIVE is non-nil, this function is called recursively for
all subdirectories of PATH."
  (let ((directories ())
        (current-files (directory-files path t nil t)))
    (dolist (file-path current-files nil)
      (let ((ignore nil))
        (dolist (regexp mip-ignored-files nil)
          (if (or (string-match regexp (file-name-nondirectory file-path))
                  (string-match regexp (file-name-directory file-path)))
              (setq ignore t)))
        (if (not ignore)
            (if (file-directory-p file-path)
                (if (not (or (string-equal "." (file-name-nondirectory file-path))
                             (string-equal ".." (file-name-nondirectory file-path))))
                    (add-to-list 'directories file-path))
              (mip-open-project-files-hash-put (file-name-nondirectory file-path) file-path)))))
    (if recursive
        (dolist (directory directories t)
          (mip-scan-path (concat (file-name-as-directory path) (file-name-nondirectory directory)))))))


(defun mip-project-files ()
  "Return a list of files belonging to the open project."
  (let ((files '()))
    (maphash (lambda (key value)
               (setq files (cons key files)))
             mip--open-project-files-hash)
    files))


(defun mip-project-of-file (file)
  "Return a name of the project of FILE.

Return nil if FILE doesn't belong to any project."
  (let ((projects (mip-scan-workspaces mip-workspaces))
        (belongs nil))
    (dolist (project projects belongs)
      (let ((project-path (mip-find-project-directory project)))
        (if (string-prefix-p (abbreviate-file-name project-path) (abbreviate-file-name file))
            (setq belongs project))))))


(defun mip-file-belongs-to-project (file project)
  "Return t if FILE belongs to PROJECT, nil otherwise."
  (if (string-prefix-p (abbreviate-file-name (mip-find-project-directory project)) (abbreviate-file-name file))
      t
    nil))


(defun mip-kill-project-buffers (project)
  "Kill all buffers belonging to PROJECT."
  (let ((interrupted nil))
    (dolist (buffer (buffer-list) t)
      (let ((filename (buffer-file-name buffer)))
        (if filename
            (if (mip-file-belongs-to-project filename project)
                (if (not (kill-buffer buffer))
                    (setq interrupted t))))))
    interrupted))


(defun mip-open-project (project)
  "Open project PROJECT if it's not already open.

Close previously open project if any."
  (if (not (string-equal mip--open-project project))
      (if (if mip--open-project
              (mip-close-project mip--open-project)
            t)
          (progn
            (let ((found nil))
              (dolist (string global-mode-string t)
                (if (eq string mip--modeline-string)
                  (setq found t)))
              (unless found
                (add-to-list 'global-mode-string mip--modeline-string t)))
            (if mip--open-project
                (mip-close-project mip--open-project))
            (setq mip--open-project project
                  mip--open-project-path (mip-find-project-directory project)
                  mip--open-project-files-hash (make-hash-table :test 'equal))
            (mip-scan-path mip--open-project-path t)
            (cd mip--open-project-path)
            (run-hooks 'mip-open-project-hook)
            (if mip-show-on-mode-line
                (setq mip--modeline-string (concat " [#" mip--open-project "]")))
            (message "project %s opened" project)))
    (message "project %s already open" project)))


(defun mip-close-project (project)
  "Close PROJECT if it's open."
  (if (and (string-equal mip--open-project project) (if mip-kill-project-buffers-on-close
                                                       (not (mip-kill-project-buffers project))
                                                      t))
      (progn
        (run-hooks 'mip-close-project-hook)
        (setq mip--open-project nil
              mip--open-project-path nil
              mip--open-project-files-hash nil
              mip--modeline-string nil)
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


(defun mip-find-file-in-open-project ()
  "Prompt for file in the currently open project.

If there's no open project, one will be opened before finding any
file."
  (interactive)
  (let ((project (if mip--open-project
                     mip--open-project
                   (mip-goto-project)))) ;; Goto a project first if not in one already
       (let ((file (ido-completing-read (concat "Find file in " mip--open-project ": ") (mip-project-files))))
         (let ((path (gethash file mip--open-project-files-hash)))
           (find-file (if path
                          path
                        (concat mip--open-project-path "/" file)))))))


(define-key mip-mode-map (kbd "C-c wg") 'mip-goto-project)
(define-key mip-mode-map (kbd "C-c wf") 'mip-find-file-in-open-project)
(define-key mip-mode-map (kbd "C-c wk") 'mip-close-open-project)
(define-key mip-mode-map (kbd "C-c wr") 'mip-refresh-open-project)
(define-key mip-mode-map (kbd "C-c wc") 'mip-get-open-project)

(define-minor-mode mip-mode
  "mip mode"
  nil
  " mip"
  mip-mode-map)

(define-globalized-minor-mode global-mip-mode mip-mode (lambda () (mip-mode t)))

(provide 'mip-mode)
;;; mip-mode.el ends here
