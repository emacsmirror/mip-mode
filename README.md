# Description

mip-mode is a minor mode to quickly switch between multiple projects and
files.

mip-mode is written for learning and personal usage.

# Dependencies

Currently mip-mode depends only on `ido`.

# Installation

Add `mip-mode.el` to your `load-path`:

```
(add-to-list 'load-path "~/.emacs.d/vendor/mip-mode")
```

mip-mode can be enabled globally and locally.  To select mip-mode
globally, enter the following to your `.emacs` file:

```
(require 'mip-mode)
(global-mip-mode t)
```

Next, mip-mode must be told where your projects are located:

```
(add-to-list 'mip-workspaces "~/workspace")
```

mip-mode considers every directory under a workspace a project.

Projects and files can be ignored:

```
(add-to-list 'mip-ignored-projects "temp-project1")
(add-to-list 'mip-ignored-projects "temp.*$") ;; Projects can also be matches using regular expressions

(add-to-list 'mip-ignored-files "^\\..*$") ;; Ignore all dot files
```

Check out the commentary for more information.

# Usage

Interactive functions provided by mip-mode:

* `mip-goto-project` (C-c pg)
  Prompts for a project to open.

* `mip-find-file-in-open-project` (C-c pf)
  Prompts for a file to open in the current project.

* `mip-close-open-project` (C-c pk)
  Closes the current project.

* `mip-refresh-open-project` (C-c pr)
  Refreshes the list of files belonging to the current project.

* `mip-get-open-project` (C-c pc)
  Shows the name of the current project in the minibuffer.

# Todo

* Multiple open projects
* Moving files within a project
* Project specific settings