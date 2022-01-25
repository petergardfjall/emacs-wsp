# About
`wsp` is a collection of functions and interactive commands that coordinate the
use of
[project.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el),
[treemacs](https://github.com/Alexander-Miller/treemacs/), and
[desktop-save-mode](https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Save-Mode.html)
to support a _workspace-centric workflow_ -- a workflow where it's easy to
quickly create and switch between different *workspaces*, being either short- or
long-lived containers for closely related directory trees (*projects*) -- such
as a collection of git repositories needed when working on a certain ticket.

`wsp` provides primitives for managing and switching between a collection of
*workspaces*, each of which tracks a set of *projects* (directory tree roots).
With a workspace open, the user can manage and switch between projects within
that workspace. New workspaces are cheap, quick to create, and easy to dispose.

Via [treemacs](https://github.com/Alexander-Miller/treemacs/) a project explorer
is provided to visualize workspace projects, as well as the location of the
current buffer within one of those project trees (if Treemacs'
[follow-mode](https://github.com/Alexander-Miller/treemacs#follow-mode) is
enabled).

Desktop save mode is used to persist the desktop state (including windows and
open buffers), such that the current frame layout is preserved when switching
between workspaces.


# Functions
The package consists of a set of utility functions:

| Function                    | Description                                                             |
| -------------               | -------------                                                           |
| `wsp-workspace-open`        | Open (create or switch to) a workspace.                                 |
| `wsp-workspace-close`       | Close the current workspace; saves and closes all workspace buffers.    |
| `wsp-workspace-delete`      | Remove a workspace (prompts for selection).                             |
| `wsp-workspace-current`     | The name of the currently open workspace (if any).                      |
| `wsp-project-add`           | Add a project (directory tree) to the current workspace.                |
| `wsp-project-list`          | Lists all projects in the current workspace.                            |
| `wsp-project-current`       | The currently open project (project to which the buffer file belongs).  |
| `wsp-project-delete`        | Remove a project (directory tree) from the current workspace.           |
| `wsp-project-switch`        | Switch to a buffer in a different project within the current workspace. |
| `wsp-project-close`         | Close all open buffers for a certain project in the current workspace.  |
| `wsp-project-close-current` | Close all open buffers in the current workspace project.                |
| `wsp-project-close-other`   | Closes all projects _other than the current_.                           |


The package provides no keybindings, you are encouraged to set them up yourself
according to taste. An example is provided in the [Install and
confiure](#install-and-configure) section.


# Install and configure

* Clone this repository and add this in your Emacs config:

        (add-to-list 'load-path "/path/to/emacs-wsp")
        (require 'wsp)

* Via [use-package](https://github.com/jwiegley/use-package):

        (use-package wsp
          :load-path "path/to/emacs-wsp"
          :bind (("C-x w o" . wsp-workspace-open))
          :config
          (define-key global-map (kbd "C-x w d") #'wsp-workspace-delete)
          (define-key global-map (kbd "C-x w k") #'wsp-workspace-close)
          (define-key global-map (kbd "C-x w c") #'wsp-workspace-current)
          (define-key global-map (kbd "C-x p a") #'wsp-project-add)
          (define-key global-map (kbd "C-x p d") #'wsp-project-delete)
          (define-key global-map (kbd "C-x p s") #'wsp-project-switch)
          (define-key global-map (kbd "C-x p c") #'wsp-project-close)
          (define-key global-map (kbd "C-x p k") #'wsp-project-close-current)
          (define-key global-map (kbd "C-x p K") #'wsp-project-close-other))


# Development
The tests need to verify that windows are properly restored when a workspace is
re-opened and therefore relies on a graphical environment. The tests, which will
start several instances of Emacs, are run via:

    make test


# Known limitations
- Within a workspace, project directory names need to be unique. This is by
  design, to prevent longer path-based naming.
