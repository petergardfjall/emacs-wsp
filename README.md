# About

`wsp` is a collection of functions and interactive commands that coordinate the
use of
[project.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el)
and
[desktop-save-mode](https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Save-Mode.html)
to support a _workspace-centric workflow_ where it's easy to quickly create and
switch between _workspaces_, each of which tracks a group of project trees,
window configurations and open buffers.

`wsp` provides primitives for managing and switching between a collection of
_workspaces_, each of which tracks a set of _projects_ (directory tree roots).
With a workspace open, the user can manage and switch between projects within
that workspace. New workspaces are cheap, quick to create, and easy to dispose.

Desktop save mode is used to persist the desktop state (including windows and
open buffers), such that the current frame layout is preserved when switching
between workspaces.

For a more complete IDE feeling, with a file explorer that displays the
currently active project, `wsp` combines well with
[emacs-projtree](https://github.com/petergardfjall/emacs-projtree).

# Functions

The package consists of a set of utility functions:

| Function                    | Description                                                             |
| --------------------------- | ----------------------------------------------------------------------- |
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
according to taste. An example is provided in the
[Install and confiure](#install-and-configure) section.

# Install and configure

- Clone this repository and add this in your Emacs config:

        (add-to-list 'load-path "/path/to/emacs-wsp")
        (require 'wsp)

- Via [use-package](https://github.com/jwiegley/use-package):

        (use-package wsp
          :load-path "path/to/emacs-wsp"
          ;; Alternatively, for users of straight, replace :load-path with
          ;;  :straight (emacs-wsp :type git :host github :repo "petergardfjall/emacs-wsp")
          :bind (("C-x w o"   . wsp-workspace-open)
                 ("C-x w k"   . wsp-workspace-close)
                 ("C-x w c"   . wsp-workspace-current)
                 ("C-x w d"   . wsp-workspace-delete)
                 ("C-x w p a" . wsp-project-add)
                 ("C-x w p d" . wsp-project-delete)
                 ("C-x w p s" . wsp-project-switch)
                 ("C-x w p c" . wsp-project-close)
                 ("C-x w p k" . wsp-project-close-current)
                 ("C-x w p K" . wsp-project-close-other)))

# Development

The tests need to verify that windows are properly restored when a workspace is
re-opened and therefore relies on a graphical environment. The tests, which will
start several instances of Emacs, are run via:

    make test

# Known limitations

- Within a workspace, project directory names need to be unique. This is by
  design, to prevent longer path-based naming.
