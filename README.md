# Rocktl - An task launcher for Emacs

Rocktl allows you to define a list of commands for your project, and to run them
easily.

## Installation

### Manually

Clone this repository. Add this to your init file:

```emacs-lisp
(add-to-list 'load-path "/path/to/rocktl")
(require 'rocktl)
```

### Using quelpa

```emacs-lisp
(quelpa (rocktl :fetcher git :url "git@bitbucket.org:stevenremot/emacs-rocktl.git"))
```

## Usage

Type `M-x rocktl-run-task` to run a task you defined in your current directory,
or one of its parents. If this task is already running, the command will show
its buffer.

Type `M-x rocktl-status` to see the status of the commands you ran.

## Configuration

Run `M-x rocktl-config-shell-tasks` to define new commands for your project.
This configuration file is located in `.emacs.d/rocktl` by default. You can
customize this by customizing the `rocktl-config-dir` variable.


