# Purpose

[![MELPA](http://melpa.org/packages/purpose-badge.svg)](http://melpa.org/#/purpose)
[![MELPA Stable](http://stable.melpa.org/packages/purpose-badge.svg)](http://stable.melpa.org/#/purpose)
[![Build Status](https://travis-ci.org/bmag/emacs-purpose.svg?branch=master)](https://travis-ci.org/bmag/emacs-purpose)

- **A full explanation can be found in the [GitHub wiki](https://github.com/bmag/emacs-purpose/wiki).**

## Introduction

Purpose provides a new window management system for Emacs, which gives
you a better control over where Emacs displays buffers.

With Purpose, each buffer has a configurable "purpose" and each window
can interactivaly be dedicated to a certain "purpose". When you dedicate
a window (`C-c , d`), Purpose makes sure that this window will be used
only for buffers which have the same purpose as the buffer that is
currently displayed in that window. The purpose of a buffer can be
customized via the variables `purpose-user-mode-purposes`,
`purpose-user-name-purposes`, `purpose-user-regexp-purposes` and
`purpose-use-default-configuration` (see the
[wiki](https://github.com/bmag/emacs-purpose/wiki/Purpose-Configuration)).

## Quickstart

### Activate Purpose
Manually: `M-x purpose-mode`

In your init file:
```elisp
(require 'purpose)
(purpose-mode)
```

### Configure Purpose
Manually: `M-x customize-group purpose`. Look at:
- "Purpose User Mode Purposes": recognize purpose according to major mode
- "Purpose User Name Purposes": recognize purpose according to buffer
  name (for exact names)
- "Purpose User Regexp Purposes": recognize purpose according to buffer
  name (for name patterns)
- "Purpose Use Default Configuration": toggle default configuration
  on/off

In init file:
```elisp
(add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
(add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
(add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
(setq purpose-use-default-configuration t) ; not really necessary, default is t
(purpose-compile-user-configuration) ; activates your changes
```

### Useful Commands
| Key         | Command                                                                                                                   |
| :---------- | :------------------------------------------------------------------------------------------------------------------------ |
| `C-c , b`   | `purpose-switch-buffer-with-purpose`: switch to a buffer with the same purpose as the current one                         |
| `C-u C-x b` | `switch-buffer-without-purpose`: switch to a buffer, but don't use Purpose for it. Handy for changing the current layout. |
| `C-c , d`   | `purpose-toggle-window-purpose-dedicated`                                                                                 |
| `C-c , D`   | `purpose-toggle-window-buffer-dedicated`                                                                                  |
| `C-c , 1`   | `purpose-delete-non-dedicated-windows`                                                                                    |
|             | `purpose-save-window-layout`: save current layout to file                                                                 |
|             | `purpose-save-frame-layout`                                                                                               |
|             | `purpose-load-window-layout`: load layout from file                                                                       |
|             | `purpose-load-frame-layout`                                                                                               |
|             | `purpose-reset-window-layout`: reload previously loaded layout                                                            |
|             | `purpose-reset-frame-layout`                                                                                              |

### Example: Simple Python Layout
How to get a simple and persistent layout for coding in Python that
looks like this:

![simple python layout](https://github.com/bmag/emacs-purpose/blob/master/images/simple-python-layout.png)

#### step 1: configuration
```elisp
(add-to-list 'purpose-user-mode-purposes '(python-mode . py))
(add-to-list 'purpose-user-mode-purposes '(inferior-python-mode . py-repl))
(purpose-compile-user-configuration)
```

#### step 2: change window layout
If you have a previously saved layout, you can load it with
`purpose-load-window-layout` and skip step 2 and step 3.

1. open a Python file
2. `C-c , d` (`purpose-toggle-window-purpose-dedicated`) so window is
   dedicated ("[py]" in the status bar will change to "[py!]")
3. `C-x 1` (`delete-other-windows`)
4. `C-x 2` (`split-window-below`)
5. `C-c C-z` (`python-shell-switch-to-shell`)
6. `C-c , d` so window is dedicated
7. `C-x o` (`other-window`) to select the python file's window
8. `C-x ^` (`enlarge-window`) until you like the sizes of the windows

#### step 3: save window layout
`M-x purpose-save-window-layout`
