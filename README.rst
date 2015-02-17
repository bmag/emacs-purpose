=======
Purpose
=======

A full explanation can found in the wiki on GitHub.

Purpose is a plugin for Emacs which introduces the concept of a
"purpose" for windows and buffers. By setting purposes for your windows
and for your buffers, Purpose helps you maintain a consistent window
layout easily.

For instance, if you find yourself switching between files, interpreter
buffers, help buffers and more, and have a hard time keeping the file
you are actually trying to work on visible, Purpose may be what you
need.


Features
========

Automatic purpose detection
---------------------------

The purpose of a window is the same as the purpose of its current
buffer. The purpose of buffer is determined by its name and its
mode. Purpose comes with a default confiuration, but you can set your
configuration by using the function purpose-set-configuration. The
documentation inside the code (purpose-configuration.el) provides more
information.

The purpose of a window is shown in its modeline, between square
brackets. If the window's purpose is dedicated, an exalamation mark
("!") is added after the purpose's name and before the closing
bracket.

Purpose-aware display
---------------------

Purpose uses advice to override the regular display functions with functions that
are purpose-aware. Also, Purpose provides you with purpose-aware commands.
The main commands are purpose-switch-buffer purpose-pop-buffer.
Furthermore, Purpose uses variable display-buffer-overriding-action to provide
purpose-awareness to function display-buffer.

Programmers can use the hook purpose-select-buffer-hook to execute code
after everytime that Purpose displays a buffer and selects its window.

Purpose dedication
------------------

Commands purpose-toggle-window-purpose-dedicated and
purpose-toggle-window-buffer-dedicated let you mark a window as dedicated
to its current purpose or to its current buffer. Marking a window's
purpose as dedicated means that Purpose won't use this window for
buffers with other purposes. Marking a window's buffer as dedicated
means that it won't be used for other buffers, even if they have the
same purpose. Buffer dedication is a built-in Emacs feature, which
Purpose knows to respect.

Persistent window layout
------------------------

Purpose lets you save and load your window layout with commands such as
purpose-save-window-layout and purpose-load-window-layout.

Programmers can use the hooks purpose-set-window-properties-functions and
purpose-get-extra-window-params-function to save/load other window
properties that may interest them.
