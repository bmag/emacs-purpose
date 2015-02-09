=======
Purpose
=======

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

Purpose provides you with commands for displaying buffers in the
correct windows. The main commands are purpose-find-file, purpose-switch-buffer
and purpose-pop-buffer. Also, Purpose uses variable
display-buffer-overriding-action to provide purpose-awareness to
function display-buffer and anything else that uses display-buffer
behind the scenes.

Programmers can use the hook purpose-display-buffer-hook to execute code
after everytime that Purpose displays a buffer.

For now, features and plugins that don't use display-buffer will not
be handled properly by Purpose. This includes switch-to-buffer,
Dired, recentf, and probably some more. Whenever you encounter a
feature that isn't handled properly by Purpose, you are welcome to
report it. If possible, I encourage you to solve it and send me the
fix via a pull-request. Often the solution is to make the feature use
purpose-switch-buffer instead of switch-to-buffer. With time, and as
Purpose gains popularity (hopefully) and becomes mature, Purpose will
handle more and more of those features and plugins.

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

Purpose lets you save your current window layout with the command
purpose-save-layout. It also lets you load a saved window layout with the
command purpose-load-layout.

Programmers can use the hooks purpose-set-window-properties-functions and
purpose-get-extra-window-params-function to save/load other window
properties that may interest them.

   
Caveats
=======

- Frame support is not yet available. Purpose may not work as expected
  when using Emacs with several frames.

- Emacs features and plugins which don't use display-buffer are not yet
  integrated with Purpose.

  Often the solution is to use purpose-switch-buffer instead of
  switch-to-buffer in strategic locations in those features' code. Bug
  reports and code contributions are very welcome, and may help with
  coming up with a more complete solution for integrating features with
  Purpose.
