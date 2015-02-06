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

1) Automatic purpose detection
------------------------------
   
   The purpose of a window is the same as the purpose of its current
   buffer. The purpose of buffer is determined by its name and its
   mode. Purpose comes with a default confiuration, but you can set your
   configuration by using the function pu:set-configuration. The
   documentation inside the code (pu-configuration.el) provides more
   information.
   
   The purpose of a window is shown in its modeline, between square
   brackets. If the window's purpose is dedicated, an exalamation mark
   ("!") is added after the purpose's name and before the closing
   bracket.
   
2) Purpose-aware display
------------------------
   
   Purpose provides you with commands for displaying buffers in the
   correct windows. The main commands are pu:find-file, pu:switch-buffer
   and pu:pop-buffer. Also, Purpose uses variable
   display-buffer-overriding-action to provide purpose-awareness to
   function display-buffer and anything else that uses display-buffer
   behind the scenes.
   
   Programmers can use the hook pu:display-buffer-hook to execute code
   after everytime that Purpose displays a buffer.
   
   For now, features and plugins that don't use display-buffer will not
   be handled properly by Purpose. This includes switch-to-buffer,
   Dired, recentf, and probably some more. Whenever you encounter a
   feature that isn't handled properly by Purpose, you are welcome to
   report it. If possible, I encourage you to solve it and send me the
   fix via a pull-request. Often the solution is to make the feature use
   pu:switch-buffer instead of switch-to-buffer. With time, and as
   Purpose gains popularity (hopefully) and becomes mature, Purpose will
   handle more and more of those features and plugins.
   
3) Purpose dedication
---------------------
   
   Commands pu:toggle-window-purpose-dedicated and
   pu:toggle-window-buffer-dedicated let you mark a window as dedicated
   to its current purpose or to its current buffer. Marking a window's
   purpose as dedicated means that Purpose won't use this window for
   buffers with other purposes. Marking a window's buffer as dedicated
   means that it won't be used for other buffers, even if they have the
   same purpose. Buffer dedication is a built-in Emacs feature, which
   Purpose knows to respect.
   
4) Persistent window layout
---------------------------
   
   Purpose lets you save your current window layout with the command
   pu:save-layout. It also lets you load a saved window layout with the
   command pu:load-layout.
   
   Programmers can use the hooks pu:set-window-properties-functions and
   pu:get-extra-window-params-function to save/load other window
   properties that may interest them.

   
Caveats
=======

- Frame support is not yet available. Purpose may not work as expected
  when using Emacs with several frames.

- Regexp support is not yet available.

- Emacs features and plugins which don't use display-buffer are not yet
  integrated with Purpose.

  Often the solution is to use pu:switch-buffer instead of
  switch-to-buffer in strategic locations in those features' code. Bug
  reports and code contributions are very welcome, and may help with
  coming up with a more complete solution for integrating features with
  Purpose.


TODO List
=========

Here are listed tasks that should be completed in the future. You are
welcome to contribute and complete any of the tasks below. The marks
[#A], [#B] and [#C] note the importance of a task.

Version 1.0 Requirements
------------------------

Version 1.0 will not be considered ready until these tasks are
completed.

- [DONE] [#A] implement function pu:find-file
  
- [#A] regexp support for purpose detection (implement
  pu:buffer-purpose-name-regexp)
  
- [#B] prefix-overload (overload commands on same key, with different
  prefix arguments)
  
- [#B] make pu:load-layout and pu:save-layout prompt for filename when
  called interactively
  
- [#B] set good key-bindings for Purpose's commands
   
Not Version-Specific
--------------------

Tasks that should be done, but not necessarily for a specific version.

- [DONE] [#C] display window purpose in modeline
  
- [#A] frame support: design and write frame-related behavior and code
  
- [#A] consider smarter function-call decision in pu:action-function
  
  Which function to use, and how to call it (pu:switch-buffer,
  pu:pop-buffer). Pending more usage-experience, suggestions and
  bug-reports.
  
- [#B] pu:switch-buffer-with-same-purpose

  Like pu:switch-buffer, but choice is limited to buffers with the same
  purpose as the current buffer.
  
- [#B] make prompts better (more like IDO, more feature-rich)
  Currently, the prompts of Purpose's commands don't have as much
  features as IDO prompts have. Maybe there is a way to get all of the
  IDO's benefits, and not only its nice completion.
  
- [#B] automatic tests
  
- [#B] adapt variables for customize
  
- [#B] add Purpose to a package repository (MELPA?)
  
- [MAYBE] [#C] improve/change default configuration
  
- [#C] refactor code (make it nicer and better)
  
  This task doesn't really have an end.
  
- [MAYBE] [#C] add function delete-non-dedicated-windows
   
Integration With Other Features
-------------------------------

A list of features which need to be integrated with Purpose. This list is expected
to grow, might become a check-list of sorts.

- <TBD>
