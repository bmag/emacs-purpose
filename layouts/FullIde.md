# Full ide guide for Emacs

<p>
This guide will head you to the fully ready to use IDE in Emacs. The guide consists of 5 parts: organize your emacs layout, browsing your projects, keep list of your opened files, edit your code, different misc functionalities, list of your functions, classes in currently opened file. This layout requires following things to be installed: neo-tree.
</p>

First of all, please see the interface of this emacs window-configuration.

![](https://github.com/sergeyglazyrindev/emacs-purpose/blob/master/layouts/full-ide.png)

## Organize your emacs layout

First of all, install window-purpose emacs package through "package-install" or "el-get". Add to your init.el following elisp code.

```elisp
(defun load-purpose-mode ()
  (interactive)
  (purpose-mode)
  (purpose-full-ide-setup))

(global-set-key (kbd "M-L") 'load-purpose-mode)

    (defun get-only-one-buffer-with-purpose (purpose)
  "Get buffers wih purpose"
  (buffer-name (nth 0 (purpose-buffers-with-purpose purpose))))
```

Then by clicking a key combination "M-L", you will get a full ide window layout. Now it’s time to setup this layout to support technologies you are working with at most.

## Browse your project and files

For this I propose to use neotree emacs plugin + projectile. Neotree initialization very easy, it’s already done in this package. Projectile installation also very simple. Please check documentation of these libraries.

How do you switch to list of the files in your project ?
If you add following to your init.el, then using combination "C-c C-l" you will be able to switch to the buffer with your directory structure.

```elisp
(define-key purpose-mode-map (kbd "C-c C-l")
  (lambda () (interactive) (purpose-switch-buffer (get-only-one-buffer-with-purpose 'Neotree))))

```

## Keep list of your opened files

How do you see the list of currently opened files in your project, in your environment ? For this purpose, we’ve added a buffer with purpose buffers and pointed to this buffer "ibuffers" emacs mode

How do you switch to this buffer ? You may add your own key combination, using an example of the code above, you need to replace only "Neotree" with "buffers" and use the key combination which fits your keyboard "habits".

## Edit your code

The emacs configuration for this need depends on technologies you are working with. I work mostly with python/js/bash, so for my needs I need no extra configuration for emacs-purpose buffer purposes. But you may decide to add your own purposes, please configure it as described below.

```elisp
(add-to-list 'purpose-user-mode-purposes
             '(EMACS_MODE . PURPOSE))
;; don’t forget to recompile user-configuration after you updated list of supported emacs modes
(purpose-compile-user-configuration)

```

You may switch to the file opened in your emacs using key combination "C-c C-c". To enable this key combination, please add the code below to your init.el

```elisp
(define-key purpose-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (purpose-switch-buffer-with-some-purpose 'edit)))

```


## Different misc functionalities

It would be great to show a search results in the misc buffer and get huge benefit of being able to simply switch to the file to edit or analyze some code. For this you need just to configure your purpose-mode-mapping for needed major modes and it will work naturally.

```elisp
(add-to-list 'purpose-user-mode-purposes
             '(inferior-python-mode . misc))
;; don’t forget to recompile user-configuration after you updated list of supported emacs modes
(purpose-compile-user-configuration)

```

It makes sense to add a simple key-combinations to increase the height of buffer to analyze a lot of findings by grep.. etc

The simple elisp solution will do that for you

## Special todo buffer for a todos in your projects

As developers we put to the code @todo markers just to remember what should be fixed, etc. Especially for this purpose we added "special" todo functionality based on org-mode. To see it in action you need to add a bash alias to your environment "collecttodotags". It could be something similar to the text below

```bash
alias collecttodotags="find 'pwd' -type d \( -name .git -o -name node_modules \) -prune -o -type f \( -name todo.org \) -prune -o -type f -print -exec grep -n ’@todo’ ’{}’ \; | create_org_mode_todo_file.py > ./todo.org"
```

You may use the simple of version of todo.org file builder from our repository. Please check out the file "create_org_mode_todo_file.py".

How do you use it efficiently ? We sort @todo mentions by number of "o" letters in the end of this phrase "@todooo". So, if you add a comment

```python
# @todoooo, the socket here should be monkey-patched by socket from gevent
```

this "todo" entry will appear higher than simple "@todo Need a fix".

## List of your functions

It is handled by ilist major mode.

You can get to the list of definitions in currently opened file using this hotkey definition

```elisp
(define-key purpose-mode-map (kbd "C-c C-d")
  (lambda () (interactive)  (purpose-switch-buffer (get-only-one-buffer-with-purpose 'ilist))))

```

Enjoy full ide in emacs
