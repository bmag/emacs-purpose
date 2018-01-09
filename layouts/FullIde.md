# Full ide guide for Emacs

<p>
This guide will head you to the fully ready to use IDE in Emacs. The guide consists of 5 parts: organize your emacs layout, browsing your projects, keep list of your opened files, edit your code, different misc functionalities, list of your functions, classes in currently opened file. This layout requires following things to be installed: neo-tree.
</p>

## Organize your emacs layout

First of all, install window-purpose emacs package through "package-install" or "el-get". Then please create directory YOUR_EMACSD_DIRECTORY/layouts and copy the file full-ide.window-layout there. Add to your init.el following elisp code.

```elisp
(defun load-purpose-mode ()
  (interactive)
  (purpose-x-code1–setup-ibuffer)
  (neotree-toggle)
  (imenu-list-minor-mode)
  (frame-or-buffer-changed-p ’purpose-full-ide-buffers-changed)
  (add-hook ’post-command-hook #’purpose-full-ide-buffers-changed)
  (purpose-load-window-layout-file „YOUREMACSD_PATH/layouts/full-ide.window-layout“)
  (todo-mode-get-buffer-create))

(global-set-key (kbd „M-L“) ’load-purpose-mode)

(defun get-only-one-buffer-with-purpose (purpose)
  „Get buffers wih purpose“
  (buffer-name (nth 0 (purpose-buffers-with-purpose purpose))))
```

Then by clicking a key combination "M-L", you will get a full ide window layout. Now it’s time to setup this layout to support technologies you are working with at most.

## Browse your project and files

For this I propose to use neotree emacs plugin + projectile. Neotree initialization very easy, it’s already done in load-purpose-mode function I mentioned above. Projectile installation also very simple. Please check documentation of these libraries.

How do you switch to list of the files in your project ?
If you add following to your init.el, then using combination "C-c C-l" you will be able to switch to the buffer with your directory structure.

```elisp
(define-key purpose-mode-map (kbd „C-c C-l“)
  (lambda () (interactive) (purpose-switch-buffer (get-only-one-buffer-with-purpose ’dired))))

```

## Keep list of your opened files

How do you see the list of currently opened files in your project, in your environment ? For this purpose, we’ve added a buffer with purpose buffers and point to this buffer "ibuffers" emacs mode

How do you switch to this buffer ? You may add your own key combination, using an example of the code above, you need to replace only "dired" with "buffers" and use the key combination which fits your keyboard "habits".

## Edit your code

The emacs configuration for this need depends on technologies you are working with. I work mostly with python/js/bash, so for my needs it’s enough to add following changes to my init el

```elisp
(add-to-list ’purpose-user-mode-purposes
             ’(css-mode . edit))
(add-to-list ’purpose-user-mode-purposes
             ’(shell-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(eshell-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(term-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(yaml-mode . edit))
(add-to-list ’purpose-user-mode-purposes
             ’(conf-unix-mode . edit))
(purpose-conf „magit-single“
    :regexp-purposes ’((„^\\*magit“ . edit)))

```

You may switch to the file opened in your emacs using key combination "C-c C-c". To enavle this key combination, please add the code below to your initel

```elisp
(define-key purpose-mode-map (kbd „C-c C-c“)
  (lambda () (interactive) (purpose-switch-buffer-with-some-purpose ’edit)))

```


## Different misc functionalities

It would be great to show a search results in the misc buffer and get huge benefit of being able to simply switch to the file to edit or analyze some code. For this you need just to configure your purpose-mode-mapping for needed major modes and it will work naturally.

```elisp
(add-to-list ’purpose-user-mode-purposes
             ’(inferior-python-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(python-inferior-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(org-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(grep-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(gdb-inferior-io-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(fundamental-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(compilation-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(shell-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(eshell-mode . misc))
(add-to-list ’purpose-user-mode-purposes
             ’(term-mode . misc))

```

As you can see I associated a lot of major modes with misc purpose, so all these major modes will open in the buffer with purpose misc.
It makes sense to add a simple key-combinations to increase the height of buffer to analyze a lot of findings by grep.. etc

The simple elisp solution will do that for you

```elisp
(global-set-key „\C-c+“ (lambda () (interactive) (enlarge-window +20)))
(global-set-key „\C-c_“ (lambda () (interactive) (enlarge-window -20)))

```

So, these key-combinations will help you to get full advantages of this buffer with so different purposes.

## List of your functions

It is handled by ilist major mode.

You can handle to the list of definitions in currently opened file using this hotkey definition

```elisp
(define-key purpose-mode-map (kbd „C-c C-d“)
  (lambda () (interactive)  (purpose-switch-buffer (get-only-one-buffer-with-purpose ’ilist))))

```

Enjoy full ide in emacs
