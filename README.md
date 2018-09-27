![https://github.com/syl20bnr/spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)
[![MELPA](https://melpa.org/packages/vue-mode-badge.svg)](https://melpa.org/#/vue-mode)
[![Melpa Stable Status](http://melpa-stable.milkbox.net/packages/vue-mode-badge.svg)](http://melpa-stable.milkbox.net/#/vue-mode)
[![Build Status](https://travis-ci.org/AdamNiederer/vue-mode.svg?branch=master)](https://travis-ci.org/AdamNiederer/vue-mode)
[![codecov](https://codecov.io/gh/AdamNiederer/vue-mode/branch/master/graph/badge.svg)](https://codecov.io/gh/AdamNiederer/vue-mode)

# vue-mode
Emacs major mode for vue.js based on `mmm-mode`.

# Preview

![2016-04-10 10 44 46](https://cloud.githubusercontent.com/assets/5436704/14410955/4f130d5e-ff6e-11e5-87a5-4fbd0008b475.png)

# Install

Emacs users may install the package from MELPA. `vue-mode` should then be
activated on all files with a `.vue` extension.

## Spacemacs

There are [two
ways](http://spacemacs.org/doc/DOCUMENTATION.html#configure-packages) to install
and configure vue-mode when using spacemacs.

#### The Simple Way, Without a Layer

Spacemacs allows the installation of additional packages outside of its layer
system using the `dotspacemacs-additional-packages` variable. Add `vue-mode` to
this list.

Edit your `~/.spacemacs` file as follows (to find it press `SPC` `f` `e` `d`):

```lisp
dotspacemacs-additional-packages '(vue-mode)
```

#### With a Layer

Creating a layer is a more complicated method of installing the package, but it
allows for greater flexibility, and faster started via autoloading. You can read
more about it here: [Spacemacs Layers](http://spacemacs.org/doc/LAYERS.html).

The following is a minimal `package.el` file for a custom `vue-mode` layer:

```lisp
(setq vue-mode-packages
  '(vue-mode))

(setq vue-mode-excluded-packages '())

(defun vue-mode/init-vue-mode ()
  "Initialize my package"
  (use-package vue-mode))
```

If you want to customize the region background color (default is highlight):

```lisp
(defun vue-mode/init-vue-mode ()
  (use-package vue-mode
               :config
               ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
               (setq mmm-submode-decoration-level 0)))
```

# Q&A

#### Why `js-mode` instead of `js2-mode`?

`js2-mode` does not yet work with "multi-mode" modes such as mmm-mode. See
https://github.com/mooz/js2-mode/issues/124.

#### How can I reload the submodes in a buffer?

Try `M-x vue-mode-reparse`.

#### How do I disable that ugly background color?

Customize `mmm-default-submode-face`. It's an `mmm-mode` default.

Add the following lines to your `.emacs.d/init.el` to set a lighter color:

```lisp
(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face "#fafafa")))
```

Or disable the background color completely in your `.emacs.d/init.el`:

```lisp
(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))
```
