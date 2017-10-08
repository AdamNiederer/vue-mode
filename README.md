![https://github.com/syl20bnr/spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)
[![MELPA](https://melpa.org/packages/vue-mode-badge.svg)](https://melpa.org/#/vue-mode)
[![Melpa Stable Status](http://melpa-stable.milkbox.net/packages/vue-mode-badge.svg)](http://melpa-stable.milkbox.net/#/vue-mode)

# vue-mode
Emacs major mode for vue.js based on `mmm-mode`.

# Preview

![2016-04-10 10 44 46](https://cloud.githubusercontent.com/assets/5436704/14410955/4f130d5e-ff6e-11e5-87a5-4fbd0008b475.png)

# Install

Emacs users may install the package from MELPA, and then add the following to their `init.el`:

```elisp
(require 'vue-mode)
```

`vue-mode` should then be activated on all files with a `.vue` extension.

## Spacemacs

### There are [two ways](http://spacemacs.org/doc/DOCUMENTATION.html#configure-packages) to install and configure Spacemacs Packages:

#### A. The fastest way: Adding the necessary configurations to the default configuration file `.spacemacs`.

   1. Within any screen in Spacemacs (including the Splash Screen, after loading), press `SPC` `f` `e` `d`.
   2. This will bring you to the edit screen of the `.spacemacs` file.
   3. Locate the section named `dotspacemacs-additional-packages '()`.
   4. The following code should go inside the above parenthesis:
   ```lisp
   (vue-mode :location (recipe
                        :fetcher github
                        :repo "codefalling/vue-mode"))
   ```
   The section should look like this, after the update:
   ```lisp
   dotspacemacs-additional-packages '(
   (vue-mode :location (recipe
                        :fetcher github
                        :repo "codefalling/vue-mode")))
   ```
   5. Locate, in the same file, the section named `defun dotspacemacs/user-config ()`.
   6. The following code should go right in that section:
   ```lisp
   (defun dotspacemacs/init-vue-mode ()
     (use-package vue-mode))
   ```
   • So the section should look like this, after the update:
   ```lisp
   (defun dotspacemacs/user-config ()
   "Configuration function for user code.
   This function is called at the very end of Spacemacs initialization after
   layers configuration.
   This is the place where most of your configurations should be done. Unless it is
   explicitly specified that a variable should be set before a package is loaded,
   you should place your code here."
   (defun dotspacemacs/init-vue-mode ()
     (use-package vue-mode)))
   ```
   7. So we made 2 changes to the `.spacemacs` file, time to save them, press `SPC` `f` `s`.
   8. With the saved file, time to make Spacemacs re-read the configuration, pressing `SPC` `f` `e` `R`.

#### B. The [more complex](http://spacemacs.org/doc/DOCUMENTATION.html#without-a-layer) way: Creating a new specific layer:

   1. Add to your `packages`:
   ```lisp
   (vue-mode :location (recipe
                        :fetcher github
                        :repo "codefalling/vue-mode"))
   ```
   2. Add to your `package.el`:

   ```lisp
   (defun your-layer-name/init-vue-mode ()
     (use-package vue-mode))
   ```


   3. If you want to customize the region background color (default is highlight):

   ```lisp
   (defun your-layer-name/init-vue-mode ()
     (use-package vue-mode
       :config
       ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
       (setq mmm-submode-decoration-level 0)))
   ```

# Q&A

## Why `js-mode` instead of `js2-mode`?

`js2-mode` does not yet work with "multi-mode" modes such as mmm-mode. See https://github.com/mooz/js2-mode/issues/124.

## Mode didn't get updated when `lang` changed

`M-x vue-mode-reparse`
