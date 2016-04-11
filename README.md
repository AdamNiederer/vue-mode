![https://github.com/syl20bnr/spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)

# vue-mode
Emacs major mode for vue.js based on `mmm-mode`.

# Preview

![2016-04-10 10 44 46](https://cloud.githubusercontent.com/assets/5436704/14410955/4f130d5e-ff6e-11e5-87a5-4fbd0008b475.png)

# Install
## Spacemacs

Add
```lisp
(vue-mode :location (recipe
                     :fetcher github
                     :repo "codefalling/vue-mode"))
```

to your `packages`.and 

```lisp
(defun your-layer-name/init-vue-mode ()
  (use-package vue-mode))
```

to your `package.el`
