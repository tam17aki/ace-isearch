ace-isearch.el
===========

## Introduction
`ace-isearch.el` provides a minor mode which combines `isearch` and [`ace-jump-mode`](https://github.com/winterTTr/ace-jump-mode).

The "default" behavior can be summrized as:
- L = 1     : `ace-jump-mode`
- 1 < L < 6 : `isearch`
- L >= 6    : `helm-swoop-from-isearch`

where L is the input string length during `isearch`.  When L is 1, after a
few seconds specified by `ace-isearch-input-idle-delay`, `ace-jump-mode` will
be invoked. Of course you can customize the above behaviour.

## Requirements

* Emacs 24 or higher
* [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode)
* [helm-swoop](https://github.com/ShingoFukuyama/helm-swoop)

## Installation

You can install `ace-isearch.el` from [MELPA](http://melpa.milkbox.net/) with `package.el`

```
 M-x package-install ace-isearch
```

Otherwise you can install it by [el-get](https://github.com/dimitri/el-get).

## Basic Usage

#### `ace-isearch-mode`

Enable `ace-isearch` minor mode:

```lisp
(ace-isearch-mode +1)
```

#### `global-ace-isearch-mode`

Enable global ace-isearch mode:

```lisp
(global-ace-isearch-mode +1)
```

## Customization

#### `ace-isearch-submode` (Default:`ace-jump-word-mode`)
Specify the function name as `ace-jump-word-mode` or `ace-jump-char-mode` utilized in invoking `ace-jump-mode`.
You can change this value by function `ace-isearch-switch-submode` interactively.

#### `ace-isearch-use-ace-jump` (Default:`t`)
If this variable is set to `nil`, `ace-jump-mode` is never invoked.

#### `ace-isearch-input-idle-delay` (Default：`0.4`)
Delay seconds for invoking `ace-jump-mode` and `ace-isearch-function-from-isearch` described below during isearch.

#### `ace-isearch-input-length` (Default：`6`)
As default behaviour, when the input string length during isearch exceeds `ace-isearch-input-length`, 
the function specified by `ace-isearch-funtion-from-isearch` will be invoked.

#### `ace-isearch-function-from-isearch` (Default:`helm-swoop-from-isearch`)
Specify the function name invoked when the input string length during isearch exceeds `ace-isearch-input-length`.
If [swoop](https://github.com/ShingoFukuyama/emacs-swoop) has been installed, swoop can be invoked:

```el
(setq ace-isearch-funtion-from-isearch 'swoop-from-isearch)
```

In this case, the following setting would be better.

```el
(define-key swoop-map (kbd "C-s") 'swoop-action-goto-line-next)
(define-key swoop-map (kbd "C-r") 'swoop-action-goto-line-prev)
```

Of course you can set this variable to `helm-occur-from-isearch`.

```el
(setq ace-isearch-funtion-from-isearch 'helm-occur-from-isearch)
```

#### `ace-isearch-use-function-from-isearch` (Default:`t`)
If you don't want to invoke `ace-isearch-funtion-from-isearch`, set this variable to `nil`.

#### `ace-isearch-set-ace-jump-after-isearch-exit`
This functionality is optional.
`ace-jump-mode` will be invoked further using the isearch query after exiting isearch.
This helps to reduce many key repeats of `C-s` or `C-r`.

You can enable this as follows:

```el
(ace-isearch-set-ace-jump-after-isearch-exit t)
```

Otherwise you can disable this as follows:

```el
(ace-isearch-set-ace-jump-after-isearch-exit nil)
```

#### `ace-isearch-toggle-ace-jump-after-isearch-exit`
Toggle the functionality described above.

#### `ace-isearch-fallback-function`  (Default:`helm-swoop-from-isearch`)
This functionality is optional.
When isearch fails and `ace-isearch-use-fallback-function` is non-nil,
`ace-isearch-fallback-function` will be invoked as a fallback function.

You shoud specify the symbol name of function which uses `isearch-string`, the query string during isearch.
For a useful example, you can specify it as follows:

```el
(defun my-fallback-function ()
  (funcall ace-isearch-submode (string-to-char isearch-string)))
  
(setq ace-isearch-use-function-from-isearch t)
(setq ace-isearch-fallback-function 'my-fallback-function)
```

In this example, you can invoke `ace-jump-mode` whenever you put one character in isearch.

#### `ace-isearch-use-function-from-isearch`  (Default:`nil`)
If this variable is set to non-nil, `ace-isearch-fallback-function` will be invoked
when isearch fails.
