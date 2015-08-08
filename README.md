ace-isearch [![MELPA](http://melpa.org/packages/ace-isearch-badge.svg)](http://melpa.org/#/ace-isearch) [![MELPA Stable](http://stable.melpa.org/packages/ace-isearch-badge.svg)](http://stable.melpa.org/#/ace-isearch)
===========

## Introduction
`ace-isearch.el` provides a minor mode which combines `isearch`,  [`ace-jump-mode`](https://github.com/winterTTr/ace-jump-mode), 
[`avy`](https://github.com/abo-abo/avy), and
[`helm-swoop`](https://github.com/ShingoFukuyama/helm-swoop).

The "default" behavior can be summrized as:
- L = 1     : `ace-jump-mode` or `avy`
- 1 < L < 6 : `isearch`
- L >= 6    : `helm-swoop`

where L is the input string length during `isearch`.  When L is 1, after a
few seconds specified by `ace-isearch-jump-delay`, `ace-jump-mode` or `avy` will
be invoked. Of course you can customize the above behaviour.

## Requirements

* Emacs 24 or higher
* [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode)
* [avy](https://github.com/abo-abo/avy)
* [helm-swoop](https://github.com/ShingoFukuyama/helm-swoop)

## Installation

You can install `ace-isearch.el` from [MELPA](http://melpa.org/#/ace-isearch) with `package.el`

```
 M-x package-install ace-isearch
```

Otherwise you can install it by [el-get](https://github.com/dimitri/el-get/blob/master/recipes/ace-isearch.rcp).

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

#### `ace-isearch-funciton` (Default:`ace-jump-word-mode`)
Specify the function name utilized in invoking `ace-jump-mode` or `avy`.
You should specify `ace-jump-word-mode`, `ace-jump-char-mode`, 
`avy-goto-word-1`, `avy-goto-subword-1`, `avy-goto-word-or-subword-1`, or `avy-goto-char`.

#### `ace-isearch-switch-function`
You can switch the value of `ace-isearch-funciton` interactively.

#### `ace-isearch-use-jump` (Default:`t`)
If this variable is set to `nil`, `ace-jump-mode` or `avy` is never invoked.

If set to `t`, it is always invoked if the length of `isearch-string` is equal to 1.

If set to `printing-char`, it is invoked only if you hit a printing character to search for as a first input.
This prevents it from being invoked when repeating a one character search, yanking a character or calling
`isearch-delete-char` leaving only one character.

#### `ace-isearch-jump-delay` (Default：`0.3`)
Delay seconds for invoking `ace-jump-mode` or `avy` during isearch.

#### `ace-isearch-func-delay` (Default：`0.0`)
Delay seconds for invoking `ace-isearch-function-from-isearch` during isearch, which is described below.

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

#### `ace-isearch-fallback-function`  (Default:`helm-swoop-from-isearch`)
This functionality is optional.
When isearch fails and `ace-isearch-use-fallback-function` is non-nil,
`ace-isearch-fallback-function` will be invoked as a fallback function.

You should specify the symbol name of function which uses `isearch-string`, the query string during isearch.
For a trivial example, you can specify it as follows:

```el
(defun my-fallback-function ()
  (message "Your isearch string is %s", isearch-string))
  
(setq ace-isearch-use-function-from-isearch t)
(setq ace-isearch-fallback-function 'my-fallback-function)
```

#### `ace-isearch-use-fallback-function`  (Default:`nil`)
If this variable is set to non-nil, `ace-isearch-fallback-function` will be invoked
when isearch fails.

#### `ace-isearch-jump-during-isearch`
With this function, `ace-jump-mode` will be invoked further during isearch, which enables to jump to the one of the isearch candidates.
This helps to reduce many key repeats of `C-s` or `C-r`.

#### `ace-isearch-pop-mark`
You can invoke `ace-jump-mode-pop-mark` or `avy-pop-mark` in accordance with the current `ace-isearch-funciton`. With this function, you can jump back to the last location of `ace-jump-mode` or `avy`. 

## Sample Configuration
```el
(require 'ace-isearch)
(global-ace-isearch-mode +1)

(custom-set-variables
 '(ace-isearch-input-length 7)
 '(ace-isearch-jump-delay 0.25)
 '(ace-isearch-function 'avy-goto-char)
 '(ace-isearch-use-jump 'printing-char))
 
(define-key isearch-mode-map (kbd "C-'") 'ace-isearch-jump-during-isearch)
```
