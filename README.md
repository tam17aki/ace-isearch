ace-isearch.el
===========

## Introduction
`ace-isearch.el` provides a minor mode which combines `isearch` and [`ace-jump-mode`](https://github.com/winterTTr/ace-jump-mode).

The "default" behavior can be summrized as:
- L = 1     : `ace-jump-mode`
- 1 < L < 6 : `isearch`
- L >= 6    : `helm-occur-from-isearch`

where L is the input string length during `isearch`.  When L is 1, after a
few seconds specified by `ace-isearch-input-idle-delay`, `ace-jump-mode` will
be invoked. Of course you can customize the above behaviour.


## Requirements

* Emacs 24 or higher
* [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode)
* [helm](https://github.com/emacs-helm/helm)

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
Specify the function name as `ace-jump-word-mode` or `ace-jump-char-mode` utilzed in invoking `ace-jump-mode`.
You can change this value by `ace-isearch-switch-submode` interactively.

#### `ace-isearch-use-ace-jump` (Default:`t`)
If this variable is set to `nil`, `ace-jump-mode` is never invoked.

#### `ace-isearch-input-idle-delay` (Default：`0.4`)
Delay seconds for invoking `ace-jump-mode` and `ace-isearch-function-from-isearch` described below during isearch.

#### `ace-isearch-input-length` (Default：`6`)
As default behaviour, when the string length during isearch exceeds `ace-isearch-input-length`, 
the function specified by `ace-isearch-funtion-from-isearch` will be invoked.

#### `ace-isearch-function-from-isearch` (Default:`helm-occur-from-isearch`)
Specify the function name invoked when the string length during isearch exceeds `ace-isearch-input-length`.
If [helm-swoop](https://github.com/ShingoFukuyama/helm-swoop) has been installed, helm-swoop can be invoked after isearch:

```el
(setq ace-isearch-funtion-from-isearch 'helm-swoop-from-isearch)
```

Otherwise [swoop](https://github.com/ShingoFukuyama/emacs-swoop) has been installed, swoop can be invoked:

```el
(setq ace-isearch-funtion-from-isearch 'swoop-from-isearch)
```

#### `ace-isearch-use-function-from-isearch` (Default:`t`)
If you don't want to invoke `ace-isearch-funtion-from-isearch`, set this variable to `nil`.

#### `ace-isearch-set-ace-jump-after-isearch-exit`
This functionality is optional.
`ace-jump-mode` will be invoked further using the isearch query after exiting isearch.

You can enable this as follows:

```el
(ace-isearch-set-ace-jump-after-isearch-exit t)
```

#### `ace-isearch-toggle-ace-jump-after-isearch-exit`
Toggle the functionality described above.
