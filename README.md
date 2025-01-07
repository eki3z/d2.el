# d2.el

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](https://melpa.org/packages/d2-badge.svg)](https://melpa.org/#/d2)

D2 diagram toolchain for emacs, provide d2-ts-mode and d2 commands.

<!-- markdown-toc start -->

## Contents

- [d2.el](#d2el)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Feature](#feature)
  - [Usage](#usage)
    - [Install treesit grammer](#install-treesit-grammer)
    - [load library](#load-library)
  - [Customization](#customization)
  - [Todo](#todo)
  - [Donate](#donate)

<!-- markdown-toc end -->

## Install

### dependencies

- Emacs, version >= 29.1
- [transient](https://github.com/magit/transient)
- [treesit-d2](https://codeberg.org/p8i/tree-sitter-d2)
- [D2](https://github.com/terrastruct/d2?tab=readme-ov-file#install)

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA].
Install with `M-x package-install` `RET` `d2` within Emacs.

## Feature

- provide major-mode `d2-ts-mode`
- provide command `d2-menu`

## Usage

### Install treesit grammar

Target d2 treesit grammar: https://codeberg.org/p8i/tree-sitter-d2

### load library

```elisp
(require 'd2)
(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-ts-mode))
```

## Customization

See defcustom in d2.el file.

## Todo

- [ ] support syntax highlight in injection code block
- [ ] support region rendering in file
- [ ] provide more indent rules

## Donate

If you think the it's helpful for you, please consider paying a cup of coffee
for me. Thank you! :smile:

<a href="https://paypal.me/liuyinz" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>
