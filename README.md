[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/liquidmetal-badge.svg)](https://melpa.org/#/liquidmetal)
[![MELPA Stable](https://stable.melpa.org/packages/liquidmetal-badge.svg)](https://stable.melpa.org/#/liquidmetal)

# Liquid Metal
> A mimetic poly-alloy of the Quicksilver scoring algorithm

[![CI](https://github.com/jcs-elpa/liquidmetal/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/liquidmetal/actions/workflows/test.yml)

Port from https://github.com/rmm5t/liquidmetal.

## Usage

```el
(liquidmetal-score "FooBar" "foo")   ; 0.950
(liquidmetal-score "FooBar" "fb")    ; 0.917
(liquidmetal-score "Foo Bar" "fb")   ; 0.929
(liquidmetal-score "Foo Bar" "baz")  ; 0.0
(liquidmetal-score "Foo Bar" "")     ; 0.8
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
