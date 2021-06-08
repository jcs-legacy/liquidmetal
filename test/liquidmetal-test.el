;;; liquidmetal-test.el --- liquidmetal tests      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jen-Chieh Shen

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for lsp-protocol.

;;; Code:

(require 'liquidmetal)
(require 'ert)
(require 'debug)

(ert-deftest liquidmetal-test-score ()
  (should (= (liquidmetal-score "FooBar" "foo") 0.9500000000000001))
  (should (= (liquidmetal-score "FooBar" "fb") 0.9166666666666669))
  (should (= (liquidmetal-score "Foo Bar" "fb") 0.9285714285714287))
  (should (= (liquidmetal-score "Foo Bar" "baz") 0.0))
  (should (= (liquidmetal-score "Foo Bar" "") 0.8)))

(provide 'liquidmetal-test)
;;; liquidmetal-test.el ends here
