;;; liquidmetal.el --- Quicksilver scoring algorithm, essentially LiquidMetal  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-06-08 12:59:19

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Quicksilver scoring algorithm, essentially LiquidMetal
;; Keyword: fuzzy matching
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/flex

;; This file is NOT part of GNU Emacs.

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
;;
;; Quicksilver scoring algorithm, essentially LiquidMetal
;;

;;; Code:

(require 'cl-lib)

(defconst liquidmetal-no-match 0.0
  "The score indicating a negative match.")

(defconst liquidmetal-match 1.0
  "The score indicating a full-match.")

(defconst liquidmetal-empty 0.8
  "The score to return when the abrreviation string is empty.")

;;;###autoload
(defun liquidmetal-score (string abbreviation)
  "Computes the score of matching STRING with ABBREVIATION.

The return value is in the range 0.0 to 1.0 the later being full-match."
  (let ((len (length abbreviation)))
    (cond ((= 0 len) flex-empty)
          ((> len (length string)) flex-no-match)
          (t (liquidmetal-build-score string abbreviation)))))

(defun liquidmetal-score-all ()
  "")

(defun liquidmetal-build-score (string abbreviation)
  "Calculates the fuzzy score of matching STRING with ABBREVIATION."
  (let ((search (downcase abbreviation)) (abbrev (downcase abbreviation)) all-scores)
    (unless (= (length all-scores) 0)
      ;;
      )
    ))

(provide 'liquidmetal)
;;; liquidmetal.el ends here
