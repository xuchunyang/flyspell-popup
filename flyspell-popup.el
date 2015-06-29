;;; flyspell-popup.el --- Correcting words with Flyspell in popup menus

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/xuchunyang/flyspell-popup
;; Version: 0.1
;; Package-Requires: ((popup "0.5.0"))
;; Created: Sun Jun 28 15:23:05 CST 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Correct the misspelled word with `flyspell' in popup menu.
;;
;; Usage
;; =====
;;
;; To use, put your cursor on or after the misspelled word and call
;; `flyspell-popup-correct'. You can of course bind it to a key as well by adding
;; this to your Emacs initialization file, e.g. ~/.emacs.d/init.el:
;;
;;   (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)

;;; Code:

(require 'flyspell)
(require 'popup)

;;;###autoload
(defun flyspell-popup-correct ()
  "Use popup for flyspell correction.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
        (word (flyspell-get-word))
        (opoint (point)))
    (if (consp word)
        (let ((word  (car word))
              (start (nth 1 word))
              (end   (nth 2 word))
              poss ispell-filter)
          ;; now check spelling of word.
          (ispell-send-string "%\n")	;put in verbose mode
          (ispell-send-string (concat "^" word "\n"))
          ;; wait until Aspell has processed word
          (while (progn
                   (accept-process-output ispell-process)
                   (not (string= "" (car ispell-filter)))))
          ;; Remove leading empty element
          (setq ispell-filter (cdr ispell-filter))
          ;; ispell process should return something after word is sent.
          ;; Tag word as valid (i.e., skip) otherwise
          (or ispell-filter
              (setq ispell-filter '(*)))
          (if (consp ispell-filter)
              (setq poss (ispell-parse-output (car ispell-filter))))
          (cond
           ((or (eq poss t) (stringp poss))
            ;; don't correct word
            t)
           ((null poss)
            ;; ispell error
            (error "Ispell: error in Ispell process"))
           (t
            ;; The word is incorrect, we have to propose a replacement.
            (let ((res
                   (popup-menu*
                    (append
                     (nth 2 poss)
                     (list
                      (popup-make-item (format "Save \"%s\"" word)
                                       :value (cons 'save word))
                      (popup-make-item (format "Accept (session) \"%s\"" word)
                                       :value (cons 'session word))
                      (popup-make-item (format "Accept (buffer) \"%s\"" word)
                                       :value (cons 'buffer word))))
                    :margin t)))
              (cond ((stringp res)
                     (flyspell-do-correct
                      res poss word cursor-location start end opoint))
                    (t
                     (let ((cmd (car res))
                           (wrd (cdr res)))
                       (flyspell-do-correct
                        cmd poss wrd cursor-location start end opoint)))))))
          (ispell-pdict-save t)))))

(provide 'flyspell-popup)
;;; flyspell-popup.el ends here
