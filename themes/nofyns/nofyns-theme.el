;;; nofyns-theme.el --- Emacs 24 theme with a dark background.

;; Copyright (C) 2014 , Aleksandr Ivanov <4lex1v@gmail.com>

;; Author: Aleksandr Ivanov <4lex1v@gmail.com>
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

(deftheme nofyns "Nofyns color theme for Emacs")

(defun colour-join (r g b)
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colour-blend (c1 c2 alpha)
  (apply #'colour-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))  

(defun blend (c1 c2 rate)
  (colour-blend
   (color-values c1)
   (color-values c2)
   rate))

(let* ((class '((class color) (min-colors 89)))
       (back  "#D6D6D6")
       (front "#000000")

       (keyword front)
       (comment "#005800")
       (str     comment)
       (warning front)

       (var     front)
       (func    front)
       (builtin front)

       (link  front)
       (const front)
       (type  front))
  (custom-theme-set-faces
   'nofyns

   ;; General
   `(default                             ((,class (:foreground ,front :background ,back)))) 
   `(region                              ((,class (:background ,"#9E9E9E")))) 
   `(cursor                              ((,class (:background ,front)))) 
   `(fringe                              ((,class (:background ,back))))
   `(hl-line                             ((,class (:background "#9E9E9E"))))
   `(link                                ((,class (:foreground ,builtin :underline t :weight bold))))
   `(link-visited                        ((,class (:foreground ,builtin :underline t :weight normal))))
   `(header-line                         ((,class (:background ,back))))
   `(icompletep-determined               ((,class (:foreground ,builtin))))
   `(slime-repl-inputed-output-face      ((,class (:foreground ,type))))
   `(trailing-whitespace                 ((,class (:foreground ,back :background ,warning))))
   `(info-quoted-name                    ((,class (:foreground ,builtin))))
   `(info-string                         ((,class (:foreground ,str))))
   `(ffap                                ((,class (:foreground ,front))))
   `(lazy-highlight                      ((,class (:foreground ,front :background ,back))))

   ;; States
   `(success                             ((,class (:foreground ,str))))
   `(warning                             ((,class (:foreground ,warning))))
   `(error                               ((,class (:foreground ,keyword))))

   ;; Mode line
   `(mode-line                           ((,class (:foreground ,back :background ,front :box nil))))
   `(mode-line-inactive                  ((,class (:foreground ,back :background "#9E9E9E" :box nil))))
   `(mode-line-highlight                 ((,class (:foreground ,front :background ,back))))
   `(mode-line-emphasis                  ((,class (:foreground ,back :background ,front :box nil))))
   
   ;; Font Lock
   `(font-lock-keyword-face              ((,class (:foreground ,keyword))))
   `(font-lock-comment-face              ((,class (:foreground ,comment))))
   `(font-lock-builtin-face              ((,class (:foreground ,keyword))))
   `(font-lock-constant-face             ((,class (:foreground ,const))))
   `(font-lock-doc-face                  ((,class (:foreground ,comment))))
   `(font-lock-doc-string-face           ((,class (:foreground ,comment))))
   `(font-lock-function-name-face        ((,class (:foreground ,builtin))))
   `(font-lock-type-face                 ((,class (:foreground ,const))))
   `(font-lock-preprocessor-face         ((,class (:foreground ,keyword))))
   `(font-lock-negation-char-face        ((,class (:foreground ,const))))
   `(font-lock-string-face               ((,class (:foreground ,str))))
   `(font-lock-variable-name-face        ((,class (:foreground ,builtin))))
   `(font-lock-warning-face              ((,class (:foreground ,keyword :underline t))))

   ;; Other UI general faces
   `(minibuffer-prompt                   ((,class (:foreground ,keyword :bold t))))
   `(linum                               ((,class (:foreground ,front :background ,back))))
   `(show-paren-match-face               ((,class (:foreground ,back :background ,front))))

   ;; ISearch
   `(isearch                             ((,class (:foreground ,const :background ,back :underline t))))
   `(isearch-fail                        ((,class (:foreground ,warning :background ,back :bold t))))

   ;; Dired
   `(dired-directory                     ((,class (:foreground ,keyword :underline t))))
   
   ;; Latex
   `(font-latex-bold-face                ((,class (:bold   t))))
   `(font-latex-italic-face              ((,class (:italic t))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   
   ;; Org-mode
   `(org-level-1                         ((,class (:bold t :foreground ,front :height 1.1 :underline t))))
   `(org-level-2                         ((,class (:bold t :foreground ,front :underline t))))
   `(org-level-3                         ((,class (:bold t :foreground ,front :underline t))))
   `(org-level-4                         ((,class (:bold t :foreground ,front :underline t))))
   `(org-checkbox                        ((,class (:bold t :foreground ,keyword))))
   `(org-checkbox-statistics-todo        ((,class (:bold t :foreground ,keyword))))
   `(org-checkbox-statistics-done        ((,class (:bold t :foreground ,str))))
   `(org-scheduled                       ((,class (:foreground ,front))))
   `(org-scheduled-today                 ((,class (:inherit 'org-scheduled))))
   `(org-upcoming-deadline               ((,class (:underline t))))
   `(org-code                            ((,class (:foreground ,front))))
   `(org-hide                            ((,class (:foreground ,front))))
   `(org-date                            ((,class (:underline t :foreground ,builtin) )))
   `(org-footnote                        ((,class (:underline t :foreground ,front))))
   `(org-link                            ((,class (:underline t :foreground ,link))))
   `(org-special-keyword                 ((,class (:foreground ,func))))
   `(org-verbatim                        ((,class (:foreground ,builtin :bold t))))
   `(org-block                           ((,class (:foreground ,front))))
   `(org-quote                           ((,class (:inherit org-block :slant italic))))
   `(org-verse                           ((,class (:inherit org-block :slant italic))))
   `(org-warning                         ((,class (:underline t :foreground ,warning))))
   `(org-ellipsis                        ((,class (:foreground ,builtin))))
   `(org-document-info-keyword           ((,class (:foreground ,func))))
   `(org-sexp-date                       ((,class (:foreground ,front))))
   `(org-tag                             ((,class (:foreground ,keyword))))
   `(org-todo                            ((,class (:foreground ,keyword))))
   `(org-done                            ((,class (:foreground ,str))))

   ;; Org-agenda
   `(org-agenda-done                     ((,class (:strike-through t :italic t :foreground ,comment))))
   `(org-agenda-structure                ((,class (:weight bold :foreground ,front :box (:color ,front) :background ,back))))
   `(org-agenda-date                     ((,class (:foreground ,var))))
   `(org-agenda-date-weekend             ((,class (:inherit 'org-agenda-date))))
   `(org-agenda-date-today               ((,class (:weight bold :foreground ,const :height 1.2))))
   `(org-agenda-dimmed-todo-face         ((,class (:foreground ,comment))))

   ;; Helm
   `(helm-header                         ((,class (:foreground ,back :background ,front :bold t))))
   `(helm-source-header                  ((,class (:foreground ,back :background ,front :underline nil :bold t))))
   `(helm-match                          ((,class (:foreground ,keyword :underline t))))
   `(helm-visible-mark                   ((,class (:foreground ,warning :background ,back))))
   `(helm-selection                      ((,class (:background "#9E9E9E")))) ;; Used to highlight the line in helm buffers
   `(helm-selection-line                 ((,class (:background ,back)))) ;; Used in helm-current-buffer
   `(helm-candidate-number               ((,class (:foreground ,back :background ,front))))
   `(helm-separator                      ((,class (:foreground ,type))))
   `(helm-time-zone-current              ((,class (:foreground ,builtin))))
   `(helm-time-zone-home                 ((,class (:foreground ,type))))
   `(helm-buffer-not-saved               ((,class (:foreground ,type))))
   `(helm-buffer-process                 ((,class (:foreground ,builtin))))
   `(helm-buffer-saved-out               ((,class (:foreground ,front))))
   `(helm-buffer-size                    ((,class (:foreground ,front))))
   `(helm-ff-directory                   ((,class (:foreground ,str :weight bold))))
   `(helm-ff-file                        ((,class (:foreground ,front :weight normal))))
   `(helm-ff-executable                  ((,class (:foreground ,front :weight normal))))
   `(helm-ff-invalid-symlink             ((,class (:foreground ,front :weight bold))))
   `(helm-ff-symlink                     ((,class (:foreground ,keyword :weight bold))))
   `(helm-ff-prefix                      ((,class (:foreground ,back :background ,keyword :weight normal))))
   `(helm-grep-cmd-line                  ((,class (:foreground ,front))))
   `(helm-grep-file                      ((,class (:foreground ,front))))
   `(helm-grep-finish                    ((,class (:foreground ,front))))
   `(helm-grep-lineno                    ((,class (:foreground ,front))))
   `(helm-grep-match                     ((,class (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running                   ((,class (:foreground ,func))))
   `(helm-moccur-buffer                  ((,class (:foreground ,func))))

   ;; Evil
   `(evil-ex-substitute-matches          ((,class (:foreground ,keyword :underline t :strike-through t))))
   `(evil-ex-substitute-replacement      ((,class (:foreground ,str :underline t))))
   
   ;; Macrostep
   `(macrostep-expansion-highlight-face  ((,class (:background ,back))))
   
   ;; Hideshow
   `(hs-face                             ((,class (:foreground ,comment :background ,back))))

   ;; Magit
   `(magit-process-ok                    ((,class (:foreground ,front))))
   `(magit-item-highlight                ((,class (:background ,back))))
   `(magit-section-heading               ((,class (:foreground ,keyword :weight bold))))
   `(magit-hunk-heading                  ((,class (:background ,back))))
   `(magit-section-highlight             ((,class (:background ,back))))
   `(magit-hunk-heading-highlight        ((,class (:background ,back))))
   `(magit-diff-context-highlight        ((,class (:background ,back :foreground ,front))))
   `(magit-diffstat-added                ((,class (:foreground ,type))))
   `(magit-diffstat-removed              ((,class (:foreground ,var))))
   `(magit-process-ok                    ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng                    ((,class (:foreground ,warning :weight bold))))
   `(magit-branch                        ((,class (:foreground ,const :weight bold))))
   `(magit-log-author                    ((,class (:foreground ,front))))
   `(magit-hash                          ((,class (:foreground ,front))))
   `(magit-diff-file-header              ((,class (:foreground ,front :background ,back))))
   `(magit-diffstat-added                ((,class (:foreground ,str))))
   `(magit-diffstat-removed              ((,class (:foreground ,keyword))))

   ;; Eshell
   `(eshell-prompt                       ((,class (:foreground ,keyword))))
   
   ;; Company
   `(company-echo-common                 ((,class (:foreground ,back :background ,front))))
   `(company-tooltip                     ((,class (:foreground ,back :background ,front))))
   `(company-tooltip-selection           ((,class (:foreground ,back :background ,front))))
   `(company-tooltip-common              ((,class (:foreground ,back :background ,front))))
   `(company-tooltip-common-selection    ((,class (:background ,front :underline t))))
   `(company-scrollbar-bg                ((,class (:background ,front))))
   `(company-scrollbar-fg                ((,class (:background ,front))))
   `(company-template-field              ((,class (:inherit region))))
   `(company-preview-search              ((,class (:inherit match))))
   
   ;; Elfeed
   `(elfeed-search-date-face             ((,class (:foreground ,const))))
   `(elfeed-search-feed-face             ((,class (:foreground ,const))))
   `(elfeed-search-tag-face              ((,class (:foreground ,keyword))))
   `(elfeed-search-title-face            ((,class (:foreground ,comment))))
   `(elfeed-search-unread-title-face     ((,class (:foreground ,front))))
   
   ;; Mardown
   `(markdown-markup-face                ((,class (:foreground ,comment :background ,back))))
   `(markdown-list-face                  ((,class (:foreground ,comment :background ,back))))
   `(markdown-link-face                  ((,class (:foreground ,str :background ,back))))
   `(markdown-url-face                   ((,class (:foreground ,keyword :background ,back))))
   `(markdown-italic-face                ((,class (:foreground ,front :background ,back))))
   `(markdown-bold-face                  ((,class (:foreground ,front :background ,back))))
   `(markdown-strike-through-face        ((,class (:foreground ,front :background ,back))))
   `(markdown-header-rule-face           ((,class (:foreground ,comment :background ,back))))
   `(markdown-header-delimiter-face      ((,class (:foreground ,comment :background ,back))))
   `(markdown-header-face-1              ((,class (:foreground ,comment :background ,back))))
   `(markdown-header-face-2              ((,class (:foreground ,comment :background ,back))))
   `(markdown-header-face-3              ((,class (:foreground ,comment :background ,back))))
   `(markdown-blockquote-face            ((,class (:foreground ,front :background ,back))))
   `(markdown-code-face                  ((,class (:foreground ,front :background ,back))))
   `(markdown-code-face                  ((,class (:foreground ,front :background ,back))))
   `(markdown-inline-code-face           ((,class (:foreground ,front :background ,back))))
   `(markdown-pre-face                   ((,class (:foreground ,front :background ,back))))
   `(markdown-language-keyword-face      ((,class (:foreground ,front :background ,back))))
   `(markdown-language-info-face         ((,class (:foreground ,front :background ,back))))
   `(markdown-missing-link-face          ((,class (:foreground ,front :background ,back))))
   `(markdown-reference-face             ((,class (:foreground ,front :background ,back))))
   `(markdown-footnote-face              ((,class (:foreground ,front :background ,back))))
   `(markdown-footnote-marker-face       ((,class (:foreground ,front :background ,back))))
   `(markdown-footnote-text-face         ((,class (:foreground ,front :background ,back))))
   `(markdown-plain-url-face             ((,class (:foreground ,front :background ,back))))
   `(markdown-link-title-face            ((,class (:foreground ,front :background ,back))))
   `(markdown-line-break-face            ((,class (:foreground ,front :background ,back))))
   `(markdown-comment-face               ((,class (:foreground ,front :background ,back))))
   `(markdown-math-face                  ((,class (:foreground ,front :background ,back))))
   `(markdown-metadata-key-face          ((,class (:foreground ,front :background ,back))))
   `(markdown-metadata-value-face        ((,class (:foreground ,front :background ,back))))
   `(markdown-gfm-checkbox-face          ((,class (:foreground ,front :background ,back))))
   `(markdown-highlight-face             ((,class (:foreground ,front :background ,back))))
   `(markdown-hr-face                    ((,class (:foreground ,front :background ,back))))

   ;; Web-Mode
   `(web-mode-keyword-face               ((,class (:foreground ,keyword))))
   `(web-mode-string-face                ((,class (:foreground ,str))))
   `(web-mode-html-attr-name-face        ((,class (:foreground ,const))))
   `(web-mode-html-attr-value-face       ((,class (:foreground ,str))))
   `(web-mode-html-tag-face              ((,class (:foreground ,builtin))))
   `(web-mode-builtin-face               ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face               ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face              ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face               ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face         ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-type-face                  ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-warning-face               ((,class (:inherit ,font-lock-warning-face))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nofyns)

;;; nofyns-theme.el ends here
