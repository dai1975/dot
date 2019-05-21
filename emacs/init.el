;;; init.el --- basic facilities
;;; Commentary:

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-g" 'goto-line)

; minibuffer
(setq max-mini-window-height 1)
(setq resize-mini-windows nil)

; search
(setq case-fold-search t)
(setq isearch-case-fold-search t)

(setq-default indent-tabs-mode nil)
(setq tab-width 5)
(setq c-basic-offset 3)

;; --- package -----------------------------------------------
(require 'package)

(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/") t)

(package-initialize)

(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; --- autoinsert -----------------------------------------------
(setq  auto-insert-directory (concat mydotdir "/emacs/autoinsert-templates/"))
(load "autoinsert" t)
(setq auto-insert-alist 
      (append (list
               '("\\.py" . "python.py")
               '("SConscript" . "python.py")
               '("SConstruct" . "python.py")
               '("daikfile" . "python.py")
               '("\\.h" . "cpp.h")
               '("\\.cpp" . "cpp.cpp")
;               '("\\.rb" . "foo.rb")
;               '("[Mm]akefile" . "Makefile")
;               '("[Mm]akefile" . "Makefile")
                ) auto-insert-alist ))
(add-hook 'find-file-hooks 'auto-insert)

;; --- face -----------------------------------------------
(set-face-foreground 'font-lock-comment-face "#f69933")
(set-face-foreground 'font-lock-comment-delimiter-face "#f69933")

;; --- w3m -----------------------------------------------
; (setq w3m-key-binding 'info)
(add-hook 'w3m-mode-hook
          '(lambda ()
             (setq w3m-key-binding 'lynx)
             (define-key w3m-mode-map [up]    'previous-line)
             (define-key w3m-mode-map [down]  'next-line)
             (define-key w3m-mode-map [left]  'backward-char)
             (define-key w3m-mode-map [right] 'forward-char)
             ))

;; --- shell/terminal -----------------------------------------------
;; http://sakito.jp/emacs/emacsshell.html
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

(cond
 ((or (eq window-system 'mac) (eq window-system 'ns))
  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は NFD (の様な物)で扱うため以下の設定をする必要がある
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 ((or (eq system-type 'cygwin) (eq system-type 'windows-nt))
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  ;; もしコマンドプロンプトを利用するなら sjis にする
  ;; (setq file-name-coding-system 'sjis)
  ;; (setq locale-coding-system 'sjis)
  ;; 古い Cygwin だと EUC-JP にする
  ;; (setq file-name-coding-system 'euc-jp)
  ;; (setq locale-coding-system 'euc-jp)
  )
 (t
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)))

(setq system-uses-terminfo nil)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; --- edit ---------------------------------------------
(use-package editorconfig :ensure t
             :init
             (editorconfig-mode 1)
             )

(use-package skk ;:ensure t
             :bind (("C-j" . skk-kakutei))
             :config
             (setq default-input-method "japanese-skk")
             (require 'skk-study)
             )

(use-package json-mode :ensure t
             :mode
             ("\\.json\\'" . json-mode)
             )
(use-package toml-mode :ensure t
             :mode
             ("\\.toml\\'" . toml-mode)
             )
(use-package yaml-mode :ensure t
             :mode
             ("\\.ya?ml\\'" . yaml-mode)
             )
(use-package text-mode ;:ensure t
             :mode
             ("\\.txt\\'" . text-mode)
             :init
             (remove-hook 'text-mode-hook 'turn-on-auto-fill)
             :config
             (add-hook 'text-mode-hook
                       (lambda ()
                         (turn-off-auto-fill)
                         ))
             )
(use-package markdown-mode :ensure t
             :mode
             ("\\.md\\'" . markdown-mode)
             )
(use-package org :ensure t
             :mode
             ("\\.org\\'" . org-mode)
             )

;; --- simplenote -----------------------------------------------
(use-package simplenote2 :ensure t
             :config
             (setq simplenote2-email "dai1975@gmail.com")
             (simplenote2-setup)
             )

;; --- programming -----------------------------------------------
(use-package lsp-mode :ensure t :commands lsp)
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)

(use-package yasnippet :ensure t)

(use-package quickrun ;:ensure t
             :init
             (global-set-key (kbd "C-c C-q") 'quickrun)
             )

(use-package flycheck :ensure t
             :config
             (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
             )
(use-package company :ensure t
             :init
             (bind-keys :map mode-specific-map
                        ("M-n" . nil)
                        ("M-p" . nil)
                        ("C-n" . company-select-next)
                        ("C-p" . company-select-previous)
                        ("C-h" . nil)
                        ("<tab>" . company-complete-common-or-cycle)
                        ("M-d" . company-show-doc-buffer))
             :config
             (setq company-minimum-prefix-length 1)
             (setq company-selection-wrap-around t)
             (set-face-attribute 'company-tooltip nil
                                 :foreground "black"
                                 :background "lightgray")
             (set-face-attribute 'company-preview-common nil
                                 :foreground "dark gray"
                                 :background "black"
                                 :underline t)
             (set-face-attribute 'company-tooltip-selection nil
                                 :background "steelblue"
                                 :foreground "white")
             (set-face-attribute 'company-tooltip-common nil
                                 :foreground "black"
                                 :underline t)
             (set-face-attribute 'company-tooltip-common-selection nil
                                 :foreground "white"
                                 :background "steelblue"
                                 :underline t)
             (set-face-attribute 'company-tooltip-annotation nil
                                 :foreground "red")
             )

;(use-package gtags-mode :ensure t
;             :init
;             (bind-keys :map mode-specific-map
;                        ("C-q s" . gtags-find-symbol)
;                        ("C-q t" . gtags-find-tag)
;                        ("C-q r" . gtags-find-rtag)
;                        ("C-q f" . gtags-find-file)
;                        ("C-q p" . gtags-pop-stack))
;             )
             
(use-package c-mode ;:ensure t
             :mode
             ("\\.c\\'" . c-mode)
             :init
             (add-hook 'c-mode-common-hook
                       (lambda ()
                         (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                           ;(gtags-mode 1)
                           )))
)
             
(use-package c++-mode ;:ensure t
             :mode (("\\.h\\'"   . c++-mode)
                    ("\\.cpp\\'" . c++-mode))
             :init
             (add-hook 'c++-mode-hook
                       (lambda ()
                         (flycheck-mode)
                         ;(c-set-style "stroustrup")
                         ;(setq indent-tabs-mode nil)
                         ;(setq c-basic-offset 3)
                         (c-set-offset 'innamespace 0)
                         (c-set-offset 'arglist-close 0)
                         ))
             )

(use-package rust-mode :ensure t
             :mode
             ("\\.rs\\'" . rust-mode)
             :init
             (add-hook 'rust-mode-hook
                       (lambda ()
                         (cargo-minor-mode)
                         (flycheck-mode)
                         (lsp)
                         ))
             )
(use-package cargo :ensure t :commands (cargo-minor-mode))


(use-package tide :ensure t)
(use-package typescript-mode :ensure t
             :mode
             ("\\.ts\\'" . typescript-mode)
             :init
             (add-hook 'typescript-mode-hook
                       (lambda ()
                         (tide-setup)
                         ;(flycheck-mode t)
                         ;(setq flycheck-check-syntax-automatically '(save mode-enabled))
                         (eldoc-mode t)
                         ;(tide-hl-identifier-mode t)
                         (company-mode t)
                         ))
             )


(use-package web-mode :ensure t
             :mode
             ("\\.html?\\'" . web-mode)
             ("\\.js\\'" . web-mode)
             ("\\.tsx\\'" . web-mode)
             ("\\.jsx\\'" . web-mode)
             :init
             (add-hook 'web-mode-hook
                       (lambda ()
                         (let ((ext (file-name-extension buffer-file-name)))
                           (cond ((string-equal "tsx" ext) (setup-tide-mode))
                                 ))))
             :config
             (setq web-mode-content-type-alist
                   '(("jsx" . "\\.js[x]?\\'")
                     ))
             (custom-set-faces
              '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
              '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
              '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
              '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
              '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
              '(web-mode-comment-face           ((t (:foreground "#587F35"))))
              '(web-mode-server-comment-face    ((t (:foreground "#587F35"))))
              
              '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
              '(web-mode-comment-face           ((t (:foreground "#587F35"))))
              '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
              '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
              '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
              '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
              )
             )


; --------------------------------------------------
(load "my-company.el")
