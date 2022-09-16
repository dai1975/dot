;;; init.el --- basic facilities
;;; Commentary:

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-g" 'goto-line)
(global-unset-key "\C-x\C-n") ; set-goal-column

; minibuffer
(setq max-mini-window-height 1)
(setq resize-mini-windows nil)

; search
(setq case-fold-search t)
(setq isearch-case-fold-search t)

; common editor settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 5)
(setq-default c-basic-offset 3)
(setq-default line-move-visual nil)

(electric-indent-mode -1)

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
               '("\\.editorconfig" . "editorconfig.ini")
               '("\\.h$" . "cpp.h")
               '("\\.cpp$" . "cpp.cpp")
;               '("\\.rb" . "foo.rb")
;               '("[Mm]akefile" . "Makefile")
;               '("[Mm]akefile" . "Makefile")
                ) auto-insert-alist ))
(add-hook 'find-file-hooks 'auto-insert)

;; --- face -----------------------------------------------
;(set-face-foreground 'font-lock-comment-face "#f69933")
;(set-face-foreground 'font-lock-comment-delimiter-face "#f69933")

;(use-package highlight-indent-guides :ensure t
;  :diminish
;  :hook
;  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
;  :custom
;  ;(highlight-indent-guides-auto-enabled t)
;  (highlight-indent-guides-auto-enabled nil)
;  (highlight-indent-guides-responsive t)
;  (highlight-indent-guides-method 'character)
                                        ;)

(require 'whitespace)
(global-whitespace-mode 1)
(setq whiltespace-style
      '(
        face
        newline
        newline-mark
        spaces
;        space-mark
        tabs
        tab-mark
        trailing
        empty
        )) ;; モードで上書きされちゃう

(setq whitespace-display-mappings
      '(
        ;(newline-mark ?\n [?\u21b2 ?\n])
        ; normal space
        (space-mark ?\u0020 [?\u0020])
        ;(space-mark ?\u0020 [?\u2e31]) ; word separator middle dot

        ; japanese zenkaku spacenormal space.
        (space-mark ?\u3000 [?\u25a1])

        ; wrong tab stop
        ;(tab-mark ?\t [?\u00bb ?\t])
        ))

(setq whitespace-space-regexp "\\(\u3000+\\|\u0020+\\)")

(set-face-foreground 'whitespace-newline "gray40")
(set-face-background 'whitespace-newline nil)

(set-face-foreground 'whitespace-space nil)
(set-face-background 'whitespace-space nil)

(set-face-background 'whitespace-empty nil)

(set-face-foreground 'whitespace-tab "DarkRed")
(set-face-underline 'whitespace-tab t)
(set-face-background 'whitespace-tab nil)

;whitespace-space-after-tab
;whitespace-space-before-tab

(set-face-foreground 'whitespace-trailing nil)
(set-face-foreground 'whitespace-hspace nil)

(set-face-foreground 'whitespace-big-indent 	nil)
(set-face-background 'whitespace-big-indent 	nil)

(set-face-foreground 'whitespace-line 	nil)
(set-face-background 'whitespace-line 	nil)


;
;(setq whitespace-display-mappings '((tab-mark ?\t [?\xBB ?\t])))



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

(setq explicit-shell-file-name "/bin/zsh")
(add-hook 'shell-mode-hook
          (lambda ()
            (face-remap-set-base 'comint-highlight-prompt :inherit nil)))
;            (set-face-attribute 'comint-highlight-prompt nil
;                    :inherit nil)

;; --- theme ---------------------------------------------
(setq custom-safe-themes t)
;; (use-package color-theme-sanityinc-tomorrow :ensure t
;;              :config
;;              ;(color-theme-sanityinc-tomorrow-day)
;;              ;(color-theme-sanityinc-tomorrow-night)
;;              ;(color-theme-sanityinc-tomorrow-blue)
;;              (color-theme-sanityinc-tomorrow-bright)
;;              ;(color-theme-sanityinc-tomorrow-eighties)
;;              )
(use-package iceberg-theme :ensure t
              :config
              (iceberg-theme-create-theme-file)
              (load-theme 'solarized-iceberg-dark t)
;;              ;(color-theme-sanityinc-tomorrow-night)
;;              ;(color-theme-sanityinc-tomorrow-blue)
;;              (color-theme-sanityinc-tomorrow-bright)
;;              ;(color-theme-sanityinc-tomorrow-eighties)
              )


;; --- edit ---------------------------------------------
(use-package editorconfig :ensure t
             :config
             (editorconfig-mode 1)
             (add-hook 'editorconfig-hack-properties-functions
                       '(lambda (props)
                          (when (derived-mode-p 'makefile-mode)
                            (puthash 'indent_style "tab" props))))
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
(use-package jsonnet-mode :ensure t
             :mode
             ("\\.jsonnet\\'" . jsonnet-mode)
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
             :commands
             (markdown-mode gfm-mode)
             :mode
             ("\\.md\\'" . gfm-mode)
             :config
             (setq markdown-command "github-markup"
                   markdown-command-needs-filename t
                   markdown-content-type "application/xhtml+xml"
                   markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css")
                   markdown-xhtml-header-content "
<style>
body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; }
</style>
<script>
document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); });
</script>
" )
             )
(use-package org :ensure t
             :mode
             ("\\.org\\'" . org-mode)
             )

;; --- simplenote -----------------------------------------------
(use-package simplenote2 :ensure t
  :commands
  (simplenote2-list-mode markdown-mode gfm-mode)
  :init
  (add-hook 'simplenote2-note-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c t") 'simplenote2-add-tag)
              (local-set-key (kbd "C-c C-c l") 'simplenote2--create-note-locally)
              (local-set-key (kbd "C-c c") 'simplenote2-push-buffer)
              (local-set-key (kbd "C-c d") 'simplenote2-pull-buffer)))
  (add-hook 'simplenote2-create-note-hook
            (lambda ()
              (simplenote2-set-markdown)
              ))
  (simplenote2-setup)
  :config
  (setq simplenote2-email "dai1975@gmail.com"
        simplenote2-markdown-notes-mode 'gfm-mode
        ;simplenote2-filter-note-tag-list '("fressets" "tech")
        )
  )

;; --- programming -----------------------------------------------
(use-package lsp-mode :ensure t :commands lsp
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("rls"))
    :major-modes '(rust-mode)
    :priority 0
    :server-id 'myrls
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace (lsp--set-configuration `(:rust (:clippy_preference "on")))))
    :notification-handlers (lsp-ht ("window/progress" 'lsp-clients--rust-window-progress))))
  )
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

(use-package cc-mode
  :defer t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;(electric-indent-mode)
              ;(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                ;(gtags-mode 1)
                ))
)
;  (use-package google-c-style
;    :ensure t
;    :init
;    (add-hook 'c-mode-common-hook
;              (lambda ()
;                (google-set-c-style)
;                (google-make-newline-indent)))
;    :config
;    (c-set-offset 'statement-case-open 0)
                                        ;    )
;  )

(use-package c-mode ;:ensure t
             :mode
             ("\\.c\\'" . c-mode)
             :init
             (add-hook 'c-mode-hook
                       (lambda ()
                         (flycheck-mode)
                         (c-set-style "ellemtel")
                                        ;(c-set-style "linux")
                         (editorconfig-apply)
                         ))
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
             :config
             (setq rust-format-on-save t)
             )
(use-package cargo :ensure t :commands (cargo-minor-mode))

(use-package scala-mode :ensure t
             :mode
             ("\\.scala\\'" . scala-mode)
             :init
             (add-hook 'scala-mode-hook
                       (lambda ()
                         (flycheck-mode)
                         ))
             )


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

(use-package go-mode :ensure t
             :mode
             ("\\.go\\'" . go-mode)
             :init
             (add-hook 'go-mode-hook
                       (lambda ()
                         (setq tab-width 3) ; golang recommends tab
                         ;(flycheck-mode)
                         ))
             :config
             (setq gofmt-command "goimports")
             (add-hook 'before-save-hook 'gofmt-before-save)
             )

(use-package solidity-mode :ensure t
             :mode
             ("\\.sol\\'" . solidity-mode)
             )


(use-package web-mode :ensure t
             :mode
             ("\\.html?\\'" . web-mode)
             ("\\.js\\'" . web-mode)
             ("\\.tsx\\'" . web-mode)
             ("\\.jsx\\'" . web-mode)
             ("\\.erb\\'" . web-mode)
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
(use-package terraform-mode :ensure t
             :mode
             ("\\.tf\\'" . terraform-mode)
             ("\\.terraformrc\$" . terraform-mode)
             )
; --------------------------------------------------
;; https://emacs.stackexchange.com/questions/31646/how-to-paste-with-indentより転載
(defun yank-with-indent ()
  (interactive)
  (let ((indent
         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (message indent)
    (yank)
    (narrow-to-region (mark t) (point))
    (pop-to-mark-command)
    (replace-string "\n" (concat "\n" indent))
    (widen)))

;; バインド
;(define-key org-mode-map (kbd "C-c C-y") 'yank-with-indent)
(global-set-key (kbd "C-c C-y") 'yank-with-indent)

;; https://emacs.stackexchange.com/questions/34966/copy-region-without-leading-indentationより転載
(defun my-copy-region-unindented (pad beginning end)
  "Copy the region, un-indented by the length of its minimum indent.

If numeric prefix argument PAD is supplied, indent the resulting
text by that amount."
  (interactive "P\nr")
  (let ((buf (current-buffer))
        (itm indent-tabs-mode)
        (tw tab-width)
        (st (syntax-table))
        (indent nil))
    (with-temp-buffer
      (setq indent-tabs-mode itm
            tab-width tw)
      (set-syntax-table st)
      (insert-buffer-substring buf beginning end)
      ;; Establish the minimum level of indentation.
      (goto-char (point-min))
      (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                  (not (eobp)))
        (let ((length (current-column)))
          (when (or (not indent) (< length indent))
            (setq indent length)))
        (forward-line 1))
      (if (not indent)
          (error "Region is entirely whitespace")
        ;; Un-indent the buffer contents by the length of the minimum
        ;; indent level, and copy to the kill ring.
        (when pad
          (setq indent (- indent (prefix-numeric-value pad))))
        (indent-rigidly (point-min) (point-max) (- indent))
        (copy-region-as-kill (point-min) (point-max))))))

;; バインド
;(define-key org-mode-map (kbd "C-c M-w") 'my-copy-region-unindented)

; --------------------------------------------------
(load "my-company.el")

; -- google-translator ------------------------------------------------
(use-package google-translate :ensure t
             )
;(require 'google-translate)

(defvar google-translate-english-chars "[:ascii:]’“”–"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (save-excursion
                 (let (s)
                   (forward-char 1)
                   (backward-sentence)
                   (setq s (point))
                   (forward-sentence)
                   (buffer-substring s (point)))))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))
(global-set-key (kbd "C-c C-t") 'google-translate-enja-or-jaen)

(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))


