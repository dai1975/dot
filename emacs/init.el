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
(setq tab-width 8)
;(setq c-basic-offset 3)

;; --- package -----------------------------------------------
(require 'package)

(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/") t)

(package-initialize)

;; --- automode -----------------------------------------------
(add-to-list 'auto-mode-alist '("\\.h\\'"        . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'"      . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'$"      . rust-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'$"    . toml-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'"      . text-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("SConscript\\'$" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'$" . python-mode))

;; --- quickrun -----------------------------------------------
(require 'quickrun)
;(push '("*quickrun*") popwin:special-display-config)
(global-set-key (kbd "C-c C-q") 'quickrun)

;; --- flycheck -----------------------------------------------
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)

;(defun flycheck-display-error-messages-unless-error-buffer (errors)
;  (unless (get-buffer-window flycheck-error-list-buffer)
;    (flycheck-display-error-messages errors)))
;(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer)

;(eval-after-load 'flycheck
;  '(setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
;  '(setq flycheck-display-errors-function nil)
;  )

;; --- c-mode -----------------------------------------------
;; -- keybind
(defun my-gtags-mode-defs ()
  (gtags-mode 1)
  (define-key c-mode-base-map (kbd "C-q s") 'gtags-find-symbol)
  (define-key c-mode-base-map (kbd "C-q t") 'gtags-find-tag)
  (define-key c-mode-base-map (kbd "C-q r") 'gtags-find-rtag)
  (define-key c-mode-base-map (kbd "C-q f") 'gtags-find-file)
  (define-key c-mode-base-map (kbd "C-q p") 'gtags-pop-stack)
)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (my-gtags-mode-defs)
            )))

(add-hook 'c++-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)
             (setq c-basic-offset 3)
             (c-set-offset 'innamespace 0)
             (c-set-offset 'arglist-close 0)
             ))


;; --- rust --------------------------------------------------------
(add-hook 'rust-mode-hook 'cargo-minor-mode)

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
;; --- text-mode -----------------------------------------------
(setq text-mode-hook 'turn-off-auto-fill)

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

; http://www.emacswiki.org/emacs/download/multi-term.el
;(require 'multi-term)
;(setq multi-term-program shell-file-name)
;(add-to-list 'term-unbind-key-list '"M-x")
;(add-hook 'term-mode-hook
;          '(lambda ()
;             (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
;             (define-key term-raw-map (kbd "C-y") 'term-paste)
;             (define-key term-raw-map (kbd "C-k")
;               (lambda (&optional arg) (interactive "P")
;                 (funcall 'kill-line arg) (term-send-raw)))
;             (setq ansi-term-color-vector
;                   [unspecified
;                    "#000000"           ; black
;                    "#ff3c3c"           ; red
;                    "#84dd27"           ; green
;                    "#eab93d"           ; yellow
;                    "#135ecc"           ; blue
;                    "#f47006"           ; magenta
;                    "#89b6e2"           ; cyan
;                    "#ffffff"]          ; white
;                   )
;             ))

(global-set-key (kbd "C-c C-t")
                '(lambda ()
                   (interactive)
                   (if (get-buffer "*terminal<1>*")
                       (switch-to-buffer "*terminal<1>*")
                     (multi-term))))

;; --- IM -----------------------------------------------
(when (require 'skk nil t)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
  (setq default-input-method "japanese-skk")
  (require 'skk-study))

;; --- ggtags -----------------------------------------------

;; --- simplenote -----------------------------------------------
(require 'simplenote)
(setq simplenote-email "dai1975@gmail.com")
(simplenote-setup)

;(require 'simplenote2)
;(setq simplenote2-email "dai1975@gmail.com")
;(simplenote2-setup)



