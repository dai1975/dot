
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-g" 'goto-line)

(setq-default indent-tabs-mode nil)
(setq tab-width 8)
(setq c-basic-offset 3)

;; --- package -----------------------------------------------
(require 'package)

(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade"    . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/") t)

(package-initialize)

;; --- automode -----------------------------------------------
(setq auto-mode-alist
      (append (list
               '("\\.h" . c++-mode)
               '("\\.cpp" . c++-mode)
               '("\\.gnus" . emacs-lisp-mode)
               '("\\.abbrev_defs" . emacs-lisp-mode)
               '("\\.el$" . emacs-lisp-mode)
               '("\\.txt$" . text-mode)
               '("\\.md" . markdown-mode)
               '("SConscript" . python-mode)
               '("SConstruct" . python-mode)
               auto-mode-alist)))

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

;; --- shell -----------------------------------------------
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
(require 'multi-term)
(setq multi-term-program shell-file-name)
(add-to-list 'term-unbind-key-list '"M-x")
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
             (define-key term-raw-map (kbd "C-y") 'term-paste)
             (define-key term-raw-map (kbd "C-k")
               (lambda (&optional arg) (interactive "P")
                 (funcall 'kill-line arg) (term-send-raw)))
             (setq ansi-term-color-vector
                   [unspecified
                    "#000000"           ; black
                    "#ff3c3c"           ; red
                    "#84dd27"           ; green
                    "#eab93d"           ; yellow
                    "#135ecc"           ; blue
                    "#f47006"           ; magenta
                    "#89b6e2"           ; cyan
                    "#ffffff"]          ; white
                   )
             ))

(global-set-key (kbd "C-c C-t")
                '(lambda ()
                   (interactive)
                   (if (get-buffer "*terminal<1>*")
                       (switch-to-buffer "*terminal<1>*")
                     (multi-term))))

;; --- ggtags -----------------------------------------------


