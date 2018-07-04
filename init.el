;;;
;;; package.el
;;; http://emacs-jp.github.io/packages/package-management/package-el.html
;;;
(require 'package)

;; MELPAを追加
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; 初期化
(package-initialize)

;; auto-install
(defvar installling-package-list
  '(
    magit
    helm
    helm-descbinds
    ))
(dolist (x installling-package-list)
  (let ((refreshed nil))
    (unless (or (package-installed-p x) refreshed)
      (package-refresh-contents)
      (setq refreshed t))
    (package-install x)
    ))

;;;
;;; Locale & coding system
;;;
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;;
;;; Font setting
;;; https://www.google.com/get/noto/help/install/
;;;
(when (display-graphic-p)  ;; GUI mode
  (defun set-font (name size)
    (let* ((font-name (apply 'concat (split-string name " ")))
	   (fontset-name (concat "fontset-" font-name))
	   (font-with-size (concat name "-" (number-to-string size)))
	   )
      (create-fontset-from-ascii-font font-with-size nil font-name)
      (set-fontset-font fontset-name 'unicode font-with-size nil 'append)
      (add-to-list 'default-frame-alist
		   (cons 'font fontset-name))
      )
    )
  ;; (set-font "IPAGothic" 12)
  (set-font "Takaoゴシック" 12)
  ;; (set-font "Noto Sans Mono CJK JP Regular" 11)
  )

;;;
;;; Tool bar & Menu bar & Scroll bar
;;;
(tool-bar-mode 0)
(if (display-graphic-p)
    (menu-bar-mode 1)  ;; GUI
  (menu-bar-mode 0))  ;; Terminal
(set-scroll-bar-mode t)

;;;
;;; backup file
;;;
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

;;;
;;; Show Line & Column number
;;;
(line-number-mode 1)
(column-number-mode 1)

;;;
;;; Color theme
;;;
;; (package-install 'solarized-theme)
;; (load-theme 'solarized-dark t)
(package-install 'monokai-theme)
(load-theme 'monokai t)

;;;
;;; Tab
;;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;;
;;; Paren
;;; https://www.emacswiki.org/emacs/NavigatingParentheses
;;;
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "\C-q" 'goto-match-paren)
(show-paren-mode 1)

;;;
;;; Helm
;;;
(package-install 'helm)
(require 'helm-config)
(helm-mode 1)

;;;
;;; Magit
;;;
(package-install 'magit)
(global-set-key "\C-xg" 'magit-status)

;;;
;;; Python
;;;
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;;;
;;; XML
;;;
(add-to-list 'auto-mode-alist '("\\.launch\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.machine\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.test\\'" . nxml-mode))

;;;
;;; Keybindings for default functions
;;;
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\M-g" 'goto-line)

;;;
;;; misc
;;;
(setq inhibit-startup-message t)  ;; No startup message
