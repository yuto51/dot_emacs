;;; init.el --- initialization settings
;;; Commentary:
;;; This file describes about my personal Emacs configuration.
;;; Code:

;;;
;;; backup file
;;;
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

;; Avoid writing custom variables at the tail of init.el (> Emacs25)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file)
  (write-region "" nil custom-file))

;;;
;;; package.el
;;; http://emacs-jp.github.io/packages/package-management/package-el.html
;;;
(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; MELPA-stableを追加
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Marmaladeを追加
;; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; 初期化
(package-initialize)

;; auto-install
(defvar installing-package-list
  '(
    auto-complete
    flycheck
    google-c-style
    helm
    helm-descbinds
    helm-flycheck
    magit
    ))
(dolist (x installing-package-list)
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
(when (member "Noto Sans Mono CJK JP" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Noto Sans Mono CJK JP 11")))

;;;
;;; Tool bar & Menu bar & Scroll bar
;;;
(tool-bar-mode 0)
(if (display-graphic-p)
    (menu-bar-mode 1)  ;; GUI
  (menu-bar-mode 0))  ;; Terminal
(set-scroll-bar-mode t)

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
(define-key global-map (kbd "C-q") 'goto-match-paren)
(show-paren-mode 1)

;;;
;;; Helm
;;;
(package-install 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-descbinds-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd  "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(delete '(find-file) helm-completing-read-handlers-alist)
(delete '(execute-extended-command) helm-completing-read-handlers-alist)

;;;
;;; Magit
;;;
(package-install 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)

;;;
;;; Auto Complete
;;;
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))

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
(add-hook 'nxml-mode-hook
          (lambda ()
            ;; スラッシュの入力で終了タグを自動補完
            (setq nxml-slash-auto-complete-flag t)
            (setq nxml-child-indent 2)
            (setq nxml-attribute-indent 4)
            (setq indent-tabs-mode nil)
            (setq nxml-bind-meta-tab-to-complete-flag t)
            ;; C-M-kで下位を含む要素全体をkillする
            (setq nxml-sexp-element-flag t)
            ;; グリフは非表示
            (setq nxml-char-ref-display-glyph-flag nil)
            (setq tab-width 4))
          )

;;;
;;; Flycheck
;;;
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c p") 'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-c l") 'flycheck-list-errors)
  )

;;;
;;; Helm interface for Flycheck
;;;
(with-eval-after-load 'flycheck
  (require 'helm-flycheck)
  (define-key flycheck-mode-map (kbd "C-c h") 'helm-flycheck)
  )

;;;
;;; Google C++ Style Guide
;;;
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-set-c-style)

;;;
;;; Keybindings for default functions
;;;
(define-key global-map (kbd "C-h") 'backward-delete-char)
(define-key global-map (kbd "C-c C-c") 'comment-region)
(define-key global-map (kbd "M-g") 'goto-line)

;;;
;;; misc
;;;
(setq inhibit-startup-message t)  ;; No startup message
(add-to-list 'display-buffer-alist  ;; open shell in the same window
     '("^\\*shell\\*$" . (display-buffer-same-window)))
(put 'erase-buffer 'disabled nil)  ;; Enable 'Erase buffer' function

;;;
;;; Add sub-dirs to load-path
;;;
(let ((default-directory "~/.emacs.d/github"))
  (normal-top-level-add-subdirs-to-load-path))

;;;
;;; Flycheck with cpplint
;;;
(with-eval-after-load 'flycheck
  (require 'flycheck-google-cpplint)
  (custom-set-variables
   '(flycheck-c/c++-googlelint-executable "cpplint")
   '(flycheck-googlelint-linelength "120")
   )
  (add-hook 'c-mode-common-hook #'(lambda () (flycheck-select-checker 'c/c++-googlelint)))
  (add-hook 'c++-mode-common-hook #'(lambda () (flycheck-select-checker 'c/c++-googlelint)))
  )

;;;
;;; flycheck catkin_lint
;;;
(with-eval-after-load 'flycheck
  (require 'flycheck-catkin-lint)
  (custom-set-variables
   '(flycheck-catkin-lint-warning-level "2")
   )
  (add-hook 'cmake-mode-hook #'(lambda () (flycheck-select-checker 'cmake-catkin-lint)))
  )

;;;
;;; Load environment-specific settings
;;;
(let ((extra "~/.emacs.d/env-specific.el"))
  (if (file-exists-p extra) (load-file extra)))

;;; init.el ends here
