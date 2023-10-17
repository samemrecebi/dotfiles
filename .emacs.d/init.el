;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; I have some idea what im doing.

;;; Code:
;;Start speedup
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      create-lockfiles nil)

;;UI fixes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq ns-pop-up-frames nil)
(setq inhibit-startup-message t)

;;OS spesific settings
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'super)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit ))

;; Clean folder
(setq make-backup-files nil)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq create-lockfiles nil)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;;Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 130)

;;Column number
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		dashboard-mode-hookx))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'org-mode-hook
	  (lambda () (electric-pair-mode -1))
	  (lambda () (electric-indent-local-mode -1)))

;;Package repos
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)


(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;Loading custom files
(add-to-list 'load-path "~/.emacs.d/custom")

;;Keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;Packages
;; Get Shell Variables
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package yasnippet)
(yas-minor-mode)
(use-package yasnippet-snippets)

;; Projectile
(use-package projectile
  :init (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Documents/Projects"))
  :bind
  ("s-p" . projectile-command-map))

;;doom-themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-city-lights t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))


(use-package doom-modeline
  :init (doom-modeline-mode 1))

;;all-the-icons
(use-package all-the-icons)

;;ivy
(use-package ivy
	:init (ivy-mode 1)
	:custom
	(setq ivy-use-virtual-buffers t
				ivy-count-format "(%d/%d) "))

(use-package ivy-hydra
  :defer t
  :after hydra)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c t") 'counsel-org-tag)

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

;;Magit
(use-package magit
  :bind ("C-x g" . magit-status))

;;Flycheck
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;Spellcheck
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(defun flyspell-turkish ()
  (interactive)
  (ispell-change-dictionary "turkish")
  (flyspell-buffer))

(defun flyspell-english ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer))

;; Completion
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends '((company-capf company-dabbrev-code :with company-yasnippet)))

;; Some text modes
(use-package markdown-mode)
(use-package yaml-mode)
(use-package json-mode)

;; Pyhon Support
(use-package elpy)
(elpy-enable)
(setq python-shell-completion-native-enable nil)

(defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; LSP Mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package which-key
    :config
    (which-key-mode))

;; Latex Integration
(use-package tex
  :ensure auctex)
(setq TeX-PDF-mode t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Org Mode
(defun company-org-mode-hook ()
  (setq-local company-backends
	      '((company-dabbrev company-ispell company-yasnippet :separate)
                company-files)))

(use-package org
  :hook
  (org-mode . visual-line-mode)
  (org-mode-hook . auto-revert-mod)
  (org-mode-hook . my-org-mode-hook)
  (org-mode-hook . company-org-mode-hook)
  :config
  (customize-set-variable 'org-babel-python-command "python3")
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-document-title ((t (:inherit default :height 2.0)))))
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(setq org-directory (concat (getenv "HOME") "/Documents/Notes/"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (matlab . t)))

(use-package org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-download-screenshot-method "screencapture")

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
	 ("C-c n d" . org-roam-dailies-capture-today)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

;; Org bullets
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;; Org Export
(use-package ox-html
  :ensure nil
  :defer 3
  :after org
  :custom
  (org-html-checkbox-type 'unicode))
;;; init.el ends here
