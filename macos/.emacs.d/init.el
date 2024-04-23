;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Uga Buga

;;; Code:

;;Start speedup

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Speedup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (setq read-process-output-max (* 4 1024 1024))
  (setq process-adaptive-read-buffering nil)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Stop native comp errors
(setq warning-minimum-level :error)

;; UI fixes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq ns-pop-up-frames nil)
(setq inhibit-startup-message t)
(setq use-short-answers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq history-length 25)
(savehist-mode 1)

;; Keys
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Mac spesific fixes
(setq make-backup-files nil)

;; Auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq create-lockfiles nil)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;;Font
(set-face-attribute 'default nil :font "BerkeleyMono Nerd Font" :weight 'light :height 160)

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Berkeley Mono Variable" :height 160 :weight medium))))
 '(fixed-pitch ((t (:family "Berkeley Mono" :height 160)))))

;;Column number
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Replace selected
(delete-selection-mode)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;;Package repos
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq package-native-compile t)
(setq use-package-always-ensure t)

;;Loading custom files
(add-to-list 'load-path "~/.emacs.d/custom")

;;Keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;Packages
;; Get Shell Variables
(use-package exec-path-from-shell)
(when (daemonp)
  (exec-path-from-shell-initialize))

;;doom-themes
(use-package doom-themes
  :config
  (setq doom-modeline-icon t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-city-lights t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))


(use-package doom-modeline
  :init (doom-modeline-mode 1))
(setq doom-modeline-icon t)

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;;all-the-icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Parantesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
	(?\" . ?\")
	(?\' . ?\')
	(?\{ . ?\})))

;; Vertigo
(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; M-s bindings (search-map)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find))
  :init
  (defun compat-string-width (&rest args)
    (apply #'string-width args))
  (setq
   consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden ."
   consult-find-args "find ."))

(add-hook 'after-init-hook #'recentf-mode)
(savehist-mode 1)
(customize-set-variable 'bookmark-save-flag 1)

;; Orderless
  (use-package orderless
    :config
    (setq completion-styles '(orderless)))

;;Magit
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Popper
(use-package popper
  :bind
  (("C-`" . popper-toggle)
   ("M-`" . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :config
  (setq popper--reference-names nil
        popper--reference-modes nil
        popper--reference-predicates nil)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
	  "\\*Async Shell Command\\*"
          "\\*Error\\*"
          "Output\\*$"
          "\\*HS-Error\\*"
          "\\*lsp-help\\*"
          "^\\*Ement compose.*\\*$"
          "^\\*Org Export Dispatcher\\*$"
          "^\\*Org Select\\*$"
          "^\\*R:[^\\*]+\\*$"
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Treemacs
(use-package treemacs
  :after (doom-themes)
  :config
  ;; read input from a minibuffer not a child frame.
  (setq treemacs-read-string-input 'from-minibuffer))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Tree siter
(use-package tree-sitter)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Code Modes
(add-hook 'prog-mode-hook #'flymake-mode)

(use-package python-mode
  :custom
  (python-shell-interpreter "python"))

;; Some text modes
(use-package markdown-mode
  :hook
  ((markdown-mode . visual-line-mode)
   (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown"))
(use-package yaml-mode)
(use-package json-mode)
(use-package nix-mode
  :mode "\\.nix\\'")
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right))

;; LSP mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)

(use-package dap-mode)

;; Completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package cape
  :after corfu
  :init
  (defun cape-setup-capf-prog ()
    "Setup cape completions for prog-mode"
    (cape-setup-capf))

  (defun cape-setup-capf-text ()
    "Setup cape completions for text-mode"
    (cape-setup-capf))

  (defun cape-setup-capf ()
    "Setup cape completions"
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-tex))
  :hook
  ((prog-mode . cape-setup-capf-prog)
   (text-mode . cape-setup-capf-text)))

;; Org Mode
(use-package org
  :hook
  (org-mode . visual-line-mode)
  (org-mode-hook . auto-revert-mod)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-src-fontify-natively t)
  (setq-default org-startup-indented t
		org-pretty-entities t
		org-use-sub-superscripts "{}"
		org-hide-emphasis-markers t
		 org-hide-leading-stars t
		org-startup-with-inline-images t
		org-image-actual-width '(300)))

(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(require 'org-indent)

;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

;; PDF View
(use-package pdf-tools
  :pin manual ;; manually update
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit width
  (setq-default pdf-view-display-size 'fit-width)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))


;;; init.el ends here
