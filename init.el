;; performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; straight
(setq package-enable-at-startup nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; https://emacs-china.org/t/package/19959
;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.
;; Make `package-autoremove' work with `use-package'

(defvar use-package-selected-packages '(use-package)
  "Packages pulled in by use-package.")

(eval-and-compile
  (define-advice use-package-handler/:ensure (:around (fn name-symbol keyword args rest state) select)
    (let ((items (funcall fn name-symbol keyword args rest state)))
      (dolist (ensure args items)
        (let ((package
               (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                   ensure)))
          (when package
            (when (consp package)
              (setq package (car package)))
            (push `(add-to-list 'use-package-selected-packages ',package) items)))))))

(when (fboundp 'package--save-selected-packages)
  (add-hook 'after-init-hook
            (lambda ()
              (package--save-selected-packages
               (seq-uniq (append use-package-selected-packages package-selected-packages))))))

(setq custom-file "~/.emacs.d/custom.el")
(setq straight-use-package-by-default t)

;; mode-line
(use-package diminish
  :ensure t)

;; hide warn
(setq warning-minimum-level :error)

;; split window
(define-key global-map (kbd "C-(") #'split-window-right)
(define-key global-map (kbd "C-)") #'split-window-below)

;; no blink cursor
(blink-cursor-mode 0)

;; highlight line
(defvar-local was-hl-line-mode-on nil)
(defun my/hl-line-mode-on()
  (setq was-hl-line-mode-on t)
  (hl-line-mode))

(add-hook 'text-mode-hook #'my/hl-line-mode-on)
(add-hook 'prog-mode-hook #'my/hl-line-mode-on)

;; line number
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(column-number-mode 1)
(line-number-mode 0)

;; disable tool-bar and scroll-bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; tab-indent
(setq-default tab-width 4)

;; auto-pair
(electric-pair-mode 1)

;; backup/autosave
(setq make-backup-files nil)
(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))

;; session management
(setq default-frame-alist
       '((height . 40)
         (width . 130)
         (left . 0)
         (top . 0)))
;; (add-hook 'desktop-not-loaded-hook #'desktop-save-mode-off)
;; (setq desktop-load-locked-desktop 'check-pid)
;; (desktop-save-mode 1)

;; for vertico
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; auto completion
(use-package company
  :ensure t
  :defer 150
  :bind
  (:map company-active-map
		("<escape>" . company-abort))
  :custom
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-idle-delay 0.1)
  (company-require-match nil)
  (global-company-mode t)
  (company-format-margin-function 'company-text-icons-margin)
  :config
  ;; We follow a suggestion by company maintainer u/hvis:
  ;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
  (defun company-completion-styles (capf-fn &rest args)
	(let ((completion-styles '(basic partial-completion)))
	  (apply capf-fn args)))

  (advice-add 'company-capf :around #'company-completion-styles))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

;; which-key
(use-package which-key
  :ensure t
  :defer 50
  :custom
  (which-key-mode t)
  :config
  (diminish 'which-key-mode)
  (which-key-enable-god-mode-support))

;; snippet
(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (diminish 'yas-minor-mode))

;; flycheck
(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package consult-flycheck
  :ensure t)

;; vertico
(use-package vertico
  :ensure t
  :straight (:files (:defaults "extensions/*")
					:includes (vertico-mouse))
  :config
  (vertico-mode)
  (vertico-mouse-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; configuration for Consult

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (
		 ;; replace isearch
		 ("C-s" . consult-line)
		 ;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;; git/project
(use-package projectile
  :ensure t
  :bind
  (:map projectile-mode-map
		("C-c p" . projectile-command-map))
  :init
  (projectile-mode 1)
  :config
  (diminish 'projectile-mode))

(use-package magit
  :hook
  (git-commit-mode . evil-insert-state)
  :ensure t)

;; markdown
(use-package markdown-mode
  :ensure t)

(defun my/ace-window ()
  "Switch to windows and disable im if needed."
  (interactive)
  (ace-select-window)
  (if (not (eq evil-state 'insert))
	  (my/evil-disable-im)
	(my/evil-restore-im)))

;; ace-window
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . my/ace-window)
  ("M-p" . ace-swap-window))

;; embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; tree-sitter
(use-package tree-sitter
  :ensure t
  :custom
  (global-tree-sitter-mode t)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (diminish 'tree-sitter-mode "TS"))

(use-package tree-sitter-langs
  :ensure t)

;; lsp
(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-restart 'ignore)
  :ensure t)

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil))

;; term
(use-package vterm
  :hook
  (vterm-mode . (lambda()(company-mode -1)))
  :custom
  (vterm-buffer-name-string "vterm %s")
  (vterm-timer-delay 0.01)
  :ensure t)

(use-package multi-vterm
  :ensure t
  :bind
  ("C-S-t" . multi-vterm))

(use-package vterm-toggle
  :ensure t
  :bind
  (("C-\\" . vterm-toggle)
   :map vterm-mode-map
   ("C-\\" . vterm-toggle))
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
			   '((lambda (buffer-or-name _)
				   (let ((buffer (get-buffer buffer-or-name)))
					 (with-current-buffer buffer
					   (or (equal major-mode 'vterm-mode)
						   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
				 (display-buffer-reuse-window display-buffer-at-bottom)
				 ;;(display-buffer-reuse-window display-buffer-in-direction)
				 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
				 ;;(direction . bottom)
				 ;;(dedicated . t) ;dedicated is supported in emacs27
				 (reusable-frames . visible)
				 (window-height . 0.3))))

;; god
(use-package god-mode
  :ensure t
  :init
  (add-hook 'buffer-list-update-hook (lambda () (god-local-mode -1)))
  (global-set-key (kbd "C-S-f") #'make-frame)
  :config
  ;; some maps
  (define-key god-local-mode-map (kbd ".") #'repeat))

;; template
(use-package yatemplate
  :ensure t
  :init
  (add-hook 'nxml-mode-hook #'yas-minor-mode)
  (setq auto-insert-query nil)
  (auto-insert-mode)
  :config
  (yatemplate-fill-alist))

;;ligature
(use-package fira-code-mode
  :config
 (if (display-graphic-p)
	(add-hook 'prog-mode-hook #'fira-code-mode)))

;; doom theme
(use-package doom-themes
  :ensure t
  :config
  (load custom-file))

;; auto-format
(use-package format-all
  :ensure t)

;; org
(use-package org
  :hook
  (org-src-mode . evil-insert-state))

(use-package org-bullets
  :ensure t
  :custom
  (org-startup-indented t)
  (org-latex-create-formula-image-program 'dvisvgm)
  :hook
  (org-mode . org-bullets-mode))

(use-package org-contrib
  :ensure t)

(use-package org2ctex
  :ensure t
  :hook
  (org-mode . org2ctex-mode))

(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-word-0))

;; evil

(defun my/evil-esc ()
  (interactive)
  (if (not (active-minibuffer-window))
	  (evil-force-normal-state)
	(progn
	  (select-window (active-minibuffer-window))
	  (abort-minibuffers))))

(defun my/evil-lsp-doc-toggle ()
  "Toggle lsp-ui-doc."
  (interactive)
  (if (not (lsp-ui-doc--frame-visible-p))(lsp-ui-doc-glance)
	(lsp-ui-doc-hide)))

(defvar my/previous-im "keyboard-us")
(defun my/evil-disable-im ()
  "Disable fcitx5."
  (interactive)
  (setq my/previous-im
		(substring
		 (shell-command-to-string "fcitx5-remote -n")
		 0 -1))
  (call-process-shell-command
   "fcitx5-remote -c"))

(defun my/evil-restore-im ()
  "Restore fcitx5."
  (interactive)
  (call-process-shell-command
   (concat "fcitx5-remote -s " my/previous-im)))

(defun my/evil-toggle-im ()
  "Toggle fcitx5."
  (interactive)
  (call-process-shell-command
   "fcitx5-remote -t"))

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq select-enable-clipboard nil)
  :hook
  (minibuffer-setup . (lambda ()
						(setq-local cursor-type 'bar)))
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global "gh" 'xref-find-references)
  (define-key global-map (kbd "C-S-v") #'clipboard-yank)
  (define-key global-map (kbd "C-S-c") #'clipboard-kill-ring-save)
  (define-key global-map (kbd "<S-insert>") #'clipboard-yank)
  ;; (evil-define-key 'insert 'global (kbd "C-\\") #'my/evil-toggle-im)
  ;; lsp
  (evil-define-key 'normal 'lsp-mode "gr" 'lsp-rename)
  (evil-define-key 'normal 'lsp-mode "K" 'my/evil-lsp-doc-toggle)
  ;; vterm
  (evil-define-key 'insert vterm-mode-map (kbd "<S-left>") 'multi-vterm-prev)
  (evil-define-key 'insert vterm-mode-map (kbd "<S-right>") 'multi-vterm-next))

(add-hook 'evil-visual-state-entry-hook
		  (lambda ()
		    (hl-line-mode -1)))
(add-hook 'evil-insert-state-exit-hook
		  #'my/evil-disable-im)
(add-hook 'evil-insert-state-entry-hook
		  #'my/evil-restore-im)
(add-hook 'evil-visual-state-exit-hook
		  (lambda ()
			(if was-hl-line-mode-on (hl-line-mode +1))))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings nil)
  :config
  (evil-collection-init)
  (diminish 'evil-collection-unimpaired-mode))

(use-package evil-god-state
  :ensure
  :config
  (evil-define-key 'normal global-map " " 'evil-execute-in-god-state)
  (define-key evil-normal-state-map (kbd "<escape>") #'my/evil-esc)
  (define-key minibuffer-mode-map (kbd "<escape>") #'abort-minibuffers)
  (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
  (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

;; vim-surround
(use-package evil-surround
  :ensure t
  :custom
  (global-evil-surround-mode t))

;; fold
(use-package ts-fold
  :ensure t
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :custom
  (global-ts-fold-mode t)
  :config
  (diminish 'ts-fold-mode))

;; comment
(use-package evil-commentary
  :ensure t
  :custom
  (evil-commentary-mode t))

;; yaml
(use-package yaml-mode
  :ensure t)

;; c/c++
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package cc-mode
  :custom
  (c-default-style "llvm.org")
  :config
  (c-add-style "llvm.org"
			   '("gnu"
				 (fill-column . 80)
				 (c++-indent-level . 2)
				 (c-basic-offset . 2)
				 (indent-tabs-mode)
				 (c-offsets-alist
				  (arglist-intro . ++)
				  (innamespace . 0)
				  (member-init-intro . ++)))))

(use-package lua-mode
  :ensure t
  :config
  :hook
  (lua-mode . lsp))

;; editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  (diminish 'editorconfig-mode))

;; ebuild
(add-hook 'ebuild-mode-hook 'company-ebuild-setup)
(add-hook 'ebuild-mode-hook 'flycheck-pkgcheck-setup)

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; smooth scroll
(use-package good-scroll
  :ensure t
  :custom
  (scroll-step 1)
  (scroll-conservatively 101)
  :config
  (good-scroll-mode))

;; evil enhancement
(use-package evil-terminal-cursor-changer
  :ensure t
  :config
  (unless (display-graphic-p)
	(evil-terminal-cursor-changer-activate)))

;; systemd
(use-package systemd
  :ensure t)

;; tree
(use-package treemacs
  :bind
  ("C-'" . treemacs-select-window)
  :ensure t)
(use-package treemacs-projectile
  :ensure t)
(use-package treemacs-magit
  :ensure t)
(use-package treemacs-evil
  :ensure t)

;; icons
(use-package treemacs-all-the-icons
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))
(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; fix pdf-hidpi blurry
(use-package doc-view
  :custom
  (doc-view-mupdf-use-svg t))

;; python
(use-package python-mode
  :ensure t
  :hook
  (python-mode . lsp))

(use-package pyvenv
    :ensure t)

(use-package auto-virtualenv
  :ensure t
  :init
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)  ;; If using projectile
)
