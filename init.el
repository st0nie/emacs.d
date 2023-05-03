;; performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max 8192)

(require 'package)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; mode-line
(use-package diminish
  :ensure t)

;; no blink cursor
(blink-cursor-mode 0)

;; highlight line
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'activate-mark-hook
  (lambda ()
    (hl-line-mode -1)))
(add-hook 'deactivate-mark-hook
  (lambda ()
	(if (derived-mode-p 'prog-mode)(hl-line-mode +1))
	))

;; line number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; disable toll-bar
(tool-bar-mode -1)

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
(add-hook 'desktop-not-loaded-hook #'desktop-save-mode-off)
(setq desktop-load-locked-desktop 'check-pid)
(desktop-save-mode 1)

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
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.1)
  (setq company-require-match nil)
  :custom
  (global-company-mode t)
  :config
  ;; orderless
  (defun just-one-face (fn &rest args)
	(let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))
  (advice-add 'company-capf--candidates :around #'just-one-face)
)
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (diminish 'company-box-mode))


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

;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
(use-package savehist
  :ensure t
  :init
  (savehist-mode))
(use-package orderless
  :ensure t
  :init
  (setq orderless-component-separator "[ &]")
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))
(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
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
  :config
  (projectile-mode 1)
  (diminish 'projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package magit
  :ensure t)

;; markdown
(use-package markdown-mode
  :ensure t)

;; ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

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
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
  :config
  (global-tree-sitter-mode)
(use-package tree-sitter-langs
  :ensure t)

;; lsp
(use-package lsp-mode
  :init
  (add-hook 'c-mode-hook #'lsp)
  (setq lsp-keymap-prefix "C-c l")
  :ensure t)

;; telega
(use-package telega
  :ensure t
  :init
  (defun my/tg-mode-hook()
	(progn
	  (setq-local god-global-mode nil)
	  (electric-pair-mode -1)))
  (add-hook 'telega-chat-mode-hook #'my/tg-mode-hook)
  (add-hook 'telega-root-mode-hook #'my/tg-mode-hook)
  ;; telega notify
  (add-hook 'telega-load-hook #'telega-notifications-mode)
  (setq telega-server-libs-prefix "/usr/")
  (define-key global-map (kbd "C-c t") telega-prefix-map))

;; term
(use-package vterm
  :hook
  (vterm-mode . (lambda()(setq-local global-company-mode nil)))
  (vterm-mode . (lambda()(company-mode -1)))
  (vterm-mode . (lambda()(setq-local god-global-mode nil)))
  :custom
  (setq vterm-timer-delay 0.01)
  :ensure t)
(use-package multi-vterm
  :ensure t
  :bind (("C-S-t" . multi-vterm)
         :map vterm-mode-map
         ("<S-right>" . multi-vterm-next)
         ("<S-left>" . multi-vterm-prev)))

;; god
(use-package god-mode
  :ensure t
  :init
  (global-set-key (kbd "<escape>") #'(lambda()(interactive)(if (not god-local-mode)(god-local-mode +1)(keyboard-escape-quit))))
  
  :config
  (god-mode)

  ;; some maps
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  
  ;; cursor and mode line
  (defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

  ;; overwrite mode
  (defun my-god-mode-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))
  (add-hook 'overwrite-mode-hook #'my-god-mode-toggle-on-overwrite)

  ;; isearch
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

  )

;; tab
(use-package all-the-icons
  :ensure t)
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-set-icons t)
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(add-hook 'ebuild-mode-hook 'company-ebuild-setup)
(add-hook 'ebuild-mode-hook 'flycheck-pkgcheck-setup)
