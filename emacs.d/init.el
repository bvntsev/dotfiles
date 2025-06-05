;; ===== Optimization =====
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq warning-minimum-level :error)
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
(setq read-process-output-max (* 1024 1024))
(prefer-coding-system 'utf-8)

;; backup fix
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq make-backup-files t) 

;; ===== Startup Cleanup =====
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; ===== Font =====
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 140)

;; ===== Package Manager =====
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; ===== Interface =====
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell t)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)

(use-package doom-themes :config (load-theme 'doom-one t))
(use-package all-the-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package which-key :init (which-key-mode))
(save-place-mode 1)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; ===== Completion & Navigation =====
(use-package vertico :init (vertico-mode))
(use-package consult
  :config
  ;; hide system buffer
  (setq consult-buffer-filter
        (list "\\` "
              (rx bos
                  (or
                   "*Async-native-compile-log*"
                   "*Compile-Log*"
                   "*Warnings*"
                   "*Messages*"
                   "*Completions*"
                   "*scratch*"
                   "*Backtrace*"
                   "*eldoc*"
                   "*Help*"
                   "*Apropos*"
                   "*Flymake log*"
                   )
                  eos))))
  
  ;; alternative variant
(use-package marginalia :init (marginalia-mode))
(use-package orderless    ; Гибкий поиск (как fzf)
  :init (setq completion-styles '(orderless)))

;; ===== Evil Mode =====
(use-package evil
  :config
  (evil-mode 1)
  (dolist (mode '(magit-status-mode magit-popup-mode magit-revision-mode
                  magit-diff-mode magit-log-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; ===== LSP =====
(use-package eglot
  :hook ((python-mode c-mode c++-mode rust-mode go-mode) . eglot-ensure)
  :config
  (setq eglot-ignored-server-capabilities '(:signatureHelpProvider))

  ;; Добавляем для makefile
  (add-to-list 'eglot-server-programs
               '(makefile-mode . ("bash-language-server" "start"))))

  ;; change clangd to ccls
  ; (add-to-list 'eglot-server-programs
  ;              '(c-mode . ("ccls")))
  ; (add-to-list 'eglot-server-programs
  ;              '(c++-mode . ("ccls"))))

(use-package dap-mode
  :after eglot
  :config
  (dap-auto-configure-mode))

;; ===== Autocompletion =====
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-auto-commit nil     
        company-auto-complete nil)

  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-y") #'company-complete-selection)

  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") nil))

;; ===== Git =====
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

;; ===== Tree View =====
(use-package treemacs
  :bind ("C-c t" . treemacs)
  :config (treemacs-git-mode 'extended))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-evil :after (treemacs evil))

;; ===== Treesitter =====
(use-package tree-sitter
  :config (global-tree-sitter-mode))
(use-package tree-sitter-langs :after tree-sitter)

;; ===== Terminal =====
(use-package vterm)

(use-package move-text)
(global-set-key (kbd "s-p") 'move-text-up)
(global-set-key (kbd "s-n") 'move-text-down)




; ===== Bufferline =====
(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode t)

  (setq centaur-tabs-set-icons t))

;; ===== Makefile support =====
(defun my-make-compile ()
  (interactive)
  (compile "make -k"))
(global-set-key (kbd "<f5>") 'my-make-compile)

;; ===== Tabs & Indentation =====
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(indent-tabs-mode nil)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq tab-width 4)))

(use-package general
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun my/next-user-buffer ()
  "move to next user buffer."
  (interactive)
  (let ((start (current-buffer)))
    (next-buffer)
    (while (and (string-prefix-p "*" (buffer-name))  ; буферы типа *Messages*
                (not (eq (current-buffer) start)))
      (next-buffer))))

(defun my/prev-user-buffer ()
  "move to prev. user buffer."
  (interactive)
  (let ((start (current-buffer)))
    (previous-buffer)
    (while (and (string-prefix-p "*" (buffer-name))
                (not (eq (current-buffer) start)))
      (previous-buffer))))

(defun my/evil-buffer-new-named ()
  "Create a new user buffer with unique name."
  (interactive)
  (let ((buf (generate-new-buffer-name "untitled")))
    (switch-to-buffer buf)
    (evil-normal-state)))

(defun my/close-buffer-or-window ()
  (interactive)
  (if (string-prefix-p "*" (buffer-name))
      (if (one-window-p)
          (kill-buffer)
        (delete-window))
    (kill-buffer)))

(my/leader-keys
  "f"  '(:ignore t :which-key "files")
  "ff" '(find-file :which-key "find file")
  "fd" '(dired :which-key "find directory")
  "fb" '(consult-buffer :which-key "buffers")
  "fl" '(consult-line :which-key "search line")
  "fc" '((lambda () (interactive) (find-file "~/.emacs.d/init.el"))
         :which-key "open config init.el")

  "cd" '(compile :which-key "compile current directory")
  "cp" '(project-compile :which-key "project compile")

  "t" '(my/evil-buffer-new-named :which-key "new buffer")
  ;"d" '((lambda () (interactive) (kill-this-buffer))
  ;      :which-key "delete buffer")  ;; ← фикс!
  "d" '(my/close-buffer-or-window :which-key "delete buffer")
  "<tab>" '(my/next-user-buffer :which-key "next-buffer")
  "<backtab>" '(my/prev-user-buffer :which-key "prev-buffer"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(global-set-key (kbd "C-c d") #'my/close-buffer-or-window)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "C-c d") #'my/close-buffer-or-window))

(with-eval-after-load 'special-mode
  (define-key special-mode-map (kbd "C-c d") #'my/close-buffer-or-window))

(dolist (map (list compilation-mode-map special-mode-map))
  (when map
    (define-key map (kbd "C-c d") #'my/close-buffer-or-window)))
