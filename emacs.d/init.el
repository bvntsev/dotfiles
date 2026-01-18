;; -*- lexical-binding: t; -*-

(setq read-process-output-max (* 2 1024 1024)) ;; 2MB
(prefer-coding-system 'utf-8)

;; backup
(setq make-backup-files t
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/auto-saves/") t))
      auto-save-default t)

;; ===== Startup Cleanup =====
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; ===== Font =====
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 130)

;; ===== Package Manager =====
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(setq native-comp-async-report-warnings-errors 'silent)
(use-package magit :defer t)
;; (use-package vterm :defer t)
(use-package treemacs :defer t)

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
(use-package marginalia :init (marginalia-mode))
(use-package orderless
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
  (setq eglot-ignored-server-capabilities
        '(:signatureHelpProvider
          :documentFormattingProvider
          :documentRangeFormattingProvider
          :documentOnTypeFormattingProvider)) ; Отключаем форматирование при вводе
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (eglot-inlay-hints-mode -1)
              (setq-local indent-tabs-mode t)
              (local-set-key (kbd "TAB") #'my/insert-literal-tab))))

(defun my/insert-literal-tab ()
  (interactive)
  (insert "\t"))

(remove-hook 'before-save-hook #'eglot-format-buffer)
(remove-hook 'before-save-hook #'format-all-buffer)

(use-package dap-mode
  :after eglot
  :config
  (dap-auto-configure-mode))


;; ===== Autocompletion =====
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t)
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
(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; ===== Terminal =====
;; (use-package vterm)

(use-package move-text)
(global-set-key (kbd "s-p") 'move-text-up)
(global-set-key (kbd "s-n") 'move-text-down)

;; ===== Bufferline =====
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
    (while (and (string-prefix-p "*" (buffer-name))
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
  "fp" '(project-dired :which-key "open project directory")
  "fc" '((lambda () (interactive) (find-file "~/.emacs.d/init.el"))
         :which-key "open config init.el")

  "cd" '(compile :which-key "compile current directory")
  "cp" '(project-compile :which-key "project compile")

  "oa" '(org-agenda :which-key "org-agenda")

  "t" '(my/evil-buffer-new-named :which-key "new buffer")
  "d" '(my/close-buffer-or-window :which-key "delete buffer")
  "<tab>" '(my/next-user-buffer :which-key "next-buffer")
  "<backtab>" '(my/prev-user-buffer :which-key "prev-buffer"))

(global-set-key (kbd "C-c b") #'consult-buffer)

(global-set-key (kbd "C-c d") #'my/close-buffer-or-window)

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "C-c d") #'my/close-buffer-or-window))

(with-eval-after-load 'special-mode
  (define-key special-mode-map (kbd "C-c d") #'my/close-buffer-or-window))

(defun my/c-newline-and-indent ()
  (interactive)
  (newline)
  (indent-to (* 4 (save-excursion
                    (beginning-of-line)
                    (back-to-indentation)
                    (/ (current-column) 4)))))

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)
            (setq-local indent-tabs-mode t)
            (local-set-key (kbd "RET") #'my/c-newline-and-indent)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)
            (setq-local indent-tabs-mode t)
            (local-set-key (kbd "RET") #'my/c-newline-and-indent)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org)
;; ===== Org-mode: dynamic agenda =====
(defun my/org-get-month-files (&optional date)
  "return list org-files from month for DATE, ignored lock-files."
  (let* ((date (or date (current-time)))
         (year (format-time-string "%Y" date))
         (month (format-time-string "%m_%B" date))
         (month-dir (expand-file-name (concat "~/sync/org/" year "/" month "/"))))
    (when (file-directory-p month-dir)
      (cl-remove-if (lambda (f) (string-prefix-p ".#" (file-name-nondirectory f)))
                    (file-expand-wildcards (concat month-dir "*.org"))))))

(defun my/org-agenda-files-for-current-week ()
  "return list org-files from current week."
  (let* ((today (current-time))
         (dow (string-to-number (format-time-string "%u" today))) ; 1 = Mon ... 7 = Sun
         (start-of-week (time-subtract today (days-to-time (1- dow))))
         (dates (cl-loop for i from 0 to 6
                         collect (time-add start-of-week (days-to-time i)))))
    (delete-dups
     (apply 'append
            (mapcar #'my/org-get-month-files dates)))))

(setq org-agenda-files (my/org-agenda-files-for-current-week))


;; ===== Org-mode: custom TODO status =====
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "REVIEW(r)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("IN-PROGRESS" . (:foreground "orange" :weight bold))
        ("WAITING" . (:foreground "yellow" :weight bold))
        ("REVIEW" . (:foreground "cyan" :weight bold))
        ("DONE" . (:foreground "gray" :weight bold))
        ("CANCELED" . (:foreground "red" :weight bold))))

(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

(use-package org
  :ensure t
  :config
  (setq org-agenda-files (my/org-agenda-files-for-current-week))
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "◆" "◇" "▶")))


(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode))


;; Org-modern — modern ui
(use-package org-modern
  :hook (org-mode . org-modern-mode))

(setq org-modern-star '("◉" "○" "●" "◆" "◇" "▶"))
(setq org-modern-hide-stars nil)

;; Org-download — quick paste picture to org files
(use-package org-download
  :after org
  :config
  (setq org-download-method 'directory
      org-download-image-dir "./images"
      org-download-heading-lvl nil))

(defun my/org-skip-old-entries ()
  (let* ((scheduled (org-get-scheduled-time (point)))
         (cutoff (time-subtract
                  (encode-time 0 0 0  ; 00:00:00 current day
                               (nth 3 (decode-time))  ; day
                               (nth 4 (decode-time))  ; month
                               (nth 5 (decode-time))) ; year
                  (* 3 86400))))      ; 3 days ago
    (when (and scheduled (time-less-p scheduled cutoff))
      (org-end-of-subtree t))))

(setq org-agenda-custom-commands
      '(("c" "My Clean agenda"
         agenda ""
         ((org-agenda-skip-function #'my/org-skip-old-entries)
          (org-agenda-span 7)))))
