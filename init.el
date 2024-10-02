;;; init.el -- summary

;; -*- lexical-binding: t -*-

;; TODO: fix rust save and format issues

;;
;; use-package available by deafult since 29 and archives initialised
;; in early-init.el
;;
(require 'use-package)

;; -- until emacs 30 hits:
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;;
;; Ensure other files get loaded if present, including customisation
;; (kept out of this file), encrypted secrets and a file specific to
;; the host we're running on.
;;
(setq custom-file "~/.emacs.d/.emacs-custom"
      gh/secrets-file "~/.emacs.d/.emacs-secrets"
      gh/system-config (concat user-emacs-directory system-name ".el"))

(defun gh/eval-extra-init-files ()
  (load custom-file 'no-error)
  (load gh/secrets-file 'no-error)
  (load gh/system-config 'no-error))

(add-hook 'after-init-hook 'gh/eval-extra-init-files)

;;
;;* Basic packages
;;
(use-package paradox :ensure t :defer t)
(use-package rainbow-delimiters :ensure t :defer t)
(use-package ess :ensure t :defer t)
(use-package edit-indirect :ensure t :defer t)


;;
;;* Small and miscellaneous customisations
;;
(use-package better-defaults
  :ensure t)

(use-package emacs
  :ensure bind-key
  :ensure beacon
  :ensure which-key
  :init
  (setq inhibit-splash-screen t
        visible-bell nil
        ring-bell-function 'ignore
        sentence-end-double-space nil
        help-window-select t
        require-final-newline t
        version-control t
        delete-old-versions t
        next-error-message-highlight t
        mouse-drag-and-drop-region-cross-program t
        mouse-drag-and-drop-region-scroll-margin t
        disabled-command-function nil
        switch-to-buffer-obey-display-actions t
        switch-to-buffer-in-dedicated-window 'pop
        backup-directory-alist '(("." . "~/.emacs.d/backups"))
	confirm-kill-processes nil
        indicate-empty-lines t
        window-sides-slots '(0 0 1 1)
        tab-always-indent 'complete)

  (pixel-scroll-precision-mode 1)
  (blink-cursor-mode 0)
  (column-number-mode 1)
  (auto-compression-mode 1)
  (delete-selection-mode 1)
  (beacon-mode 1)
  (which-key-mode 1)

  (require 'epa-file)
  (setq epa-pinentry-mode 'loopback)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'auto-tail-revert-mode 'tail-mode)

  (require 'autorevert)
  (global-auto-revert-mode t)

  (dolist (mode '(dired ibuffer package-menu process-menu org-agenda locate))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'gh/turn-on-hl-line-mode))

  (setq display-buffer-alist
        '(("\\*.*ompilation\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . 80)
           (window-parameters (no-delete-other-windows . t)))
          ("\\*info\\*\\|\\*Help\\*\\|\\*Shortdoc.*\\*\\|\\*Apropos\\*\\|\\*Man.*\\*\\|\\*WoMan\\*\\|\\*eww\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . 80)
           (window-parameters (no-delete-other-windows . t)))
          ("\\*e?shell.*\\*.*\\|\\*scratch\\*\\|\\*ielm\\*\\|\\*inferior-.*\\*\\|\\*.*repl\\*\\|\\*Python\\*\\|\\*Claude*\\|\\*.*GPT*\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 0.4)
           (window-parameters (no-delete-other-windows . t)))))

  :hook
  ((text-mode . turn-on-auto-fill)
   (text-mode . turn-on-flyspell))

  :bind (("C-+" . global-text-scale-adjust)
         ("C--" . global-text-scale-adjust)
         ("C-o" . query-replace)
         ("<f8>" . toggle-truncate-lines)
         ("S-<f8>" . display-line-numbers-mode)))

;;
;;* MacOS specifics
;;
(when (eq system-type 'darwin)

  (setq dired-use-ls-dired nil)
  (setq locate-command "mdfind")
  (setq consult-locate-args "mdfind")

  ;; I resolve the UK MBP `#` character problem by putting it on cmd-3
  ;; so alt-3 is still available for command prefix numerics
  (bind-key "s-3" (kbd "#") 'key-translation-map)

  (use-package exec-path-from-shell
    :ensure t
    :init  (exec-path-from-shell-initialize))

  ;; auth-source keychain stuff
  (require 'subr-x)
  (defun gh/password-from-keychain (label)
    (let ((buffer-name "*sec-output*")
	  (error-buffer-name "*sec-error*"))
      (with-temp-buffer buffer-name
			(shell-command (format "/usr/bin/security find-generic-password -w -l %s" label)
				       buffer-name
				       error-buffer-name)
			(with-current-buffer buffer-name
			  (let ((result (string-trim (buffer-string))))
			    (unless (string-empty-p result)
			      result)))))))



;;
;;* Fonts & Modeline
;;
;; Call (all-the-icons-install-fonts) if fonts aren't installed
(use-package doom-modeline
  :ensure t
  :ensure all-the-icons
  :hook (after-init . doom-modeline-mode))

;;
;;* Markdown
;; TODO: consider leanpub.com/markdown-mode/
;;
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . gfm-mode)
         "\\.apib$"))

;;
;;* Minibuffer
;;

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-count 16))

(use-package recentf
  :ensure t
  :defer t
  :init
  (recentf-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :ensure consult-projectile
  :bind (("M-y" . consult-yank-pop)))

;;
;;* Window configuration
;;
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

(use-package winner
  :defer t
  :init (winner-mode 1))

(use-package transpose-frame
  :ensure t
  :commands transpose-frame)

(defun toggle-window-dedicated ()
  "Set window as dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: dedicated window"
     "%s: undedicated window")
   (current-buffer)))

;; quick access to occur from interactive search
(define-key isearch-mode-map (kbd "C-o")
	    (lambda () (interactive)
	      (let ((case-fold-search isearch-case-fold-search))
		(occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;
;;* Completion
;;

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t
        corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

;;
;;* Projects
;;

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'default)
  (projectile-switch-project-action #'projectile-commander)
  (projectile-create-missing-test-files t)
  (projectile-project-root-functions '(projectile-root-local
                                       projectile-root-marked
                                       projectile-root-top-down
                                       projectile-root-bottom-up
                                       projectile-root-top-down-recurring))
  :config
  (def-projectile-commander-method ?S
				   "Open a *shell* buffer for the project."
				   (projectile-run-shell))

  (def-projectile-commander-method ?F
				   "Refresh: fetch from git and go to magit status"
				   (magit-fetch-from-upstream)
				   (projectile-vc))

  (projectile-global-mode))


;;
;;* Dired
;;
(use-package dired
  :init
  (setq dired-listing-switches "-alh"
	dired-kill-when-opening-new-dired-buffer t))

;;
;;* Git
;;
(use-package magit
  :ensure t
  :ensure git-timemachine
  :ensure git-auto-commit-mode
  :bind (("C-c g" . magit-status)
         ("C-c b" . magit-blame)
         ("C-c t" . git-timemachine))
  :init
  (setq magit-restore-window-configuration t
	magit-log-auto-more t
	magit-status-buffer-switch-function 'switch-to-buffer))

(require 'epg)
(setq epg-pinentry-mode 'loopback)

;;
;;* AI
;;

(defun gh/retrieve-openai-key () (gh/password-from-keychain "gptshell"))
(defun gh/retrieve-anthropic-key () (gh/password-from-keychain "anthropic-api-key"))

(use-package gptel
  :ensure t
  :bind (("C-c <RET>" . gptel-send))
  :config
  (setq gptel-api-key 'gh/retrieve-openai-key)
  (gptel-make-anthropic "Claude" :stream t :key 'gh/retrieve-anthropic-key)
  (setq gptel-default-mode 'org-mode))


(use-package copilot
  :vc (:fetcher github :repo copilot-emacs/copilot.el)
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (("C-<tab>" . copilot-accept-completion)
	 ("C-c C-v" . copilot-accept-completion-by-word)
	 ("C-c C-n" . copilot-next-completion)
	 ("C-c C-p" . copilot-previous-completion)
	 ("C-c C-d" . copilot-clear-overlay)))

;;
;;* Programming
;;

;;
;;** General programming mode settings
;;

(use-package prog-mode
  :ensure ws-butler
  :preface

  (defun gh/local-comment-auto-fill ()
    (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (auto-fill-mode nil))

  (defun gh/set-comment-columns ()
    (set (make-local-variable 'comment-fill-column) 80))

  (defun gh/turn-on-hl-line-mode ()
    (when (> (display-color-cells) 8)
      (hl-line-mode t)))

  (defun gh/add-watchwords ()
    (font-lock-add-keywords
     nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|QQ\\)"
            1 font-lock-warning-face t))))

  (defun gh/truncate-lines ()
    (setq truncate-lines t))

  :hook
  ((prog-mode . gh/local-comment-auto-fill)
   (prog-mode . gh/set-comment-columns)
   (prog-mode . prettify-symbols-mode)
   (prog-mode . gh/add-watchwords)
   (prog-mode . gh/truncate-lines)
   (prog-mode . ws-butler-mode)
   (prog-mode . turn-on-eldoc-mode))

  :init
  (use-package iedit
    :ensure t
    :bind ("C-;" . iedit-mode)))


(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (require 'smartparens-rust)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :config
  (sp-use-smartparens-bindings))


;;
;;** Language server protocol
;;
(use-package eglot
  :ensure t
  :init
  ;; don't lose flymake errors in the minibuffer behind eldoc
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose))

;;
;;** Rust
;;
;; (use-package rustic
;;   :ensure t

;;   :preface
;;   (defun rk/rustic-mode-hook ()
;;     ;; so that run C-c C-c C-r works without having to confirm
;;     (when buffer-file-name
;;       (setq-local buffer-save-without-query t)))

;;   :bind (:map rustic-mode-map
;;               ("C-c C-c r" . eglot-rename))
;;   :commands rustic
;;   :hook (rustic-mode-hook . rk/rustic-mode-hook)
;;   :config
;;   (setq rustic-format-on-save t
;; 	rustic-lsp-client 'eglot))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :bind (:map rustic-mode-map
              ("C-c C-c r" . eglot-rename))
  :mode (("\\.rs$" . rustic-mode))
  :init
  (setq rustic-format-on-save t
	rustic-lsp-client 'eglot))

;;
;;** Python
;;
(use-package python
  :ensure t
  :ensure pet
  :ensure blacken
  :commands (run-python)
  :mode (("\\.py$" . python-mode))
  :custom
  (python-shell-completion-native-enable nil)
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook ((python-mode . eglot-ensure)
	 (python-mode . pet-mode)
	 (python-mode . blacken-mode)) )

;;
;;** Lisps
;;
(use-package lispy
  :ensure t
  :init (setq lispy-compat '(edebug cider)))

(dolist (mode '(scheme emacs-lisp lisp clojure racket fennel))
  (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
    (add-hook hook (lambda () (lispy-mode 1)))
    (add-hook hook 'rainbow-delimiters-mode)))

;; hacks for kapow
(use-package lisp-mode
  :disabled t
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
              (setq tab-width 2)
              (put '$define! 'lisp-indent-function 1)
              (put '$vau 'lisp-indent-function 'defun)
              (put '$lambda 'lisp-indent-function 'defun)
              (put '$provide! 'lisp-indent-function 1)
              (put '$let 'lisp-indent-function 1)
              (put '$let* 'lisp-indent-function 1)
              (put '$letrec 'lisp-indent-function 1)
              (put '$letrec* 'lisp-indent-function 1)
              (put '$let-redirect 'lisp-indent-function 1)
              (put '$let-safe 'lisp-indent-function 1))))

;;
;;** Clojure
;;
(use-package cider
  :ensure t
  :ensure seq
  :ensure clojure-mode
  :ensure clj-refactor
  :mode (("\\.clj\\[scx\\]?$" . clojure-mode)
         ("\\.boot$" . clojure-mode))

  :preface
  (defun gh/set-indents ()
    (define-clojure-indent
     (ANY 2)
     (DELETE 2)
     (GET 2)
     (HEAD 2)
     (POST 2)
     (PUT 2)
     (with 2)
     (match 1)
     (domonad 1)
     (context 2)
     (for-all 1)
     (defui '(1 nil nil (1)))
     (defroutes 'defun)))

  :init
  (setq cider-auto-select-error-buffer t
        cider-repl-use-clojure-font-lock t
        nrepl-hide-special-buffers t
	cljr-favor-prefix-notation t
	cider-print-quota (* 1024 10)
	lispy-thread-last-macro "->>")
  :hook ((clojure-mode . gh/set-indents)
         (clojure-mode . (lambda ()
                           (clj-refactor-mode 1)
                           (cljr-add-keybindings-with-prefix "C-c r")))
         (cider-repl-mode . subword-mode)))


;;
;;** Java
;;
(use-package java-mode
  :mode "\\.java$"
  :hook
  (java-mode-hook . (lambda () (setq tab-width 2) (subword-mode t))))


;;;
;;;** C++
;;;
(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")


;;
;;** Golang
;;
(use-package go-mode
  :ensure t
  :ensure go-autocomplete
  :ensure go-projectile
  :mode "\\.go"
  :init
  (add-hook 'before-save-hook 'gofmt-before-save))


;;
;;** More rarely used languages
;;
(use-package fennel-mode :defer t)
(use-package racket-mode :ensure t :defer t)
(use-package kotlin-mode :defer t)
(use-package swift-mode :defer t)
(use-package tuareg :defer t)           ; OCaml
(use-package erlang :defer t)
(use-package elixir :defer t)
(use-package julia :defer t)
(use-package scala :defer t)
(use-package terraform-mode :ensure t :defer t)
(use-package docker :ensure t :ensure dockerfile-mode :defer t)

;;
;;** Haskell, Elm, Frege
;;
(use-package haskell-mode
  :ensure t
  :ensure hindent
  :mode (("\\.hs" . haskell-mode)
         ("\\.fr" . haskell-mode))
  :init
  (setq haskell-compile-cabal-build-command "stack build --test --fast --file-watch --copy-bins --exec 'hlint .'")
  :hook (
         (haskell-mode-hook . turn-on-haskell-indentation)
         (haskell-mode-hook . hindent-mode)))

(use-package elm-mode :defer t)


;;
;;** CSS
;;
(use-package css-mode
  :ensure rainbow-mode
  :mode "\\.css"
  :hook (css-mode . rainbow-mode)
  :init
  (add-hook 'css-mode-hook 'rainbow-mode))

;;
;;** Other development modes
;;
(use-package log4j-mode
  :ensure t
  :mode "\\.log$"
  :hook
  (log4j-mode . gh/turn-on-hl-line-mode)
  (log4j-mode . (lambda () (read-only-mode t) (view-mode t))))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

(use-package restclient :ensure t :mode "\\.http")

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;
;;** Treesitter
;;
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (fennel "https://github.com/TravonteD/tree-sitter-fennel")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode))
  :preface

  (defun gh/ensure-grammars-installed ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (message (car (cdr grammar)))
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (mapping
           '((typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :hook ((tsx-ts-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
	 (json-ts-mode . eglot-ensure))

  :config
  (gh/ensure-grammars-installed))

;;
;;** Ruby - barely ever use it so experiment with tree-sitter
;;
(use-package ruby-ts-mode
  :ensure t
  :mode ("\\.rb$"
         "\\.rake$"
         "\\.gemspec$"
         "\\.ru$"
         "\\.thor$"
         "Rakefile$"
         "Thorfile$"
         "Gemfile$"
         "Gapfile$"
         "Vagrantfile$"))

;;
;;* Shells and Terminals
;;

;;
;;** Eshell
;;
(use-package eshell
  :init
  (defalias 'x 'delete-window)
  (defalias 'd 'dired)
  (defalias 'ff 'find-file)
  (defalias 'g 'magit-status)

  :commands (eshell))

;;
;;** Remote shells
;;
(use-package tramp
  :init
  (set-default 'tramp-auto-save-directory (expand-file-name "~/temp")))

;;
;;* Eat terminal
;;
(use-package eat
  :disabled t                             ; terminfo seems broken on macos
  :ensure t)

;;
;;** Toggle access
;;
(defun gh/toggle-eshell (&optional arg)
  "Call eshell to launch or reuse an eshell, unless we're in one and there are no args - then close."
  (interactive "P")
  (if arg
      (eshell arg)
    (if (string-match-p "\\*eshell.*" (buffer-name))
	(quit-window)
      (eshell))))

(defun gh/toggle-shell (&optional arg)
  "Call gptel to launch or reuse gptel session, unless we're in one and there are no args - then close."
  (interactive "P")
  (if current-prefix-arg
      (call-interactively 'shell)
    (if (string-match-p "\\*shell.*" (buffer-name))
	(quit-window)
      (call-interactively 'shell))))

(defun gh/toggle-gptel (&optional arg)
  "Call gptel to launch or reuse gptel session, unless we're in one and there are no args - then close."
  (interactive "P")
  (if-let ((buf (if current-prefix-arg
		    (call-interactively 'gptel)
		  (if gptel-mode
		      (quit-window)
		    (call-interactively 'gptel)))))
      ;; Don't know why gptel window isn't automatically selected...
      (select-window (get-buffer-window buf))))

(bind-keys
 ("<f9>" . gh/toggle-eshell)
 ("S-<f9>" . gh/toggle-shell)
 ("M-<f9>" . gh/toggle-gptel)
 ("C-<f9>" . run-python))

;;
;;* Org Mode
;;

;; HACK: `org-directory', `org-default-notes-file', `gh/refile-file' are
;; different on my different systems - ensure they're set in
;; `gh/system-config'
(use-package org
  :ensure ob-restclient

  :hook ((org-mode . turn-on-visual-line-mode)
         (auto-save . org-save-all-org-buffers))

  :custom

  (org-adapt-indentation nil)
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-cycle-separator-lines 0)
  (org-insert-heading-respect-content t)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-use-speed-commands t)
  (org-catch-invisible-edits 'show-and-error)

  (org-todo-keywords '((sequence "NEXT(n)"
                                 "TODO(t)"
                                 "PROJECT(p)"
                                 "|" "DONE(d)")
                       (sequence "WAITING(w@/!)"
                                 "HOLD(h@/!)"
                                 "SOMEDAY(s@/!)"
                                 "DEFERRED(f@/!)"
                                 "|" "CANCELLED(c@/!)")))

  (org-tags-exclude-from-inheritance '("PROJECT"))
  (org-stuck-projects '("+TODO=\"PROJECT\"" ("NEXT") nil "\\<IGNORE\\>"))

  ;; Agenda
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              ;; Indent todo items by level to show nesting
                              (todo . " %i %-12:c%l")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-span 'day)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-compact-blocks t)
  (org-agenda-custom-commands
   '(("-" "Grand Unified Agenda"
      ((agenda ""
               ((org-agenda-ndays 1)
                (org-agenda-overriding-header "Today")))
       (tags "FLAGGED"
             ((org-agenda-overriding-header "Flagged")
              (org-show-context-detail 'lineage)))
       (tags "refile|tidy"
             ((org-agenda-overriding-header "To Refile / Tidy")))
       (tags-todo "PRIORITY=\"A\"-SCHEDULED={.+}"
                  ((org-agenda-overriding-header "Unscheduled High Priority")))
       (org-agenda-list-stuck-projects)))))

  ;; clock
  (org-clock-into-drawer t)
  (org-clock-continuously t)
  (org-clock-in-resume t)
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)

  ;; Refile
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Archival
  (org-archive-location "archive/%s_archive::datetree/")

  (org-capture-templates
   '(("s" "start day" entry (function org-journal-find-location)
      (file "templates/template-day.org")
      :clock-in t
      :unnarrowed t)

     ("t" "todo" entry (file gh/refile-file)
      "* TODO %?\n")

     ("r" "respond" entry (file gh/refile-file)
      "* NEXT Respond to %? \nSCHEDULED: %t\n%U\n%a\n"
      :clock-in t
      :clock-resume t
      :immediate-finish t)

     ("n" "note" entry (file gh/refile-file)
      "* %? :NOTE:\n%U\n\n"
      :clock-in t
      :clock-resume t)

     ("i" "interruption" entry (file gh/refile-file)
      "* %? :INTERRUPTION:\n\n\n"
      :clock-resume t
      :clock-in t)

     ("m" "meeting" entry (file gh/refile-file)
      "* MEETING %? :MEETING:\n%U"
      :clock-in t
      :clock-resume t)))

  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-babel-clojure-backend 'cider)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("<f12>" . org-agenda)
         ("C-c c" . org-capture))

  :init
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (clojure . t)
                                 (python . t)
                                 (shell . t)
                                 (dot . t)
                                 (restclient . t))))



;;
;;* Appearance
;;

;;
;;** Fonts
;;
(require 'dash)
(defun set-font-from (fonts)
  (when window-system
      (when-let (font (-first (lambda (x) (not (eq (font-info x) nil))) fonts))
	(set-frame-font font))))

(set-font-from '("Hack" "Hack Nerd Font Mono" "FiraCode Nerd Font Mono" "Consolas"))

;;
;;** Themes
;;
(use-package solarized-theme :ensure t :defer t)
(use-package tao-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package nimbus-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package twilight-bright-theme :ensure t :defer t)
(use-package base16-theme :ensure t :defer t)
(use-package plan9-theme :ensure t :defer t)

(defun gh/clear-themes ()
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes))

(defun gh/enable-theme (theme)
  (progn
    (gh/clear-themes)
    (message (format "Switched to theme: %s" theme))
    (load-theme theme t)))

(setq gh/dark-themes '(zenburn
		       nimbus
		       tao-yin
		       base16-kimber
                       base16-monokai
                       sanityinc-tomorrow-bright
                       sanityinc-tomorrow-eighties
                       sanityinc-tomorrow-blue
		       base16-greenscreen
		       material))

(setq gh/light-themes '(tao-yang
			leuven
			material-light
			twilight-bright))

(defun gh/cycle-themes (themes)
  (let* ((current-theme (car custom-enabled-themes))
         (tail (member current-theme themes))
         (next-theme (if (or (null tail)
                             (null (cdr tail)))
                         (car themes)
                       (cadr tail))))
    (gh/enable-theme next-theme)))

(defun gh/cycle-dark-themes ()
  (interactive)
  (gh/cycle-themes gh/dark-themes))

(defun gh/cycle-light-themes ()
  (interactive)
  (gh/cycle-themes gh/light-themes))

(bind-key "<f6>" 'gh/cycle-dark-themes)
(bind-key "S-<f6>" 'gh/cycle-light-themes)

(gh/enable-theme 'nimbus)


;;
;;** Transparency / Focus
;;
(defvar gh/transparent nil)

;; nb. could use alpha-background as of 29.1 but the main use case is
;; occasionally to see through the window (on small displays) so alpha
;; works pretty well
(defun gh/set-alpha (alpha-value)
  (set-frame-parameter (selected-frame) 'alpha alpha-value)
  (add-to-list 'default-frame-alist (list 'alpha alpha-value)))

(defun gh/toggle-transparency ()
  (interactive)
  (if gh/transparent
      (progn
	(setq gh/transparent nil)
	(gh/set-alpha '(100 . 100)))
    (progn
      (setq gh/transparent t)
      (gh/set-alpha '(82 . 72)))))

(defun gh/toggle-frame-header ()
  (interactive)
  (set-frame-parameter nil 'undecorated-round (not (frame-parameter nil 'undecorated-round))))

(defun gh/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed else narrow to appropriate region."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
	((region-active-p) (narrow-to-region (region-beginning) (region-end)))
	((derived-mode-p 'org-mode)

	 (cond ((ignore-errors (org-edit-src-code) t) (delete-other-windows))
	       ((ignore-errors (org-narrow-to-block) t))
	       (t (org-narrow-to-subtree))))
	(t (narrow-to-defun))))

(bind-keys
 ("s-u" . gh/toggle-transparency)
 ("<f7>" . gh/narrow-or-widen-dwim))


(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-define-global "jj" 'ibuffer)
    (key-chord-define-global "j0" 'delete-window)
    (key-chord-define-global "j1" 'delete-other-windows)
    (key-chord-define-global "JJ" 'magit-status)
    (key-chord-define-global ",," 'consult-buffer)
    (key-chord-define-global "hh" 'consult-buffer-other-window)
    (key-chord-define-global "jk" 'transpose-frame)
    (key-chord-define-global "jf" 'consult-recent-file)
    (key-chord-define-global "jp" 'consult-projectile)
    (key-chord-define-global "jw" 'ace-window)
    (key-chord-define-global "kk" 'quit-window)
    (key-chord-define-global "YY" 'consult-yank-pop)
    (key-chord-mode 1)))

;;
;; Enable server / emacsclient
;;
(server-start)




;;
;; Experimental dispatch functions for access by an experimental
;; Alfred workflow I'm working on. Any functions prefixed `alfred/x`
;; are available in Alfred as `e/x`.
;;
(defun alfred/repl (text)
  "Send TEXT to a current CIDER repl."
  (cider--switch-to-repl-buffer (cadar (cider-sessions)))
  (cider-insert-in-repl text t))
