;;; init.el
;;
;; Much of this is stolen from elsehwere, in particular Phil
;; Hagelberg's emacs-starter-kit, while it existed.
;;

(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(use-package
                      better-defaults
                      ido-ubiquitous
                      exec-path-from-shell
                      idle-highlight-mode
                      paredit
                      rainbow-delimiters
                      groovy-mode
                      scala-mode
                      lua-mode
                      puppet-mode
                      ess
                      powershell
                      zenburn-theme
                      soft-stone-theme
                      soft-morning-theme
                      color-theme-solarized
                      color-theme-sanityinc-tomorrow
                      base16-theme
                      melpa)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case err
        (package-install p)
      (error
       (message "%s" (error-message-string err))))))

(defun gh/package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
   are installed and are not in `my-packages'.  useful for
   cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(defun gh/package-list-installed-packages ()

  (interactive)
  (package-show-package-list
   (remove-if-not
    'package-installed-p
    (mapcar 'car package-archive-contents))))

(defmacro gcr/on-osx (statement &rest statements)
  "Evaluate the enclosed body only when run on OSX."
  `(when (eq system-type 'darwin)
     ,statement
     ,@statements))

(gcr/on-osx
 ;; try and get appropriate path by looking at what shell does
 (exec-path-from-shell-initialize)
 ;; and make sure we have a few other auth settings
 (exec-path-from-shell-copy-envs '("AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY"))
 ;; typing hash on a UK mac in emacs is tricky
 (global-set-key (kbd "s-3") '(lambda () (interactive) (insert "#")))
 ;; work around "empty or unsupported pasteboard type" bug
 ;; should be fixed in 24.4 so can remove at that point
 (setq save-interprogram-paste-before-kill nil))

;;
;; Load other files
;;
(setq gh/system-config (concat user-emacs-directory system-name ".el")
      gh/user-config (concat user-emacs-directory user-login-name ".el")
      gh/user-dir (concat user-emacs-directory user-login-name))


(defun gh/eval-after-init (form)
    "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
    (let ((func (list 'lambda nil form)))
      (add-hook 'after-init-hook func)
      (when after-init-time
        (eval form))))

(gh/eval-after-init 
 '(progn
    (when (file-exists-p gh/system-config) (load gh/system-config))
    (when (file-exists-p gh/user-config) (load gh/user-config))
    (when (file-exists-p gh/user-dir)
      (mapc 'load (directory-files gh/user-dir t "^[^#].*el$")))))

;; use package
(require 'use-package)

;;
;; Markdown
;;
(use-package markdown-mode
  :ensure t
  :mode ("\\.md$"
         "\\.apib$"))

;;
;; helm / ido / smex / completion / jump / switching
;;

(use-package helm-config
  :bind (("M-x" . helm-M-x)
         ("C-x f" . helm-multi-files)
         ("C-x b" . helm-mini)
         ("C-h a" . helm-apropos))
  :config

  (use-package helm
    :diminish helm-mode
    :init (helm-mode 1))
  
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package helm-swoop :ensure t)

(use-package ido
  :init
  ;; ido with flex-matching already turned on in better-defaults.el
  (setq ido-enable-prefix nil
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window)
  :config
  (ido-ubiquitous t))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

; quick access to occur from interactive search
(define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; company mode wherever - unless it gets slow
(use-package company
  :ensure t
  :defer 10
  :diminish company-mode
  :config (global-company-mode))

;; snippets
(use-package yasnippet
  :ensure t
  :defer 30
  :diminish yas-minor-mode
  :commands yas/hippie-try-expand
  :init (progn
          (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)
          (yas-global-mode 1)))

;; projectile mode everywhere
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :defer 5
  :config
  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode))

(defun gh/kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

;; undo for window config C-c <- and C-c ->
(use-package winner
  :init
  (winner-mode 1))

;;
;; basic defaults
;;
(prefer-coding-system 'utf-8)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)

(setq require-final-newline t)
(setq inhibit-splash-screen t)
(blink-cursor-mode 0)
(auto-compression-mode 1)
(setq visible-bell nil)
(setq ring-bell-function nil)
(delete-selection-mode 1)

;; make mouse and gesture scroll work sanely
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; prefer side-by-side window splits if the window is wide
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;;
;; Keep customize-based settings separate
;;
(setq custom-file "~/.emacs.d/.emacs-custom")
(load custom-file 'no-error)

;;
;; backups already in .emacs.d/backups -
;; keep several old versions but delete excess without prompting
(setq version-control t)
(setq delete-old-versions t)

;;
;; Magit
;;
(use-package magit
  :ensure t
  :ensure git-timemachine
  :bind (("C-c g" . magit-status)
         ("C-c G" . magit-status-with-prefix))
  :init
  (progn
    (setq magit-restore-window-configuration t)
    (setq magit-log-maybe-show-more-commits t)
    (setq magit-status-buffer-switch-function 'switch-to-buffer)))

;;
;; lambdas and todos
;;
(defun gh/local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun gh/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun gh/turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun gh/pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun gh/pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u0192"
                                                           'decompose-region)))))))

(defun gh/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun gh/truncate-lines ()
  (setq truncate-lines t))

(add-hook 'prog-mode-hook 'gh/local-column-number-mode)
(add-hook 'prog-mode-hook 'gh/local-comment-auto-fill)
(add-hook 'prog-mode-hook 'gh/pretty-lambdas)
(add-hook 'prog-mode-hook 'gh/add-watchwords)
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'gh/truncate-lines)
(add-hook 'prog-mode-hook 'electric-indent-mode)

(defun gh/prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

;;
;; Lisp modes
;;
(dolist (mode '(scheme emacs-lisp lisp clojure racket))
  (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
    (add-hook hook 'gh/prog-mode-hook)
    (add-hook hook 'paredit-mode)
    (add-hook hook 'rainbow-delimiters-mode)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;
;; Haskell mode
;;
(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

;;
;; OCaml
;;
(use-package tuareg
  :ensure t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

;;
;; Javascript
;;
(use-package js
  :ensure js2-mode
  :ensure json-mode
  :ensure ac-js2
  :ensure skewer-mode
  :ensure js-comint
  :init
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js-mode-hook 'skewer-mode)
    (add-hook 'js2-mode-hook 'ac-js2-mode)
    (add-hook 'inferior-js-mode-hook 'ansi-color-for-comint-mode-on)
    (setenv "NODE_NO_READLINE" "1")
    (setq tab-width 2)
    (setq js-indent-level 2)
    (setq js2-highlight-level 3)
    (setq inferior-js-program-command "node --interactive"))
  :config
  (progn
    (font-lock-add-keywords
     'js-mode `(("\\(function *\\)("
                 (0 (progn (compose-region (match-beginning 1)
                                           (match-end 1) "\u0192")
                           nil)))))
    (bind-key "{" 'paredit-open-curly js-mode-map)
    (bind-key "}" 'paredit-close-curly js-mode-map)))

;;
;; TypeScript mode
;;
(use-package typescript-mode
  :ensure t)

;;
;; CSS mode
;;
(use-package css-mode
  :ensure rainbow-mode
  :init
  (progn
    (add-hook 'css-mode-hook 'gh/prog-mode-hook)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)))

;;
;; HTML
;;
(use-package html-mode
  :init
  (add-hook 'html-mode-hook 'skewer-mode))

;;
;; Ensure that lisp-interaction can still evaluate on ctrl-j...
;;
(defadvice paredit-newline (around eval-print-last-sexp activate)
  (if (eq major-mode 'lisp-interaction-mode)
      (eval-print-last-sexp)
    (paredit-newline)))

;;
;; Org Mode
;;
(setq default-major-mode 'org-mode)
(setq org-directory "~/dropbox/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-mobile-directory "~/dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/dropbox/from-mobile.org")

(setq org-ditaa-jar-path "~/.emacs.d/deps/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/deps/plantuml.jar")
(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://www.gmorpheme.net/theme/css/main.css\">
<script type=\"text/javascript\">
WebFontConfig = { fontdeck: { id: '35882' } }; (function() {
  var wf = document.createElement('script');
  wf.src = ('https:' == document.location.protocol ? 'https' : 'http') +
  '://ajax.googleapis.com/ajax/libs/webfont/1/webfont.js';
  wf.type = 'text/javascript';
  wf.async = 'true';
  var s = document.getElementsByTagName('script')[0];
  s.parentNode.insertBefore(wf, s);
})();</script>")

(setq org-html-head-include-default-style nil)

(defvar gh/org-mobile-sync-timer nil)

(defun gh/org-mobile-sync ()
  (message "Syncing org-mobile...")
  (org-mobile-pull)
  (org-mobile-push))

(defun gh/org-mobile-start-sync ()
  "Start automated `org-mobile-push'"
  (interactive)
  (setq gh/org-mobile-sync-timer
        (run-with-idle-timer (* 60 20) t 'gh/org-mobile-sync)))

(defun gh/org-mobile-stop-sync ()
  "Stop automated `org-mobile-push'"
  (interactive)
  (cancel-timer gh/org-mobile-sync-timer))

(gh/org-mobile-start-sync)

(setq org-use-speed-commands t)

(setq org-clock-into-drawer t)
;; Remove zero second clocks
(setq org-clock-out-remove-zero-time-clocks t)
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK" "RESULTS"))

;; exclude certain tags from inheritance
(setq org-tags-exclude-from-inheritance '("PROJECT"))

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-compact-blocks t)
(setq org-agenda-custom-commands
      '(("y" "Grand Unified Agenda"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-agenda-overriding-header "=== Today")))
          (tags-todo "ORGANISE"
                     ((org-agenda-overriding-header "=== Daily Admin")))
          (tags "REFILE|TIDY"
                ((org-agenda-overriding-header "=== To Refile / Tidy")))
          (tags "WAITING"
                ((org-agenda-overriding-header "=== To Chase")))
          (tags-todo "PRIORITY=\"A\"-SCHEDULED={.+}"
                     ((org-agenda-overriding-header "=== Unscheduled High Priority")))))
        ("3" tags-todo "T3")
        ("e" tags-todo "ERRAND")))

(setq org-capture-templates
      '(("s" "Start of day" entry (file+datetree org-default-notes-file)
         (file "template-day.org")
         :clock-in t)
        
        ("w" "Weekly review" entry (file+datetree org-default-notes-file)
         (file "template-week.org")
         :clock-in t)
        
        ("m" "End of month review" entry (file+datetree org-default-notes-file)
         (file "template-month.org") :clock-in t)
        
        ("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
         "* TODO %?\n  %i\n  %a")
        
        ("d" "Distraction / called away" entry (file+headline org-default-notes-file "Inbox")
         "* %?\n %i\n %a\n" :clock-resume t :clock-in t)))

(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Archival
(setq org-archive-location "%s_archive::datetree/")

;; Key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;; Distraction on f12
(define-key global-map [f12]
  (lambda () (interactive) (org-capture nil "d")))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (plantuml . t))))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(setq org-confirm-babel-evaluate nil)

;;
;; Check for modifications to open files.
;;
(require 'autorevert)
(global-auto-revert-mode t)

;;
;; Horizontal line hightlighting in list modes
;;
(dolist (mode '(dired ibuffer package-menu process-menu org-agenda))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'gh/turn-on-hl-line-mode))

;;
;; windows reg files are UTF-16 little endian - please read them properly.
;;
(modify-coding-system-alist 'file "\\.reg\\'" 'utf-16-le)

;;
;; Python
;;
(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook 'gh/prog-mode-hook))

;;
;; Windows specifics
;;
(if (eq system-type 'windows-nt)
    (progn
      (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
                                        ; stop hangs?
      (setq w32-get-true-file-attributes nil)
      (remove-hook 'text-mode-hook 'turn-on-flyspell)))

;;
;; Theme
;;
(defun gh/clear-themes ()
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes))

(defun gh/enable-theme (theme)
  (progn
    (gh/clear-themes)
    (message (format "Switched to theme: %s" theme))
    (load-theme theme)))

(setq gh/dark-themes '(zenburn
                       sanityinc-tomorrow-blue
                       sanityinc-tomorrow-bright
                       sanityinc-tomorrow-eighties
                       base16-monokai-dark
                       base16-mocha-dark
                       base16-ocean-dark))

(setq gh/light-themes '(soft-morning
                        soft-stone
                        solarized
                        base16-embers-light))

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

(global-set-key [(f6)] 'gh/cycle-dark-themes)
(global-set-key [(shift f6)] 'gh/cycle-light-themes)

;;
;; CIDER / Clojure / ClojureScript / clj-refactor
;;
(use-package cider
  :ensure clojure-mode
  :ensure clj-refactor
  :ensure t
  :mode (("\\.clj\\[scx\\]?$" . clojure-mode)
         ("\\.boot$" . clojure-mode))
  :init
  (progn
    (setq cider-repl-popup-stacktraces t
          cider-auto-select-error-buffer t
          cider-repl-print-length 200
          cider-prompt-save-file-on-load nil
          cider-repl-use-clojure-font-lock t
          nrepl-hide-special-buffers t)
    (add-hook 'clojure-mode-hook
              (lambda ()
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
                  (defroutes 'defun))))
    (add-hook 'clojure-mode-hook (lambda ()
                                   (clj-refactor-mode 1)
                                   (cljr-add-keybindings-with-prefix "C-c C-m")))
    (add-hook 'clojure-mode-hook 'gh/pretty-fn)
    (add-hook 'cider-repl-mode-hook #'subword-mode)
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'subword-mode)
    (add-hook 'cider-repl-mode-hook #'paredit-mode))
  :config
  (use-package midje-mode)
  (use-package eval-sexp-fu
    :ensure t)
  (use-package cider-eval-sexp-fu
    :ensure t))

;;
;; REST client
;;
(use-package restclient
  :ensure t
  :init (add-hook 'restclient-mode-hook
                  (lambda () (setq tab-width 2))))

;;
;; Ruby
;;
(use-package enh-ruby-mode
  :ensure t
  :ensure rvm
  :ensure robe
  :mode ("\\.rb$"
         "\\.rake$"
         "\\.gemspec$"
         "\\.ru$"
         "\\.thor$"
         "Rakefile$"
         "Thorfile$"
         "Gemfile$"
         "Gapfile$"
         "Vagrantfile$")
  :interpreter "ruby"
  :init
  (progn
    (defadvice inf-ruby-console-auto
        (before activate-rvm-for-robe activate)
      (rvm-activate-corresponding-ruby))
    (add-hook 'enh-ruby-mode-hook 'robe-mode))
  :config
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-hanging-brace-indent-level 2))

;;
;; Erlang
;;
(use-package erlang
  :ensure t
  :ensure edts
  :config
  (require 'edts-start))

;;
;; Racket
;;
(use-package racket-mode
  :ensure t)

;;
;; Terraform
;;
(use-package terraform-mode
  :ensure t)

;;
;; EShell
;;
(use-package eshell
  :init
  (progn
    (use-package em-smart)
    (setq eshell-where-to-jump 'begin)
    (setq eshell-review-quick-commands nil)
    (setq eshell-smart-space-goes-to-end t)))

;;
;; Remote shells
;;
(use-package tramp
  :init
  (set-default 'tramp-auto-save-directory (expand-file-name "~/temp")))

;;
;; Octave
;;
;; for some reason this is in octave-mod on my mac 
(autoload 'octave-mode "octave-mod" nil t)
(use-package octave-mode
  :mode ("\\.m$" . octave-mode))

;;
;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;;
;; Julia
;;
(use-package julia-mode
  :ensure t)

;;
;; My bindings
;;
(global-set-key "\C-o" 'query-replace)
(global-set-key [(control next)] 'scroll-other-window)
(global-set-key [(control prior)] 'scroll-other-window-down)
(global-set-key [(f8)] 'toggle-truncate-lines)
(global-set-key [(shift f8)] 'linum-mode)
;; put all kinds of shells on f9...
(global-set-key [(f9)] 'py-shell)
(global-set-key [(shift f9)] 'shell) 
(global-set-key [(ctrl f9)] 'eshell)

;;
;; keychords and hydras to combat emacs-pinky
;;

(use-package hydra
  :ensure t)

(use-package transpose-frame
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-define-global "jj" 'ibuffer)
    (key-chord-define-global "j1" 'delete-other-windows)
    (key-chord-define-global "JJ" 'magit-status)
    (key-chord-define-global "jz" 'magit-dispatch-popup)
    (key-chord-define-global ",," 'helm-mini)
    (key-chord-define-global "hh" 'ido-switch-buffer-other-window)
    (key-chord-define-global "jk" 'transpose-frame)
    (key-chord-define-global "jf" 'ido-find-file)
    (key-chord-define-global "jg" 'org-agenda)
    (key-chord-define-global "jp" 'projectile-find-file)
    (key-chord-define-global "jt" 'ace-jump-mode)
    (key-chord-define-global "jw" 'ace-window)
    (key-chord-define-global "kk" 'gh/kill-current-buffer)
    (key-chord-mode 1)))

(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window (:color amaranth)
   "window"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
        "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
        "horz")
   ("t" transpose-frame "'" :color blue)
   ("o" delete-other-windows "one" :color blue)
   ("a" ace-window "ace")
   ("s" ace-swap-window "swap")
   ("d" ace-delete-window "del")
   ("i" ace-maximize-window "ace-one" :color blue)
   ("b" ido-switch-buffer "buf")
   ("q" nil "cancel")))
