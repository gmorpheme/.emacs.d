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

(defvar my-packages '(better-defaults
                      smex
                      ace-jump-mode
                      ace-window
                      magit
                      company
                      projectile
                      key-chord
                      clojure-mode
                      clojure-test-mode
                      clojurescript-mode
                      clojure-snippets
                      rainbow-delimiters
                      python-mode
                      groovy-mode
                      ruby-mode
                      puppet-mode
                      powershell
                      yasnippet
                      zenburn-theme
                      rainbow-mode
                      cider
                      midje-mode
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

;;
;; ido / smex / completion / jump / switching
;;
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(ido-ubiquitous t)
; ido with flex-matching already turned on in better-defaults.el
(setq ido-enable-prefix nil
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
; rebind other window to superior ace-window
(define-key global-map (kbd "C-x o") 'ace-window)

; quick access to occur from interactive search
(define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; company mode wherever - unless it gets slow
(add-hook 'after-init-hook 'global-company-mode)

(defun gh/kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

;; prefer side-by-side window splits if the window is wide
(setq split-height-threshold nil)
(setq split-width-threshold 160)


;;
;; basic defaults
;;
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

(setq inhibit-splash-screen t)
(auto-compression-mode 1)
(setq visible-bell t)
(delete-selection-mode 1)

;;
;; Magit
;;
(global-set-key (kbd "C-c g") 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

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
(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
  (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
    (add-hook hook 'gh/prog-mode-hook)
    (add-hook hook 'paredit-mode)
    (add-hook hook 'rainbow-delimiters-mode)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;
;; Javascript
;;
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;;;###autoload
(eval-after-load 'js
  '(progn (define-key js-mode-map "{" 'paredit-open-curly)
          (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
          (add-hook 'js-mode-hook 'gh/paredit-nonlisp)
          (setq js-indent-level 2)
          ;; fixes problem with pretty function font-lock
          (define-key js-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))

;;
;; CSS mode
;;

(add-hook 'css-mode-hook 'gh/prog-mode-hook)
(add-hook 'css-mode-hook 'rainbow-mode)

;;
;; Lots of files are really ruby these days
;;
(add-hook 'ruby-mode-hook 'gh/prog-mode-hook)
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;;
;; Snippets
;;
(yas-global-mode 1)
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

;;
;; Ensure that lisp-interaction can still evaluate on ctrl-j...
;;
(defadvice paredit-newline (around eval-print-last-sexp activate)
  (if (eq major-mode 'lisp-interaction-mode)
      (eval-print-last-sexp)
    (paredit-newline)))

;;
;; Keep customize-based settings separate
;;
(setq custom-file "~/.emacs.d/.emacs-custom")
(load custom-file 'no-error)

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

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-compact-blocks t)
(setq org-agenda-custom-commands
      '(("y" "Grand Unified Agenda"
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-agenda-overriding-header "=== Today")))
          (tags "REFILE"
                ((org-agenda-overriding-header "=== To Refile")))
          (tags-todo "ORGANISE"
                     ((org-agenda-overriding-header "=== Daily Admin")))
          (tags "WAITING"
                ((org-agenda-overriding-header "=== To Chase")))
          (todo "NEXT"
                ((org-agenda-overriding-header "=== Unscheduled NEXT")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
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

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key [f12] 'org-capture)

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
;; modeline stuff
;;
(display-time)

;;
;; Python
;;
(require 'python-mode)
(add-hook 'python-mode-hook 'gh/prog-mode-hook)

;;
;; My bindings
;;
(global-set-key "\C-o" 'query-replace)
(global-set-key [(control next)] 'scroll-other-window)
(global-set-key [(control prior)] 'scroll-other-window-down)
(global-set-key [(f8)] 'toggle-truncate-lines)
;; put all kinds of shells on f9...
(global-set-key [(f9)] 'py-shell)
(global-set-key [(shift f9)] 'shell) 
(global-set-key [(ctrl f9)] 'eshell)

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
(load-theme 'zenburn t)
;;(load-theme 'solarized-light t t) ; don't enable

;;
;; Clojure indentation
;;
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
              (domonad 1)
              (context 2)
              (defroutes 'defun))))

(require 'midje-mode)
(require 'clojure-jump-to-file)
(add-hook 'clojure-mode-hook 'gh/pretty-fn)
(add-hook 'clojurescript-mode-hook 'gh/pretty-fn)

;;
;; Remote shells
;;
(require 'tramp)
(set-default 'tramp-auto-save-directory (expand-file-name "~/temp"))
(set-default 'tramp-default-method "plinkx")

;;
;; Octave
;;
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;
;; keychords to combat emacs-pinkie
;;

(key-chord-define-global "jj" 'ibuffer)
(key-chord-define-global "JJ" 'magit-status)
(key-chord-define-global "jb" 'ido-switch-buffer)
(key-chord-define-global "jg" 'org-agenda)
(key-chord-define-global "jt" 'ace-jump-mode)
(key-chord-define-global "jw" 'ace-window)
(key-chord-define-global "kk" 'gh/kill-current-buffer)
(key-chord-mode 1)
