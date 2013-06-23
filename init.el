;;=============================================================================
;; package.el / starter kit based configuration
;;=============================================================================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit 
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-js
                      clojure-mode
                      clojure-snippets
                      ;clojure-test-mode ;; use midje mode instead
                      ;midje-mode ;; use local dev version
                      rainbow-delimiters
                      python-mode
                      groovy-mode
                      powershell
                      yasnippet
                      zenburn-theme
                      nrepl
                      ac-nrepl
                      nrepl-ritz)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `my-packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                              (not (package-built-in-p x))
                              (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))


(yas-global-mode 1)
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

;;=============================================================================
;; Ensure that lisp-interaction can still evaluate on ctrl-j...
;;=============================================================================
(defadvice paredit-newline (around eval-print-last-sexp activate)
  (if (eq major-mode 'lisp-interaction-mode)
      (eval-print-last-sexp)
    (paredit-newline)))


;;=============================================================================
;; Keep customize-based settings separate
;;=============================================================================
(setq custom-file "~/.emacs.d/.emacs-custom")
(load custom-file 'no-error)

;;=============================================================================
;; Org Mode
;;=============================================================================

(setq org-feed-alist
      '(("RTM"
         "http://www.rememberthemilk.com/atom/gmorpheme/25471103/?tok=eJwFwYENQjEIBcCJmvBaKO04UEBN-NGo*8c7rUnHvOIAAUI3zvAB1k7cZS4uzygsPim7k-lJoiplYsGgdrten-c9r2zPx-fXurACNFrqIUyR6bBTGjQs54jp6dpzbOe0CAKOl4nSlhELDNnman39ARvxK1Y"
         "~/dropbox/notes/notes.org" "Feed: RTM")
        ("JIRA"
         "http://jira.iweb.chp.co.uk/sr/jira.issueviews:searchrequest-rss/12990/SearchRequest-12990.xml?tempMax=1000"
         "~/dropbox/notes/notes.org" "Feed: JIRA (My Issues)")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;=============================================================================
;; Enable disabled features
;;=============================================================================
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;==============================================================================
;; Check for modifications to open files.
;==============================================================================
(require 'autorevert)
(global-auto-revert-mode t)

;==============================================================================
;; Visual settings
;==============================================================================
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(auto-compression-mode 1)
(setq visible-bell t)
(setq default-major-mode 'org-mode)
(delete-selection-mode 1)

;==============================================================================
;; Ido settings
;==============================================================================
(setq ido-auto-merge-work-directories-length -1)

;;==============================================================================
;; Horizontal line hightlighting in dired, not programming modes.
;;==============================================================================
(add-hook 'dired-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;;=============================================================================
;; windows reg files are UTF-16 little endian - please read them properly.
;;=============================================================================
(modify-coding-system-alist 'file "\\.reg\\'" 'utf-16-le)

;;=============================================================================
;; modeline stuff
;;=============================================================================
(display-time)

;;=============================================================================
;; Python
;;=============================================================================
(require 'python-mode)

;;=============================================================================
;; My bindings
;;=============================================================================
(global-set-key "\C-o" 'replace-string)
(global-set-key [(control next)] 'scroll-other-window)
(global-set-key [(control prior)] 'scroll-other-window-down)
(global-set-key [(f8)] 'toggle-truncate-lines)
;; put all kinds of shells on f9...
(global-set-key [(f9)] 'py-shell)
(global-set-key [(shift f9)] 'shell) 
(global-set-key [(ctrl f9)] 'eshell)

;;=============================================================================
;; Windows specifics
;;=============================================================================
(if (eq system-type 'windows-nt)
  (progn
    (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
    ; stop hangs?
    (setq w32-get-true-file-attributes nil)
    (remove-hook 'text-mode-hook 'turn-on-flyspell)))

;;=============================================================================
;; Theme
;;=============================================================================
(load-theme 'zenburn)
(enable-theme 'zenburn)

;; todo (if / when needed): yaml, n3, psvn?, rst?

;;=============================================================================
;; Clojure indentation
;;=============================================================================
(add-hook 'clojure-mode-hook
          (lambda ()
            (define-clojure-indent
              (ANY 2)
              (DELETE 2)
              (GET 2)
              (HEAD 2)
              (POST 2)
              (PUT 2)
              (domonad 1)
              (context 2)
              (defroutes 'defun))))

(require 'nrepl)
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
 
;; Ritz middleware
(require 'nrepl-ritz)
(define-key nrepl-interaction-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-interaction-mode-map (kbd "C-c C-a") 'nrepl-apropos)
(define-key nrepl-mode-map (kbd "C-c C-a") 'nrepl-apropos)

;; (require 'ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))

(add-to-list 'load-path "/local/dev/midje-mode")
(require 'midje-mode)
(require 'clojure-jump-to-file)

