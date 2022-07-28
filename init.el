(setq user-full-name         "Knut Berg"
      user-mail-address      "knut.berg@nord.no"
      calendar-latitude      67.289
      calendar-longitude     14.560
      calendar-location-name "Bodø, Norway")

;;; init.el --- -*- lexical-binding: t -*-
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq user-init-file     (concat user-emacs-directory "init.el"))
(setq user-init-org-file (concat user-emacs-directory "init.org"))

(defun knube/reload-init ()
  "Reload ~/.emacs.d/init.el."
  (interactive)
  (org-babel-tangle-file user-init-org-file)
  (load-file user-init-file))

(defun knube/open-init ()
  "Open ~/.emacs.d/init.org."
  (interactive)
  (find-file user-init-org-file))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defconst *IS-MAC*    (eq system-type 'darwin))
(defconst *IS-WIN*    (eq system-type 'windows-nt))
(defconst *IS-WIN-WSL (and (eq system-type 'windows-nt) (getenv "WSLENV")))
(defconst *IS-LINUX*  (eq system-type 'gnu/linux))

(setq utf-translate-cjk-mode nil     ; disable CJK coding/encoding
      locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(when *IS-MAC*
  (setq mac-command-modifier      'meta
        mac-option-modifier       nil
        mac-right-option-modifier nil
        mac-function-modifier     'super))

;; Increase this if stuttering occurs. Decrease if freezes occurs.
(defvar knube-gc-cons-threshold (* 64 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold       knube-gc-cons-threshold
                  gc-cons-percentage      0.1
                  debug-on-error          nil
                  file-name-handler-alist startup-file-name-handler-alist)
            (makunbound 'startup-file-name-handler-alist)))

;; Do gc when out of focus. Avoid gc when using minibuffer.
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* knube-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold knube-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(setq scroll-step                     1
      scroll-conservatively           101
      scroll-preserve-screen-position 'always
      next-screen-context-lines       5
      debugger-stack-frame-as-list    t
      mouse-wheel-follow-mouse        t
      mouse-wheel-scroll-amount       '(1 ((shift) . 1))
      mouse-wheel-progressive-speed   nil
      mouse-yank-at-point             t)

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(straight-use-package 'no-littering)

(setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory)
      no-littering-var-directory
	(expand-file-name "data/" user-emacs-directory))

(require 'no-littering)
(require 'recentf)

(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(when *IS-MAC*
  (straight-use-package 'osx-trash)
  (osx-trash-setup)
  (setq delete-by-moving-to-trash t))

(straight-use-package 'exec-path-from-shell)

(exec-path-from-shell-initialize)

(straight-use-package 'which-key)

(which-key-mode +1)

(straight-use-package 'general)

(add-hook 'prog-mode-hook   'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook
 (lambda ()
   (when buffer-file-name
     (let ((dir (file-name-directory buffer-file-name)))
       (when (and (not (file-exists-p dir))
                  (y-or-n-p
                   (format "Directory %s does not exist. Create it?" dir)))
         (make-directory dir t))))))

(blink-cursor-mode       0)
(delete-selection-mode   1)
(transient-mark-mode     1) ; https://www.emacswiki.org/emacs/TransientMarkMode
(save-place-mode         1) ; https://www.emacswiki.org/emacs/SavePlace
(show-paren-mode         1) ; Indicate matching pairs of parentheses
(column-number-mode      1)
(global-font-lock-mode   t) ; is this really a good idea?
(global-auto-revert-mode t) ; refresh buffer on file change

(setq-default cursor-type            'bar
              indent-tabs-mode       nil  ; indent with space
              fill-column            80   ; always break at 80
              abbrev-mode            t
              dired-listing-switches "-alh")

(require 'uniquify)
(setq uniquify-buffer-name-style          'forward ; unique buffer names
      show-paren-delay                    0.0
      tab-width                           2
      delete-selection-mode               t
      sentence-end-double-space           nil
      vc-follow-symlinks                  t
      default-directory                   "~/"
      confirm-kill-emacs                  'y-or-n-p
      require-final-newline               t
      visible-bell                        t
      save-interprogram-paste-before-kill t
      apropos-do-all                      t
      save-abbrevs                        'silently
      large-file-warning-threshold        (* 15 1024 1024)
      global-mark-ring-max                500  ; we have buttloads of
      mark-ring-max                       500  ; memory, might as well
      kill-ring-max                       500) ; use it

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 180
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka"
                    :height 180
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka"
                    :height 180
                    :weight 'medium)

(straight-use-package 'modus-themes)

(setq modus-themes-org-blocks 'gray-background)

(modus-themes-load-themes)
;(modus-themes-load-operandi) ; light theme
(modus-themes-load-vivendi)  ; dark theme

(global-hl-line-mode +1)

(straight-use-package 'minions)
(setq minions-mode-line-lighter    "☰"
      minions-mode-line-delimiters '("" . ""))

(minions-mode +1)

(straight-use-package 'telephone-line)

(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment
                   telephone-line-airline-position-segment))
        (accent . (telephone-line-buffer-name-segment))
        (nil    . (telephone-line-buffer-modified-segment)))

      telephone-line-rhs
      '((nil    . (telephone-line-minions-mode-segment))
        (accent . (telephone-line-vc-segment))
        (nil    . (telephone-line-misc-info-segment))))

(setq display-time-24hr-format            t
      display-time-day-and-date           t
      display-time-default-load-average   nil
      display-time-load-average           nil
      display-time-load-average-threshold nil)

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode +1))

(display-time-mode   +1)
(telephone-line-mode +1)

(straight-use-package 'writeroom-mode)

(add-hook 'writeroom-mode-enable-hook  #'(lambda () (text-scale-adjust 2)))
(add-hook 'writeroom-mode-disable-hook #'(lambda () (text-scale-adjust 0)))

(straight-use-package 'smartparens)

(require 'smartparens-config)

(smartparens-global-mode +1)

(straight-use-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(straight-use-package 'vertico)
(straight-use-package 'savehist)
(straight-use-package 'orderless)

(setq completion-styles             '(orderless basic)
      completion-category-defaults  nil
      completion-category-overrides '((file (styles partial-completion)))
      vertico-cycle                 t)

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(vertico-mode  +1)
(savehist-mode +1)

(general-define-key
  :keymaps 'vertico-map
  "C-j"    #'vertico-next
  "C-k"    #'vertico-previous)

(straight-use-package 'marginalia)

(marginalia-mode +1)

(straight-use-package 'consult)

(general-define-key
 [remap apropos]                       #'consult-apropos
 [remap bookmark-jump]                 #'consult-bookmark
 [remap evil-show-marks]               #'consult-mark
 [remap evil-show-jumps]               #'+vertico/jump-list
 [remap evil-show-registers]           #'consult-register
 [remap goto-line]                     #'consult-goto-line
 [remap imenu]                         #'consult-imenu
 [remap locate]                        #'consult-locate
 [remap load-theme]                    #'consult-theme
 [remap man]                           #'consult-man
 [remap recentf-open-files]            #'consult-recent-file
 [remap switch-to-buffer]              #'consult-buffer
 [remap switch-to-buffer-other-window] #'consult-buffer-other-window
 [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
 [remap yank-pop]                      #'consult-yank-pop
 [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)

(straight-use-package 'embark)
(straight-use-package 'embark-consult)

(general-define-key
 [remap describe-bindings] #'embark-bindings
 "C-."                     #'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(straight-use-package 'corfu)
(straight-use-package 'corfu-doc)

(setq corfu-cycle              t     ; Allows cycling through candidates
      corfu-auto               t     ; Enable auto completion
      corfu-auto-prefix        2     ; Complete with less prefix keys
      corfu-auto-delay         0.0   ; No delay for completion
      corfu-echo-documentation 0.25) ; Echo docs for current completion option

(global-corfu-mode 1)

(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(general-define-key
 :keymaps 'corfu-map
 "M-p" #'corfu-doc-scroll-down
 "M-n" #'corfu-doc-scroll-up
 "M-d" #'corfu-doc-toggle)

(straight-use-package 'cape)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-tex)

(straight-use-package 'citar)

(setq citar-bibliography '("~/Dropbox/org/bibs/references.bib"))

(general-define-key
 "C-c b" #'citar-insert-citation)

(general-define-key
 :keymaps 'minibuffer-local-map
 "M-b" #'citar-insert-citation)

;; use consult-completing-read for enhanced interface
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

(straight-use-package 'evil)

(setq evil-want-integration         t
      evil-want-keybinding          nil
      evil-want-C-i-jump            nil
      evil-respect-visual-line-mode t
      evil-undo-system              'undo-redo
      evil-want-C-i-jump            t
      evil-want-Y-yank-to-eol       t
      evil-want-fine-undo           t)

(evil-mode +1)

(evil-select-search-module 'evil-search-module 'evil-search)

(straight-use-package 'evil-collection)

(evil-collection-init)

(straight-use-package 'evil-nerd-commenter)

(evilnc-default-hotkeys)

(straight-use-package 'evil-surround)

(global-evil-surround-mode +1)

(straight-use-package 'evil-embrace)

(add-hook 'org-mode-hook 'embrace-org-mode-hook)

(evil-embrace-enable-evil-surround-integration)

(straight-use-package 'org)
(straight-use-package 'org-contrib)

(setq org-list-allow-alphabetical      t
      org-fontify-whole-heading-line   t
      org-startup-indented             t     ; indent sections
      org-indent-indentation-per-level 2
      org-adapt-indentation            nil
      org-src-tab-acts-natively        t     ; tab works as in any major mode
      org-src-preserve-indentation     t
      org-log-into-drawer              t     ; wtf is this?
      org-src-fontify-natively         t     ; highlight code
      org-log-done                     'time ; add dates on completion of TODOs
      org-support-shift-select         t     ; select holding down shift
      org-startup-truncated            nil
      org-directory                    "~/Dropbox/org"
      org-agenda-files                 '("~/Dropbox/org/agenda/")
      org-ellipsis                     " ⤵"
      org-src-window-setup             'current-window
      org-latex-pdf-process            (list "latexmk -xelatex -f %f"))

(add-hook 'org-mode-hook (lambda ()
                           (add-to-list 'org-structure-template-alist
                                        '("se" . "src emacs-lisp"))))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (latex      . t)))

(straight-use-package 'org-download)

(setq-default org-download-image-dir "~/bilder/")

(add-hook 'dired-mode-hook 'org-download-enable)

(with-eval-after-load 'org
    (org-download-enable))

(straight-use-package 'org-modern)

(global-org-modern-mode +1)

(straight-use-package 'auctex)
(straight-use-package '(auctex-latexmk :type git :host github :repo "knutberg/auctex-latexmk"))

(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(setq-default TeX-master nil
              TeX-engine 'xetex)

(setq TeX-source-correlate-method 'synctex
      TeX-source-correlate        t
      TeX-PDF-mode                t
      TeX-auto-save               t
      TeX-save-query              nil
      TeX-parse-self              t
      reftex-plug-into-AUCTeX     t
      TeX-view-program-list       '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"))
      TeX-view-program-selection  '((output-pdf "Skim"))
      TeX-clean-confirm           nil)

;; make sure everything works fine with latexmk
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(auctex-latexmk-setup)

(straight-use-package 'cdlatex)

(add-hook 'org-mode-hook   'turn-on-org-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(setq cdlatex-env-alist
      '(("equation*" "\\begin{equation*}\n?\n\\end{equation*}\n" nil)))

(straight-use-package 'bug-hunter)

(straight-use-package 'avy)

(general-define-key
 "C-;"   #'avy-goto-char
 "C-:"   #'avy-goto-char-2
 "M-g f" #'avy-goto-line)
