;;; init.el --- -*- lexical-binding: t -*-
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq user-info-file (concat user-emacs-directory "user-info.el"))
(when (file-exists-p user-info-file)
  (load user-info-file 'noerror))

(require 'cl-seq)
(setq load-path
      (cl-remove-if
       (lambda (x)
         (string-match-p "org$" x))
       load-path))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; use use-package by default
(setq straight-use-package-by-default t
      use-package-always-demand       t)

(use-package auto-compile)

(use-package no-littering)

(use-package general
  :config
  (general-unbind
    "s-p"       ; no one needs print
    "C-x f"     ; set-fill-column is always 80
    "C-x C-n")) ; set-goal-column is just annoying

(setq mac-command-modifier      'meta
      mac-option-modifier       nil
      mac-right-option-modifier nil
      mac-function-modifier     nil)

(use-package which-key
  :config
  (setq which-key-idle-delay    0.8
        which-key-separator     " "
        which-key-sort-order    'which-key-description-order
        which-key-prefix-prefix "+")
  (which-key-mode +1))

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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LANG"
                                    "LC_ALL"
                                    "PYTHONPATH")))

(use-package crux
  :general
  ("C-c o"   'crux-open-with)
  ("C-k"     'crux-smart-kill-line)
  ("C-S-RET" 'crux-smart-open-line-above)
  ("S-RET"   'crux-smart-open-line)
  ("C-c n"   'crux-cleanup-buffer-or-region)
  ("C-c f"   'crux-recentf-find-file)
  ("C-c F"   'crux-recentf-find-directory)
  ("C-c e"   'crux-eval-and-replace)
  ("C-c D"   'crux-delete-file-and-buffer)
  ("M-/"     'hippie-expand)
  ("C-x C-b" 'ibuffer)
  ("M-z"     'zap-up-to-char)
  ("C-s"     'isearch-forward-regexp)
  ("C-r"     'isearch-backward-regexp)
  ("C-M-s"   'isearch-forward)
  ("C-M-r"   'isearch-backward)
  ("M-;"     'knube/comment-or-uncomment))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(defun knube/comment-or-uncomment ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1))

(setq utf-translate-cjk-mode nil     ; disable CJK coding/encoding
      locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(setq scroll-step                     1
      scroll-conservatively           101
      scroll-preserve-screen-position 'always
      next-screen-context-lines       5
      debugger-stack-frame-as-list    t
      mouse-wheel-follow-mouse        t
      mouse-wheel-scroll-amount       '(1 ((shift) . 1))
      mouse-wheel-progressive-speed   nil
      mouse-yank-at-point             t)

(toggle-frame-maximized)

(general-define-key
 "M-<f10>"   'toggle-frame-maximized
 "M-S-<f10>" 'toggle-frame-fullscreen)

(add-hook 'prog-mode-hook   'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

(blink-cursor-mode       0)
(delete-selection-mode   1)
(transient-mark-mode     1) ; https://www.emacswiki.org/emacs/TransientMarkMode
(save-place-mode         1) ; https://www.emacswiki.org/emacs/SavePlace
;;(save-hist-mode          1)
(show-paren-mode         1) ; Indicate matching pairs of parentheses
(column-number-mode      1)
(global-font-lock-mode   t) ; is this really a good idea?
(global-auto-revert-mode t) ; refresh buffer on file change

(setq-default cursor-type           'bar
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

(set-face-attribute
 'default        nil :family "Fira Code" :height 180 :weight 'light)
(set-face-attribute
 'fixed-pitch    nil :family "Fira Code" :height 180 :weight 'light)
(set-face-attribute
 'variable-pitch nil :family "Fira Code" :height 180 :weight 'light)

(defun knube/fix-org-blocks ()
  (interactive)
  (eval-after-load 'org
    (lambda ()
      (set-face-attribute
       'org-block nil :extend t)
      (set-face-attribute 'org-block-begin-line nil :extend t
                          :underline nil :overline nil
                          :slant 'italic)
      (set-face-attribute 'org-block-end-line nil :extend t
                          :underline nil :overline nil
                          :slant 'italic))))

(use-package modus-themes
  :init
  (setq modus-themes-org-blocks     'tinted-background
        modus-themes-scale-headings t)
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi)
  (knube/fix-org-blocks)
  :general
  ("<f5>" 'knube/toggle-themes))

(defun knube/toggle-themes ()
  (interactive)
  (modus-themes-toggle)
  (knube/fix-org-blocks))

(use-package minions
  :init
  (setq minions-mode-line-lighter    "☰"
        minions-mode-line-delimiters '("" . ""))
  :config
  (minions-mode +1))

(use-package telephone-line
  :init
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
  :config
  (unless (equal "Battery status not available"
                 (battery))
    (display-battery-mode +1))
  (display-time-mode +1)
  (telephone-line-mode +1))

(use-package writeroom-mode
  :general
  ("<f6>" 'writeroom-mode))

(use-package evil
  :init
  (setq evil-want-keybinding  nil
        evil-want-integration t
        evil-want-fine-undo   t)
  :config
  (evil-mode +1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode +1))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode +1))

(use-package selectrum
  :general
  ("C-x C-z" 'selectrum-repeat)
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; Example configuration for Consult
(use-package consult
  :after org
  :general
  (;; C-c bindings (mode-specific-map)
   "C-c h" 'consult-history
   "C-c m" 'consult-mode-command
   "C-c b" 'consult-bookmark
   "C-c k" 'consult-kmacro

   ;; C-x bindings (ctl-x-map)
   "C-x M-:" 'consult-complex-command     ;; orig. repeat-complex-command
   "C-x b"   'consult-buffer              ;; orig. switch-to-buffer
   "C-x 4 b" 'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
   "C-x 5 b" 'consult-buffer-other-frame  ;; orig. switch-to-buffer-other-frame

   ;; Custom M-# bindings for fast register access
   "M-#"   'consult-register-load
   "M-'"   'consult-register-store ;; orig. abbrev-prefix-mark (unrelated)
   "C-M-#" 'consult-register

   ;; Other custom bindings
   "M-y"      'consult-yank-pop ;; orig. yank-pop
   "<help> a" 'consult-apropos  ;; orig. apropos-command

   ;; M-g bindings (goto-map)
   "M-g e"   'consult-compile-error
   "M-g f"   'consult-flymake       ;; Alternative: consult-flycheck
   "M-g g"   'consult-goto-line     ;; orig. goto-line
   "M-g M-g" 'consult-goto-line     ;; orig. goto-line
   "M-g o"   'consult-outline       ;; Alternative: consult-org-heading
   "M-g m"   'consult-mark
   "M-g k"   'consult-global-mark
   "M-g i"   'consult-imenu
   "M-g I"   'consult-imenu-multi

   ;; M-s bindings (search-map)
   "M-s f" 'consult-find
   "M-s F" 'consult-locate
   "M-s g" 'consult-grep
   "M-s G" 'consult-git-grep
   "M-s r" 'consult-ripgrep
   "M-s l" 'consult-line
   "M-s L" 'consult-line-multi
   "M-s m" 'consult-multi-occur
   "M-s k" 'consult-keep-lines
   "M-s u" 'consult-focus-lines

   ;; Isearch integration
   "M-s e" 'consult-isearch)
  (:keymaps 'isearch-mode-map
            "M-e"   'consult-isearch     ;; orig. isearch-edit-string
            "M-s e" 'consult-isearch     ;; orig. isearch-edit-string
            "M-s l" 'consult-line        ;; needed by consult-line to detect isearch
            "M-s L" 'consult-line-multi) ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay    0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
           "M-S-a" 'marginalia-cycle)
  :config
  (marginalia-mode +1))

(use-package embark
  :general
  ("C-."   'embark-act
   "C-;"   'embark-dwim        ;; good alternative: M-.
   "C-h B" 'embark-bindings) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(setq knube/bibs '("~/Dropbox/org/bibfiles/references.bib"))
(use-package citeproc)
(use-package bibtex-actions
  :after (embark org)
  :config
  (require 'oc-bibtex-actions)
  (setq bibtex-completion-bibliography             knube/bibs
        bibtex-completion-additional-search-fields '(doi url)
        bibtex-actions-at-point-function           'embark-act
        org-cite-global-bibliography               knube/bibs
        org-cite-insert-processor                  'oc-bibtex-actions
        org-cite-follow-processor                  'oc-bibtex-actions
        org-cite-activate-processor                'basic)
  (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
  (add-to-list 'embark-keymap-alist   '(bibtex . bibtex-actions-map))
  (add-to-list 'embark-keymap-alist   '(citation-key . bibtex-actions-buffer-map))
  :general
  ("C-c [" 'org-cite-insert
   "M-o"   'org-open-at-point)
  (:keymaps 'minibuffer-local-map
            "M-b" 'bibtex-actions-insert-preset))

;; Use consult-completing-read for enhanced interface.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

(use-package company
  :init
  (setq company-idle-delay                0.5
        company-show-numbers              t
        company-tooltip-limit             10
        company-minimum-prefix-length     2
        company-tooltip-align-annotations t
        ;; invert the navigation direction if the the completion
        ;; popup-isearch-match is displayed on top (happens near the bottom of
        ;; windows)
        company-tooltip-flip-when-above   t)
  :config
  (global-company-mode +1))


(use-package company-prescient
  :config
  (company-prescient-mode +1))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode +1))

  (use-package org-contrib)
  (use-package org
    :config
    (require 'oc)
    (require 'oc-basic)
    (require 'oc-csl)
    (require 'oc-biblatex)
    (require 'oc-natbib)
    ;; (require 'ox-bibtex)
    ;; (require 'ob-latex)
    ;; (require 'ob-emacs-lisp)

    (setq org-list-allow-alphabetical      t
          org-fontify-whole-heading-line   t
          org-startup-indented             nil  ; indent sections
          org-indent-indentation-per-level 0
          org-adapt-indentation            nil
          org-src-tab-acts-natively        t     ; tab works as in any major mode
          org-src-preserve-indentation     t
          org-log-into-drawer              t     ; wtf is this?
          org-src-fontify-natively         t     ; highlight code
          org-log-done                     'time ; add dates on completion of TODOs
          org-support-shift-select         t     ; select holding down shift
          org-startup-truncated            nil
          org-directory                    "~/Dropbox/org"
          org-agenda-files                 '("~/Dropbox/org/agenda")
          org-ellipsis                     " ⤵"
          org-src-window-setup             'current-window
          org-latex-pdf-process            (list "latexmk -xelatex -f %f"))

    (add-hook 'org-mode-hook (lambda ()
                               (add-to-list 'org-structure-template-alist
                                            '("se" . "src emacs-lisp"))))

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (latex      . t)))
    (general-unbind
      :keymaps 'org-mode-map
      "C-c '"  ; redefined below
      "C-c [") ; I have no need to "put whatever to the front of the agenda"

    :general
    (:keymaps 'org-mode-map
              "C-c C-'" 'org-edit-special)
    (:keymaps 'org-src-mode-map
              "C-c C-'" 'org-edit-src-exit))

(use-package company-org-block
  :after (org company)
  :init
  (setq company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-org-block))))

(straight-use-package 'auctex)

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
(straight-use-package 'auctex-latexmk)

(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(auctex-latexmk-setup)

(straight-use-package 'cdlatex)

(add-hook 'org-mode-hook   'turn-on-org-cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(setq cdlatex-env-alist
      '(("equation*" "\\begin{equation*}\n?\n\\end{equation*}\n" nil)))

;; (straight-use-package 'company-auctex)
;; (company-auctex-init)
