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

(straight-use-package 'use-package)

(straight-use-package' auto-compile)

(straight-use-package 'no-littering)

(setq mac-command-modifier      'meta
      mac-option-modifier       nil
      mac-right-option-modifier nil
      mac-function-modifier     nil)

(straight-use-package 'which-key)
(setq which-key-idle-delay    0.5
      which-key-separator     " "
      which-key-sort-order    'which-key-description-order
      which-key-prefix-prefix "+")
(which-key-mode +1)

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

(straight-use-package 'exec-path-from-shell)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("LANG"
                                  "LC_ALL"
                                  "PYTHONPATH"))

(straight-use-package 'crux)
;; todo: more keybindings to add? need to go through and see what should be
;; remapped

(global-set-key (kbd "C-c o")     'crux-open-with)
(global-set-key [remap kill-line] 'crux-smart-kill-line) ; C-k
(global-set-key (kbd "C-S-RET")   'crux-smart-open-line-above)
(global-set-key (kbd "S-RET")     'crux-smart-open-line)
(global-set-key (kbd "C-c n")     'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-c f")     'crux-recentf-find-file)
(global-set-key (kbd "C-c F")     'crux-recentf-find-directory)
(global-set-key (kbd "C-c e")     'crux-eval-and-replace)
(global-set-key (kbd "C-c D")     'crux-delete-file-and-buffer)
(global-set-key (kbd "M-/")       'hippie-expand)
(global-set-key (kbd "C-x C-b")   'ibuffer)
(global-set-key (kbd "M-z")       'zap-up-to-char)
(global-set-key (kbd "C-s")       'isearch-forward-regexp)
(global-set-key (kbd "C-r")       'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")     'isearch-forward)
(global-set-key (kbd "C-M-r")     'isearch-backward)
(global-set-key (kbd "M-;")       'knube/comment-or-uncomment)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(defun knube/comment-or-uncomment ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode +1)

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

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(global-set-key (kbd "M-<f10>")   'toggle-frame-maximized)
(global-set-key (kbd "M-S-<f10>") 'toggle-frame-fullscreen)

(global-unset-key (kbd "s-p"))     ; no one needs print
(global-unset-key (kbd "C-x f"))   ; set-fill-column is always 80
(global-unset-key (kbd "C-x C-n")) ; set-goal-column is just annoying

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

(straight-use-package 'modus-themes)

(setq modus-themes-org-blocks     'tinted-background
      modus-themes-scale-headings t)
(modus-themes-load-themes)
(modus-themes-load-operandi)
(knube/fix-org-blocks)

(global-set-key (kbd "<f5>") 'knube/toggle-themes)

(defun knube/toggle-themes ()
  (interactive)
  (modus-themes-toggle)
  (knube/fix-org-blocks))

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

(display-time-mode +1)
(telephone-line-mode +1)

(straight-use-package 'writeroom-mode)
(global-set-key (kbd "<f6>") 'writeroom-mode)

(straight-use-package 'consult)
;; todo: one gazillion keybindings

(straight-use-package 'vertico)
(vertico-mode)

(straight-use-package 'orderless)
(setq completion-styles             '(orderless)
      completion-category-defaults  nil
      completion-category-overrides '((file (styles partial-completion))))

;; Persist history over emacs restart
(savehist-mode +1)

(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(straight-use-package 'marginalia)

(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

(marginalia-mode +1)

(straight-use-package 'embark)

(global-set-key (kbd "C-.")   'embark-act)
(global-set-key (kbd "C-;")   'embark-dwim)     ; good alternative: M-.
(global-set-key (kbd "C-h B") 'embark-bindings) ; embark's `describe-bindings'

;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'display-buffer-alist '("\\`\\*Embark Collect
 \\(Live\\|Completions\\)\\*" nil (window-parameters (mode-line-format .
 none))))

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(straight-use-package 'company)

(setq company-idle-delay                0.5
      company-show-numbers              t
      company-tooltip-limit             10
      company-minimum-prefix-length     2
      company-tooltip-align-annotations t
      ;; invert the navigation direction if the the completion
      ;; popup-isearch-match is displayed on top (happens near the bottom of
      ;; windows)
      company-tooltip-flip-when-above   t)

(global-company-mode +1)


(straight-use-package 'company-prescient)

(company-prescient-mode +1)

(straight-use-package 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode +1)

(straight-use-package 'org-contrib)
(straight-use-package 'org)

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

(straight-use-package 'company-org-block)

(setq company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline

(add-hook 'org-mode-hook (lambda ()
                           (add-to-list (make-local-variable 'company-backends)
                                        'company-org-block)))

(setq knube/bibs '("~/Dropbox/org/bibs/references.bib"))
(straight-use-package 'citeproc)
(straight-use-package 'bibtex-actions)

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

(define-key org-mode-map         (kbd "C-c b") 'org-cite-insert)
(define-key org-mode-map         (kbd "M-o")   'org-open-at-point)
(define-key minibuffer-local-map (kbd "M-b")   'bibtex-actions-insert-preset)

;; Use consult-completing-read for enhanced interface.
(advice-add #'completing-read-multiple
            :override #'consult-completing-read-multiple)

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
