;;; init.el --- -*- lexical-binding: t -*-
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq user-info-file (concat user-emacs-directory "user-info.el"))
(when (file-exists-p user-info-file)
  (load user-info-file 'noerror))

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

(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(straight-use-package' auto-compile)
(auto-compile-on-load-mode +1)
(auto-compile-on-save-mode +1)

(straight-use-package 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(require 'no-littering)
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(straight-use-package 'restart-emacs)

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

(straight-use-package 'undo-fu) ;; move this!
(straight-use-package 'evil)
(setq evil-want-integration t ;; This is optional since it's already set to t by default.
      evil-want-keybinding  nil
      evil-want-fine-undo   t
      evil-undo-system      'undo-fu)
(evil-mode +1)

(straight-use-package 'evil-collection)
(evil-collection-init)

(straight-use-package 'evil-surround)
(global-evil-surround-mode +1)

(straight-use-package 'evil-embrace)
(evil-embrace-enable-evil-surround-integration)

(straight-use-package 'evil-lion)
(evil-lion-mode +1)

(straight-use-package 'evil-nerd-commenter)
(evilnc-default-hotkeys)

(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode +1)
(straight-use-package 'evil-smartparens)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(straight-use-package 'general)
(general-auto-unbind-keys)

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

(straight-use-package 'crux)

(set-face-attribute 'default        nil :family "Iosevka"  :height 180 :weight 'light)
(set-face-attribute 'fixed-pitch    nil :family "Iosevka"  :height 180 :weight 'light)
(set-face-attribute 'variable-pitch nil :family "Iosevka"  :height 180 :weight 'light)

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

(setq knube/dark-theme-enabled-p nil)

(knube/fix-org-blocks)

(straight-use-package 'dashboard)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-center-content t)
(setq dashboard-banner-logo-title "Welcome to knubemacs")
(dashboard-setup-startup-hook)

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

(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(straight-use-package 'all-the-icons)

(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(straight-use-package 'consult)
(general-define-key
 [remap apropos]                       #'consult-apropos
 [remap bookmark-jump]                 #'consult-bookmark
 [remap evil-show-marks]               #'consult-mark
 [remap evil-show-jumps]               #'+vertico/jump-list
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

(straight-use-package 'marginalia)

(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

(marginalia-mode +1)

(straight-use-package 'embark)


;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'display-buffer-alist '("\\`\\*Embark Collect
 \\(Live\\|Completions\\)\\*" nil (window-parameters (mode-line-format .
 none))))

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(straight-use-package 'company)
(straight-use-package 'company-prescient)

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
(company-prescient-mode +1)

(straight-use-package 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode +1)

(straight-use-package 'org-contrib)
(straight-use-package 'org)

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

(straight-use-package 'company-org-block)

(setq company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline

(add-hook 'org-mode-hook (lambda ()
                           (add-to-list (make-local-variable 'company-backends)
                                        'company-org-block)))

(straight-use-package 'evil-org)
(add-hook 'org-mode-hook
  (lambda ()
     (evil-org-mode)
     (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
     (require 'evil-org-agenda)
     (evil-org-agenda-set-keys)))

(setq knube/bibs '("~/Dropbox/org/bibs/references.bib"))

(straight-use-package 'citeproc)
(straight-use-package '(bibtex-actions :type git :host github :repo "bdarcus/bibtex-actions"))

(require 'oc)
(require 'oc-basic)
(require 'oc-csl)
(require 'oc-biblatex)
(require 'oc-natbib)
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

(straight-use-package 'evil-tex)
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

(general-unbind
  "s-p"      ; no one needs print
  "C-x f"    ; set-fill-column is always 80
  "C-x C-n") ; set-goal-column is just annoying

(straight-use-package 'major-mode-hydra) ;; this includes hydra and pretty-hydra

(general-create-definer knube/spc-leader
  :states '(normal insert visual emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

;; (general-create-definer knube/local-spc-leader
;;   :states '(normal insert visual emacs)
;;   :prefix "SPC m"
;;   :non-normal-prefix "M-SPC m")

;; (knube/local-spc-leader
;;   "" '(:which-key "local" :ignore))

(knube/spc-leader
 "" '(knube/spc-hydra/body :which-key "knubemacs main hydra"))

(pretty-hydra-define knube/spc-hydra
  (:foreign-keys warn :quit-key "q")
  ("emacs"
   (("q"   nil                      "    quit this hydra")
    ("Q"   save-buffers-kill-emacs  "    exit emacs"    :exit t)
    ("R"   restart-emacs            "    restart emacs" :exit t)
    ("<spc>" execute-extended-command "M-x"))
   ""
   (("<tab>" other-window                    "other window")
    ("a"   embark-act                        "     embark act")     ; add spaces for prettier alignment
    ("z"   selectrum-repeat                  "    repeat command")
    ("d"   (switch-to-buffer "*dashboard*")  "    dashboard"))
   "hydras"
   (("m" nil                        "major mode..." :exit t)
    ("b" nil                        "buffer..."     :exit t)
    ("f" nil                        "file..."       :exit t)
    ("e" nil                        "edit..."       :exit t))
))



    ;; ("Q"   save-buffers-kill-emacs  "save and exit")
    ;; ("R"   restart-emacs            "restart emacs")
(defun knube/reload-config ()
  (interactive)
  (load-file user-init-file))

(defun knube/open-config ()
  (interactive)
  (find-file (concat user-emacs-directory "init.org")))

(general-define-key
 [remap kill-line] 'crux-smart-kill-line ; C-k
 "C-S-RET"         'crux-smart-open-line-above
 "S-RET"           'crux-smart-open-line
 "M-/"             'hippie-expand)

(general-define-key
 "C-."   'embark-act
 "C-;"   'embark-dwim     ; good alternative: M-.
 "C-h B" 'embark-bindings) ; embark's `describe-bindings'
