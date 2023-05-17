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

(setq utf-translate-cjk-mode nil     ; disable CJK coding/encoding
      locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(setq mac-command-modifier      'meta
      mac-option-modifier       nil
      mac-right-option-modifier nil
      mac-function-modifier     'super)

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

(straight-use-package 'osx-trash)
(osx-trash-setup)
(setq delete-by-moving-to-trash t)

(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(straight-use-package 'which-key)
(which-key-mode +1)

(straight-use-package 'bug-hunter)

(straight-use-package 'avy)

(global-set-key (kbd "C-;")   #'avy-goto-char)
(global-set-key (kbd "C-:")   #'avy-goto-char-2)
(global-set-key (kbd "M-g f") #'avy-goto-line)

(straight-use-package 'crux)
(global-set-key (kbd "C-c o") #'crux-open-with)

(global-set-key [remap kill-line]       #'crux-smart-kill-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key (kbd "C-S-k")           #'crux-kill-line-backwards)
(global-set-key (kbd "s-k")             #'crux-kill-and-join-forward)

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)

(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [(shift return)]         'crux-smart-open-line)

(global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-c f") 'crux-recentf-find-file)
(global-set-key (kbd "C-c F") 'crux-recentf-find-directory)
(global-set-key (kbd "C-c u") 'crux-view-url)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c c") 'crux-copy-file-preserve-attributes)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c r") 'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)


(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c z")   'crux-indent-defun)
(global-set-key (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)

(global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)

(global-set-key (kbd "C-x C-u") 'crux-upcase-region)
(global-set-key (kbd "C-x C-l") 'crux-downcase-region)
(global-set-key (kbd "C-x M-c") 'crux-capitalize-region)

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
                    :height 160
                    :weight 'light)
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka"
                    :height 160
                    :weight 'light)
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka"
                    :height 160
                    :weight 'light)

(straight-use-package 'modus-themes)

(setq modus-themes-org-blocks 'gray-background)
(load-theme 'modus-operandi :no-confirm)
; (load-theme 'modus-vivendi :no-confirm)
(global-hl-line-mode +1)

(straight-use-package 'nano-modeline)

(nano-modeline-mode)

(setq default-frame-alist
      (append (list
               '(min-height . 1)  '(height . 45)
               '(min-width  . 1)  '(width  . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe . 0)
               '(right-fringe . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(straight-use-package 'writeroom-mode)

(add-hook 'after-init-hook 'toggle-frame-maximized)

(global-set-key (kbd "<f9>")  #'writeroom-mode)
(global-set-key (kbd "<f10>") #'toggle-frame-maximized)
(global-set-key (kbd "<f11>") #'toggle-frame-fullscreen)
(global-set-key (kbd "<f12>") #'toggle-frame-tab-bar)

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

(setq completion-styles               '(orderless basic)
        completion-category-defaults  nil
        completion-category-overrides '((file (styles . (partial-completion))))
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

(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)

(straight-use-package 'marginalia)

(marginalia-mode +1)

(straight-use-package 'consult)

;; C-c bindings in `mode-specific-map'
(global-set-key (kbd "C-c M-x")     #'consult-mode-command)
(global-set-key (kbd "C-c h")       #'consult-history)
(global-set-key (kbd "C-c k")       #'consult-kmacro)
(global-set-key (kbd "C-c m")       #'consult-man)
(global-set-key (kbd "C-c i")       #'consult-info)
(global-set-key [remap Info-search] #'consult-info)

;; C-x bindings in `ctl-x-map'
(global-set-key (kbd "C-x M-:") #'consult-complex-command)     ;; orig. repeat-complex-command
(global-set-key (kbd "C-x b")   #'consult-buffer)              ;; orig. switch-to-buffer
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
(global-set-key (kbd "C-x r b") #'consult-bookmark)            ;; orig. bookmark-jump
(global-set-key (kbd "C-x p b") #'consult-project-buffer)      ;; orig. project-switch-to-buffer

;; Custom M-# bindings for fast register access
(global-set-key (kbd "M-#")   #'consult-register-load)
(global-set-key (kbd "M-'")   #'consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") #'consult-register)

;; Other custom bindings
(global-set-key (kbd "M-y") #'consult-yank-pop) ;; orig. yank-pop

;; M-g bindings in `goto-map'
(global-set-key (kbd "M-g e")   #'consult-compile-error)
(global-set-key (kbd "M-g f")   #'consult-flymake)   ;; Alternative: consult-flycheck
(global-set-key (kbd "M-g g")   #'consult-goto-line) ;; orig. goto-line
(global-set-key (kbd "M-g M-g") #'consult-goto-line) ;; orig. goto-line
(global-set-key (kbd "M-g o")   #'consult-outline)   ;; Alternative: consult-org-heading
(global-set-key (kbd "M-g m")   #'consult-mark)
(global-set-key (kbd "M-g k")   #'consult-global-mark)
(global-set-key (kbd "M-g i")   #'consult-imenu)
(global-set-key (kbd "M-g I")   #'consult-imenu-multi)

;; M-s bindings in `search-map'
(global-set-key (kbd "M-s d") #'consult-find)
(global-set-key (kbd "M-s D") #'consult-locate)
(global-set-key (kbd "M-s g") #'consult-grep)
(global-set-key (kbd "M-s G") #'consult-git-grep)
(global-set-key (kbd "M-s r") #'consult-ripgrep)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s L") #'consult-line-multi)
(global-set-key (kbd "M-s k") #'consult-keep-lines)
(global-set-key (kbd "M-s u") #'consult-focus-lines)

;; Isearch integration
(global-set-key (kbd "M-s e") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)   ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)            ;; needed by consult-line to detect isearch
(define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi)      ;; needed by consult-line to detect isearch

;; Minibuffer history
(define-key minibuffer-local-map (kbd "M-s") #'consult-history) ;; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") #'consult-history) ;; orig. previous-matching-history-element

(straight-use-package 'embark)
(straight-use-package 'embark-consult)

(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.")               #'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(straight-use-package 'corfu)


(global-corfu-mode)

;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-quit-no-match 'separator
      corfu-preselect 'prompt) ;; Always preselect the prompt

(define-key corfu-map (kbd "TAB")   #'corfu-next)
(define-key corfu-map (kbd "S-TAB") #'corfu-previous)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Swap M-/ and C-M-/
(global-set-key (kbd "M-/")   #'dabbrev-completion)
(global-set-key (kbd "C-M-/") #'dabbrev-expand)

;; Other useful Dabbrev configurations.
(setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))



(straight-use-package 'cape)

(global-set-key (kbd "C-c p p") #'completion-at-point)

(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;(add-to-list 'completion-at-point-functions #'cape-history)
;;(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-tex)
;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;(add-to-list 'completion-at-point-functions #'cape-dict)
(add-to-list 'completion-at-point-functions #'cape-symbol)
;;(add-to-list 'completion-at-point-functions #'cape-line)

(straight-use-package 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode +1)

(straight-use-package 'citar)

(setq citar-bibliography '("~/Dropbox/org/bibs/references.bib"))
(add-hook 'LaTeX-mode-hook #'citar-capf-setup)
(add-hook 'org-mode-hook   #'citar-capf-setup)

(straight-use-package 'citar-embark)
(citar-embark-mode)

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
      org-src-fontify-natively         nil   ; fontify code
      org-log-done                     'time ; add dates on completion of TODOs
      org-support-shift-select         t     ; select holding down shift
      org-startup-truncated            nil
      org-directory                    "~/Dropbox/org"
      org-agenda-files                 '("~/Dropbox/org/agenda/")
      org-ellipsis                     " ⤵"
      org-src-window-setup             'current-window
      org-latex-compiler               "xelatex"
      org-latex-pdf-process            (list "latexmk -xelatex -f %f"))

(add-hook 'org-mode-hook (lambda ()
                           (add-to-list 'org-structure-template-alist
                                        '("se" . "src emacs-lisp"))))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (latex      . t)))

(require 'ox-latex)

(with-eval-after-load 'ox-latex
  (setq org-latex-classes nil)
  (add-to-list 'org-latex-classes
               '("article"
"\\documentclass{article}

[DEFAULT-PACKAGES]

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{tikz-cd}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("beamer"
"\\documentclass{beamer}

[DEFAULT-PACKAGES]

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{tikz-cd}

\\usetheme{metropolis}
\\usefonttheme{professionalfonts}
\\mode<presentation>{}
\\metroset{block=fill}
\\hypersetup{colorlinks=true,urlcolor=[RGB]{0 84 147},citecolor=[RGB]{0 144 81}}
\\setbeamertemplate{navigation symbols}{}
\\setbeamertemplate{footline}{
  \\hspace{1pt}
  \\includegraphics[height=0.7cm]{/Users/knube/logo_NORD_transp.png}
  \\vspace{1pt} \\hfill \\inserttitle \\quad
  \\insertframenumber\\,/\\,\\inserttotalframenumber\\kern1em}
\\setbeamertemplate{logo}{}
\\setbeamertemplate{frametitle continuation}{(\\insertcontinuationcount)}"

                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

(straight-use-package 'org-download)

(setq-default org-download-image-dir "./bilder/")
(setq org-download-display-inline-images nil)

(add-hook 'dired-mode-hook 'org-download-enable)

(with-eval-after-load 'org
    (org-download-enable))

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

(add-hook 'org-mode-hook   #'turn-on-org-cdlatex)
(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)

(setq cdlatex-env-alist
      '(("equation*" "\\begin{equation*}\n?\n\\end{equation*}\n" nil)))
