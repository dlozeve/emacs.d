;;; ~/.emacs.d/init.el --- Configuration file for Emacs

;;; Commentary:

;;; Code:

;; Always load newest byte code
(setq load-prefer-newer t)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(blink-cursor-mode 0)

(setq auto-window-vscroll nil)

;; Dired human readable sizes
(setq dired-listing-switches "-alh")

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Newline at end of file
(setq require-final-newline t)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; System locale to use for formatting time values.  Make sure that
;; the weekdays in the time stamps of your Org mode files and in the
;; agenda appear in English.
(setq system-time-locale "C")

(setq-default c-basic-offset 2)
(setq c-default-style "linux")
(setq-default c-doc-comment-style
              '((c-mode    . doxygen)
                (c++-mode  . doxygen)))

(let ((my-font "Iosevka Term")
      (my-height 120))
  (set-face-attribute 'default nil :family my-font :height my-height)
  (set-face-attribute 'fixed-pitch nil :family my-font :height my-height))

(set-face-attribute 'variable-pitch nil :family "Linux Libertine O" :height 140)

;; Better renaming rules for buffers with the same name
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
;; rename after killing uniquified
(setq uniquify-after-kill-buffer-p t)
;; don't muck with special buffers
(setq uniquify-ignore-buffers-re "^\\*")

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun random-todo ()
  (interactive)
  (let ((points nil))
    (goto-char (point-min))
    (while (search-forward-regexp "\*\sTODO" nil t)
      (setq points (cons (1+ (match-end 0)) points)))
    (goto-char (seq-random-elt points))))

;; Straight.el setup
(setq straight-repository-branch "develop"
      straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Better than default: act on the region if active
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)
(bind-key [remap count-words-region] #'count-words)

(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key [remap zap-to-char] #'zap-up-to-char)
(bind-key [remap buffer-menu] #'ibuffer)

(bind-key "M-o" #'other-window)

;;; Appearance

(use-package paren
  :config
  (show-paren-mode 1))

;; Highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode -1))

(use-package emacs
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-org-blocks '(gray-background))
  :config
  ;; (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  (load-theme 'modus-vivendi)
  :bind ("<f12>" . modus-themes-toggle))

(use-package mood-line
  :straight t
  :config
  (mood-line-mode t))

;;; Environment variables

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
    (exec-path-from-shell-copy-env "PYENV_ROOT")
    (exec-path-from-shell-copy-env "PYENV_ROOT")
    (exec-path-from-shell-copy-env "GAMBIT")
    (exec-path-from-shell-copy-env "GERBIL_HOME")))

;;; Menus and completion

(use-package vertico
  :straight t
  :init
  (vertico-mode +1))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides nil
	completion-category-overrides '((file (styles partial-completion
						      orderless)))))

(use-package consult
  :straight t
  :bind (("C-s" . consult-line)
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode +1))

(use-package embark
  :straight t
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ;("M-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (global-corfu-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge management: org-mode, org-roam, bibliography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pour accéder rapidement à l'organisation
(defun gtd ()
  "Find the planner file."
  (interactive)
  (find-file "~/notes/planner.org"))

(defun bib ()
  "Find the bibliography file."
  (interactive)
  (find-file "~/notes/bibliography/bibliography.bib")
  (end-of-buffer))

(use-package org
  :straight t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-agenda-files (list "~/notes/planner.org"))
  (setq org-default-notes-file "~/notes/planner.org")
  ;; List numbering with a. b. a) b), etc.
  (setq org-list-allow-alphabetical t)
  ;; Fontify code in code blocks
  (setq org-src-fontify-natively t)
  ;; Tabs in src blocks
  (setq org-src-tab-acts-natively t)
  ;; Full contents opened by default
  (setq org-startup-folded nil)
  ;; Only one empty line is enough to separate headings when folded
  (setq org-cycle-separator-lines 2)
  ;; Refile to paths in the file (level1/level2/level3)
  (setq org-refile-targets '((nil :maxlevel . 5)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-special-ctrl-a/e nil)

  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/notes")

  (setq org-agenda-block-separator ?─
	org-agenda-time-grid '((daily today require-timed)
			       (800 1000 1200 1400 1600 1800 2000)
			       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	org-agenda-current-time-string
	"⭠ now ─────────────────────────────────────────────────")
  (setq org-agenda-custom-commands
	(quote
	 (("z" "Agenda and TODOs"
	   ((agenda ""
		    ((org-agenda-span 7)
		     (org-agenda-start-on-weekday 1)
		     (org-agenda-prefix-format "  %t%s ")
		     (org-agenda-repeating-timestamp-show-all t)
		     (org-agenda-hide-tags-regexp "\\work")))
	    (tags-todo "work"
		       ((org-agenda-prefix-format "")
			(org-agenda-sorting-strategy '(priority-down tag-up))))
	    (tags-todo "personal"
		       ((org-agenda-prefix-format "")
			(org-agenda-sorting-strategy '(tag-up priority-down)))))
	   ((org-agenda-remove-tags 'prefix)
	    (org-agenda-todo-ignore-scheduled 'future)
	    (org-agenda-todo-ignore-deadlines 'future)
	    (org-agenda-tags-todo-honor-ignore-options t))
	   ("fortnight.html"))
	  ("n" "Agenda and all TODOs"
	   ((agenda "" nil)
	    (alltodo "" nil))
	   nil))))

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq org-capture-templates
	(quote
	 (("t" "Task" entry
	   (file+olp "~/notes/planner.org" "Inbox")
	   "** TODO %?")
	  ("n" "Note" entry
	   (file+olp "~/notes/planner.org" "Inbox")
	   "** %?")
	  ("e" "Event" entry
	   (file+olp "~/notes/planner.org" "Inbox")
	   "** %?\n%^T"))))
  (setq org-log-into-drawer t)
  (setq org-structure-template-alist
	(quote
	 (("a" . "export ascii")
	  ("c" . "center")
	  ("C" . "comment")
	  ("e" . "example")
	  ("E" . "export")
	  ("h" . "export html")
	  ("l" . "export latex")
	  ("q" . "quote")
	  ("s" . "src")
	  ("v" . "verse")
	  ("d" . "definition")
	  ("t" . "theorem")
	  ("p" . "proposition")
	  ("P" . "proof"))))

  ;; Org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (python . t)
     (awk . t)
     (C . t)
     (ditaa . t)
     (dot . t)
     (latex . t)
     (lisp . t)
     (shell . t)
     (sqlite . t)))

  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (setq org-src-preserve-indentation nil
	org-edit-src-content-indentation 0)
  (setq
   org-startup-with-latex-preview t
   org-format-latex-options (plist-put org-format-latex-options :scale 1))

  (require 'ox-latex)
  (add-to-list 'org-latex-classes
	       '("koma-article" "\\documentclass{scrartcl}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-packages-alist
	       '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
	       '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
  (setq org-latex-pdf-process
	'("latexmk -shell-escape -lualatex -bibtex -pdf %f"))
  (setq org-latex-default-class "koma-article")

  (require 'ox-md))

(use-package ox-pandoc
  :straight t
  :after (org)
  :config
  (setq org-pandoc-options '((standalone . t)
			     (bibliography . "~/notes/bibliography/bibliography.bib"))))

(use-package ox-gfm
  :straight t
  :after (org)
  :config
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

(use-package ox-reveal
  :straight t)

(use-package ob-mermaid
  :straight t
  :after (org))

(use-package org-fragtog
  :straight t
  :after (org)
  :hook (org-mode . org-fragtog-mode))

(use-package citeproc
  :straight t)

(use-package bibtex
  :config
  (setq bibtex-dialect 'biblatex
	bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator ""
	bibtex-autokey-year-title-separator "_"
	bibtex-autokey-titleword-separator "_"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1))

(use-package oc
  :straight nil
  :init
  (require 'oc-csl)
  (require 'oc-biblatex))

(use-package citar
  :straight t
  :custom
  (org-cite-global-bibliography '("~/notes/bibliography/bibliography.bib"))
  (org-cite-export-processors '((beamer . biblatex)
				(latex . biblatex)
				(t . (csl "~/notes/bibliography/chicago-author-date.csl"))))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-citeproc-csl-styles-dir "~/notes/bibliography/")
  (citar-citeproc-csl-locales-dir "~/notes/bibliography/")
  (citar-notes-paths '("~/notes/notes"))
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :straight t
  :after citar embark
  :init
  (setq citar-at-point-function 'embark-act)
  :config (citar-embark-mode))

(use-package org-roam
  :straight (:type git :flavor melpa :files (:defaults "extensions/*" "org-roam-pkg.el") :host github :repo "org-roam/org-roam" :branch "main")
  :after org
  :custom
  (org-roam-directory (file-truename "~/notes/notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n d n" . org-roam-dailies-capture-today)
	 ("C-c n d t" . org-roam-dailies-goto-today)
	 ("C-c n d d" . org-roam-dailies-capture-date)
	 ("C-c n d g" . org-roam-dailies-goto-date))
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-completion-system 'default)
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-graph-executable "dot")
  (setq org-roam-graph-extra-config '(("overlap" . "false") ("rankdir" . "LR")))
  (require 'org-roam-protocol)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry "* %U %?"
	   :empty-lines-before 1
	   :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
"))))
  (require 'org-roam-export))

(use-package org-roam-bibtex
  :straight t
  :after (org-roam citar)
  :hook (org-roam-mode . org-roam-bibtex-mode))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package deft
  :straight t
  :after org
  :bind ("C-c n s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/notes/notes")
  (deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  (deft-use-filename-as-title t))

(defun dl/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep.  With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package deadgrep
  :straight t
  :bind (("<f5>" . deadgrep)))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :straight t :after yasnippet)
  :hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (yas-reload-all)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))

(use-package eglot
  :straight nil
  :config
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode nil))

(use-package eldoc-box
  :straight t
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package format-all
  :straight t
  :bind ("C-c C-f" . format-all-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming languages modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :straight t)

(use-package json-mode
  :straight t)

(use-package markdown-mode
  :straight t)

(use-package cmake-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package terraform-mode
  :straight t)

(use-package mermaid-mode
  :straight t)

(use-package haskell-mode
  :straight t)

(use-package rust-mode
  :straight t)

(use-package pyvenv
  :straight t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package julia-mode
  :straight t)

(use-package julia-repl
  :straight t
  :hook (julia-mode . julia-repl-mode)
  :init
  (setenv "JULIA_NUM_THREADS" "6")
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package ess
  :straight t
  :config
  (defun my-ess-startup-directory-function ()
    "Force ESS to use `default-directory' as its startup directory."
    default-directory)
  (setq ess-startup-directory-function 'my-ess-startup-directory-function))

(use-package paredit
  :straight t
  :hook ((emacs-lisp-mode
	  eval-expression-minibuffer-setup
	  ielm-mode
	  lisp-mode
	  lisp-interaction-mode
	  scheme-mode
	  inferior-scheme-mode
	  slime-repl-mode
	  racket-mode
	  racket-repl-mode
	  gerbil-mode) . enable-paredit-mode))

(use-package slime
  :straight t
  :config
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy slime-repl slime-quicklisp))
  (setq slime-lisp-implementations
	'((sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))
  (setq slime-net-coding-system 'utf-8-unix)
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  (setq lisp-indent-function 'lisp-indent-function)
  ;; Use the offline hyperspec from the "clhs" quicklisp package:
  ;; https://www.hexstreamsoft.com/libraries/clhs/
  (load "/home/dimitri/quicklisp/clhs-use-local.el" t))

(use-package geiser-racket
  :straight t)

(use-package geiser-chicken
  :straight t)

(use-package geiser-gambit
  :straight t)

(use-package geiser-guile
  :straight t)

(use-package gerbil-mode
  :when (getenv "GERBIL_HOME")
  :straight nil
  :defer t
  :mode (("\\.ss\\'"  . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :init
  (setf gambit (getenv "GAMBIT"))
  (setf gerbil (getenv "GERBIL_HOME"))
  (autoload 'gerbil-mode
    (concat gerbil "/etc/gerbil-mode.el") "Gerbil editing mode." t)
  :hook
  ((inferior-scheme-mode . gambit-inferior-mode))
  :config
  (require 'gambit (concat gambit "/misc/gambit.el"))
  (setf scheme-program-name (concat gerbil "/bin/gxi"))

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (visit-tags-table (concat gerbil "/src/TAGS")))

;; Use APL font face in current buffer
(defun my-buffer-face-mode-apl ()
  "Use the APL font in current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:family "APL385 Unicode" :height 150))
  (buffer-face-mode))

(use-package dyalog-mode
  :straight t
  :hook (dyalog-mode . my-buffer-face-mode-apl)
  :custom
  (dyalog-fix-whitespace-before-save t))

(use-package gnu-apl-mode
  :straight t)

(use-package bqn-mode
  :straight (:host github :repo "museoa/bqn-mode")
  :after gnu-apl-mode
  :bind (:map bqn-mode-map
	      (("C-c d" . bqn-help-symbol-info-at-point)
	       ("C-c C-d" . bqn-help-symbol-info-at-point)))
  :custom (bqn-key-prefix ?ù)
  :config
  (defface bqn-default
    '((t (:height 140 :family "BQN386 Unicode"))) "BQN default face.")

  (defun bqn-init ()
    (setq buffer-face-mode-face 'bqn-default)
    (buffer-face-mode))

  (setq bqn-keymap-mode-reference
	"
  ┌───────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┐
  ┊(AltGr)┊~ ¬ ┊# ⍟ ┊{ ⊣ ┊[ ← ┊|   ┊` ˜	┊\\   ┊^ ⎊ ┊@   ┊] → ┊} ⊢ ┊
┌─┴──┬────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼─────────┐
│    │1 ˘ │2 ¨ │3 ⁼ │4 ⌜ │5 ´ │6 ˝ │7   │8 ∞ │9 ¯ │0 • │°   │+ ⋆ │Backspace│
│²   │& ⍎ │é   │\" ˙ │' ↩ │( ⟨ │- ÷ │è   │_ √ │ç   │à   │) ⟩ │= × │         │
├────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬──────┤
│Tab    │A ↖ │Z ⋈ │E ⍷ │R 𝕣 │T ⍋ │Y   │U   │I ⊑ │O ⊒ │P ⍳ │¨   │£   │Enter │
│       │a ⍉ │z ⥊ │e ∊ │r ↑ │t ∧ │y   │u ⊔ │i ⊏ │o ⊐ │p π │^ ⎊ │$ ◶ │      │
├───────┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┐     │
│Caps    │Q ↙ │S 𝕊 │D   │F 𝔽 │G 𝔾 │H « │J   │K ⌾ │L » │M ≢ │% ⊘ │µ   │     │
│Lock    │q ⌽ │s 𝕤 │d ↕ │f 𝕗 │g 𝕘 │h ⊸ │j ∘ │k ○ │l ⟜ │m ≡ │ù   │* ⍕ │     │
├────────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴────┴─────┤
│Shift      │W 𝕎 │X 𝕏 │C   │V ⍒ │B ⌈ │N   │? ⇐ │. ≍ │/ ≠ │§ ⎉ │Shift       │
│           │w 𝕨 │x 𝕩 │c ↓ │v ∨ │b ⌊ │n   │, ∾ │; ⋄ │: · │! ⎉ │            │
└───────────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────────────┘
                             Space: ‿
")

  (add-hook 'bqn-mode-hook 'bqn-init)
  (add-hook 'bqn-comint-mode-hook 'bqn-init)
  (add-hook 'bqn-keymap-mode-hook 'bqn-init))

(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4)
  (setq graphviz-dot-preview-extension "svg"))

;;; Natural languages

(use-package tex-site
  :defer t
  :straight auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-engine 'luatex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package flyspell
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_GB")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))
  (add-hook 'text-mode-hook #'flyspell-mode)
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (global-set-key (kbd "<f8>") 'ispell-word)
  (global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
  (global-set-key (kbd "M-<f8>") 'flyspell-buffer)
  (global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External media
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elfeed
  :straight t
  :bind ("C-c f" . elfeed)
  :config
  (setq shr-width 100))

(use-package elfeed-org
  :straight t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/notes/elfeed.org")))

(use-package mastodon
  :straight t
  :config
  (setq mastodon-instance-url "https://mathstodon.xyz"
	mastodon-active-user "dlzv"))

(use-package pdf-tools
  :straight t
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(defun dl/view-exif-data (file)
  "View EXIF data of FILE."
  (interactive "fFile: ")
  (let ((buf-name (concat "*EXIF " file "*")))
    ;; If the buffer already exists, kill it.
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    ;; Create a new buffer and window.
    (let ((buf (get-buffer-create buf-name))
	  (window (split-window nil)))
      (call-process "exiftool"
		    nil buf t
		    (expand-file-name file))
      (with-current-buffer buf
	(goto-char (point-min))
	;; Read-only, q to close the window, C-u q to close and kill.
	(special-mode))
      (set-window-buffer window buf))))

(defun dl/set-exif-data (file tag-name tag-value)
  "In FILE, set EXIF tag TAG-NAME to value TAG-VALUE."
  (interactive "fFile: \nsTag: \nsValue: ")
  (let ((options '("-%t=%v" "-overwrite_original" "%f"))
	(spec (list
	       (cons ?f (expand-file-name file))
	       (cons ?t tag-name)
	       (cons ?v tag-value))))
    (apply #'call-process "exiftool" nil nil nil
	   (mapcar (lambda (arg) (format-spec arg spec)) options))))

(use-package nov
  :straight t
  :custom
  (nov-text-width 80)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package hledger-mode
  :straight t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :init
  (setq hledger-jfile (expand-file-name "~/.hledger.journal"))
  :config
  (add-to-list 'company-backends 'hledger-company)
  (global-set-key (kbd "C-c e") 'hledger-jentry)
  (global-set-key (kbd "C-c j") 'hledger-run-command))

(use-package vterm
  :straight t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (global-set-key (kbd "C-x RET RET") 'vterm-other-window)
  (define-key vterm-mode-map (kbd "<C-backspace>")
	      (lambda () (interactive) (vterm-send-key (kbd "C-w")))))

;; configuration file for secrets (API keys, etc)
(setq secrets-file (expand-file-name "secrets.el" user-emacs-directory))

(when (file-exists-p secrets-file)
  (load secrets-file))


;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))


;;; init.el ends here
