;; -*- lexical-binding: t -*-

;;; ~/.emacs.d/init.el --- Configuration file for Emacs

;;; Commentary:

;;; Code:

;;; Elpaca setup

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
;; Block until package is installed/activated so we can use it at the top-level below.
(elpaca-wait)

;;; Appearance and general behavior

(use-package emacs
  :ensure nil
  :config
  ;; Always load newest byte code
  (setq load-prefer-newer t)

  ;; Display configuration
  (setq inhibit-startup-screen t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (unless (eq window-system 'ns)
    (menu-bar-mode -1))
  (setq visible-bell 1)
  (blink-cursor-mode 0)
  (show-paren-mode 1)
  (global-hl-line-mode -1)
  (global-hi-lock-mode 1)
  (setq tab-bar-show 1)
  ;; More useful frame title, that show either a file or a buffer name
  ;; (if the buffer isn't visiting a file)
  (setq frame-title-format
	'((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

  ;; Font configuration
  (let ((my-font "Iosevka Term")
	(my-height (if (eq window-system 'ns) 130 120)))
    (set-face-attribute 'default nil :family my-font :height my-height)
    (set-face-attribute 'fixed-pitch nil :family my-font :height my-height))
  (set-face-attribute 'variable-pitch nil :family "Libertinus Serif" :height 160)

  ;; Mode line configuration
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)
  (setq display-time-24hr-format t)
  (display-time-mode 1)

  ;; Behavior configuration
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq auto-window-vscroll nil)
  (setq frame-resize-pixelwise t)
  (setq split-height-threshold 100)
  (setq view-read-only t)
  (setq buffer-save-without-query t)
  ;; Newline at end of file
  (setq require-final-newline t)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; System locale to use for formatting time values.  Make sure that
  ;; the weekdays in the time stamps of your Org mode files and in the
  ;; agenda appear in English.
  (setq system-time-locale "C")

  ;; Enable disabled commands
  (put 'narrow-to-region 'disabled nil)

  ;; MacOS specific configuration
  (setq ns-right-alternate-modifier nil
	ns-command-modifier 'meta)

  ;; Dired human readable sizes
  (setq dired-listing-switches "-Alh")

  ;; C mode configuration
  (setq-default c-basic-offset 2)
  (setq c-default-style "linux")
  (setq-default c-doc-comment-style
		'((c-mode    . doxygen)
                  (c++-mode  . doxygen)))

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

  :bind (;; ("M-o" . other-window) ;; Replaced with ace-window
	 ([remap buffer-menu] . ibuffer)
	 ([remap count-words-region] . count-words)
	 ([remap just-one-space] . cycle-spacing)
	 ([remap zap-to-char] . zap-up-to-char)
	 ;; Better than default: act on the region if active
	 ([remap upcase-word] . upcase-dwim)
	 ([remap downcase-word] . downcase-dwim)
	 ([remap capitalize-word] . capitalize-dwim)))

(use-package project
  :ensure nil
  :config
  (keymap-set project-prefix-map "t" 'eat-project)
  (keymap-set project-prefix-map "RET" 'eat-project-other-window)
  (keymap-set project-prefix-map "g" 'magit-project-status)
  (setq project-switch-commands
	'((project-find-file "Find file")
	  (project-find-dir "Find directory")
	  (project-eshell "Eshell")
	  (eat-project "Terminal")
	  (eat-project-other-window "Terminal other window")
	  (magit-project-status "Magit"))))

;; Theme configuration
(use-package ef-themes
  :ensure t
  :demand t
  :init
  (modus-themes-include-derivatives-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-to-toggle '(ef-light ef-dark))
  (modus-themes-load-theme 'ef-dark)
  (when (eq window-system 'ns)
    (defun dl/themes-toggle-with-system (appearance)
      "Load dark or light theme depending on system appearance on macOS."
      (pcase appearance
	('light (modus-themes-load-theme 'ef-light))
	('dark (modus-themes-load-theme 'ef-dark))))
    (add-hook 'ns-system-appearance-change-functions #'dl/themes-toggle-with-system))
  :bind ("<f12>" . modus-themes-toggle))

(use-package diminish
  :ensure t
  :diminish visual-line-mode)

(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll" :branch "main")
  :custom
  (scroll-conservatively 3)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;;; Environment variables

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (setenv "SSH_AUTH_SOCK" (file-name-concat (getenv "XDG_RUNTIME_DIR") "ssh-agent.socket"))
  (when (memq window-system '(mac ns x pgtk))
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;;; Menus and completion

(use-package vertico
  :ensure t
  :init
  (vertico-mode +1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
	 ("C-s" . consult-line)                    ;; orig. isearch-forward
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode +1))

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ;;("M-;" . embark-dwim)
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
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
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
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :init
  (global-corfu-mode))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame))

(use-package avy
  :ensure t
  :config  (avy-setup-default)
  :bind (("M-j" . avy-goto-char-timer)
	 ("C-c C-j" . avy-resume)))

(use-package vundo
  :ensure t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)))

(use-package casual
  :ensure t
  :config
  (require 'casual-dired)
  (require 'casual-calc)
  (require 'casual-editkit)
  (require 'casual-image)
  (require 'casual-info)
  (require 'casual-bibtex)
  :bind ( :map dired-mode-map
	  ("C-o" . casual-dired-tmenu)
	  ("s" . casual-dired-sort-by-tmenu)
	  ("/" . casual-dired-search-replace-tmenu)
	  :map calc-mode-map
	  ("C-o" . casual-calc-tmenu)
	  :map calc-alg-map
	  ("C-o" . casual-calc-tmenu)
	  :map image-mode-map
	  ("C-o" . casual-image-tmenu)
	  :map Info-mode-map
	  ("C-o" . casual-info-tmenu)
	  :map bibtex-mode-map
	  ("C-o" . casual-bibtex-tmenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Knowledge management: org-mode, org-roam, bibliography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq notes-dir (if (eq window-system 'ns) "~/Documents/personal/notes" "~/Documents/notes"))

;; Pour accéder rapidement à l'organisation
(defun gtd ()
  "Find the planner file."
  (interactive)
  (find-file (file-name-concat notes-dir "planner.org")))

(defun bib ()
  "Find the bibliography file."
  (interactive)
  (find-file (file-name-concat notes-dir "bibliography/bibliography.bib"))
  (end-of-buffer))

(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :custom
  (org-agenda-files (list (file-name-concat notes-dir "planner.org")))
  (org-default-notes-file (file-name-concat notes-dir "planner.org"))
  ;; List numbering with a. b. a) b), etc.
  (org-list-allow-alphabetical t)
  ;; Fontify code in code blocks
  (org-src-fontify-natively t)
  ;; Tabs in src blocks
  (org-src-tab-acts-natively t)
  ;; Full contents opened by default
  (org-startup-folded nil)
  ;; Only one empty line is enough to separate headings when folded
  (org-cycle-separator-lines 2)
  ;; Refile to paths in the file (level1/level2/level3)
  (org-refile-targets '((nil :maxlevel . 5)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-special-ctrl-a/e nil)
  ;; Set to the location of your Org files on your local system
  (org-directory notes-dir)
  ;; Agenda
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
			  (800 1000 1200 1400 1600 1800 2000)
			  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-custom-commands
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
  (org-capture-templates
   (quote
    (("t" "Task" entry
      (file+olp (file-name-concat notes-dir "planner.org") "Inbox")
      "** TODO %?")
     ("n" "Note" entry
      (file+olp (file-name-concat notes-dir "planner.org") "Inbox")
      "** %?")
     ("e" "Event" entry
      (file+olp (file-name-concat notes-dir "planner.org") "Inbox")
      "** %?\n%^T"))))
  (org-clock-persist 'history)
  (org-log-into-drawer t)
  (org-structure-template-alist
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
  ;; Org babel
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-with-latex-preview t)
  :hook ((org-mode . visual-line-mode)
	 (org-babel-after-execute . org-redisplay-inline-images))
  :config
  (org-clock-persistence-insinuate)


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

  (require 'ox-latex)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.6))

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

(use-package org-mime
  :ensure t
  :after (org)
  :config
  (setq org-mime-export-options
	'(:section-numbers nil :with-author nil :with-toc nil))
  (setq org-mime-export-ascii 'utf-8)
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                             "#E6E1DC" "#232323"))))
  ;; the following can be used to nicely offset block quotes in email bodies
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))))

(use-package ox-pandoc
  :ensure t
  :after (org)
  :custom
  (org-pandoc-options '((standalone . t)
			(bibliography . (file-name-concat notes-dir "bibliography/bibliography.bib")))))

(use-package ox-gfm
  :ensure t
  :after (org)
  :config
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

(use-package ox-reveal
  :ensure t)

(use-package ob-mermaid
  :ensure t
  :after (org))

(use-package citeproc
  :ensure t)

(use-package bibtex
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "")
  (bibtex-autokey-year-title-separator "_")
  (bibtex-autokey-titleword-separator "_")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1))

(use-package oc
  :ensure nil
  :init
  (require 'oc-csl)
  (require 'oc-biblatex))

(use-package citar
  :ensure t
  :custom
  (org-cite-global-bibliography `(,(file-name-concat notes-dir "bibliography/bibliography.bib")))
  (org-cite-csl-styles-dir (file-name-concat notes-dir "bibliography/"))
  (org-cite-export-processors '((beamer . (biblatex))
				(latex . (biblatex))
				(t . (csl "chicago-author-date.csl"))))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :custom-face
  ;; Have citation link faces look closer to as they were for `org-ref'
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic)))))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :diminish citar-embark-mode
  :custom
  (citar-at-point-function 'embark-act)
  :config (citar-embark-mode))

(defun dl/formatted-citation-at-point ()
  "Kill the formatted citation for the reference at point using Pandoc."
  (interactive)
  (let* ((bibfile (expand-file-name (car org-cite-global-bibliography)))
	 (cslfile (expand-file-name (concat org-cite-csl-styles-dir "chicago-author-date.csl")))
	 (datum (org-element-context))
         (datum-type (org-element-type datum))
         (key (if (eq datum-type 'citation-reference) (org-element-property :key datum) (citar-select-ref)))
	 (citation (shell-command-to-string
		    (format
		     (concat
		      "echo cite:%s"
		      " | pandoc --citeproc --bibliography=%s --csl=%s -f org -t markdown_strict"
		      " | tail -n +3"
		      " | tr '\n' ' '")
		     key bibfile cslfile))))
    (message citation)
    (kill-new citation)))

(defun dl/get-bibtex (query)
  "Use Wikipedia's Citoid service to get a bibtex citation of the given QUERY."
  (interactive "sQuery: ")
  (let ((target-buf (current-buffer))
	(target-point (point))
	(url (concat "https://en.wikipedia.org/api/rest_v1/data/citation/bibtex/"
		     (url-hexify-string query))))
    (message "Querying %s..." url)
    (with-current-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies 5)
      (-let* (((headers s) (split-string (buffer-string) "\n\n\n"))
	      (code (string-to-number (cadr (split-string headers)))))
	(if (eq code 200)
	    (with-current-buffer target-buf
	      (save-excursion
		(goto-char target-point)
		(insert s)))
	  (message "HTTP error %s" code))))))

(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory (file-truename (file-name-concat notes-dir "notes")))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-completion-system 'default)
  :custom
  (org-roam-graph-executable "dot")
  (org-roam-graph-extra-config '(("overlap" . "false") ("rankdir" . "LR")))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %U %?"
      :empty-lines-before 1
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
"))))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  (require 'org-roam-export))

(use-package org-roam-bibtex
  :ensure t
  :after (org-roam citar)
  :hook (org-roam-mode . org-roam-bibtex-mode))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n B" . consult-org-roam-backlinks-recursive)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(use-package org-download
  :ensure t
  :custom
  (org-download-method 'attach)
  :hook (dired-mode . org-download-enable))

(use-package org-roam-ui
  :ensure
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.ts\\'" . typescript-ts-mode))
  :preface
  (defun dl/setup-install-grammars (update)
    "Install Tree-sitter grammars if they are absent. Set UPDATE to non-nil to force grammar installation even if it already exists."
    (interactive)
    (dolist (grammar
             '((c "https://github.com/tree-sitter/tree-sitter-c")
	       (json "https://github.com/tree-sitter/tree-sitter-json")
	       (python "https://github.com/tree-sitter/tree-sitter-python")
	       (rust "https://github.com/tree-sitter/tree-sitter-rust")
	       (toml "https://github.com/tree-sitter/tree-sitter-toml")
	       (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	       (css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	       (typst "https://github.com/uben0/tree-sitter-typst")
	       (kdl "https://github.com/tree-sitter-grammars/tree-sitter-kdl")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed or if `update' is non-nil.
      (unless (and (not update) (treesit-language-available-p (car grammar)))
	(message "Installing tree-sitter language grammars")
        (treesit-install-language-grammar (car grammar)))))
  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((c-mode . c-ts-mode)
		     (json-mode . json-ts-mode)
		     (js-json-mode . json-ts-mode)
		     (python-mode . python-ts-mode)
		     (rust-mode . rust-ts-mode)
		     (toml-mode . toml-ts-mode)
		     (css-mode . css-ts-mode)
		     (typescript-mode . typescript-ts-mode)
		     (js2-mode . js-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (dl/setup-install-grammars nil))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :after transient
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package git-link
  :ensure t
  :after transient
  :config
  (require 'git-link-transient))

(use-package eglot
  :ensure nil
  :hook
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode nil)))
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package eldoc-box
  :ensure t
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode))

(use-package apheleia
  :ensure t
  :diminish apheleia-mode
  :config
  (apheleia-global-mode +1)
  (setq apheleia-formatters-respect-indent-level nil)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
	'(ruff))
  (setf (alist-get 'python-mode apheleia-mode-alist)
	'(ruff))
  (setf (alist-get 'ormolu apheleia-formatters)
	'("ormolu" "--stdin-input-file" filepath))
  (setf (alist-get 'haskell-mode apheleia-mode-alist)
	'(ormolu)))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming languages modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package kdl-ts-mode
  :ensure (:host github :repo "dataphract/kdl-ts-mode"))

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package mermaid-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package pet
  :ensure t
  :config
  ;; The -10 tells `add-hook' to makes sure the function is called as early as
  ;; possible whenever it is added to the hook variable
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package paredit
  :ensure t
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

(use-package bqn-mode
  :ensure (:host github :repo "museoa/bqn-mode")
  :bind (:map bqn-mode-map
	      (("C-c d" . bqn-help-symbol-info-at-point)
	       ("C-c C-d" . bqn-help-symbol-info-at-point)
	       ("C-c C-c" . bqn-comint-send-dwim)))
  :custom (bqn-key-prefix ?\\))

(use-package graphviz-dot-mode
  :ensure t
  :custom
  (graphviz-dot-indent-width 4)
  (graphviz-dot-preview-extension "svg"))

;;; Natural languages

(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode"))

(use-package jinx
  :ensure t
  :diminish jinx-mode
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US fr_FR")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External media
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dirvish
  :ensure t
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")))
  (dirvish-header-line-height 16)
  (dirvish-mode-line-height 16)
  (dirvish-default-layout '(1 0.21 0.35))
  :config
  (dirvish-override-dired-mode))

(use-package pdf-tools
  :ensure t
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))


(defun dl/edit-exif-metadata ()
  "Edit the EXIF metadata of the file at point or of the current file."
  (interactive)
  (let* ((file (or (thing-at-point 'existing-filename) (buffer-file-name)))
         (buffer (switch-to-buffer-other-window "*EXIFTool Metadata*"))
         (metadata (with-temp-buffer
                     (call-process "exiftool" nil t nil file)
                     (buffer-string))))
    ;; Display current metadata
    (with-current-buffer buffer
      (erase-buffer)
      (insert metadata)
      (goto-char (point-min))
      (while (re-search-forward "\\(.*\\): \\(.*\\)" nil t)
        (replace-match "\\1:\t\\2")))
    (switch-to-buffer buffer)
    (local-set-key (kbd "C-c C-c") (lambda ()
				     (interactive)
				     (dl/save-exif-metadata file buffer)))
    (local-set-key (kbd "C-c C-k") 'kill-buffer)))

(defun dl/save-exif-metadata (file buffer)
  "Save the edited EXIF metadata in BUFFER to the FILE."
  (interactive)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((arguments '()))
      (while (re-search-forward "\\(.*?\\):\\(.*\\)" nil t)
        (let ((tag (string-trim (match-string 1)))
              (value (string-trim (match-string 2))))
          (push (format "-%s=%s" tag value) arguments)))
      ;; Write metadata changes to file using exiftool
      (apply #'call-process
	     "exiftool" nil nil nil (append arguments (list "-overwrite_original" file)))
      (kill-buffer)
      (message "EXIF metadata updated."))))

(use-package nov
  :ensure t
  :custom
  (nov-text-width 80)
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package hledger-mode
  :ensure t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :init
  (setq hledger-jfile (expand-file-name "~/.hledger.journal"))
  :custom
  (hledger-currency-string " ")
  (hledger-top-income-account "revenues")
  (hledger-ratios-assets-accounts "assets")
  (hledger-ratios-income-accounts "revenues")
  (hledger-ratios-liquid-asset-accounts "assets:checking assets:cash assets:tickets-resto")
  (hledger-ratios-debt-accounts "liabilities")
  (hledger-ratios-essential-expense-accounts "expenses:banking expenses:food expenses:rent expenses:insurance expenses:medical expenses:taxes expenses:transport expenses:utilities")
  :bind
  (("C-c e" . hledger-jentry)
   ("C-c j" . hledger-run-command)))

(use-package osm
  :ensure t
  :bind ("C-c m" . osm-prefix-map) ;; Alternatives: `osm-home' or `osm'
  :custom
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright nil)   ;; Display the copyright information
  (osm-home '(48.853495 2.348391 12)))

(use-package eat
  :ensure (:host codeberg :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
			 "*.ti" ("terminfo/e" "terminfo/e/*")
			 ("terminfo/65" "terminfo/65/*")
			 ("integration" "integration/*")
			 (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (("C-x RET RET" . eat-other-window))
  :config
  ;; Enable M-o in semi-char-mode
  (add-to-list 'eat-semi-char-non-bound-keys [?\e ?o])
  (eat-update-semi-char-mode-map)
  (eat-reload)
  ;; Appearance
  (setq eat-term-scrollback-size nil)
  (setq eat-enable-shell-prompt-annotation nil)
  (setopt eat-very-visible-cursor-type '(t nil nil))
  (setopt eat-default-cursor-type '(t nil nil))
  (setq eshell-visual-commands '())
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (setq process-adaptive-read-buffering nil)
  (setq read-process-output-max (* 4 1024 1024)))


;; configuration file for secrets (API keys, etc)
(setq secrets-file (expand-file-name "secrets.el" user-emacs-directory))

(add-hook 'elpaca-after-init-hook
	  (lambda () (when (file-exists-p custom-file)
		       (load secrets-file 'noerror))))


;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook
	  (lambda () (when (file-exists-p custom-file)
		       (load custom-file 'noerror))))

(use-package gptel
  :ensure t)

(message "Successfully loaded entire config!")
;;; init.el ends here
