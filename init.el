;;; ~/.emacs.d/init.el --- Configuration file for Emacs

;;; Commentary:

;;; Code:

;;; Straight.el setup

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

;;; Appearance and general behavior

(use-package emacs
  :straight nil
  :config
  ;; Always load newest byte code
  (setq load-prefer-newer t)

  ;; Display configuration
  (setq inhibit-startup-screen t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
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
	(my-height 130))
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
  :straight nil
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
  :straight t
  :init
  (setq ef-themes-mixed-fonts t)
  (setq ef-themes-to-toggle '(ef-light ef-dark))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark :no-confirm)
  (when (eq window-system 'ns)
    (defun dl/themes-toggle-with-system (appearance)
      "Load dark or light theme depending on system appearance on macOS."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
	('light (load-theme 'ef-light :no-confirm))
	('dark (load-theme 'ef-dark :no-confirm))))
    (add-hook 'ns-system-appearance-change-functions #'dl/themes-toggle-with-system))
  :bind ("<f12>" . ef-themes-toggle))

(use-package diminish
  :straight t
  :diminish visual-line-mode)

;;; Environment variables

(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (when (memq window-system '(mac ns x pgtk))
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;;; Menus and completion

(use-package vertico
  :straight t
  :init
  (vertico-mode +1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :straight t
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
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

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

(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))

(use-package avy
  :straight t
  :config  (avy-setup-default)
  :bind (("M-j" . avy-goto-char-timer)
	 ("C-c C-j" . avy-resume)))

(use-package vundo
  :straight t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)))

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
  :straight nil
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
   org-format-latex-options (plist-put org-format-latex-options :scale 0.6))

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

(use-package org-mime
  :straight t
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
  (org-cite-csl-styles-dir "~/notes/bibliography/")
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
  :straight t
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

(use-package treesit
  :straight nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
	 ("\\.ts\\'" . typescript-ts-mode))
  :preface
  (defun dl/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
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
	       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
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
                     (yaml-mode . yaml-ts-mode)
		     (css-mode . css-ts-mode)
		     (typescript-mode . typescript-ts-mode)
		     (js2-mode . js-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (dl/setup-install-grammars))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package rg
  :straight t
  :config
  (rg-enable-default-bindings))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package forge
  :straight t
  :after magit)

(use-package eglot
  :straight nil
  :config
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode nil)
  (setq eglot-events-buffer-size 0)
  (fset #'jsonrpc--log-event #'ignore))

(use-package eldoc-box
  :straight t
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package apheleia
  :straight t
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
  :straight t
  :hook (python-mode . flymake-ruff-load))

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
  :straight t)

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

(use-package racket-mode
  :straight t)

(use-package geiser-chicken
  :straight t)

(use-package geiser-gambit
  :straight t)

(use-package geiser-guile
  :straight t)

(use-package gerbil-mode
  :straight nil
  :defer t
  :mode (("\\.ss\\'"  . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :init
  (if (eq system-type 'darwin)
      (add-to-list 'load-path "/opt/homebrew/opt/gerbil-scheme/share/emacs/site-lisp")
    (add-to-list 'load-path "/opt/gerbil/share/emacs/site-lisp"))
  (require 'gambit)
  (autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
  :hook
  ((inferior-scheme-mode . gambit-inferior-mode))
  :config
  (setf scheme-program-name "gxi")

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (visit-tags-table "~/build/gerbil/src/TAGS"))

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

(use-package bqn-mode
  :straight (:host github :repo "museoa/bqn-mode")
  :bind (:map bqn-mode-map
	      (("C-c d" . bqn-help-symbol-info-at-point)
	       ("C-c C-d" . bqn-help-symbol-info-at-point)
	       ("C-c C-c" . bqn-comint-send-dwim)))
  :custom (bqn-key-prefix ?\\)
  :config
  (require 'bqn-keymap-mode)
  (require 'bqn-glyph-mode))

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

(use-package jinx
  :straight t
  :diminish jinx-mode
  :hook (emacs-startup . global-jinx-mode)
  :config
  (setq jinx-languages "en_US fr_FR")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External media
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dirvish
  :straight t
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")))
  (dirvish-header-line-height 16)
  (dirvish-mode-line-height 16)
  (dirvish-default-layout '(1 0.21 0.35))
  :config
  (dirvish-override-dired-mode))

(use-package restclient
  :straight t)

(use-package ob-restclient
  :straight t
  :after (org))


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
  (interactive (let ((fname (thing-at-point 'existing-filename)))
		 (list
		  (read-string (format "File (%s): " (file-name-nondirectory fname))
			       nil nil fname))))
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
  (interactive (list (let ((fname (thing-at-point 'existing-filename)))
		       (read-string (format "File (%s): " (file-name-nondirectory fname))
				    nil nil fname))
		     (read-string "Tag (Title): " nil nil "Title")
		     (read-string "Value: ")))
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
  :straight t
  :bind ("C-c m" . osm-prefix-map) ;; Alternatives: `osm-home' or `osm'
  :custom
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright nil)   ;; Display the copyright information
  (osm-home '(48.853495 2.348391 12)))

(use-package eat
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
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
  (setq eat-enable-shell-prompt-annotation nil)
  (setopt eat-very-visible-cursor-type '(t nil nil))
  (setopt eat-default-cursor-type '(t nil nil))
  (setq eshell-visual-commands '())
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (setq process-adaptive-read-buffering nil)
  (setq read-process-output-max (* 4 1024 1024)))


;; configuration file for secrets (API keys, etc)
(setq secrets-file (expand-file-name "secrets.el" user-emacs-directory))

(when (file-exists-p secrets-file)
  (load secrets-file))


;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))


;;; init.el ends here
