;;; ~/.emacs.d/init.el --- Configuration file for Emacs

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

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

(setq auto-window-vscroll nil)

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

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; System locale to use for formatting time values.  Make sure that
;; the weekdays in the time stamps of your Org mode files and in the
;; agenda appear in English.
(setq system-time-locale "C")

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

(global-set-key (kbd "M-z") #'zap-up-to-char)

;; Better than default: act on the region if active
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)

(setq-default c-basic-offset 2)
(setq c-default-style "linux")

(let ((my-font "Iosevka:pixelsize=18"))
  (set-frame-font my-font nil t)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  (set-face-attribute 'default t :font my-font)
  (set-face-attribute 'default nil :font my-font))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;;; built-in packages
(use-package paren
  :config
  (show-paren-mode 1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode -1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Theme
(use-package base16-theme
  :ensure t
  :init
  (setq base16-theme-256-color-source 'base16-shell)
  :config
  (load-theme 'base16-default-dark t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package deadgrep
  :ensure t
  :bind (("<f5>" . deadgrep)))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?q ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package forge
  :ensure t
  :after magit)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :ensure t :after yasnippet)
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

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-tags-command "ctags-exuberant -Re")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package format-all
  :ensure t
  :bind ("C-c C-f" . format-all-buffer))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

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

(use-package langtool
  :ensure t
  :init
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*"))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :hook ((rust-mode . lsp)
	 (c-mode . lsp))
  :config
  (setq lsp-clients-clangd-args
	'("-j=2"
	  "--background-index"
	  "--clang-tidy"
	  "--completion-style=bundled"
	  "--pch-storage=memory"
	  "--header-insertion=never"
	  "--header-insertion-decorators=0"
	  "--suggest-missing-includes")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :config
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'top))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :ensure t)

(use-package python-docstring
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (add-hook 'python-mode-hook 'python-docstring-mode)
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-hook 'elpy-mode-hook 'projectile-mode)
  (add-hook 'elpy-mode-hook (lambda () (flycheck-select-checker 'python-mypy)))
  (setq elpy-eldoc-show-current-function nil)
  (define-key elpy-mode-map (kbd "C-c C-f") nil)
  (define-key python-mode-map (kbd "C-c C-f") nil)

  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    ;; (flycheck-add-next-checker 'python-pylint '(warning . python-mypy) t)
    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    (add-to-list 'flycheck-disabled-checkers 'python-pylint)))

(use-package julia-mode
  :ensure t)

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package lispy
  :ensure t
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode lisp-mode lisp-interaction-mode scheme-mode slime-repl-mode) . lispy-mode))

(use-package slime
  :ensure t
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

(use-package racket-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

;; Use APL font face in current buffer
(defun my-buffer-face-mode-apl ()
  "Use the APL font in current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:family "APL385 Unicode" :height 150))
  (buffer-face-mode))

(use-package matlab
  :ensure matlab-mode
  :defer t
  :config
  ;; This is a simple script to set the required environment variables
  ;; before launching Matlab in Emacs. This prevents an issue where
  ;; all plot windows are blank. See Arch wiki for more details.

  ;; #!/usr/bin/env bash
  ;; export _JAVA_AWT_WM_NONREPARENTING=1
  ;; /usr/local/bin/matlab "$@"
  (setq matlab-shell-command "~/.local/bin/run_matlab.sh")
  (setq matlab-shell-command-switches '("-nodesktop"))
  (setq mlint-program "/usr/local/bin/mlint")
  (setq-default matlab-show-mlint-warnings t)
  (set-face-attribute 'linemark-stop-face nil
		      :background 'unspecified
		      :underline '(:color "red3" :style wave))
  (set-face-attribute 'linemark-caution-face nil
		      :background 'unspecified
		      :underline '(:color "yellow4" :style wave))
  (set-face-attribute 'linemark-go-face nil
		      :background 'unspecified
		      :underline '(:color "green4" :style wave))
  (set-face-attribute 'linemark-funny-face nil
		      :background 'unspecified
		      :underline '(:color "blue3" :style wave)))

(use-package dyalog-mode
  :ensure t
  :hook (dyalog-mode . my-buffer-face-mode-apl)
  :custom
  (dyalog-fix-whitespace-before-save t))

(use-package gnu-apl-mode
  :ensure t
  :config
  (defface gnu-apl-default
    '((t (:height 1.2 :family "APL385 Unicode"))) t)
  (defun em-gnu-apl-init ()
    (setq buffer-face-mode-face 'gnu-apl-default)
    (buffer-face-mode))
  (add-hook 'gnu-apl-interactive-mode-hook 'em-gnu-apl-init)
  (add-hook 'gnu-apl-mode-hook 'em-gnu-apl-init))

(use-package tex-site
  :defer t
  :ensure auctex
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

(use-package restclient
  :ensure t
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

(use-package elfeed
  :ensure t
  :bind ("C-c f" . elfeed))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/notes/elfeed.org")))

(use-package pdf-tools
  :ensure t
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 100))

(use-package twittering-mode
  :ensure t
  :config
  (setq twittering-reverse-mode nil))

(use-package mathpix.el
  :load-path "site-lisp"
  ;; You also need to configure `mathpix-app-id` and
  ;; `mathpix-app-key`, for instance in secrets.el
  :bind
  ("C-x m" . mathpix-screenshot))

;; Org-mode
;; Pour accéder rapidement à l'organisation
(defun gtd ()
  "Find the planner/GTD file."
  (interactive)
  (find-file "~/notes/planner.org"))

(defun journal ()
  "Find the journal file."
  (interactive)
  (find-file "~/notes/journal.org"))

(defun bib ()
  "Find the bibliography file."
  (interactive)
  (find-file "~/notes/bibliography/bibliography.bib"))

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb)
	 ("C-c c" . org-capture))
  :config
  (setq org-agenda-files (list "~/notes/planner.org"))
  (setq org-default-notes-file "~/notes/planner.org")
  ;; autorise les listes avec numérotation en a. b. a) b), etc.
  (setq org-list-allow-alphabetical t)
  ;; fontify code in code blocks
  (setq org-src-fontify-natively t)
  ;; tabs in src blocks
  (setq org-src-tab-acts-natively t)

  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/notes")

  (setq org-agenda-custom-commands
	(quote
	 (("z" "Agenda and TODOs"
	   ((agenda ""
		    ((org-agenda-span 7)
		     (org-agenda-start-on-weekday 1)
		     (org-agenda-prefix-format " → %t%s ")
		     (org-agenda-repeating-timestamp-show-all t)))
	    (tags-todo "work"
		       ((org-agenda-prefix-format "[ ] ")
			(org-agenda-sorting-strategy '(tag-up priority-down))))
	    (tags-todo "personal"
		       ((org-agenda-prefix-format "[ ] ")
			(org-agenda-sorting-strategy '(tag-up priority-down)))))
	   ((org-agenda-remove-tags t))
	   ("fortnight.html"))
	  ("n" "Agenda and all TODOs"
	   ((agenda "" nil)
	    (alltodo "" nil))
	   nil))))

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (require 'ox-md nil t)

  (setq org-capture-templates
	(quote
	 (("t" "Task" entry
	   (file+olp "~/notes/planner.org" "Inbox")
	   "** TODO %?")
	  ("e" "Event" entry
	   (file+olp "~/notes/planner.org" "Inbox")
	   "** %?"))))
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
     (shell . t)))

  (setq org-confirm-babel-evaluate nil)

  (setq org-latex-pdf-process
	'("latexmk -shell-escape -bibtex -pdf %f"))

  ;; System locale to use for formatting time values.  Make sure that
  ;; the weekdays in the time stamps of your Org mode files and in the
  ;; agenda appear in English.
  (setq system-time-locale "C")

  (with-eval-after-load "ox-latex"
    (add-to-list 'org-latex-classes
		 '("koma-article" "\\documentclass{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (setq org-latex-default-class "koma-article"))

(use-package ox-pandoc
  :ensure t)

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/notes/bibliography/bibliography.bib"))
  (setq org-ref-bibliography-notes "~/notes/bibliography/notes.org"
	org-ref-default-bibliography '("~/notes/bibliography/bibliography.bib")
	org-ref-pdf-directory "~/notes/bibliography/files/")

  (setq org-ref-completion-library 'org-ref-helm-bibtex)
  (setq bibtex-dialect 'biblatex)

  (add-to-list 'org-ref-formatted-citation-formats
	       '("custom"
		 ("article" . "${author}, ${title}, ${journal}, ${volume}(${number}), ${pages} (${year}). ${doi} ${url}")
		 ("inproceedings" . "${author}, ${title}, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}. ${url}")
		 ("book" . "${author}, ${title} (${year}), ${address}: ${publisher}. ${url}")
		 ("phdthesis" . "${author}, ${title} (Doctoral dissertation) (${year}). ${school}, ${address}. ${url}")
		 ("inbook" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}. ${url}")
		 ("incollection" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}. ${url}")
		 ("proceedings" . "${editor} (Eds.), ${booktitle} (${year}). ${address}: ${publisher}. ${url}")
		 ("unpublished" . "${author}, ${title} (${year}). Unpublished manuscript. ${url}")
		 (nil . "${author}, ${title} (${year}). ${url}")))
  (setq org-ref-formatted-citation-backend "custom")

  (require 'doi-utils)
  (require 'org-ref-isbn)
  (require 'org-ref-arxiv)

  (setq bibtex-autokey-year-length 4))

(defun formatted-citation-at-point ()
  "Kill the formatted citation for the reference at point using Pandoc."
  (interactive)
  (let* ((bibfile (expand-file-name (car (org-ref-find-bibliography))))
	 (cslfile (concat (file-name-directory bibfile) "chicago-author-date.csl")))
    (kill-new
     (shell-command-to-string
      (format
       "echo cite:%s | pandoc --filter=pandoc-citeproc --bibliography=%s --csl=%s -f org -t markdown_strict | tail -n +3 | tr '\n' ' '"
       (org-ref-get-bibtex-key-under-cursor)
       bibfile
       cslfile)))))

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/notes/notes")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :config
  (setq org-roam-graph-executable "dot")
  (setq org-roam-graph-extra-config '(("overlap" . "false") ("rankdir" . "LR")))
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-link-use-custom-faces 'everywhere)
  (require 'org-roam-protocol))

(use-package deft
  :ensure t
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/notes/notes"))

(use-package ox-gfm
  :ensure t
  :after (org)
  :config
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

(use-package mu4e
  :load-path "~/build/mu-1.4.13/mu4e"
  :bind ("C-c m" . mu4e)
  :custom
  ;; Folders
  (mu4e-sent-folder   "/sent")       ;; folder for sent messages
  (mu4e-drafts-folder "/drafts")     ;; unfinished messages
  (mu4e-trash-folder  "/trash")      ;; trashed messages
  (mu4e-refile-folder "/archive")    ;; saved messages
  ;; Sync
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300) ;       ;; update every 5 min
  ;; Personal details
  (user-mail-address "dimitri.lozeve@sysnav.fr")
  (user-full-name "Dimitri Lozeve")
  ;; Behaviour
  (message-kill-buffer-on-exit t)
  (mu4e-confirm-quit nil)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-sent-messages-behavior 'delete)  ;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
  (mail-user-agent 'mu4e-user-agent)  ;; default program for sending mail in Emacs
  ;; View and compose
  (mu4e-use-fancy-chars nil)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-headers-show-threads t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-format-flowed t)
  :config
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
	starttls-use-gnutls t
	smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	smtpmail-auth-credentials '(("smtp.gmail.com" 587 "dimitri.lozeve@sysnav.fr" nil))
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587))

(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (global-set-key (kbd "C-x RET RET") 'vterm)
  (define-key vterm-mode-map (kbd "<C-backspace>")
    (lambda () (interactive) (vterm-send-key (kbd "C-w")))))

(defun my-insert-char ()
  "Search for a unicode character and insert it."
  (interactive)
  (with-temp-buffer
    (call-interactively 'insert-char)
    (kill-ring-save (point-min) (point-max))))


;; configuration file for secrets (API keys, etc)
(setq secrets-file (expand-file-name "secrets.el" user-emacs-directory))

(when (file-exists-p secrets-file)
  (load secrets-file))


;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;(setq initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n")
;;(setq initial-scratch-message ";;                 ____ \\__ \\ \n;;                 \\__ \\__/ / __\n;;                 __/ ____ \\ \\ \\    ____\n;;                / __ \\__ \\ \\/ / __ \\__ \\ \n;;           ____ \\ \\ \\__/ / __ \\/ / __/ / __\n;;      ____ \\__ \\ \\/ ____ \\/ / __/ / __ \\ \\ \\ \n;;      \\__ \\__/ / __ \\__ \\__/ / __ \\ \\ \\ \\/\n;;      __/ ____ \\ \\ \\__/ ____ \\ \\ \\ \\/ / __\n;;     / __ \\__ \\ \\/ ____ \\__ \\ \\/ / __ \\/ /\n;;     \\ \\ \\__/ / __ \\__ \\__/ / __ \\ \\ \\__/\n;;      \\/ ____ \\/ / __/ ____ \\ \\ \\ \\/ ____\n;;         \\__ \\__/ / __ \\__ \\ \\/ / __ \\__ \\ \n;;         __/ ____ \\ \\ \\__/ / __ \\/ / __/ / __\n;;        / __ \\__ \\ \\/ ____ \\/ / __/ / __ \\/ /\n;;        \\/ / __/ / __ \\__ \\__/ / __ \\/ / __/\n;;        __/ / __ \\ \\ \\__/ ____ \\ \\ \\__/ / __\n;;       / __ \\ \\ \\ \\/ ____ \\__ \\ \\/ ____ \\/ /\n;;       \\ \\ \\ \\/ / __ \\__ \\__/ / __ \\__ \\__/\n;;        \\/ / __ \\/ / __/ ____ \\ \\ \\__/\n;;           \\ \\ \\__/ / __ \\__ \\ \\/\n;;            \\/      \\ \\ \\__/ / __\n;;                     \\/ ____ \\/ /\n;;                        \\__ \\__/\n;;                        __/\n\n\n")
;;(setq initial-scratch-message ";;            ____\n;;           /.../\\\n;;          /.../--\\ \n;;         /.../----\\ \n;;        /.../------\\\n;;       /.../---/\\---\\ \n;;      /.../---/\\\\\\---\\\n;;     /.../---/\\\\\\\\\\---\\\n;;    /.../===/__\\\\\\\\\\---\\\n;;   /............\\\\\\\\\\---\\\n;;  /..............\\\\\\\\\\---\\\n;;  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\--/\n;;   \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\/\n\n\n")


;;; init.el ends here
