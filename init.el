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

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

(global-set-key (kbd "M-z") #'zap-up-to-char)

(set-frame-font "Iosevka:pixelsize=18" nil t)
(add-to-list 'default-frame-alist '(font .  "Iosevka:pixelsize=18"))
(set-face-attribute 'default t :font "Iosevka:pixelsize=18")
(set-face-attribute 'default nil :font  "Iosevka:pixelsize=18")

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
  (global-hl-line-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Theme
(use-package "monokai-theme"
  :ensure t
  :config
  (load-theme 'monokai t))

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

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (global-set-key (kbd "<f8>") 'ispell-word)
  (global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
  (global-set-key (kbd "M-<f8>") 'flyspell-buffer)
  (global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word))

(use-package langtool
  :ensure t
  :init
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*"))

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

  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    ;; (flycheck-add-next-checker 'python-pylint '(warning . python-mypy) t)
    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    (add-to-list 'flycheck-disabled-checkers 'python-pylint)))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode lisp-mode lisp-interaction-mode scheme-mode slime-repl-mode) . enable-paredit-mode))

(use-package slime
  :ensure t
  :config
  (require 'slime-autoloads)
  (add-to-list 'slime-contribs 'slime-fancy)
  (setq slime-lisp-implementations
	'((sbcl ("/usr/bin/sbcl")))))

(use-package gnu-apl-mode
  :ensure t
  :config
  (defun em-gnu-apl-init ()
    (setq buffer-face-mode-face 'gnu-apl-default)
    (buffer-face-mode))
  (add-hook 'gnu-apl-interactive-mode-hook 'em-gnu-apl-init)
  (add-hook 'gnu-apl-mode-hook 'em-gnu-apl-init))

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
	 (("z" "Fortnight and TODOs"
	   ((agenda ""
		    ((org-agenda-span 14)
		     (org-agenda-start-on-weekday 1)
		     (org-agenda-prefix-format " → %t%s ")
		     (org-agenda-repeating-timestamp-show-all t)))
	    (alltodo ""
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
	 (("w" "Work" entry
	   (file+olp "~/notes/planner.org" "Tasks" "Work")
	   "*** TODO %?" :prepend t)
	  ("e" "Event" entry
	   (file "~/Nextcloud/org/gcal.org")
	   "*** %?
%^t" :prepend t)
	  ("t" "Task" entry
	   (file+olp "~/notes/planner.org" "Tasks" "Misc")
	   "*** TODO %?" :prepend t)
	  ("j" "Journal Entry" entry
	   (file+datetree "~/notes/journal.org")
	   "* %T %^{Header: |Log|Note|Idea}
  %?%i" :empty_lines 1)
	  ("p" "Project" entry
	   (file+olp "~/notes/planner.org" "Projects")
	   "" :prepend t)
	  ("n" "Note" entry
	   (file+olp "~/notes/planner.org" "Notes")
	   "** %?" :prepend t))))
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
     (forth . t)
     (haskell . t)
     (java . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (scheme . t)
     (shell . t)
     (clojure . t)))

  (setq org-latex-pdf-process
	'("latexmk -shell-escape -bibtex -pdf %f")))

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


;; email configuration file
(setq email-file (expand-file-name "email.el" user-emacs-directory))

(when (file-exists-p email-file)
  (load email-file))


;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;(setq initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n")
;;(setq initial-scratch-message ";;                 ____ \\__ \\ \n;;                 \\__ \\__/ / __\n;;                 __/ ____ \\ \\ \\    ____\n;;                / __ \\__ \\ \\/ / __ \\__ \\ \n;;           ____ \\ \\ \\__/ / __ \\/ / __/ / __\n;;      ____ \\__ \\ \\/ ____ \\/ / __/ / __ \\ \\ \\ \n;;      \\__ \\__/ / __ \\__ \\__/ / __ \\ \\ \\ \\/\n;;      __/ ____ \\ \\ \\__/ ____ \\ \\ \\ \\/ / __\n;;     / __ \\__ \\ \\/ ____ \\__ \\ \\/ / __ \\/ /\n;;     \\ \\ \\__/ / __ \\__ \\__/ / __ \\ \\ \\__/\n;;      \\/ ____ \\/ / __/ ____ \\ \\ \\ \\/ ____\n;;         \\__ \\__/ / __ \\__ \\ \\/ / __ \\__ \\ \n;;         __/ ____ \\ \\ \\__/ / __ \\/ / __/ / __\n;;        / __ \\__ \\ \\/ ____ \\/ / __/ / __ \\/ /\n;;        \\/ / __/ / __ \\__ \\__/ / __ \\/ / __/\n;;        __/ / __ \\ \\ \\__/ ____ \\ \\ \\__/ / __\n;;       / __ \\ \\ \\ \\/ ____ \\__ \\ \\/ ____ \\/ /\n;;       \\ \\ \\ \\/ / __ \\__ \\__/ / __ \\__ \\__/\n;;        \\/ / __ \\/ / __/ ____ \\ \\ \\__/\n;;           \\ \\ \\__/ / __ \\__ \\ \\/\n;;            \\/      \\ \\ \\__/ / __\n;;                     \\/ ____ \\/ /\n;;                        \\__ \\__/\n;;                        __/\n\n\n")
;;(setq initial-scratch-message ";;            ____\n;;           /.../\\\n;;          /.../--\\ \n;;         /.../----\\ \n;;        /.../------\\\n;;       /.../---/\\---\\ \n;;      /.../---/\\\\\\---\\\n;;     /.../---/\\\\\\\\\\---\\\n;;    /.../===/__\\\\\\\\\\---\\\n;;   /............\\\\\\\\\\---\\\n;;  /..............\\\\\\\\\\---\\\n;;  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\--/\n;;   \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\/\n\n\n")


;;; init.el ends here

