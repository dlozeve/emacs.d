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

(let ((my-font "Iosevka Term")
      (my-height 120))
  (set-face-attribute 'default nil :family my-font :height my-height)
  (set-face-attribute 'fixed-pitch nil :family my-font :height my-height))

(set-face-attribute 'variable-pitch nil :family "Linux Libertine O" :height 140)

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
(setq straight-repository-branch "develop")
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

;; Better than default: act on the region if active
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)
(bind-key [remap count-words-region] #'count-words)

(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key [remap zap-to-char] #'zap-up-to-char)
(bind-key [remap buffer-menu] #'ibuffer)

;;; Built-in packages
(use-package paren
  :config
  (show-paren-mode 1))

;; Highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode -1))

;; Better renaming rules for buffers with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Theme
;; (use-package base16-theme
;;   :straight t
;;   :init
;;   (setq base16-theme-256-color-source 'base16-shell)
;;   :config
;;   (load-theme 'base16-default-dark t))

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package mood-line
  :straight t
  :config
  (mood-line-mode t))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
    (exec-path-from-shell-copy-env "PYENV_ROOT")
    (exec-path-from-shell-copy-env "PYENV_ROOT")
    (exec-path-from-shell-copy-env "GAMBIT")
    (exec-path-from-shell-copy-env "GERBIL_HOME")))

(use-package vertico
  :straight t
  :init (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

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

(use-package deadgrep
  :straight t
  :bind (("<f5>" . deadgrep)))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)))

(use-package forge
  :straight t
  :after magit)

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

(use-package flycheck
  :straight t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package projectile
  :straight t
  :init
  (setq projectile-completion-system 'default)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-tags-command "ctags-universal -Re")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package yaml-mode
  :straight t)

(use-package json-mode
  :straight t)

(use-package cmake-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

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
  :straight t
  :init
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*"))

(use-package lsp-mode
  :commands lsp
  :straight t
  :hook ((rust-mode . lsp)
	 (c-mode . lsp)
	 (python-mode . lsp))
  :config
  (setq lsp-clients-clangd-args
	'("-j=2"
	  "--background-index"
	  "--clang-tidy"
	  "--completion-style=bundled"
	  "--pch-storage=memory"
	  "--header-insertion=never"
	  "--header-insertion-decorators=0"
	  "--suggest-missing-includes"))
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  (setq lsp-completion-provider :capf)
  (setq lsp-file-watch-threshold nil)
  (setq lsp-zig-zls-executable "zls"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :straight t
  :config
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'top))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp) ; or lsp-deferred
			 (local-unset-key (kbd "C-f")))))

(use-package pyvenv
  :straight t)

(use-package julia-mode
  :straight t)

(use-package julia-repl
  :straight t
  :hook (julia-mode . julia-repl-mode)
  :init
  (setenv "JULIA_NUM_THREADS" "6")
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

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

(use-package racket-mode
  :straight t)

(use-package geiser-chicken
  :straight t
  :custom (geiser-chicken-binary "chicken-csi"))

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
  (setf scheme-program-name "gxi")

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (visit-tags-table (concat gerbil "/src/TAGS")))

(use-package zig-mode
  :straight t
  :config
  (setq zig-format-on-save nil))

(use-package matlab
  :straight matlab-mode
  :config
  ;; This is a simple script to set the required environment variables
  ;; before launching Matlab in Emacs. This prevents an issue where
  ;; all plot windows are blank. See Arch wiki for more details.

  ;; #!/usr/bin/env bash
  ;; export _JAVA_AWT_WM_NONREPARENTING=1
  ;; /usr/local/bin/matlab "$@"
  (setq matlab-shell-command "~/.local/bin/run_matlab")
  (setq matlab-shell-command-switches '("-nodesktop"))
  ;; :custom-face
  ;; (linemark-stop-face ((t (:background 'unspecified)
  ;; 			  (:underline '(:color "red3" :style wave)))))
  ;; (linemark-caution-face ((t (:background 'unspecified)
  ;; 			     (:underline '(:color "yellow4" :style wave)))))
  ;; (linemark-go-face ((t (:background 'unspecified)
  ;; 			(:underline '(:color "green4" :style wave)))))
  ;; (linemark-funny-face ((t (:background 'unspecified)
  ;; 			   (:underline '(:color "blue3" :style wave)))))
  )

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
  :custom (bqn-key-prefix ?ù)
  :config
  (defface bqn-default
    '((t (:height 140 :family "BQN386 Unicode"))) "BQN default face.")

  (defun bqn-init ()
    (setq buffer-face-mode-face 'bqn-default)
    (buffer-face-mode))

  (add-hook 'bqn-mode-hook 'bqn-init)

  (setq bqn-interpreter-path "~/build/CBQN/BQN")
  (setq bqn-keyboard-map
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
  (add-hook 'bqn-keymap-mode-hook 'bqn-init))

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

(use-package format-all
  :straight t
  :bind ("C-c C-f" . format-all-buffer))

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

(use-package pdf-tools
  :straight t
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package mathpix.el
  :straight (:host github :repo "jethrokuan/mathpix.el")
  ;; You also need to configure `mathpix-app-id` and
  ;; `mathpix-app-key`, for instance in secrets.el
  :bind
  ("C-x m" . mathpix-screenshot))

;; Org-mode
;; Pour accéder rapidement à l'organisation
(defun gtd ()
  "Find the planner file."
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
  (setq org-cycle-separator-lines 1)
  ;; Refile to paths in the file (level1/level2/level3)
  (setq org-refile-use-outline-path t)

  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/notes")

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
			(org-agenda-sorting-strategy '(tag-up priority-down))))
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
     (plantuml . t)
     (shell . t)))

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation nil
	org-edit-src-content-indentation 0)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

(use-package ox-latex
  :after (org)
  :config
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
  (setq org-latex-default-class "koma-article"))

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

(use-package ox-md
  :after (org))

(use-package ob-plantuml
  :after (org)
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/build/plantuml.jar")))

(use-package org-fragtog
  :straight t
  :after (org)
  :hook (org-mode . org-fragtog-mode))

(use-package citeproc
  :straight t)

(use-package org-ref
  :straight (:host github :repo "jkitchin/org-ref" :files ("*.el" "*.org" "citeproc" :defaults))
  :after (org citeproc)
  :config
  (setq bibtex-completion-bibliography '("~/notes/bibliography/bibliography.bib")
	bibtex-completion-library-path "~/notes/bibliography/files/"
	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1} ${year:4} ${author:36} ${title:*}")))
  (setq reftex-default-bibliography '("~/notes/bibliography/bibliography.bib"))

  (require 'bibtex)
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator ""
	bibtex-autokey-year-title-separator "_"
	bibtex-autokey-titleword-separator "_"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)

  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)

  (require 'org-ref)

  (require 'doi-utils)
  (require 'org-ref-isbn)
  (require 'org-ref-arxiv)
  (require 'org-ref-isbn)

  (defun dl/formatted-citation-at-point ()
    "Kill the formatted citation for the reference at point using Pandoc."
    (interactive)
    (let* ((bibfile (expand-file-name (car (org-ref-find-bibliography))))
	   (cslfile (concat (file-name-directory bibfile) "chicago-author-date.csl")))
      (kill-new
       (shell-command-to-string
	(format
	 "echo cite:%s | pandoc --citeproc --bibliography=%s --csl=%s -f org -t markdown_strict | tail -n +3 | tr '\n' ' '"
	 (org-ref-get-bibtex-key-under-cursor)
	 bibfile
	 cslfile)))))

  (defun dl/formatted-citation-from-doi (doi)
    "Kill the formatted citation for the given DOI."
    (interactive
     (list (read-string
            "DOI: "
            ;; now set initial input
            (doi-utils-maybe-doi-from-region-or-current-kill))))
    (let ((url-mime-accept-string "text/x-bibliography; style=chicago-author-date"))
      (with-temp-buffer
	(url-insert-file-contents (format "https://doi.org/%s" doi))
	(kill-new (buffer-string))))))

(use-package org-roam
  :straight t
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
")))))

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

(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4)
  (setq graphviz-dot-preview-extension "svg"))

(use-package company-graphviz-dot)

(use-package hledger-mode
  :straight t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :init
  (setq hledger-jfile (expand-file-name "~/.hledger.journal"))
  :config
  (add-to-list 'company-backends 'hledger-company)
  (global-set-key (kbd "C-c e") 'hledger-jentry)
  (global-set-key (kbd "C-c j") 'hledger-run-command))

(use-package mu4e
  :load-path "~/build/mu-1.4.13/mu4e"
  :bind ("C-c m" . mu4e)
  :custom
  ;; Folders
  ;; (mu4e-sent-folder   "/sent")    ;; folder for sent messages
  ;; (mu4e-drafts-folder "/drafts")  ;; unfinished messages
  ;; (mu4e-trash-folder  "/trash")   ;; trashed messages
  ;; (mu4e-refile-folder "/archive") ;; saved messages
  ;; Sync
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300) ;; update every 5 min
  ;; Personal details
  ;; (user-mail-address "dimitri@lozeve.com")
  ;; (user-full-name "Dimitri Lozeve")
  ;; Behaviour
  (message-kill-buffer-on-exit t)
  (mu4e-confirm-quit nil)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-sent-messages-behavior 'delete)	;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
  (mail-user-agent 'mu4e-user-agent) ;; default program for sending mail in Emacs
  ;; View and compose
  (mu4e-use-fancy-chars nil)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  (mu4e-headers-show-threads t)
  (mu4e-compose-dont-reply-to-self t)
  ;; enable format=flowed
  ;; - mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
  ;; - each paragraph is a single long line; at sending, emacs will add the
  ;;   special line continuation characters.
  ;; - also see visual-line-fringe-indicators setting below
  (mu4e-compose-format-flowed t)
  ;; because it looks like email clients are basically ignoring format=flowed,
  ;; let's complicate their lives too. send format=flowed with looong lines. :)
  ;; https://www.ietf.org/rfc/rfc2822.txt
  (fill-flowed-encode-column 998)
  ;; in mu4e with format=flowed, this gives me feedback where the soft-wraps are
  (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  :config
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-context-policy 'pick-first)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode))

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
