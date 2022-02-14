;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dylan Ritchings"
      user-mail-address "DylanRitchings1998@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(defun shell-vert ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (vterm "/usr/bin/zsh")
  )

(defun shell-this-window ()
  (interactive)
  (vterm "/usr/bin/zsh")
  )

(defun shell-hori ()
  (interactive)
  (split-window-below 55)
  (other-window 1)
  (vterm "/usr/bin/zsh")
  )


(map! :leader
      (:prefix ("z" . "Shell")
      :desc "Shell this window"
      "t" #'shell-this-window
      :desc "Verticle shell"
      "v" #'shell-vert
      :desc "Horizontal shell"
      "h" #'shell-hori
      :desc "Close shell"
      "d" #'kill-buffer-and-window))

;; New file new window stuff

(setq evil-vsplit-window-right t
      evil-split-window-below t)


;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (consult-buffer))

(map! :leader
      ;; "SPC" #'rotate-layout

      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right
      "M-<right>" #'evil-window-vsplit
      "M-<down>" #'evil-window-split)

(map! :leader
      :desc "Pull"
      "gp" #'magit-pull)


;; XREF JUMP

(defun my*projectile-find-tag (orig-fn)
  (let ((xref-backend-functions '(etags--xref-backend t)))
    (funcall orig-fn)))
(advice-add #'projectile-find-tag :around #'my*projectile-find-tag)

(setq xref-prompt-for-identifier nil)


;; ENTER IN EVIL STATE

(defun evil-ret ()
  (interactive)
  (+default/newline-below)
  (evil-normal-state)
)

;; ;; BACK IN EVIL STATE
;; (defun evil-backward-char ()
;;   (interactive)
;;   (backward-delete-char-untabify 1)
;; )




;; BACKWARD KILL WORD FIX
(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword          ;; when backword contains space
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))

(global-set-key  [C-backspace]
            'aborn/backward-kill-word)



;; ALT DELETE + BACK

(global-set-key (kbd "M-<backspace>") (lambda ()
				       (interactive)
				       (kill-line 0)))
(global-set-key (kbd "M-DEL") 'kill-line)

;; FIX CURSOR TODO: FIX
 (define-advice evil-ex-hl-match-hook (:around (fn hl) add-priority-reset-hook)
    (or (funcall fn hl)
        (lambda (_ ov)
          (overlay-put ov 'priority 0))))



;; CENTAUR TABS

(setq centaur-tabs-style "bar"
      centaur-tabs-headline-match t
      centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-set-modified-marker t
      centaur-tabs-modifier-marker "~"
      centaur-tabs-gray-out-icons t)
(centaur-tabs-mode t)

(add-hook! 'vterm-mode-hook 'centaur-tabs-local-mode)
(add-hook! 'comint-mode-hook 'centaur-tabs-local-mode)
(add-hook! 'special-mode-hook 'centaur-tabs-local-mode)


;; XREF and CENTAUR
(map! :leader
      :desc "tab forward"
      "l" #'centaur-tabs-forward
      :desc "tab backwards"
      "k" #'centaur-tabs-backward
      )
(map! :leader
      (:prefix ("c")
      :mode lsp-ui-mode-map
      :desc "xref back"
      "b" #'xref-go-back
))

(map! :leader
      (:prefix ("q")
      :desc "Load saved session"
      "l" #'doom/load-session "/.doom.d/sessions/"
      )
      (:prefix ("q")
      :desc "Save session"
      "s" #'doom/save-session "/.doom.d/sessions/"
))

;; (setq lsp-terraform-server "/usr/terraform-lsp")
;; (use-package lsp-mode
;;   :defer t
;;   :commands lsp
;;   :custom
;;   (lsp-auto-guess-root t)
;;   (lsp-prefer-flymake nil) ; use flycheck instead of flymake
;;   ;; (lsp-enable-file-watchers nil)
;;   (lsp-enable-folding nil)
;;   (read-process-output-max (* 1024 1024))
;;   ;; (lsp-keep-workspace-alive nil)
;;   ;; (lsp-eldoc-hook nil)
;; ;;/usr/bin/terraform-ls
;;   :config
;;   (lsp-register-client
;;   (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/bin/terraform-ls" "serve"))
;; 		    :major-modes '(terraform-mode)
;; 		    :server-id 'terraform-lsp))
;;   :hook ((java-mode python-mode go-mode rust-mode
;; 		    js2-mode typescript-mode web-mode
;; 		    c-mode c++-mode objc-mode terraform-mode shell-mode) . lsp-deferred)
;;   :hook ((lsp-mode . lsp-ui-mode))
;;   :config
;;   (defun lsp-update-server ()
;;     "update lsp server."
;;     (interactive)
;;     (lsp-install-server t)))

(global-visual-line-mode t)
(after! lsp-ui
(setq lsp-ui-sideline t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-position "top")
(setq lsp-ui-flycheck-enable t)
(setq lsp-ui-sideline-show-flycheck t)
)



;; (after! flycheck
;;   (setq flycheck-tflint-variable-files (list variables.tf data.tf))
;;   )


;; COMPANY

(setq company-backends
    '((company-files :with company-yasnippet company-terraform company-tabnine)
      (company-capf :with company-yasnippet company-terraform company-tabnine)
      (company-dabbrev-code company-gtags company-etags company-keywords :with company-yasnippet company-terraform company-tabnine)
      (company-dabbrev :with company-yasnippet company-terraform company-tabnine)))
(company-quickhelp-mode)
(setq global-company-mode t)


;; COMPANY FUZZY

(defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn))
    (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around))
(setq company-fuzzy-sorting-backend 'flx)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(map! :n "<tab>" 'company-capf)
(require 'company-box)
(add-hook! 'company-mode-hook 'company-box-mode)


(setq global-company-fuzzy-mode t)
;; (add-hook! 'company-mode 'company-fuzzy-mode)

;; (after! company-mode
;;   (add-hook! 'prog-mode-hook 'company-fuzzy-mode)
;;   ;; (setq company-backends (company-fuzzy-all-other-backends))
;;   (company-fuzzy-mode)
;;   )
;; (advice-add 'company-mode :after 'company-fuzzy-mode 1)
;; (advice-add 'open-file :after 'company-fuzzy-mode 1)
(after! doom-company
  (setq company-fuzzy-mode 1)
  (add-hook! 'find-file-hook 'company-fuzzy-mode)
)
;; (after! +company-init-backends-h
;;     (company-fuzzy-mode)
;;   )
;;(setq flycheck-check-syntax-automatically t)

(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#111"))))))
  (buffer-face-set 'default))
(add-hook! 'buffer-list-update-hook 'highlight-selected-window)

;;'(highlight-numbers-number ((t (:foreground "#f0ad6d"))))
(require 'which-key)
(setq which-key-idle-delay 0.1)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(map! :map evil-motion-state-map "j" 'evil-backward-char)
(map! :map evil-motion-state-map "k" 'evil-next-line)
(map! :map evil-motion-state-map "l" 'evil-previous-line)
(map!  :map evil-motion-state-map ";" 'evil-forward-char)


;; RANGER Bind
(map! :after ranger
      :map ranger-mode-map
      "j" 'ranger-up-directory
      ";" 'ranger-find-file
      "k" 'ranger-next-file
      "l" 'ranger-prev-file)

(map! :after magit
      :map magit-mode-map
      "j" 'magit-dispatch
      ";" 'magit-log
      "k" 'evil-next-visual-line
      "l" 'evil-previous-visual-line)

;; CUSTOM COLOURS
(custom-set-faces!
  '(line-number :foreground "#b8b8b8")
  '(line-number-current-line :foreground "#9c78e3")
  '(font-lock-comment-face :foreground "SlateBlue1")
  )


;; Solaire mode
(solaire-global-mode -1)


(setq doom-modeline-vcs-max-length 50)

;;close terminal
(defadvice flymake-start-syntax-check-process (after
                                               cheeso-advice-flymake-start-syntax-check-1
                                               (cmd args dir)
                                               activate compile)
  ;; set flag to allow exit without query on any
  ;;active flymake processes
  (set-process-query-on-exit-flag ad-return-value nil))


(global-set-key (kbd "<XF86Paste>") 'evil-paste-after)
(global-set-key (kbd "<XF86Copy>") 'evil-yank)
(global-set-key (kbd "M-;") 'comment-line)
(setq confirm-kill-emacs nil)




;; (flycheck-define-checker tflint
;;   "Terraform linter tflint"
;;   :command ("tflint"
;;             source-inplace)
;;   :error-parser flycheck-parse-checkstyle
;;   :error-filter flycheck-dequalify-error-ids
;;   :modes (terraform-mode))

;; (add-to-list 'flycheck-checkers 'tflint)


;; (defun flycheck-compile-tflint ()
;;   (interactive)
;;   (flycheck-compile 'tflint))

;; (setq-default uniquify-buffer-name-style 'forward)
;; (setq-default uniquify-strip-common-suffix t)
;; (setq uniquify-after-kill-buffer-p t)
;; (after! evil-surround
;;   (let ((pairs '((?g "$" . "$")
;;                  (?h "(" . ")")
;;                  (?j "[" . "]")
;;                  (?k "{" . "}")
;;                  (?l "<" . ">")
;;                  (?ø "'" . "'")
;;                  (?æ "\"" . "\""))))
;;     (prependq! evil-surround-pairs-alist pairs)
;;     (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))
;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(add-hook! 'lsp-mode-hook 'lsp-ui-mode)
(add-hook! 'python-mode-hook 'lsp!)
;; (add-hook! 'prog-mode-hook 'lsp!)
(add-hook! 'terraform-mode-hook 'lsp!)
(setq lsp-prefer-flymake nil)
(setq lsp-ui-flycheck-enable nil)
(setq lsp-ui-sideline-show-flycheck t)
(setq flycheck-terraform-tflint-executable "/usr/local/bin/tflint")
(setq evil-kill-on-visual-paste nil)

;; Show pre commit buffer
(add-hook! 'server-switch-hook 'magit-commit-diff)

;;LSP START
(defun dotfiles--lsp-deferred-if-supported ()
  "Run `lsp-deferred' if it's a supported mode."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (lsp-deferred)))

(add-hook! 'prog-mode-hook 'dotfiles--lsp-deferred-if-supported)

;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))



;; RUN IN VTERM COMMAND TODO make stay open, make open in other window, make git workflow
(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

;;END RUN IN VTERM

(defun open-in-other-window (func)
  (interactive)
  (evil-window-split)
  (funcall func)
)

(defun pre-commit()
  (interactive)
  (run-in-vterm "pre-commit run -a")
    )

(defun cleantf()
  (interactive)
  (run-in-vterm "cleantf"))

(defun commit-with-pre()
  (interactive)
  (magit-stage-modified)
  (split-window-below 55)
  (other-window 1)
  (pre-commit)
  ;; (split-window-vertically)
  (magit-commit-create)
  )


(map! :leader
      "g" nil)

;; (λ! (open-in-other-window 'commit)
(map! :leader
      (:prefix ("g" . "git")
       "p" 'magit-pull
       "P" 'magit-push
       "b" 'magit-checkout
       :desc "Commit"
       "c" 'commit-with-pre
       "B" 'magit-blame
       "j" (λ! (open-in-other-window 'pre-commit))
       ;; "d" 'open-in-other-window(magit-diff)
       ;; "C" 'magit-clone--format-url
       ;; "l" 'open-in-other-window(magit-log-all)
       ;; "s" 'open-in-other-window(magit-status)
       ))

(map! :mode vterm-mode-hook
      :after evil-normal-state
      :nv "q" #'kill-buffer-and-window)


;; (rainbow-delimiters-mode-enable)

(after! prog-mode
  (add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)
)
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))
