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
  (term "/usr/bin/zsh")
  )

(defun shell-this-window ()
  (interactive)
  (term "/usr/bin/zsh")
  )

(defun shell-hori ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (term "/usr/bin/zsh")
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
      "c" #'kill-buffer-and-window))

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


(setq lsp-terraform-server "/usr/bin/terraform-ls")
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; use flycheck instead of flymake
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-hook nil)
  :config
  (lsp-register-client
  (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/bin/terraform-ls" "serve"))
		    :major-modes '(terraform-mode)
		    :server-id 'terraform-lsp))
  :hook ((java-mode python-mode go-mode rust-mode
		    js2-mode typescript-mode web-mode
		    c-mode c++-mode objc-mode terraform-mode) . lsp-deferred)
  :hook ((lsp-mode . lsp-ui-mode))
  :config
  (defun lsp-update-server ()
    "update lsp server."
    (interactive)
    (lsp-install-server t)))

;;(setq flycheck-tflint-variable-files (list "data.tf" "variables.tf"))
(global-visual-line-mode t)


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

(company-fuzzy-mode 1)


;;(setq flycheck-check-syntax-automatically t)

(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#111"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)

;;'(highlight-numbers-number ((t (:foreground "#f0ad6d"))))
(require 'which-key)
(setq which-key-idle-delay 0.1)

(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

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
