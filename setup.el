

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(define-key evil-motion-state-map "j" 'evil-backward-char)
(define-key evil-motion-state-map ";" 'evil-forward-char)
(define-key evil-motion-state-map "k" 'evil-next-line)
(define-key evil-motion-state-map "l" 'evil-previous-line)

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

(map! :leader
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

(global-set-key (kbd "<XF86Paste>") 'evil-paste-after)
(global-set-key (kbd "<XF86Copy>") 'evil-yank)
(setq evil-kill-on-visual-paste nil)

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
      "d" #'kill-buffer-and-window)
      )

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

(global-set-key (kbd "M-<backspace>") (lambda ()
				       (interactive)
				       (kill-line 0)))
(global-set-key (kbd "M-DEL") 'kill-line)

(map! :leader
      (:prefix ("c")
      :mode lsp-ui-mode-map
      :desc "xref back"
      "b" #'xref-go-back
))

(global-set-key (kbd "M-;") 'comment-line)

(after! save-buffer
  (set-buffer-file-coding-system unix))

(setq confirm-kill-emacs nil)

(setq doom-modeline-vcs-max-length 50)

(require 'which-key)
(setq which-key-idle-delay 0.1)
(which-key-mode)

(setq centaur-tabs-style "bar"
      centaur-tabs-headline-match t
      centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-set-modified-marker t
      centaur-tabs-modifier-marker "~"
      centaur-tabs-gray-out-icons t)
(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project))
(centaur-tabs-mode t)

(map! :leader
      :desc "tab forward"
      "l" #'centaur-tabs-forward
      :desc "tab backwards"
      "k" #'centaur-tabs-backward
      :desc "buffer-forward"
      ";" #'next-buffer
      :desc "buffer-backwards"
      "j" #'previous-buffer
      )

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

(defun dotfiles--lsp-deferred-if-supported ()
  "Run `lsp-deferred' if it's a supported mode."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (lsp-deferred)))

(add-hook! 'prog-mode-hook 'dotfiles--lsp-deferred-if-supported)
(add-hook! 'terraform-mode 'lsp-mode)
(add-hook! 'python-mode 'lsp-mode)

(setq company-backends
    '(
      ;; (company-files :with company-yasnippet company-terraform company-tabnine)
      (company-capf :with company-yasnippet  company-tabnine)
;;       (company-dabbrev-code company-gtags company-etags company-keywords :with company-yasnippet company-terraform company-tabnine)
;;       (company-dabbrev :with company-yasnippet company-terraform company-tabnine)
      ))
(company-quickhelp-mode)
;; (setq global-company-mode t)
(add-hook! 'lsp-managed-mode-hook (lambda () (setq-local company-backends )))
(company-terraform-init)
;; (add-hook! 'terraform-mode (lambda () (setq-local company-backends '((company-capf :with company-terraform)))))
(add-hook! 'after-init-hook 'company-flx-mode)
(add-hook! 'after-init-hook 'global-company-mode)

;; (defun jcs--company-complete-selection--advice-around (fn)
;;     "Advice execute around `company-complete-selection' command."
;;     (let ((company-dabbrev-downcase t))
;;       (call-interactively fn))
;;     (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around))

;; (after! terraform-mode
;; (setq company-fuzzy-sorting-backend 'flx)

;; (setq company-minimum-prefix-length 1
;;       company-idle-delay 0.0) ;; default is 0.2

;; (map! :n "<tab>" 'company-capf)



;; ;; (company-fuzzy-mode 1)

;; ;; (after! doom-company
;; ;;   (setq company-fuzzy-mode 1)
;; ;;   (add-hook! 'find-file-hook 'company-fuzzy-mode)
;; ;; )
;; (after! company
;;   (setq company-fuzzy-mode 1)
;;   (add-hook! 'find-file-hook 'company-fuzzy-mode)
;; )
;; )
;; ;; (add-hook! 'terraform-mode-hook 'company-fuzzy-mode)

;; ;; (setq global-company-mode t)

;; ;; (after! terraform-mode
;; ;;   (company-fuzzy-mode))
;; ;; (company-fuzzy-mode)
;; ;; (company-fuzzy-mode)



(add-hook! scala-mode-hook dap-mode)
(add-hook! scala-mode-hook dap-ui-mode)

(defun vterm-sbt ()
  (interactive)
  (split-window-below 55)
  (other-window 1)
  (vterm "/usr/bin/zsh" "sbt")
  )

(map! :leader
      (:prefix ("z" . "Shell")
      :desc "Run Scala"
      "s" #'vterm-sbt))

;; (after! terraform-mode

;;                 )

(map! :leader
      (:prefix ("v" . "Bookmark")
      :mode lsp-ui-mode-map
      :desc "toggle bookmark"
      "b" #'bm-toggle
      :desc "bookmark prev"
      "k" #'bm-previous
      :desc "bookmark next"
      "l" #'bm-next
))

(add-hook! 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook! 'prog-mode-hook 'visual-line-mode)
(add-hook! 'prog-mode-hook 'popwin-mode)

;; (push "*format-all-errors*" popwin:special-display-config)
;; (push '(dired-mode :position top) popwin:special-display-config)
;; ;; vc
;; (push "*vc-diff*" popwin:special-display-config)
;; (push "*vc-change-log*" popwin:special-display-config)

(setq compilation-window-height 15)

;; ;; Helper for compilation. Close the compilation window if
;; ;; there was no error at all. (emacs wiki)
;; (defun compilation-exit-autoclose (status code msg)
;;   ;; If M-x compile exists with a 0
;;   (when (and (eq status 'exit) (zerop code))
;;     ;; then bury the *compilation* buffer, so that C-x b doesn't go there
;;     (bury-buffer)
;;     ;; and delete the *compilation* window
;;     (delete-window (get-buffer-window (get-buffer "*format-all-errors*"))))
;;   ;; Always return the anticipated result of compilation-exit-message-function
;;   (cons msg code))
;; ;; Specify my function (maybe I should have done a lambda function)
;; (setq compilation-exit-message-function 'compilation-exit-autoclose)

;; start-server
(add-hook! 'after-init-hook 'server-mode)


(map! :leader
      (:prefix ("w")
      :desc "Horizontal window"
      "h" #'split-window-below)
      )
