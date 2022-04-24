

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(define-key evil-motion-state-map "j" 'evil-backward-char)
(define-key evil-motion-state-map ";" 'evil-forward-char)
(define-key evil-motion-state-map "k" 'evil-next-line)
(define-key evil-motion-state-map "l" 'evil-previous-line)

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
      :desc "tab forward"
      "l" #'centaur-tabs-forward
      :desc "tab backwards"
      "k" #'centaur-tabs-backward
      )

(global-set-key (kbd "M-;") 'comment-line)

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
(centaur-tabs-mode t)
