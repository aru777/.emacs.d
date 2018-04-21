;; network proxy if any
(setq proxy-file (expand-file-name "proxy.el" user-emacs-directory))
(when (file-exists-p proxy-file)
  (load proxy-file))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elisp-format projectile company super-save counsel swiper ivy use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; highlight current line
(global-hl-line-mode +1)

;; disable emacs autosave mode because of super-save
(setq auto-save-default nil)

;; keep windows balanced all the time
(defadvice split-window-below (after restore-balanace-below activate)
  (balance-windows))
(defadvice split-window-right (after restore-balance-right activate)
  (balance-windows))
(defadvice delete-window (after restore-balance activate)
  (balance-windows))

;; package related stuff
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

;; TODO: read other features of counsel
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package projectile
  :ensure t
  :bind ("s-p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode +1))
