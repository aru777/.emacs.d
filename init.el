;; network proxy if any
(setq proxy-file (expand-file-name "proxy.el" user-emacs-directory))
(when (file-exists-p proxy-file)
  (load proxy-file))

;; package related stuff
(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA"        . 10)
        ("ORG"          . 7)
        ("GNU ELPA"     . 5)
        ("MELPA Stable" . 0)))
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

(setq org-config (expand-file-name "config.org" user-emacs-directory))
(when (file-exists-p org-config)
  (org-babel-load-file org-config))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "Inconsolata for Powerline")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (diminish smex evil-surround auto-yasnippet keyfreq rainbow-delimiters projectile company-irony counsel swiper ivy company evil org-bullets yasnippet-snippets yasnippet spaceline solarized-theme ace-window thrift super-save which-key use-package))))
