;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(after! lsp-mode
  (map! :leader
        (:prefix ("l" . "LSP")
         :desc "LSP execute code action" "a" #'lsp-execute-code-action
         :desc "LSP format buffer" "f" #'lsp-format-buffer
         :desc "LSP find declaration" "d" #'lsp-find-declaration
         :desc "LSP find definition" "D" #'lsp-find-definition
         :desc "LSP find implementation" "i" #'lsp-find-implementation
         :desc "LSP find references" "r" #'lsp-find-references
         :desc "LSP rename" "R" #'lsp-rename
         ;; Add more keybindings as needed
         )))







;; Use web-mode for LitElement templates in .js and .ts files
(use-package! web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.ts\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("jsx" . "\\.js\\'")
          ("tsx" . "\\.ts\\'")))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js\\'")
          ("tsx" . "\\.ts\\'")))
  (setq web-mode-enable-auto-quoting nil)  ;; Disable auto-quoting for LitElement's template literals
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-engine-detection t)
  (add-hook 'web-mode-hook 'hs-minor-mode)
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

;; Ensure proper syntax highlighting for LitElement's html and css tags
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (when (and buffer-file-name
             (string-match-p "\\.\\(js\\|ts\\)\\'" buffer-file-name))
    (setq web-mode-content-type "jsx")))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Optional: Enable lsp-mode for JavaScript and TypeScript
(use-package! lsp-mode
  :commands lsp
  :hook ((web-mode . lsp)
         (typescript-mode . lsp)
         (js2-mode . lsp))
  :config
  (setq lsp-completion-provider t))

;; Configure hideshow
(use-package! hideshow
  :config
  (setq hs-isearch-open t)
  ;; Add web-mode to hideshow
  (setq hs-special-modes-alist
        (append
         '((web-mode "<!--\\|<[^/>]*[^/]>" "-->" "<!--" nil))
         hs-special-modes-alist)))

;; Optional: Keybindings for code folding
(map! :map web-mode-map
      :localleader
      :desc "Toggle fold" "z" #'hs-toggle-hiding
      :desc "Fold all" "Z" #'hs-hide-all
      :desc "Show all" "S" #'hs-show-all)





;; Use lsp-mode for TypeScript and JavaScript
(use-package! lsp-mode
  :commands lsp
  :hook ((typescript-mode . lsp)
         (js2-mode . lsp))
  :config
  (setq lsp-completion-provider t))

;; Ensure typescript-mode is used for .ts and .tsx files
(use-package! typescript-mode
  :mode ("\\.ts\\'" "\\.js\\'" "\\.tsx\\'")
  :hook (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))

;; Use prettier for formatting
(use-package! prettier-js
  :hook ((typescript-mode . prettier-js-mode)
         (js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--single-quote" "--trailing-comma" "all")))

;; Optional: Configure keybinding to format buffer
(map! :leader
      :desc "Prettier format" "m p" #'prettier-js)


(use-package! tree-sitter
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (js2-mode . tree-sitter-hl-mode)
         (web-mode . tree-sitter-hl-mode))
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

(use-package! tree-sitter-langs
  :after tree-sitter)


(use-package! emmet-mode
  :hook (web-mode css-mode)
  :config
  (setq emmet-expand-jsx-className? t))  ;; Enable JSX className expansion



(use-package! xref
  :after lsp-mode
  :config
  (setq xref-prompt-for-identifier nil))



(use-package! lsp-tailwindcss)

(setq auth-sources '((:source "~/.authinfo")))

;; (map! :leader
;;       (:prefix ("l" . "LSP")
;;        :desc "LSP execute code action" "a" #'lsp-execute-code-action
;;        :desc "LSP format buffer" "f" #'lsp-format-buffer
;;        :desc "LSP find declaration" "d" #'lsp-find-declaration
;;        :desc "LSP find definition" "D" #'lsp-find-definition
;;        :desc "LSP find implementation" "i" #'lsp-find-implementation
;;        :desc "LSP find references" "r" #'lsp-find-references
;;        :desc "LSP rename" "R" #'lsp-rename
;;        ;; Add more keybindings as needed
;;        ))

(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

;; Enable this to make it easier to copy minibuffer messages
(defun copy-minibuffer-message ()
  "Copy the minibuffer message to the kill ring."
  (interactive)
  (let ((message (current-message)))
    (when message
      (kill-new message)
      (message "Copied minibuffer message: %s" message))))

;; Bind this function to a global keybinding
(global-set-key (kbd "C-c C-m") 'copy-minibuffer-message)



;; Evil multiple cursors
(after! evil-mc
  (define-key evil-normal-state-map (kbd "g z n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "g z p") 'evil-mc-make-and-goto-prev-match)
  (define-key evil-normal-state-map (kbd "g z u") 'evil-mc-undo-last-added-cursor)
  (define-key evil-normal-state-map (kbd "g z a") 'evil-mc-make-all-cursors)
  (define-key evil-normal-state-map (kbd "g z q") 'evil-mc-undo-all-cursors))

(use-package! all-the-icons
  :if (display-graphic-p))



(use-package! ripgrep
  :commands (ripgrep-regexp))

(use-package! projectile-ripgrep
  :after projectile
  :commands (projectile-ripgrep))



;; Set PATH for Emacs
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
(setq exec-path (append exec-path '("/usr/bin")))


;; ~/.doom.d/config.el

(use-package! consult
  :bind (("C-s" . consult-line)
         :map projectile-command-map
         ("s r" . consult-ripgrep))
  :init
  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
  ;; Configure other variables and consult functions as needed
  )

(defun consult-ripgrep-to-buffer (search-term output-buffer-name)
  "Run consult-ripgrep with SEARCH-TERM and redirect output to OUTPUT-BUFFER-NAME."
  (interactive "sEnter search term (regex): \nsOutput buffer name: ")
  (let ((output-buffer (get-buffer-create output-buffer-name))
        (consult-ripgrep-args (concat "rg -U --with-filename --no-heading --line-number --color=never -e \"" search-term "\"")))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Ripgrep results for: %s\n\n" search-term))
        (insert (shell-command-to-string consult-ripgrep-args))
        (goto-char (point-min))
        (compilation-mode)))
    (display-buffer output-buffer '(display-buffer-pop-up-window . ((side . right) (slot . 1) (window-width . 0.5))))
    (message "Search results saved to buffer: %s" output-buffer-name)))

(map! :leader
      :desc "Consult ripgrep to buffer"
      "z s R" #'consult-ripgrep-to-buffer)



;; ~/.doom.d/config.el

(after! compile
  ;; Add support for ripgrep output in compilation-mode
  (add-to-list 'compilation-error-regexp-alist-alist
               '(ripgrep
                 "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):"
                 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'ripgrep))



;; ;; Add the directory containing the .so file to the load path
;; (add-to-list 'load-path "~/.config/emacs/tree-sitter/")

;; ;; Ensure Tree-sitter and Tree-sitter-langs are loaded
;; (use-package! tree-sitter
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package! tree-sitter-langs
;;   :after tree-sitter)

;; ;; Add the JSON grammar to Tree-sitter
;; (use-package! tree-sitter
;;   :config
;;   (tree-sitter-load 'json "~/.config/emacs/tree-sitter/libtree-sitter-json.so")
;;   (tree-sitter-load 'sql "~/.config/emacs/tree-sitter/libtree-sitter-sql.so"))



;; Install and enable sql-indent
(use-package! sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; Optional: Customize indentation settings
(after! sql-indent
  (setq sqlind-basic-offset 4))  ;; Set the basic indentation level to 4 spaces

;; Install and configure sqlformat
(use-package! sqlformat
  :hook (sql-mode . sqlformat-on-save-mode)  ;; Automatically format SQL on save
  :config
  (setq sqlformat-command 'pgformatter)        ;; Set the command to use for formatting
  (setq sqlformat-args '("-s2" "-g")))

;; Keybinding for manually formatting the buffer
(map! :map sql-mode-map
 
