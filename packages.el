;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
                                        ;
                                        ;

;; (unpin! lsp-mode)
;; (unpin! magit)
(unpin! pyvenv)
(unpin! pyenv-mode)
;; (unpin! vertico)
(unpin! which-key)
(unpin! flycheck)

;; (unpin! t)

;; tools/tldr
(package! tldr)

;; ui/modeline
(package! anzu)
(package! evil-anzu)

;; ui/indent-guides
(package! highlight-indent-guides)

;; themes
(package! nord-theme)

(package! nvm)
(package! ansible-vault)

(package! mu4e-alert)
(package! lsp-mode
  :recipe (:host github :repo "SteVwonder/lsp-mode" :branch "master")
  )

(package! hotfuzz :recipe
  (:host github
   :repo "axelf4/hotfuzz"
   :files ("hotfuzz.el")))

(unpin! emacs-format-all-the-code)
(package! emacs-format-all-the-code :recipe
  (:host github
   :repo "lassik/emacs-format-all-the-code"
   :branch "master"))

(package! format-all :recipe
  (:host github
   :repo "lassik/emacs-format-all-the-code"
   :branch "master"))

(package! pyimport
  ;; :type "virtual"
  ;; :recipe (:local-repo "~/projects/open_source/pyimport"
  ;;          :build (:not compile))
  :recipe (:host github
           :repo "iamriel/pyimport"
           :branch "master")
  )

(package! vertico-posframe :recipe
  (:host github :repo "tumashu/vertico-posframe"))

(package! vertico-prescient)

(package! ox-gfm)

;; (package! org-present)
(package! jenkinsfile-mode)
(package! keychain-environment)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! chatgpt
  :recipe (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))
