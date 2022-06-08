(require 'f)

(defvar composable-support-path
  (f-dirname load-file-name))

(defvar composable-features-path
  (f-parent composable-support-path))

(defvar composable-root-path
  (f-parent composable-features-path))

(add-to-list 'load-path composable-root-path)

(when (require 'undercover nil t)
  (undercover "*.el"
	      (:report-format 'lcov)
	      (:send-report nil)))

(require 'composable)
(require 'espuds)
(require 'ert)
(require 'which-key)

(Setup
 ;; Before anything has run
 (message emacs-version)
 (composable-mode)
 )

(Before
 ;; Before each scenario is run
 (transient-mark-mode 1)
 (which-key-mode 1)
 (composable-mark-mode 1)
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
