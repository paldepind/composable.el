(require 'f)

(defvar composable-support-path
  (f-dirname load-file-name))

(defvar composable-features-path
  (f-parent composable-support-path))

(defvar composable-root-path
  (f-parent composable-features-path))

(add-to-list 'load-path composable-root-path)

(message composable-root-path)
(require 'composable)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 (composable-mode)
 (composable-mark-mode)
 )

(Before
 ;; Before each scenario is run
 (transient-mark-mode 1)
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
