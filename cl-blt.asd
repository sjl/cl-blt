(asdf:defsystem :cl-blt
  :description "Common Lisp wrapper for bearlibterminal"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:cffi :trivial-main-thread)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:module "low-level" :serial t
                  :components ((:file "bearlibterminal")))
                 (:module "high-level" :serial t
                  :components ((:file "bearlibterminal")
                               (:file "boxes")))))))
