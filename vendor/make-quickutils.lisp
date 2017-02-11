(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-boolean
               :ensure-list
               :mkstr
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "BEARLIBTERMINAL.QUICKUTILS")
