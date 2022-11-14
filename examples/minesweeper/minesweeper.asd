(defsystem "minesweeper"
  :author "Marco Zala <marco.e.zala@gmail.com>"
  :version "0.0.1"
  :licence "MIT"
  :description "Terminal based Minesweeper game, implemented in a functional style."
  :depends-on (:functional-cl :cl-ppcre)
  :pathname "src/"
  :components ((:file "sweeper" :depends-on ("io"))
               (:file "io")))