(in-package :cl)
(defpackage icfp-2015-cl-asd (:use :cl :asdf))
(in-package :icfp-2015-cl-asd)

(defsystem icfp-2015-cl
  :version "0.1"
  :author ("Oleg Sivokon <olegsivokon@gmail.com>"
           "Zach Kost-Smith <zachkostsmith@gmail.com>")
  :license "MIT"
  :depends-on (:alexandria :iterate :split-sequence :optima :cl-containers)
  :serial t
  :components ((:file "package"))
  :description "ICFP 2015")
