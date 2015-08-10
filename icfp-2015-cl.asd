(in-package #:asdf-user)
(asdf:defsystem :icfp-2015-cl
  :version "0.1"
  :author ("Oleg Sivokon <olegsivokon@gmail.com>"
           "Zach Kost-Smith <zachkostsmith@gmail.com>"
           "Jacob MacDonald <jaccarmac@gmail.com>"
           "Joshua Kordani <joshua.kordani@gmail.com>")
  :license "MIT"
  :depends-on ("alexandria" "iterate" "split-sequence"
                            "drakma" "cl-json" "cl-svg" "log4cl"
                            "cl-ppcre" "hunchentoot" "unix-opts")
  :build-operation program-op
  :entry-point "icfp-2015-cl::entry-point"
  :serial t
  :components
  ((:file "package")
   (:file "main")
   (:module "src"
            :serial t
            :components
            ((:file "lcg")
             (:file "submit")
             (:file "stats")
             (:file "render-svg")
             (:file "rankings")
             (:file "game")
             (:file "scoring")
             (:file "server")
             (:file "player"))))
  :description "ICFP 2015")
