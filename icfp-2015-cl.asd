(defsystem "icfp-2015-cl"
  :version "0.1"
  :author ("Oleg Sivokon <olegsivokon@gmail.com>"
           "Zach Kost-Smith <zachkostsmith@gmail.com>"
           "Jacob MacDonald <jaccarmac@gmail.com>"
	   "Joshua Kordani <joshua.kordani@gmail.com>")
  :license "MIT"
  :depends-on ("alexandria" "iterate" "split-sequence" "optima" "cl-containers")
  :build-operation program-op
  :entry-point "icfp-2015-cl::entry-point"
  :serial t
  :components
  ((:file "package")
   (:file "main")
   (:module "src"
            :serial t
            :components
            ((:file "lcg"))))
  :description "ICFP 2015")
