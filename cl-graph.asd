(asdf:defsystem #:cl-graph
  :name "cl-graph"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :description "Directed graph data structure"
  :serial t
  :components ((:file "package")
               (:file "base")
               (:file "api"))
  :depends-on (alexandria anaphora))
