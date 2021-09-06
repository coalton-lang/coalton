(in-package #:coalton-tests)

(deftest test-graph-basic ()
  (let ((g (coalton-user::make-graph coalton-user::Unit)))
    (coalton-user::graph-add-node "a" g)
    (coalton-user::graph-add-node "b" g)
    (coalton-user::graph-add-node "c" g)
    (is (equal (coalton-user::graph-node-count g)
	       3))
    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 0)
     (coalton-user::NodeIndex 1)
     g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 1)
     (coalton-user::NodeIndex 0)
     g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 0)
     (coalton-user::NodeIndex 2)
     g)

    (is (equal (coalton-user::graph-edge-count g)
	       3))

    (coalton-user::graph-remove-node (coalton-user::NodeIndex 1) g)

    (is (equal (coalton-user::graph-edge-count g)
	       1))

    (is (equal (coalton-user::graph-node-count g)
	       2))))
