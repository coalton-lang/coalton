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

(deftest test-tarjan-scc-2 ()
  (let ((g (coalton-user::make-digraph coalton-user::Unit)))
    (coalton-user::graph-add-node "a" g)
    (coalton-user::graph-add-node "b" g)
    (coalton-user::graph-add-node "c" g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 0)
     (coalton-user::NodeIndex 1)
     g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 1)
     (coalton-user::NodeIndex 2)
     g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 2)
     (coalton-user::NodeIndex 0)
     g)

    (is (equalp (coalton-user::tarjan-scc g)
		(coalton-user::make-list
		 (coalton-user::make-list
		  (coalton-user::NodeIndex 0)
		  (coalton-user::NodeIndex 1)
		  (coalton-user::NodeIndex 2))))))

  (let ((g (coalton-user::make-digraph coalton-user::Unit)))
    (coalton-user::graph-add-node "a" g)
    (coalton-user::graph-add-node "b" g)
    (coalton-user::graph-add-node "c" g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 0)
     (coalton-user::NodeIndex 0)
     g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 1)
     (coalton-user::NodeIndex 2)
     g)

    (coalton-user::graph-add-edge
     coalton-user::Unit
     (coalton-user::NodeIndex 2)
     (coalton-user::NodeIndex 1)
     g)

    (is (equalp (coalton-user::tarjan-scc g)
		(coalton-user::make-list
		 (coalton-user::make-list
		  (coalton-user::NodeIndex 0))
		 (coalton-user::make-list 
		  (coalton-user::NodeIndex 1)
		  (coalton-user::NodeIndex 2))))))

  (let ((g (coalton-user::make-digraph coalton-user::Unit)))
    (let  ((a (coalton-user::graph-add-node "a" g))
	   (b (coalton-user::graph-add-node "b" g))
	   (c (coalton-user::graph-add-node "c" g))
	   (d (coalton-user::graph-add-node "d" g))
	   (e (coalton-user::graph-add-node "e" g)))

      (coalton-user::graph-add-edge
       coalton-user::Unit
       a 
       b 
       g)

      (coalton-user::graph-add-edge
       coalton-user::Unit
       b 
       c 
       g)

      (coalton-user::graph-add-edge
       coalton-user::Unit
       b 
       e 
       g)

      (coalton-user::graph-add-edge
       coalton-user::Unit
       c 
       a 
       g)

      (coalton-user::graph-add-edge
       coalton-user::Unit
       c 
       d 
       g)
       
      (coalton-user::graph-add-edge
       coalton-user::Unit
       d 
       e 
       g)
       
      (coalton-user::graph-add-edge
       coalton-user::Unit
       e 
       d 
       g)
       
      (is (equalp (coalton-user::tarjan-scc g)
		  (coalton-user::make-list
		   (coalton-user::make-list
		    (coalton-user::NodeIndex 4)
		    (coalton-user::NodeIndex 3))
		   (coalton-user::make-list
		    (coalton-user::NodeIndex 0)
		    (coalton-user::NodeIndex 1)
		    (coalton-user::NodeIndex 2)))))))

  (let* ((graph_ (coalton-user::make-digraph coalton-user::Unit))
	 (a (coalton-user::graph-add-node "a" graph_))
	 (b (coalton-user::graph-add-node "b" graph_))
	 (c (coalton-user::graph-add-node "c" graph_))
	 (d (coalton-user::graph-add-node "d" graph_))
	 (e (coalton-user::graph-add-node "e" graph_))
	 (f (coalton-user::graph-add-node "f" graph_))
	 (g (coalton-user::graph-add-node "g" graph_))
	 (h (coalton-user::graph-add-node "h" graph_)))

    (coalton-user::graph-add-edge coalton-user::Unit a b graph_)

    (coalton-user::graph-add-edge coalton-user::Unit b c graph_)
    (coalton-user::graph-add-edge coalton-user::Unit b e graph_)
    (coalton-user::graph-add-edge coalton-user::Unit b f graph_)

    (coalton-user::graph-add-edge coalton-user::Unit c d graph_)
    (coalton-user::graph-add-edge coalton-user::Unit c g graph_)

    (coalton-user::graph-add-edge coalton-user::Unit d c graph_)
    (coalton-user::graph-add-edge coalton-user::Unit d h graph_)

    (coalton-user::graph-add-edge coalton-user::Unit e a graph_)
    (coalton-user::graph-add-edge coalton-user::Unit e f graph_)

    (coalton-user::graph-add-edge coalton-user::Unit e a graph_)
    (coalton-user::graph-add-edge coalton-user::Unit e f graph_)

    (coalton-user::graph-add-edge coalton-user::Unit f g graph_)

    (coalton-user::graph-add-edge coalton-user::Unit g f graph_)

    (coalton-user::graph-add-edge coalton-user::Unit h d graph_)
    (coalton-user::graph-add-edge coalton-user::Unit h g graph_)

    (is (equalp (coalton-user::tarjan-scc graph_)
		(coalton-user::make-list
		 (coalton-user::make-list f g)
		 (coalton-user::make-list c d h)
		 (coalton-user::make-list a b e)))))

  (let* ((graph_ (coalton-user::make-digraph coalton-user::Unit))
	 (a (coalton-user::graph-add-node "a" graph_))
	 (b (coalton-user::graph-add-node "b" graph_))
	 (c (coalton-user::graph-add-node "c" graph_))
	 (d (coalton-user::graph-add-node "d" graph_))
	 (e (coalton-user::graph-add-node "e" graph_))
	 (f (coalton-user::graph-add-node "f" graph_))
	 (g (coalton-user::graph-add-node "g" graph_))
	 (h (coalton-user::graph-add-node "h" graph_))
	 (i (coalton-user::graph-add-node "i" graph_))
	 (j (coalton-user::graph-add-node "j" graph_))
	 (k (coalton-user::graph-add-node "k" graph_))
	 (l (coalton-user::graph-add-node "l" graph_))
	 (m (coalton-user::graph-add-node "m" graph_))
	 (n (coalton-user::graph-add-node "n" graph_))
	 (o (coalton-user::graph-add-node "o" graph_))
	 (p (coalton-user::graph-add-node "p" graph_)))

    (coalton-user::graph-add-edge coalton-user::Unit a c graph_)

    (coalton-user::graph-add-edge coalton-user::Unit b a graph_)
    (coalton-user::graph-add-edge coalton-user::Unit b f graph_)

    (coalton-user::graph-add-edge coalton-user::Unit c b graph_)
    (coalton-user::graph-add-edge coalton-user::Unit c e graph_)

    (coalton-user::graph-add-edge coalton-user::Unit d c graph_)
    (coalton-user::graph-add-edge coalton-user::Unit d m graph_)

    (coalton-user::graph-add-edge coalton-user::Unit e d graph_)
    (coalton-user::graph-add-edge coalton-user::Unit e f graph_)
    (coalton-user::graph-add-edge coalton-user::Unit e n graph_)

    (coalton-user::graph-add-edge coalton-user::Unit f g graph_)
    (coalton-user::graph-add-edge coalton-user::Unit f i graph_)
    (coalton-user::graph-add-edge coalton-user::Unit f k graph_)

    (coalton-user::graph-add-edge coalton-user::Unit g h graph_)

    (coalton-user::graph-add-edge coalton-user::Unit h i graph_)
    (coalton-user::graph-add-edge coalton-user::Unit h j graph_)

    (coalton-user::graph-add-edge coalton-user::Unit i g graph_)
    (coalton-user::graph-add-edge coalton-user::Unit i j graph_)

    (coalton-user::graph-add-edge coalton-user::Unit k j graph_)
    (coalton-user::graph-add-edge coalton-user::Unit k l graph_)

    (coalton-user::graph-add-edge coalton-user::Unit l k graph_)

    (coalton-user::graph-add-edge coalton-user::Unit m n graph_)

    (coalton-user::graph-add-edge coalton-user::Unit n o graph_)
    (coalton-user::graph-add-edge coalton-user::Unit n p graph_)

    (coalton-user::graph-add-edge coalton-user::Unit o m graph_)

    (coalton-user::graph-add-edge coalton-user::Unit p o graph_)
    (coalton-user::graph-add-edge coalton-user::Unit p k graph_)

    ;; TODO: enable this test when debugging is improved
    #+ignore
    (is (equalp (coalton-user::tarjan-scc graph_)
		(coalton-user::make-list
		 (coalton-user::make-list j)
		 (coalton-user::make-list k l)
		 (coalton-user::make-list g h i)
		 (coalton-user::make-list m n o p)
		 (coalton-user::make-list f)
		 (coalton-user::make-list a b c d e))))))
