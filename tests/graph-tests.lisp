(in-package #:coalton-tests)

(deftest test-graph-basic ()
  (let ((g (coalton-library::make-graph coalton-library::Unit)))
    (coalton-library::graph-add-node "a" g)
    (coalton-library::graph-add-node "b" g)
    (coalton-library::graph-add-node "c" g)
    (is (equal (coalton-library::graph-node-count g)
	       3))
    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 0)
     (coalton-library::NodeIndex 1)
     g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 1)
     (coalton-library::NodeIndex 0)
     g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 0)
     (coalton-library::NodeIndex 2)
     g)

    (is (equal (coalton-library::graph-edge-count g)
	       3))

    (coalton-library::graph-remove-node (coalton-library::NodeIndex 1) g)

    (is (equal (coalton-library::graph-edge-count g)
	       1))

    (is (equal (coalton-library::graph-node-count g)
	       2))))

(deftest test-tarjan-scc-2 ()
  (let ((g (coalton-library::make-digraph coalton-library::Unit)))
    (coalton-library::graph-add-node "a" g)
    (coalton-library::graph-add-node "b" g)
    (coalton-library::graph-add-node "c" g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 0)
     (coalton-library::NodeIndex 1)
     g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 1)
     (coalton-library::NodeIndex 2)
     g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 2)
     (coalton-library::NodeIndex 0)
     g)

    (is (equalp (coalton-library::tarjan-scc g)
		(coalton-library::make-list
		 (coalton-library::make-list
		  (coalton-library::NodeIndex 0)
		  (coalton-library::NodeIndex 1)
		  (coalton-library::NodeIndex 2))))))

  (let ((g (coalton-library::make-digraph coalton-library::Unit)))
    (coalton-library::graph-add-node "a" g)
    (coalton-library::graph-add-node "b" g)
    (coalton-library::graph-add-node "c" g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 0)
     (coalton-library::NodeIndex 0)
     g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 1)
     (coalton-library::NodeIndex 2)
     g)

    (coalton-library::graph-add-edge
     coalton-library::Unit
     (coalton-library::NodeIndex 2)
     (coalton-library::NodeIndex 1)
     g)

    (is (equalp (coalton-library::tarjan-scc g)
		(coalton-library::make-list
		 (coalton-library::make-list
		  (coalton-library::NodeIndex 0))
		 (coalton-library::make-list 
		  (coalton-library::NodeIndex 1)
		  (coalton-library::NodeIndex 2))))))

  (let ((g (coalton-library::make-digraph coalton-library::Unit)))
    (let  ((a (coalton-library::graph-add-node "a" g))
	   (b (coalton-library::graph-add-node "b" g))
	   (c (coalton-library::graph-add-node "c" g))
	   (d (coalton-library::graph-add-node "d" g))
	   (e (coalton-library::graph-add-node "e" g)))

      (coalton-library::graph-add-edge
       coalton-library::Unit
       a 
       b 
       g)

      (coalton-library::graph-add-edge
       coalton-library::Unit
       b 
       c 
       g)

      (coalton-library::graph-add-edge
       coalton-library::Unit
       b 
       e 
       g)

      (coalton-library::graph-add-edge
       coalton-library::Unit
       c 
       a 
       g)

      (coalton-library::graph-add-edge
       coalton-library::Unit
       c 
       d 
       g)
       
      (coalton-library::graph-add-edge
       coalton-library::Unit
       d 
       e 
       g)
       
      (coalton-library::graph-add-edge
       coalton-library::Unit
       e 
       d 
       g)
       
      (is (equalp (coalton-library::tarjan-scc g)
		  (coalton-library::make-list
		   (coalton-library::make-list
		    (coalton-library::NodeIndex 4)
		    (coalton-library::NodeIndex 3))
		   (coalton-library::make-list
		    (coalton-library::NodeIndex 0)
		    (coalton-library::NodeIndex 1)
		    (coalton-library::NodeIndex 2)))))))

  (let* ((graph_ (coalton-library::make-digraph coalton-library::Unit))
	 (a (coalton-library::graph-add-node "a" graph_))
	 (b (coalton-library::graph-add-node "b" graph_))
	 (c (coalton-library::graph-add-node "c" graph_))
	 (d (coalton-library::graph-add-node "d" graph_))
	 (e (coalton-library::graph-add-node "e" graph_))
	 (f (coalton-library::graph-add-node "f" graph_))
	 (g (coalton-library::graph-add-node "g" graph_))
	 (h (coalton-library::graph-add-node "h" graph_)))

    (coalton-library::graph-add-edge coalton-library::Unit a b graph_)

    (coalton-library::graph-add-edge coalton-library::Unit b c graph_)
    (coalton-library::graph-add-edge coalton-library::Unit b e graph_)
    (coalton-library::graph-add-edge coalton-library::Unit b f graph_)

    (coalton-library::graph-add-edge coalton-library::Unit c d graph_)
    (coalton-library::graph-add-edge coalton-library::Unit c g graph_)

    (coalton-library::graph-add-edge coalton-library::Unit d c graph_)
    (coalton-library::graph-add-edge coalton-library::Unit d h graph_)

    (coalton-library::graph-add-edge coalton-library::Unit e a graph_)
    (coalton-library::graph-add-edge coalton-library::Unit e f graph_)

    (coalton-library::graph-add-edge coalton-library::Unit e a graph_)
    (coalton-library::graph-add-edge coalton-library::Unit e f graph_)

    (coalton-library::graph-add-edge coalton-library::Unit f g graph_)

    (coalton-library::graph-add-edge coalton-library::Unit g f graph_)

    (coalton-library::graph-add-edge coalton-library::Unit h d graph_)
    (coalton-library::graph-add-edge coalton-library::Unit h g graph_)

    (is (equalp (coalton-library::tarjan-scc graph_)
		(coalton-library::make-list
		 (coalton-library::make-list f g)
		 (coalton-library::make-list c d h)
		 (coalton-library::make-list a b e)))))

  (let* ((graph_ (coalton-library::make-digraph coalton-library::Unit))
	 (a (coalton-library::graph-add-node "a" graph_))
	 (b (coalton-library::graph-add-node "b" graph_))
	 (c (coalton-library::graph-add-node "c" graph_))
	 (d (coalton-library::graph-add-node "d" graph_))
	 (e (coalton-library::graph-add-node "e" graph_))
	 (f (coalton-library::graph-add-node "f" graph_))
	 (g (coalton-library::graph-add-node "g" graph_))
	 (h (coalton-library::graph-add-node "h" graph_))
	 (i (coalton-library::graph-add-node "i" graph_))
	 (j (coalton-library::graph-add-node "j" graph_))
	 (k (coalton-library::graph-add-node "k" graph_))
	 (l (coalton-library::graph-add-node "l" graph_))
	 (m (coalton-library::graph-add-node "m" graph_))
	 (n (coalton-library::graph-add-node "n" graph_))
	 (o (coalton-library::graph-add-node "o" graph_))
	 (p (coalton-library::graph-add-node "p" graph_)))

    (coalton-library::graph-add-edge coalton-library::Unit a c graph_)

    (coalton-library::graph-add-edge coalton-library::Unit b a graph_)
    (coalton-library::graph-add-edge coalton-library::Unit b f graph_)

    (coalton-library::graph-add-edge coalton-library::Unit c b graph_)
    (coalton-library::graph-add-edge coalton-library::Unit c e graph_)

    (coalton-library::graph-add-edge coalton-library::Unit d c graph_)
    (coalton-library::graph-add-edge coalton-library::Unit d m graph_)

    (coalton-library::graph-add-edge coalton-library::Unit e d graph_)
    (coalton-library::graph-add-edge coalton-library::Unit e f graph_)
    (coalton-library::graph-add-edge coalton-library::Unit e n graph_)

    (coalton-library::graph-add-edge coalton-library::Unit f g graph_)
    (coalton-library::graph-add-edge coalton-library::Unit f i graph_)
    (coalton-library::graph-add-edge coalton-library::Unit f k graph_)

    (coalton-library::graph-add-edge coalton-library::Unit g h graph_)

    (coalton-library::graph-add-edge coalton-library::Unit h i graph_)
    (coalton-library::graph-add-edge coalton-library::Unit h j graph_)

    (coalton-library::graph-add-edge coalton-library::Unit i g graph_)
    (coalton-library::graph-add-edge coalton-library::Unit i j graph_)

    (coalton-library::graph-add-edge coalton-library::Unit k j graph_)
    (coalton-library::graph-add-edge coalton-library::Unit k l graph_)

    (coalton-library::graph-add-edge coalton-library::Unit l k graph_)

    (coalton-library::graph-add-edge coalton-library::Unit m n graph_)

    (coalton-library::graph-add-edge coalton-library::Unit n o graph_)
    (coalton-library::graph-add-edge coalton-library::Unit n p graph_)

    (coalton-library::graph-add-edge coalton-library::Unit o m graph_)

    (coalton-library::graph-add-edge coalton-library::Unit p o graph_)
    (coalton-library::graph-add-edge coalton-library::Unit p k graph_)

    ;; TODO: enable this test when debugging is improved
    #+ignore
    (is (equalp (coalton-library::tarjan-scc graph_)
		(coalton-library::make-list
		 (coalton-library::make-list j)
		 (coalton-library::make-list k l)
		 (coalton-library::make-list g h i)
		 (coalton-library::make-list m n o p)
		 (coalton-library::make-list f)
		 (coalton-library::make-list a b c d e))))))
