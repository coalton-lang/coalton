(cl:in-package #:coalton-library)

;;
;; Library module for graphs and graph algorithms
;;


;; The graph data representation and operations in this file are
;; adapted from
;;
;;    https://github.com/petgraph/petgraph
;;
;; which is licensed under the MIT License
;; (https://github.com/petgraph/petgraph/blob/master/LICENSE-MIT)

;; Copyright (c) 2015
;;
;; Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated
;; documentation files (the "Software"), to deal in the
;; Software without restriction, including without
;; limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions
;; of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
;; ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
;; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
;; IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; NOTE: This library is still under active development

(coalton-toplevel

  ;;
  ;; Index types
  ;;

  (define-type (IndexPair :a)
    "A pair of indices representing the incoming and outgoing edges on a node."
    (IndexPair :a :a))

  (declare index-pair-incoming ((IndexPair :a) -> :a))
  (define (index-pair-incoming p)
    (match p
      ((IndexPair _ x) x)))

  (declare index-pair-outgoing ((IndexPair :a) -> :a))
  (define (index-pair-outgoing p)
    (match p
      ((IndexPair x _) x)))

  (define-type EdgeIndex
    (EdgeIndex Integer))

  (declare edge-index-value (EdgeIndex -> Integer))
  (define (edge-index-value idx)
    (match idx
      ((EdgeIndex idx)
       idx)))

  (define-instance (Into EdgeIndex Integer)
    (define (into x)
      (match x
        ((EdgeIndex x) x))))

  (define-instance (Eq EdgeIndex)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (EdgeIndex a) (EdgeIndex b))
         (== a b))))
    (define (/= a b)
      (not (== a b))))

  (define-instance (Show EdgeIndex)
    (define (show x)
      (match x
        ((EdgeIndex x) (show x)))))

  (define-type NodeIndex
    (NodeIndex Integer))

  (define-instance (Show NodeIndex)
    (define (show x)
      (match x
        ((NodeIndex x) (show x)))))

  (declare node-index-value (NodeIndex -> Integer))
  (define (node-index-value idx)
    (match idx
      ((NodeIndex idx)
       idx)))

  (define-instance (Into NodeIndex Integer)
    (define (into x)
      (match x
        ((NodeIndex x) x))))

  (define-instance (Eq NodeIndex)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (NodeIndex a) (NodeIndex b))
         (== a b))))
    (define (/= a b)
      (not (== a b))))

  ;;
  ;; Node and edge types
  ;;

  (define-type (Node :data)
    (Node :data (IndexPair (Optional EdgeIndex))))

  (declare node-data ((Node :data) -> :data))
  (define (node-data node)
    (match node
      ((Node x _) x)))

  (declare node-edge-pair ((Node :data) -> (IndexPair (Optional EdgeIndex))))
  (define (node-edge-pair node)
    "Gets the IndexPair of the first incoming and outgoing edge for the node."
    (match node
      ((Node _ x) x)))

  (define-type (Edge :data)
    (Edge :data (IndexPair (Optional EdgeIndex)) (IndexPair NodeIndex)))

  (declare edge-data ((Edge :data) -> :data))
  (define (edge-data edge)
    (match edge
      ((Edge data _ _)
       data)))

  (declare edge-next-pair ((Edge :data) -> (IndexPair (Optional EdgeIndex))))
  (define (edge-next-pair edge)
    (match edge
      ((Edge _ next _)
       next)))

  (declare edge-node-pair ((Edge :data) -> (IndexPair NodeIndex)))
  (define (edge-node-pair edge)
    (match edge
      ((Edge _ _ nodes)
       nodes)))

  (declare edge-from-index ((Edge :data) -> NodeIndex))
  (define (edge-from-index edge)
    (match edge
      ((Edge _ _ (IndexPair from _))
       from)))

  (declare edge-to-index ((Edge :data) -> NodeIndex))
  (define (edge-to-index edge)
    (match edge
      ((Edge _ _ (IndexPair _ to))
       to)))


  ;;
  ;; Graph type
  ;;

  (define-type GraphType
    Undirected
    Directed)

  (define-type (Graph :node-data :edge-data)
    "A graph using adjacency list representation"
    (Graph GraphType
           (Vector (Node :node-data))
           (Vector (Edge :edge-data))))


  (declare make-graph (Unit -> (Graph :node-data :edge-data)))
  (define (make-graph _)
    "Create a new empty undirected graph"
    (Graph Undirected
           (make-vector Unit)
           (make-vector Unit)))

  (declare make-digraph (Unit -> (Graph :node-data :edge-data)))
  (define (make-digraph _)
    "Create a new directed graph"
    (Graph Directed
           (make-vector Unit)
           (make-vector Unit)))

  (declare graph-nodes ((Graph :node-data :edge-data) -> (Vector (Node :node-data))))
  (define (graph-nodes graph)
    "Returns the nodes in a graph"
    (match graph
      ((Graph _ nodes _) nodes)))

  (declare graph-edges ((Graph :node-data :edge-data) -> (Vector (Edge :edge-data))))
  (define (graph-edges graph)
    "Returns the edges in a graph"
    (match graph
      ((Graph _ _ edges) edges)))

  (declare graph-is-directed ((Graph :node-data :edge-data) -> Boolean))
  (define (graph-is-directed graph)
    (match graph
      ((Graph (Directed) _ _) True)
      ((Graph (Undirected) _ _) False)))


  (declare graph-node-count ((Graph :node-data :edge-data) -> Integer))
  (define (graph-node-count graph)
    "Returns the number of nodes in a graph"
    (vector-length (graph-nodes graph)))

  (declare graph-edge-count ((Graph :node-data :edge-data) -> Integer))
  (define (graph-edge-count graph)
    "Returns the number of edges in a graph"
    (vector-length (graph-edges graph)))


  (declare graph-lookup-node (NodeIndex -> (Graph :node-data :edge-data) -> (Optional (Node :node-data))))
  (define (graph-lookup-node idx g)
    "Lookup a node with index IDX in graph G"
    (vector-index (into idx) (graph-nodes g)))

  (declare graph-lookup-edge (EdgeIndex -> (Graph :node-data :edge-data) -> (Optional (Edge :edge-data))))
  (define (graph-lookup-edge idx g)
    "Lookup a node with index IDX in graph G"
    (vector-index (into idx) (graph-edges g)))


  (declare graph-add-node (:node-data -> (Graph :node-data :edge-data) -> NodeIndex))
  (define (graph-add-node node-data graph)
    "Add a node with associated data to the graph, returning the index of the new node."
    (match graph
      ((Graph _ nodes _)
       (progn
         (vector-push (Node node-data (IndexPair None None)) nodes)
         (NodeIndex (- (vector-length nodes) 1))))))

  (declare graph-remove-node (NodeIndex -> (Graph :node-data :edge-data) -> (Optional :node-data)))
  (define (graph-remove-node index graph)
    "Remove a node and all edges connecting to it from GRAPH"
    (let ((remove-edges
            (fn (idx accessor)
              (do (node_ <- (vector-index idx (graph-nodes graph)))
                  (match (accessor (node-edge-pair node_))
                    ((None)
                     None)
                    ((Some edge_)
                     (progn
                       (fromSome "Internal bug" (graph-remove-edge edge_ graph))
                       (remove-edges idx accessor))))))))
      (do (node_ <- (vector-index (into index) (graph-nodes graph)))
          (remove-edges (into index) index-pair-incoming)
        (remove-edges (into index) index-pair-outgoing)
        (node_ <- (vector-swap-remove (into index) (graph-nodes graph)))
        (match (vector-index (into index) (graph-nodes graph))
          ((None)
           (Some (node-data node_)))
          ((Some ed)
           (let ((swap-edges (node-edge-pair ed))
                 (old-index (NodeIndex (vector-length (graph-nodes graph))))
                 (accessor index-pair-incoming)

                 (adjust-edges
                   (fn (idx dir-node-accessor dir-edge-accessor dir-update)
                     (match (graph-lookup-edge idx graph)
                       ((None)
                        None)
                       ((Some e)
                        (progn
                          (unless (== (dir-node-accessor (edge-node-pair e))
                                      old-index)
                            (error "Found edge with incorrect node index"))

                          (vector-set (into idx)
                                      (Edge (edge-data e)
                                            (edge-next-pair e)
                                            (dir-update (edge-node-pair e) index))
                                      (graph-edges graph))

                          (match (dir-edge-accessor (edge-next-pair e))
                            ((None)
                             None)
                            ((Some next-idx)
                             (adjust-edges next-idx dir-node-accessor dir-edge-accessor dir-update)))))))))
             (progn
               (match (index-pair-incoming swap-edges)
                 ((None) None)
                 ((Some edge_)
                  (adjust-edges edge_
                                index-pair-incoming
                                index-pair-incoming
                                (fn (pair val)
                                  (IndexPair (index-pair-outgoing pair)
                                             val)))))

               (match (index-pair-outgoing swap-edges)
                 ((None) None)
                 ((Some edge_)
                  (adjust-edges edge_
                                index-pair-outgoing
                                index-pair-outgoing
                                (fn (pair val)
                                  (IndexPair val
                                             (index-pair-incoming pair))))))

               (Some (node-data node_)))))))))


  (declare graph-add-edge (:edge-data -> NodeIndex -> NodeIndex -> (Graph :node-data :edge-data) -> EdgeIndex))
  (define (graph-add-edge edge-data from to graph)
    "Add an edge with associated data from node FROM to node TO in the graph."
    ;; Create a new edge index with the current length of the vector
    (let ((edge-idx (EdgeIndex (graph-edge-count graph))))
      (progn
        ;; Check that both indices are valid
        (let node-count = (graph-node-count graph))
        (unless (and (< (node-index-value from) node-count)
                     (< (node-index-value to)   node-count))
          (error "Invalid node index when adding graph edge"))

        (if (== from to)
            ;; If FROM == TO then we are creating a self-cycle
            (progn
              ;; Pull the exiting node out of the graph
              (let node_ = (fromSome "unreachable" (graph-lookup-node from graph)))
              ;; Construct the edge, pointing to the previous heads of the edge lists
              (let edge_ = (Edge edge-data
                                 (node-edge-pair node_)
                                 (IndexPair from to)))
              ;; Construct a new node with the new edge as the head of the incoming/outgoing edge lists
              (let node__ = (Node (node-data node_)
                                  (IndexPair (Some edge-idx) (Some edge-idx))))

              ;; Finally, add the edge and replace the node
              (vector-push edge_ (graph-edges graph))
              (vector-set (into from) node__ (graph-nodes graph))
              ;; And return the index of the new edge
              edge-idx)

            ;; Otherwise, create non-cycle (using two nodes)
            (progn
              ;; Pull the exiting nodes out of the graph
              (let from-node = (fromSome "unreachable" (graph-lookup-node from graph)))
              (let to-node   = (fromSome "unreachable" (graph-lookup-node to graph)))
              ;; Construct the edge, pointing to the previous heads of the edge lists
              (let edge_ = (Edge edge-data
                                 (IndexPair (index-pair-outgoing (node-edge-pair from-node))
                                            (index-pair-incoming (node-edge-pair to-node)))
                                 (IndexPair from to)))
              ;; Construct new nodes with the new edge as the head of the incoming/outgoing edge lists
              (let from-node_ = (Node (node-data from-node)
                                      (IndexPair (Some edge-idx)
                                                 (index-pair-incoming (node-edge-pair from-node)))))
              (let to-node_   = (Node (node-data to-node)
                                      (IndexPair (index-pair-outgoing (node-edge-pair to-node))
                                                 (Some edge-idx))))

              ;; Finally, add the edge and replace the nodes
              (vector-push edge_ (graph-edges graph))
              (vector-set (into from) from-node_ (graph-nodes graph))
              (vector-set (into to) to-node_ (graph-nodes graph))
              ;; And return the index of the new edge
              edge-idx)))))

  (declare graph-remove-edge (EdgeIndex -> (Graph :node-data :edge-data) -> (Optional :edge-data)))
  (define (graph-remove-edge index graph)
    "Remove an edge from GRAPH"
    (do (edge <- (vector-index (into index) (graph-edges graph)))
        (let _ = (change-edge-links (edge-node-pair edge) index (edge-next-pair edge) graph))
      (remove-edge-adjust-indices index graph)))

  ;; Helpers for above function

  (declare change-edge-links ((IndexPair NodeIndex) -> EdgeIndex -> (IndexPair (Optional EdgeIndex)) -> (Graph :node-data :edge-data) -> Unit))
  (define (change-edge-links edge-node index edge-next graph)
    (let ((change-edge-links-direction
            ;; NOTE: We need two different accessors because we are
            ;; not able to polymorphise on arguments (when pulling
            ;; parts of IndexPairs out)
            (fn (dir-node-accessor dir-edge-accessor dir-update)
              (let ((node_ (fromSome "Edge endpoint not found"
                                     (graph-lookup-node (dir-node-accessor edge-node) graph))))
                (let ((fst (dir-edge-accessor (node-edge-pair node_))))
                  (if (if (isSome fst)
                          (== index (fromSome "" fst))
                          False)
                      (const Unit
                             (vector-set (into (dir-node-accessor edge-node))
                                         (Node (node-data node_)
                                               (dir-update (node-edge-pair node_)
                                                           (dir-edge-accessor edge-next)))
                                         (graph-nodes graph)))
                      (let ((replace-edge
                              (fn (idx)
                                (match (graph-lookup-edge idx graph)
                                  ((None)
                                   Unit)
                                  ((Some e)
                                   (let ((next-pair (edge-next-pair e)))
                                     (match (dir-edge-accessor next-pair)
                                       ((None)
                                        Unit)
                                       ((Some next-idx)
                                        (if (== next-idx index)
                                            (const Unit
                                                   (vector-set (into idx)
                                                               (Edge (edge-data e)
                                                                     (dir-update next-pair
                                                                                 (dir-edge-accessor edge-next))
                                                                     (edge-node-pair e))
                                                               (graph-edges graph)))
                                            (replace-edge next-idx))))))))))
                        (replace-edge index))))))))

      (progn
        ;; First replace the incoming side
        (change-edge-links-direction
         index-pair-incoming
         index-pair-incoming
         (fn (pair val)
           (IndexPair (index-pair-outgoing pair)
                      val)))

        ;; Then replace the outgoing
        (change-edge-links-direction
         index-pair-outgoing
         index-pair-outgoing
         (fn (pair val)
           (IndexPair val
                      (index-pair-incoming pair)))))))

  (declare remove-edge-adjust-indices (EdgeIndex -> (Graph :node-data :edge-data) -> (Optional :edge-data)))
  (define (remove-edge-adjust-indices index graph)
    (do (edge_ <- (vector-swap-remove (into index) (graph-edges graph)))
        (match (vector-index (into index) (graph-edges graph))
          ((None)
           (Some (edge-data edge_)))
          ((Some ed)
           (let ((swap (edge-node-pair ed))
                 (swapped-edge (EdgeIndex (vector-length (graph-edges graph)))))
             (progn
               (change-edge-links swap swapped-edge (IndexPair (Some index) (Some index)) graph)
               (Some (edge-data edge_))))))))

  ;; TODO: add function to output graphviz where subgraphs are sccs
  (declare graph-viz (Show :node-data => ((Graph :node-data :edge-data) -> String)))
  (define (graph-viz graph_)
    (progn
      (let elements = (make-vector Unit))
      (if (graph-is-directed graph_)
          (vector-push "digraph {" elements)
          (vector-push "graph {" elements))
      (vector-foreach-index
       (fn (i node)
         (vector-push
          (msum
           (make-list
            (show i)
            " [label=\""
            (show (node-data node))
            "\"]"))
          elements))
       (graph-nodes graph_))
      (vector-foreach
       (fn (edge)
         (vector-push
          (msum
           (make-list
            (show (edge-from-index edge))
            (if (graph-is-directed graph_)
                " -> "
                " -- ")
            (show (edge-to-index edge))))
          elements))
       (graph-edges graph_))
      (vector-push "}" elements)
      (msum (intersperse (show #\NEWLINE) (vector-to-list elements)))))


  ;;
  ;; Tarjan strongly connected components algorithm
  ;;

  (define-type TarjanNode
    (TarjanNode (Optional Integer) Integer Boolean))

  (declare tarjan-node-index (TarjanNode -> (Optional Integer)))
  (define (tarjan-node-index node)
    (match node
      ((TarjanNode index _ _)
       index)))

  (declare tarjan-node-low-link (TarjanNode -> Integer))
  (define (tarjan-node-low-link node)
    (match node
      ((TarjanNode _ low-link _)
       low-link)))

  (declare tarjan-node-on-stack (TarjanNode -> Boolean))
  (define (tarjan-node-on-stack node)
    (match node
      ((TarjanNode _ _ on-stack)
       on-stack)))


  (declare tarjan-scc ((Graph :node-data :edge-data) -> (List (List NodeIndex))))
  (define (tarjan-scc graph)
    (let ((current-index (make-cell 0))
          (tarjan-data (make-vector Unit))
          (stack (make-vector Unit))
          (output-sccs (make-vector Unit)))

      (progn
        ;; Pre-populate the tarjan data
        (vector-foreach-index
         (fn (i x)
           (vector-push (TarjanNode None i False) tarjan-data))
         (graph-nodes graph))

        (let visit-node =
          (fn (node-idx)
            ;; Only act on unvisited nodes
            (unless (isSome (tarjan-node-index (vector-index-unsafe node-idx tarjan-data)))
              (progn
                ;; Update the tarjan entry
                (vector-set node-idx
                            (TarjanNode (Some (cell-read current-index))
                                        (cell-read current-index)
                                        True)
                            tarjan-data)

                ;; Push it on to the stack
                (vector-push node-idx stack)

                ;; Increment the current index
                (cell-write (+ 1 (cell-read current-index)) current-index)

                ;; Consider successors
                (let visit-edge =
                  (fn (edge-idx)
                    (let ((edge-entry (vector-index-unsafe edge-idx (graph-edges graph)))
                          (w (+ 0 (into (edge-to-index edge-entry))))
                          (w-entry (vector-index-unsafe w tarjan-data))
                          (w-index (tarjan-node-index w-entry)))
                      (progn
                        (cond
                          ((isNone w-index)
                           (progn
                             ;; The successor has not been visited. Visit it
                             (visit-node w)

                             ;; Update the lowlink of the successor
                             (let w-entry = (vector-index-unsafe w tarjan-data))
                             (let v-entry = (vector-index-unsafe node-idx tarjan-data))
                             (vector-set
                              node-idx
                              (TarjanNode
                               (tarjan-node-index v-entry)
                               (min (tarjan-node-low-link w-entry)
                                    (tarjan-node-low-link v-entry))
                               (tarjan-node-on-stack v-entry))
                              tarjan-data)
                             Unit))
                          ((tarjan-node-on-stack w-entry)
                           (progn
                             (let w-entry = (vector-index-unsafe w tarjan-data))
                             (let v-entry = (vector-index-unsafe node-idx tarjan-data))
                             (vector-set
                              node-idx
                              (TarjanNode
                               (tarjan-node-index v-entry)
                               (min (fromSome "unreachable" (tarjan-node-index w-entry))
                                    (tarjan-node-low-link v-entry))
                               (tarjan-node-on-stack v-entry))
                              tarjan-data)

                             Unit))
                          (True
                           Unit))

                        (match (index-pair-outgoing (edge-next-pair edge-entry))
                          ((None)
                           Unit)
                          ((Some (EdgeIndex idx))
                           (visit-edge idx)))))))

                ;; Visit the first edge for the node
                (match (index-pair-outgoing (node-edge-pair (vector-index-unsafe node-idx (graph-nodes graph))))
                  ((None)
                   Unit)
                  ((Some (EdgeIndex idx))
                   (visit-edge idx)))

                (let v-entry = (vector-index-unsafe node-idx tarjan-data))
                (when (== (tarjan-node-low-link v-entry)
                          (fromSome "unreachable" (tarjan-node-index v-entry)))
                  (let ((build-scc
                          (fn (scc)
                            (let ((w (vector-pop-unsafe stack)))
                              (progn
                                ;; Update W to not be on the stack
                                (let w-entry = (vector-index-unsafe w tarjan-data))
                                (vector-set
                                 w
                                 (TarjanNode
                                  (tarjan-node-index w-entry)
                                  (tarjan-node-low-link w-entry)
                                  False)
                                 tarjan-data)

                                ;; And recurse
                                (if (== w node-idx)
                                    (Cons (NodeIndex w) scc)
                                    (build-scc (Cons (NodeIndex w) scc))))))))
                    ;; Build and emit the scc
                    (vector-push (build-scc Nil) output-sccs)))
                Unit))))

        (vector-foreach-index
         (fn (i _)
           (visit-node i))
         (graph-nodes graph))

        (vector-to-list output-sccs)))))
