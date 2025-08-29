(defpackage #:coalton-impl/algorithm/tarjan-scc
  (:use #:cl)
  (:export
   #:tarjan-scc))

(in-package #:coalton-impl/algorithm/tarjan-scc)

;;;;
;;;; Tarjan's Strongly Connected Components Algorithm
;;;;
;;;; This module implements Robert Tarjan's algorithm for finding
;;;; strongly connected components (SCCs) in a directed graph.
;;;; Coalton uses SCCs for:
;;;;
;;;; - Dependency analysis of mutually recursive types
;;;;
;;;;   For example:
;;;;
;;;;     (define-type (Tree :a) (Node :a (Forest :a)))
;;;;     (define-type (Forest :a) (List (Tree :a)))
;;;;
;;;; - Calculating value-binding dependencies: functions that call
;;;;   each other must be compiled together to handle mutual
;;;;   recursion. SCCs determine which functions can be compiled
;;;;   independently vs. which must be grouped.
;;;;
;;;; - Detecting cyclic superclass dependencies and determining the
;;;;   order of class processing.
;;;;
;;;; - Compilation ordering: the topological order of SCCs expresses
;;;;   that dependencies come before dependents.
;;;;
;;;; The algorithm performs a depth-first search, while maintaining:
;;;;
;;;; - A "discovery index" for when each vertex is first visited
;;;; - A "low-link value" tracking the lowest-numbered vertex reachable
;;;; - A stack of vertices in the current path
;;;;
;;;; When a vertex's low-link equals its discovery index, it's an SCC
;;;; root, and all vertices above it on the stack form a single
;;;; strongly connected component.
;;;;
;;;; Time complexity: O(V + E) where V is vertices and E is edges.
;;;; Space complexity: O(V) for the auxiliary data structures.
;;;;
;;;; See: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
;;;;

(defstruct tarjan-node
  (index    nil :type (or null integer))
  (low-link nil :type integer)
  (on-stack nil :type boolean))

(declaim (ftype (function (list) list) tarjan-scc))
(defun tarjan-scc (graph)
  "Find strongly connected components in a directed graph using Tarjan's algorithm.

GRAPH is a list of vertices and their dependencies in the format:
  ((vertex-name dependency-1 dependency-2 ...)
   (vertex-name-2 dependency-a dependency-b ...)
   ...)

Where all vertex names and dependencies are symbols.

Returns a list of SCCs, where each SCC is a list of mutually dependent vertices.
The SCCs are returned in reverse topological order, meaning that if SCC-A depends
on vertices in SCC-B, then SCC-B will appear before SCC-A in the result list.

Example:
  (tarjan-scc '((a b c) (b c) (c)))
  => ((C) (B) (A))  ; C has no deps, B depends on C, A depends on B and C

With mutually recursive definitions:
  (tarjan-scc '((f g) (g f) (h)))  
  => ((H) (F G))    ; H is independent, F and G are mutually recursive"
  (let ((symbol-to-index (make-hash-table))
        (edge-vector (make-array (length graph))))
    ;; Populate table mapping graph vertices to indices
    (loop :for (vertex . edges) :in graph
          :for i :from 0
          :do (setf (gethash vertex symbol-to-index) i
                    (aref edge-vector i) edges))
    (let ((index 0)
          (stack nil)
          (node-data (make-array (length graph)))
          (sccs nil))

      ;; Initialize our node-data vector
      (dotimes (i (length graph))
        (setf (aref node-data i)
              (make-tarjan-node :index nil
                                :low-link 0
                                :on-stack nil)))

      (labels ((visit (vertex)
                 (let ((vertex-index (gethash vertex symbol-to-index)))
                   ;; If we have already visited the node then there
                   ;; is nothing to do.
                   (when (not (null (tarjan-node-index (aref node-data vertex-index))))
                     (return-from visit))

                   ;; Set the index and lowlink of the current vertex
                   ;; to the current index and increment index
                   (setf (tarjan-node-index (aref node-data vertex-index)) index
                         (tarjan-node-low-link (aref node-data vertex-index)) index)
                   (incf index)

                   ;; Add the current vertex to the stack
                   (setf (tarjan-node-on-stack (aref node-data vertex-index)) t)
                   (push vertex stack)

                   ;; Check all neighbors
                   (let ((edges (aref edge-vector vertex-index)))
                     (dolist (edge edges)
                       (let ((edge-index (gethash edge symbol-to-index)))
                         (cond
                           ;; EDGE has been visited
                           ((not (null (tarjan-node-index (aref node-data edge-index))))
                            (when (tarjan-node-on-stack (aref node-data edge-index))
                              (setf (tarjan-node-low-link (aref node-data vertex-index))
                                    (min (tarjan-node-low-link (aref node-data vertex-index))
                                         (tarjan-node-index (aref node-data edge-index))))))
                           ;; EDGE has not been visited
                           (t
                            ;; If we haven't seen this edge yet then visit it
                            (visit edge)
                            ;; And update our lowlink if theirs is smaller
                            (setf (tarjan-node-low-link (aref node-data vertex-index))
                                  (min (tarjan-node-low-link (aref node-data vertex-index))
                                       (tarjan-node-low-link (aref node-data edge-index)))))))))

                   ;; If we are a root node then pop the stack and add
                   ;; ourself to the sccs
                   (when (= (tarjan-node-low-link (aref node-data vertex-index))
                            (tarjan-node-index (aref node-data vertex-index)))
                     (let ((new-scc nil))
                       (loop :for item = (pop stack)
                             :do (let ((item-index (gethash item symbol-to-index)))
                                   (setf (tarjan-node-on-stack (aref node-data item-index)) nil)
                                   (push item new-scc))
                             :while (not (eql vertex item)))
                       (push new-scc sccs))))))
        (dolist (vertex graph)
          (visit (car vertex)))
        (nreverse sccs)))))
