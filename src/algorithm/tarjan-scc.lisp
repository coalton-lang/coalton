(defpackage #:coalton-impl/algorithm/tarjan-scc
  (:use #:cl)
  (:export
   #:tarjan-scc ; FUNCTION
   ))

(in-package #:coalton-impl/algorithm/tarjan-scc)

(defstruct tarjan-node
  (index    nil :type (or null integer))
  (low-link nil :type integer)
  (on-stack nil :type boolean))

(defun tarjan-scc (dag)
  "Perform Tarjan's strongly-connected component algorithm on DAG.

DAG is of format ((node-name dependency*)
                  (...)
                 )
where all names and dependencies are symbols"
  (let ((symbol-to-index (make-hash-table))
        (edge-vector (make-array (length dag))))
    ;; Populate table mapping dag vertices to indices
    (loop :for (vertex . edges) :in dag
          :for i :from 0
          :do (setf (gethash vertex symbol-to-index) i
                    (aref edge-vector i) edges))
    (let ((index 0)
          (stack nil)
          (node-data (make-array (length dag)))
          (sccs nil))

      ;; Initialize our node-data vector
      (dotimes (i (length dag))
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
        (dolist (vertex dag)
          (visit (car vertex)))
        sccs))))
