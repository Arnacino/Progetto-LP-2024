;;Ferrillo Samuele 900210
;;Antonico Lorenzo 904775

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))


(defun is-graph (graph-id)
    (gethash graph-id *graphs*))

(defun new-graph (graph-id)
    (or (gethash graph-id *graphs*)
        (setf (gethash graph-id *graphs*) graph-id))))

(defun delete-graph (graph-id)
    (remhash graph-id *graphs*)
    ;; manca un pezzo
)

(defun new-vertex (graph-id vertex-id)
    (setf (gethash (list ’vertex graph-id vertex-id) *vertices*))
          (list ’vertex graph-id vertex-id)))

(defun graph-vertices (graph-id)
  (let ((result '()))
    (maphash (lambda (key value)
               (when (and (eq (first key) 'vertex)
                          (eq (second key) graph-id))
                 (push (third key) result)))
             *vertices*)
    result))

(defun new-edge (graph-id vertex-id vertex-id2 &optional weight)
    (setf (gethash (list ’edge graph-id vertex-id vertex-id2)
    *edges*)
    (if weight
        (list ’edge graph-id vertex-id vertex-id2 weight)
        (list ’edge graph-id vertex-id vertex-id2))))

(defun graph-edges (graph-id) 
    (let ((result '()))
    (maphash (lambda (key value)
               (when (and (eq (first key) 'edge)
                          (eq (second key) graph-id))
                 (push (list (third key) (fourth key)) result)))
             *edges*)
    result))

(defun graph-vertex-neighbors (graph-id vertex-id)
    ())