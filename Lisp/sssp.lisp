;;Ferrillo Samuele 900210
;;Antonico Lorenzo 904775

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))


(defun is-graph (graph-id)
    (gethash graph-id *graphs*)) ;; se esiste ritorna il grafo, altrimenti nil

(defun new-graph (graph-id)
    (or (gethash graph-id *graphs*)
        (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
    (remhash graph-id *graphs*))
    ;; manca un pezzo

(defun new-vertex (graph-id vertex-id)
    (setf (gethash (list 'vertex graph-id vertex-id) *vertices*) nil)
    (list 'vertex graph-id vertex-id))
