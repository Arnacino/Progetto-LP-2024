;;Ferrillo Samuele 900210
;;Antonico Lorenzo 904775

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

; --------------------- Creazione e manipolazione grafi ---------------------- ;

(defun is-graph (graph-id)
    (gethash graph-id *graphs*)) ;; se esiste ritorna il grafo, altrimenti nil

(defun new-graph (graph-id)
    (or (gethash graph-id *graphs*)
        (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
    (clrhash graph-id *graphs*)
    (clrhash (graph-vertices (graph-id)) *vertices*)
    (clrhash (graph-edges (graph-id)) *edges*))

(defun new-vertex (graph-id vertex-id)
    (setf (gethash (list 'vertex graph-id vertex-id) *vertices*) t)
    (list 'vertex graph-id vertex-id))

(defun is-vertex (graph-id vertex-id)
    (not (null (gethash (list 'vertex graph-id vertex-id) *vertices*)))) 

(defun graph-vertices (graph-id)
  (let ((vertex-rep-list '()))
    (maphash (lambda (k v)
               (when (and (listp k) (eq (first k) 'vertex) (eq (second k) graph-id))
                 (push k vertex-rep-list)))
             *vertices*)
    vertex-rep-list))

(defun new-edge (graph-id vertex-id vertex-id2 &optional weight) 
    (if (and (not (null weight)) (numberp weight) (> weight 0))
        (setf 
         (gethash (list 'edge graph-id vertex-id vertex-id2) *edges*) 
         weight)
        (setf 
         (gethash (list 'edge graph-id vertex-id vertex-id2) 
          *edges*) 
         1)))

(defun graph-edges (graph-id)
    (let ((vertex-rep-list '()))
        (maphash (lambda (k _)
                  (when (and 
                         (listp k) 
                         (eq (first k) 'edge) 
                         (eq (second k) graph-id))
                   (push k vertex-rep-list)))
             *edges*)
     vertex-rep-list))

(defun graph-vertex-neighbors (graph-id vertex-id) 
    (let ((neighbors '()))
        (maphash (lambda (k v)
                  (when (and 
                         (listp k)
                         (eq (first k) 'edge)
                         (eq (second k) graph-id)
                         (eq (third k) vertex-id))
                        (push (list k v) neighbors))) *edges*)neighbors))

(defun graph-print (graph-id)
    (format t "Vertices: ~a~%" (graph-vertices graph-id))
    (format t "Edges: ~a~%" (graph-edges graph-id)))
