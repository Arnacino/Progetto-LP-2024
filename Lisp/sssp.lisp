;;Ferrillo Samuele 900210
;;Antonico Lorenzo 904775

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

; --------------------- Creazione e manipolazione grafi ---------------------- ;

(defun is-graph (graph-id)
    (gethash graph-id *graphs*)) 

(defun new-graph (graph-id)
    (or (gethash graph-id *graphs*)
        (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
    (remhash graph-id *graphs*)
    (delete-vertices graph-id)
    (delete-edges graph-id))

(defun new-vertex (graph-id vertex-id)
    (if (not (is-graph graph-id))
        (error "Graph not found"))
    (setf (gethash (list 'vertex graph-id vertex-id) *vertices*) t)
    (list 'vertex graph-id vertex-id))

(defun is-vertex (graph-id vertex-id)
    (and (is-graph graph-id)
     (not (null (gethash (list 'vertex graph-id vertex-id) *vertices*))))) 

(defun delete-vertices (graph-id)
  (maphash 
    (lambda (k v) 
      (when (and 
             (listp k) 
             (eq (first k) 'vertex) 
             (eq (second k) graph-id))
        (remhash k *vertices*))) 
    *vertices*))

(defun graph-vertices (graph-id)
  (let ((vertex-rep-list '()))
    (maphash (lambda (k v)
               (when (and (listp k) (eq (first k) 'vertex) (eq (second k) graph-id))
                 (push k vertex-rep-list)))
             *vertices*)
    vertex-rep-list))

(defun new-edge (graph-id vertex-id vertex-id2 &optional weight)
    (if (not (and
              (is-vertex graph-id vertex-id) 
              (is-vertex graph-id vertex-id2)))
        (error "Vertex not found"))
    (if (and (not (null weight)) (numberp weight) (> weight 0))
        (setf 
         (gethash (list 'edge graph-id vertex-id vertex-id2) *edges*) 
         weight)
        (setf 
         (gethash (list 'edge graph-id vertex-id vertex-id2) 
          *edges*) 
         1)))

(defun delete-edges (graph-id)
  (maphash 
    (lambda (k v) 
      (when (and 
             (listp k) 
             (eq (first k) 'edge) 
             (eq (second k) graph-id))
        (remhash k *edges*))) 
    *edges*))


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
  (format t "Edges: ~a~%" (graph-edges graph-id))
  (values))

; -------------------------- Algoritmo di MinHeap ---------------------------- ;
    

(defun new-heap (heap-id &optional (capacity 42))
   (or (gethash heap-id *heaps*))
   (setf (gethash heap-id *heaps*))
   (list ’heap heap-id 0 (make-array capacity)))


(defun heap-delete(heap-id)
    (clrhash heap-id *heaps*))

;; heap-empty    
