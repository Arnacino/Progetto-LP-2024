;;Ferrillo Samuele 900210
;;Antonico Lorenzo 904775


; -------------------------- Creazione Hash-Table ---------------------------- ;


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

(defun delete-vertices (graph-id)
  (maphash 
   (lambda (k v)
     (declare (ignore v)) 
     (when (and 
            (listp k) 
            (eq (first k) 'vertex) 
            (eq (second k) graph-id))
       (remhash k *vertices*))) 
   *vertices*))

(defun delete-edges (graph-id)
  (maphash 
   (lambda (k v)
     (declare (ignore v)) 
     (when (and 
            (listp k) 
            (eq (first k) 'edge) 
            (eq (second k) graph-id))
       (remhash k *edges*))) 
   *edges*))

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

(defun graph-vertices (graph-id)
  (let ((vertex-rep-list '()))
    (maphash (lambda (k v)
               (declare (ignore v))
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
       (list 'edge graph-id vertex-id vertex-id2 weight))
      (setf 
       (gethash (list 'edge graph-id vertex-id vertex-id2) 
                *edges*)
       (list 'edge graph-id vertex-id vertex-id2 1))))

(defun graph-edges (graph-id)
  (let ((vertex-rep-list '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and 
                      (listp k) 
                      (eq (first k) 'edge) 
                      (eq (second k) graph-id))
                 (push k vertex-rep-list)))
             *edges*)
    vertex-rep-list))

(defun get-weight (graph-id vertex-id vertex-id2)
  (if (not (and
            (is-vertex graph-id vertex-id) 
            (is-vertex graph-id vertex-id2)))
      (error "Vertex not found"))
  (fifth (gethash (list 'edge graph-id vertex-id vertex-id2) *edges*)))

(defun graph-vertex-neighbors (graph-id vertex-id) 
  (let ((neighbors '()))
    (maphash (lambda (k v)
               (when (and 
                      (listp k)
                      (eq (first k) 'edge)
                      (eq (second k) graph-id)
                      (eq (third k) vertex-id))
                 (push v neighbors))) *edges*)
    neighbors))

(defun graph-print (graph-id)
  (format t "Vertices: ~a~%" (graph-vertices graph-id))
  (format t "Edges: ~a~%" (graph-edges graph-id))
  (values))


; -------------------------- Algoritmo di MinHeap ---------------------------- ;


(defun new-heap (heap-id &optional capacity)
  (or (gethash heap-id *heaps*)
      (when (and (not (null capacity))
                 (numberp capacity)
                 (> capacity 0))
        (setf (gethash heap-id *heaps*) 
              (list 'heap heap-id 0 (make-array capacity :initial-element nil))))
      (setf (gethash heap-id *heaps*) 
            (list 'heap heap-id 0 (make-array 42 :initial-element nil)))))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

(defun heap-id (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (second (gethash heap-id *heaps*)))

(defun heap-size (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (third (gethash heap-id *heaps*)))

(defun heap-actual-heap (heap-id)
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (fourth (gethash heap-id *heaps*)))

(defun heap-length (heap-id)
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (length (heap-actual-heap heap-id)))

(defun set-heap-size (heap-id new-size)
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (if (> new-size (heap-length heap-id))
      (error "New size is greater than heap capacity"))
  (setf (third (gethash heap-id *heaps*)) new-size))

(defun heap-array-key (heap-id i)
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (first (aref (fourth (gethash heap-id *heaps*)) i)))

(defun heap-array-value (heap-id i)
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (second (aref (fourth (gethash heap-id *heaps*)) i)))

(defun heap-empty (heap-id)
  (if (not (gethash heap-id *heaps*))
      (error "Heap not found"))
  (= (third (gethash heap-id *heaps*)) 0))    

(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))

(defun heap-head (heap-id)
  (if (heap-not-empty heap-id)
      (aref (fourth (gethash heap-id *heaps*)) 0)
      (error "Heap is empty")))

(defun swap (heap-id i j)
  (let ((aux (aref (heap-actual-heap heap-id) i)))
    (setf (aref (heap-actual-heap heap-id) i) 
          (aref (heap-actual-heap heap-id) j))
    (setf (aref (heap-actual-heap heap-id) j) aux)))

(defun check-duplicate-value (arr value)
  (let ((lst (coerce arr 'list)))
    (when lst
      (if (equal (second (car lst)) value)
          value
          (check-duplicate-value (cdr lst) value)))))

(defun heapify-up (heap-id i)
  (when (/= i 0)
    (let ((parent (floor (/ (- i 1) 2))))   
      (when (< (heap-array-key heap-id i) 
               (heap-array-key heap-id parent))
        (swap heap-id i parent)
        (heapify-up heap-id parent)))))

(defun heap-insert (heap-id key value)
  (if (not (null (check-duplicate-value (heap-actual-heap heap-id) value)))
      (return-from heap-insert nil))
  (if (not (gethash heap-id *heaps*))
      (return-from heap-insert nil))
  (if (= (heap-size heap-id) (heap-length heap-id))
      (return-from heap-insert nil))
  (if (not (numberp key))
      (return-from heap-insert nil))
  (let ((i (heap-size heap-id)))
    (set-heap-size heap-id (+ (heap-size heap-id) 1))
    (setf (aref (heap-actual-heap heap-id) i) (list key value))
    (heapify-up heap-id i))
  (return-from heap-insert t))

(defun heapify-down (heap-id i)
  (when (> (heap-size heap-id) 1)
    (let ((leftc (+ (* i 2) 1))
          (rightc (+ (* i 2) 2)))
      (cond ((and 
              (not (null (aref (heap-actual-heap heap-id) leftc)))
              (not (null (aref (heap-actual-heap heap-id) rightc))))
             (let ((smaller-child (if (<= (heap-array-key heap-id leftc) 
                                          (heap-array-key heap-id rightc))
                                      leftc
                                      rightc)))
               (when (> (heap-array-key heap-id i) 
                        (heap-array-key heap-id smaller-child))
                 (swap heap-id i smaller-child)
                 (heapify-down heap-id smaller-child))))
            ((not (null (aref (heap-actual-heap heap-id) leftc)))
             (when (> (heap-array-key heap-id i) 
                      (heap-array-key heap-id leftc))
               (swap heap-id i leftc)
               (heapify-down heap-id leftc)))
            ((not (null (aref (heap-actual-heap heap-id) rightc)))
             (when (> (heap-array-key heap-id i) 
                      (heap-array-key heap-id rightc))
               (swap heap-id i rightc)
               (heapify-down heap-id rightc)))))))

(defun heap-extract (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (return-from heap-extract nil))
  (if (= (heap-size heap-id) (heap-length heap-id))
      (return-from heap-extract nil))
  (if (heap-empty heap-id)
      (return-from heap-extract nil))
  (set-heap-size heap-id (- (heap-size heap-id) 1))
  (let ((k (heap-array-key heap-id 0))
        (v (heap-array-value heap-id 0)))
    (setf (aref (heap-actual-heap heap-id) 0) nil)
    (setf (aref (heap-actual-heap heap-id) 0) 
          (aref (heap-actual-heap heap-id) (heap-size heap-id)))
    (setf (aref (heap-actual-heap heap-id) (heap-size heap-id)) nil)
    (heapify-down heap-id 0)
    (list k v)))

(defun modify-key (heap-id new-key old-key v)
  (if (not (gethash heap-id *heaps*))
      (return-from modify-key nil))
  (if (heap-empty heap-id)
      (return-from modify-key nil))
  (if (not (numberp new-key))
      (return-from modify-key nil))
  (if (not (numberp old-key))
      (return-from modify-key nil))
  (let ((i (position 
            (list old-key v) 
            (heap-actual-heap heap-id) :test #'equal)))
    (if (null i)
        (return-from modify-key nil)
        (setf (aref (heap-actual-heap heap-id) i) (list new-key v)))
    (if (< new-key old-key)
        (heapify-up heap-id i)
        (heapify-down heap-id i))
    (return-from modify-key t)))

(defun heap-print (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (return-from heap-print nil))
  (format t "Heap: ~a~%" (heap-actual-heap heap-id))
  (values))

; --------------------------- Algoritmo di Dijkstra ------------------------- ;


(defun sssp-dist (graph-id vertex-id)
  (is-vertex graph-id vertex-id)
  (values (gethash (list graph-id vertex-id) *distances*)))

(defun sssp-previous (graph-id vertex-id)
  (is-vertex graph-id vertex-id)
  (values (gethash (list graph-id vertex-id) *previous*)))

(defun sssp-visited (graph-id vertex-id)
  (is-vertex graph-id vertex-id)
  (let ((visited (gethash (list graph-id vertex-id) *visited*)))
    (if visited T NIL)))

(defun sssp-change-dist (graph-id vertex-id new-dist) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *distances*) new-dist))

(defun sssp-change-previous (graph-id vertex-id new-previous) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *previous*) new-previous))

(defun sssp-set-visited (graph-id vertex-id) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*) t))

(defun sssp-set-not-visited (graph-id vertex-id) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*) nil))

(defun sssp-is-visited (graph-id vertex-id) 
  (is-vertex graph-id vertex-id)
  (gethash (list graph-id vertex-id) *visited*))

(defun sssp-reset (graph-id &optional vertices)
  (if vertices
      (progn
        (sssp-reset graph-id (cdr vertices))
        (sssp-change-dist graph-id (third (car vertices)) nil)
        (sssp-set-not-visited graph-id (third (car vertices))))
    (progn
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (and (listp k) (eq (second k) graph-id))
                   (sssp-change-dist graph-id (third k) nil)
                   (sssp-set-not-visited graph-id (third k))))
               *vertices*))))

(defun sssp-init-distance (graph-id vertices source)
  (mapc (lambda (vertex)
          (let ((v (third vertex)))
            (if (equal v source)
                (progn
                  (sssp-change-dist graph-id source 0)
                  (heap-insert graph-id 0 source))
              (progn
                (sssp-change-dist graph-id v 1000000)
                (heap-insert graph-id 1000000 v)))))
        vertices))

(defun process-neighbors (graph-id vertex-id neighbors)
  (is-vertex graph-id vertex-id)
  (mapc #'(lambda (pair)
            (let* ((k (fifth pair))
                   (vDest (fourth pair))
                   (newD (+  
                          (gethash (list graph-id vertex-id) *distances*) 
                          (get-weight graph-id vertex-id vDest)))
                   (oldD (gethash (list graph-id vDest) *distances*)))
              (declare (ignore k))
              (when (< newD oldD)
                (sssp-change-dist graph-id vDest newD)
                (sssp-change-previous graph-id vDest vertex-id)
                (modify-key graph-id newD oldD vDest))))
       neighbors))

(defun dijkstra (graph-id heap-id)
  (if (heap-empty heap-id)
      (return-from dijkstra nil))
  (let* 
      ((vertex (second (heap-head heap-id)))
       (neighbors (graph-vertex-neighbors graph-id vertex)))
    (process-neighbors graph-id vertex neighbors)
    (sssp-set-visited graph-id vertex)
    (heap-extract heap-id)
    (dijkstra graph-id heap-id)))

(defun sssp-dijkstra (graph-id source)
  (is-vertex graph-id source)
  (sssp-reset graph-id)
  (new-heap graph-id)
  (let ((heap-id (heap-id graph-id))
        (vertices (graph-vertices graph-id))
        (neighbors (graph-vertex-neighbors graph-id source)))
    (sssp-init-distance graph-id vertices source)
    (process-neighbors graph-id source neighbors)
    (heap-extract heap-id)
    (sssp-set-visited graph-id source)
    (dijkstra graph-id heap-id)))

(defun build-path (graph-id current-v previous-v path)
  (is-vertex graph-id current-v)
  (is-vertex graph-id previous-v)
  (if (equal current-v previous-v)
      path
    (let ((edge (gethash (list 'edge graph-id previous-v current-v) *edges*)))
      (when edge
        (push edge path))
      (build-path graph-id previous-v (sssp-previous graph-id previous-v) path))))

(defun shortest-path (graph-id source vertex-id)
  (is-vertex graph-id source)
  (is-vertex graph-id vertex-id)
  (sssp-dijkstra graph-id source)
  (let ((path '()))
    (build-path graph-id source vertex-id path)))


; ---------------------------------- TESTS ---------------------------------- ;

(defun print-hash-table (hash-table)
    (maphash (lambda (k v)
               (format t "~a -> ~a~%" k v))
             hash-table))
    
(defun test1 ()
    (new-graph 'g1)
    (new-vertex 'g1 's)
    (new-vertex 'g1 'a)
    (new-vertex 'g1 'b)
    (new-vertex 'g1 'c)
    (new-vertex 'g1 'd)
    (new-vertex 'g1 'e)
    (new-vertex 'g1 'f)

    (new-edge 'g1 's 'a 2.5)
    (new-edge 'g1 's 'd 8)
    (new-edge 'g1 'd 'e 3)
    (new-edge 'g1 'd 'c 2)
    (new-edge 'g1 'a 'c 2)
    (new-edge 'g1 'a 'b 6)
    (new-edge 'g1 'b 'f 5)
    (new-edge 'g1 'e 'f 1)
    (new-edge 'g1 'c 'e 9)
    
    (print-hash-table *vertices*)
    (print-hash-table *edges*)
    (print-hash-table *graphs*)
    (print-hash-table *visited*)
    (print-hash-table *distances*)
    (print-hash-table *previous*)
    (print-hash-table *heaps*))

(defun run-tests ()
  ;; Create a new heap
  (new-heap 'heap1)

  ;; Insert some elements
  (heap-insert 'heap1 2131 'd)
  (heap-insert 'heap1 12 'e)
  (heap-insert 'heap1 3 'f)
  (heap-insert 'heap1 1231 'a)
  (heap-insert 'heap1 323 'b)
  (heap-insert 'heap1 44 'c)
  (heap-insert 'heap1 300 'g)
  (heap-insert 'heap1 40 'h)
  (heap-insert 'heap1 1200 'i)

  (heap-actual-heap 'heap1))

(defun test2 ()
  ;; Create a new heap
  (new-heap 'h1)

  ;; Insert some elements
  (heap-insert 'h1 66 'a)
  (heap-insert 'h1 92 'b)
  (heap-insert 'h1 2 'c)
  (heap-insert 'h1 40 'd)
  (heap-insert 'h1 100 'e)
  (heap-insert 'h1 12 'f)
  (heap-insert 'h1 82 'g)
  (heap-insert 'h1 73 'h)
  (heap-insert 'h1 97 'i)
  (heap-insert 'h1 23 'j)

  (heap-actual-heap 'h1))
