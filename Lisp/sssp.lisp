;;Ferrillo Samuele 900210
;;Antonico Lorenzo 904775


;; -------------------- Creazione Hash-Table ----------------------- ;;


(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))


;; ---------------- Creazione e manipolazione grafi ----------------- ;;


;; Controlla se un grafo è presente nella tabella hash
(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

;; Crea un nuovo grafo
(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

;; Cancella tutti i vertici di un grafo
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

;; Cancella tutti gli archi di un grafo
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

;; Cancella un grafo intero, archi e vertici inclusi
(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (delete-vertices graph-id)
  (delete-edges graph-id))

;; Crea un nuovo vertice
(defun new-vertex (graph-id vertex-id)
  (if (not (is-graph graph-id))
      (error "Graph not found"))
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*) t)
  (list 'vertex graph-id vertex-id))


;; Controlla se un vertice è presente in un grafo
(defun is-vertex (graph-id vertex-id)
  (and (is-graph graph-id)
       (not (null (gethash (list 'vertex graph-id vertex-id) *vertices*))))) 

;; Ritorna una lista di vertici nel grafo
(defun graph-vertices (graph-id)
  (let ((vertex-list '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (listp k) (eq (first k) 'vertex) (eq (second k) 
							       graph-id))
                 (setf vertex-list (cons k vertex-list))))
             *vertices*)
    vertex-list))

;; Crea un nuovo arco
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

;; Ritorna una lista di archi nel grafo
(defun graph-edges (graph-id)
  (let ((edge-list '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and 
                      (listp k) 
                      (eq (first k) 'edge) 
                      (eq (second k) graph-id))
                 (setf edge-list (cons k edge-list))))
             *edges*)
    edge-list))

;; Ritorna il peso di un arco
(defun get-weight (graph-id vertex-id vertex-id2)
  (if (not (and
            (is-vertex graph-id vertex-id) 
            (is-vertex graph-id vertex-id2)))
      (error "Vertex not found"))
  (fifth (gethash (list 'edge graph-id vertex-id vertex-id2) *edges*)))


;; Ritorna una lista di vertici adiacenti ad un vertice
(defun graph-vertex-neighbors (graph-id vertex-id) 
  (let ((neighbors '()))
    (maphash (lambda (k v)
               (when (and 
                      (listp k)
                      (eq (first k) 'edge)
                      (eq (second k) graph-id)
                      (eq (third k) vertex-id))
                 (setf neighbors (cons v neighbors)))) *edges*)
    neighbors))

;; Stampa un grafo
(defun graph-print (graph-id)
  (format t "Vertices: ~a~%" (graph-vertices graph-id))
  (format t "Edges: ~a~%" (graph-edges graph-id))
  (values))


;; -------------- Implementazione MinHeap ---------------- ;;


;; Crea una nuova heap
(defun new-heap (heap-id &optional capacity)
  (or (gethash heap-id *heaps*)
      (when (and (not (null capacity))
                 (numberp capacity)
                 (> capacity 0))
        (setf (gethash heap-id *heaps*) 
              (list 'heap heap-id 0 (make-array capacity :initial-element 
						nil))))
      (setf (gethash heap-id *heaps*) 
            (list 'heap heap-id 0 (make-array 42 :initial-element nil)))))

;; Cancella una heap
(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

;; Ritorna l'id della heap
(defun heap-id (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (return-from heap-id nil))
  (second (gethash heap-id *heaps*)))

;; Ritorna la dimensione attuale della heap
(defun heap-size (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (return-from heap-size nil))
  (third (gethash heap-id *heaps*)))

;; Ritorna l'array che corrisponde all'heap
(defun heap-actual-heap (heap-id)
  (if (not (gethash heap-id *heaps*))
      (return-from heap-actual-heap nil))
  (fourth (gethash heap-id *heaps*)))

;; Ritorna la capacità totale della heap
(defun heap-length (heap-id)
  (if (not (gethash heap-id *heaps*))
      (return-from heap-length nil))
  (length (heap-actual-heap heap-id)))

;; Imposta la nuova dimensione della heap
(defun set-heap-size (heap-id new-size)
  (if (not (gethash heap-id *heaps*))
      (return-from set-heap-size nil))
  (if (> new-size (heap-length heap-id))
      (error "New size is greater than heap capacity"))
  (setf (third (gethash heap-id *heaps*)) new-size))

;; Ritorna la chiave di un elemento della heap
(defun heap-array-key (heap-id i)
  (if (not (gethash heap-id *heaps*))
      (return-from heap-array-key nil))
  (first (aref (heap-actual-heap heap-id) i)))

;; Ritorna il valore di un elemento della heap
(defun heap-array-value (heap-id i)
  (if (not (gethash heap-id *heaps*))
      (return-from heap-array-value nil))
  (second (aref (heap-actual-heap heap-id) i)))

;; Ritorna T se la heap è vuota
(defun heap-empty (heap-id)
  (if (not (gethash heap-id *heaps*))
      (return-from heap-empty nil))
  (= (third (gethash heap-id *heaps*)) 0))    

;; Ritorna T se la heap non è vuota
(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))

;; Ritorna la testa della heap senza cancellarla
(defun heap-head (heap-id)
  (if (heap-not-empty heap-id)
      (aref (heap-actual-heap heap-id) 0)
      (error "Heap is empty")))

;; Scambia di posizione due elementi della heap dati gli indici
(defun swap (heap-id i j)
  (let ((aux (aref (heap-actual-heap heap-id) i)))
    (setf (aref (heap-actual-heap heap-id) i) 
          (aref (heap-actual-heap heap-id) j))
    (setf (aref (heap-actual-heap heap-id) j) aux)))

;; Controlla se un valore è duplicato nella heap
(defun check-duplicate-value (arr value)
  (let ((lst (coerce arr 'list)))
    (when lst
      (if (equal (second (car lst)) value)
          value
          (check-duplicate-value (cdr lst) value)))))

;; Aggiusta la heap dopo l'inserimento di un nuovo elemento
(defun heapify-up (heap-id i)
  (when (/= i 0)
    (let ((parent (floor (/ (- i 1) 2))))   
      (when (< (heap-array-key heap-id i) 
               (heap-array-key heap-id parent))
        (swap heap-id i parent)
        (heapify-up heap-id parent)))))

;; Inserisce un elemento nella heap
(defun heap-insert (heap-id key value)
  (if (not (gethash heap-id *heaps*))
      (return-from heap-insert nil))
  (if (not (null (check-duplicate-value (heap-actual-heap heap-id) value)))
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

;; Aggiusta la heap dopo l'estrazione di un elemento
(defun heapify-down (heap-id i)
  (when (> (heap-size heap-id) 1)
    (let ((leftc (+ (* i 2) 1))
          (rightc (+ (* i 2) 2)))
      (cond ((and 
              (< leftc (heap-size heap-id))
              (< rightc (heap-size heap-id)))
             (let ((smaller-child (if (<= (heap-array-key heap-id leftc) 
                                          (heap-array-key heap-id rightc))
                                      leftc
                                      rightc)))
               (when (> (heap-array-key heap-id i) 
                        (heap-array-key heap-id smaller-child))
                 (swap heap-id i smaller-child)
                 (heapify-down heap-id smaller-child))))
            ((and (< leftc (heap-size heap-id))
                  (> (heap-array-key heap-id i) 
                     (heap-array-key heap-id leftc)))
             (swap heap-id i leftc)
             (heapify-down heap-id leftc))
            ((and (< rightc (heap-size heap-id))
                  (> (heap-array-key heap-id i) 
                     (heap-array-key heap-id rightc)))
             (swap heap-id i rightc)
             (heapify-down heap-id rightc))))))

;; Estrae un elemento dalla heap
(defun heap-extract (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (progn
        (print "Heap not found")
        (return-from heap-extract nil)))
  (if (heap-empty heap-id)
      (progn
        (print "Heap is empty")
        (return-from heap-extract nil)))
  (set-heap-size heap-id (- (heap-size heap-id) 1))
  (let ((k (heap-array-key heap-id 0))
        (v (heap-array-value heap-id 0)))
    (setf (aref (heap-actual-heap heap-id) 0) nil)
    (setf (aref (heap-actual-heap heap-id) 0) 
          (aref (heap-actual-heap heap-id) (heap-size heap-id)))
    (setf (aref (heap-actual-heap heap-id) (heap-size heap-id)) nil)
    (heapify-down heap-id 0)
    (list k v)))

;; Modifica la chiave di un elemento della heap
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

;; Stampa una heap
(defun heap-print (heap-id) 
  (if (not (gethash heap-id *heaps*))
      (return-from heap-print nil))
  (format t "Heap: ~a~%" (heap-actual-heap heap-id))
  (values))


;; -------------------- Algoritmo di Dijkstra ------------------ ;;


;; Ritorna la distanza di un vertice dalla sorgente
(defun sssp-dist (graph-id vertex-id)
  (is-vertex graph-id vertex-id)
  (values (gethash (list graph-id vertex-id) *distances*)))

;; Ritorna il predecessore di un vertice 
(defun sssp-previous (graph-id vertex-id)
  (is-vertex graph-id vertex-id)
  (values (gethash (list graph-id vertex-id) *previous*)))

;; Ritorna T se un vertice è stato visitato
(defun sssp-visited (graph-id vertex-id)
  (is-vertex graph-id vertex-id)
  (let ((visited (gethash (list graph-id vertex-id) *visited*)))
    (if visited T NIL)))

;; Cambia la distanza di un vertice dalla sorgente
(defun sssp-change-dist (graph-id vertex-id new-dist) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *distances*) new-dist))

;; Cambia il predecessore di un vertice
(defun sssp-change-previous (graph-id vertex-id new-previous) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *previous*) new-previous))

;; Imposta un vertice come visitato
(defun sssp-set-visited (graph-id vertex-id) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*) t))

;; Imposta un vertice come non visitato
(defun sssp-set-not-visited (graph-id vertex-id) 
  (is-vertex graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*) nil))

;; Ritorna T se un vertice è stato visitato
(defun sssp-is-visited (graph-id vertex-id) 
  (is-vertex graph-id vertex-id)
  (gethash (list graph-id vertex-id) *visited*))

;; Cancella tutte le distanze, i predecessori e i vertici visitati
(defun sssp-reset (graph-id)
  (progn
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (listp k) (eq (second k) graph-id))
                 (sssp-change-dist graph-id (third k) nil)
                 (sssp-set-not-visited graph-id (third k))))
             *vertices*)))

;; Inizializza le distanze dei vertici
(defun sssp-init-distance (graph-id vertices source)
  (mapc (lambda (vertex)
          (let ((v (third vertex)))
            (if (equal v source)
                (progn
                  (sssp-change-dist graph-id source 0)
                  (heap-insert graph-id 0 source))
		(progn
                  (sssp-change-dist graph-id v most-positive-double-float)
                  (heap-insert graph-id most-positive-double-float v)))))
        vertices))

;; Processa le nuove distanze dei vertici adiacenti ad un vertice
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

;; Algoritmo di Dijkstra (parte ricorsiva)
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

;; Algoritmo di Dijkstra (parte principale)
(defun sssp-dijkstra (graph-id source)
  (is-vertex graph-id source)
  (sssp-reset graph-id)
  (if (heap-id graph-id)
      (heap-delete (heap-id graph-id)))
  (new-heap graph-id (length (graph-vertices graph-id)))
  (let ((heap-id (heap-id graph-id))
        (vertices (graph-vertices graph-id))
        (neighbors (graph-vertex-neighbors graph-id source)))
    (sssp-init-distance graph-id vertices source)
    (process-neighbors graph-id source neighbors)
    (heap-extract heap-id)
    (sssp-set-visited graph-id source)
    (dijkstra graph-id heap-id)))

;; Costruisce il percorso minimo dalla sorgente a un vertice
(defun build-path (graph-id current-v previous-v path &optional start)
  (is-vertex graph-id current-v)
  (is-vertex graph-id previous-v)
  (if (equal current-v previous-v)
      path
      (let ((edge (gethash (list 'edge graph-id previous-v current-v) *edges*)))
	(if (and edge (not start))
            (build-path graph-id previous-v (sssp-previous graph-id previous-v) 
			(cons edge path))
            (build-path graph-id previous-v (sssp-previous graph-id previous-v) 
			path)))))

;; Ritorna il percorso minimo dalla sorgente a un vertice
(defun shortest-path (graph-id source dest)
  (is-vertex graph-id source)
  (is-vertex graph-id dest)
  (sssp-dijkstra graph-id source)
  (let ((path '()))
    (build-path graph-id source dest path T)))
