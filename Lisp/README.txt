Progetto scritto da: 
    Ferrillo Samuele 
    Antonico Lorenzo 


~~~~~~~~~~~~~~~~~~~~~~~~~~~ Dichiarazione HashTables ~~~~~~~~~~~~~~~~~~~~~~~~~~~


(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

    Queste sono le variabili globali inizializzate come HashTables. 
    Vengono usate per salvare dinamicamente e controllare tutte le funzioni 
    dichiarate sotto.


~~~~~~~~~~~~~~~~~~~~~~~~~~~ Manipolazione di grafi ~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(is-graph graph-id)

    Questa funzione verifica se un grafo esiste nella HashTable *graphs*.
    Prende come argomento l'id del grafo da verificare graph-id.
    Ritorna NIL se il grafo non esiste, altrimenti ritorna il grafo
    corrispondente a graph-id.


(new-graph graph-id)

    Questa funzione crea un nuovo grafo e lo aggiunge alla HashTable dei grafi, 
    se non esiste già. Prende come argomento l'id del grafo da creare graph-id.
    Se il grafo con l'id fornito esiste già nella HashTable dei grafi, 
    la funzione ritorna il grafo esistente. 
    Altrimenti, crea un nuovo grafo con l'id fornito, 
    lo aggiunge alla HashTable dei grafi, e ritorna il nuovo grafo.


(delete-vertices graph-id)

    Questa funzione elimina tutti i vertici di un grafo specifico 
    dalla HashTable dei vertici. 
    Prende come argomento l'id del grafo da cui eliminare i vertici graph-id.
    La funzione scorre tutte le voci nella HashTable dei vertici.
    Per ogni elemento, se la chiave è una lista che rappresenta un vertice del 
    grafo specificato, allora l'elemento viene rimosso dalla HashTable
    dei vertici.


(delete-edges graph-id)

    Questa funzione elimina tutti gli archi di un grafo specifico dalla
    HashTable degli archi. 
    Prende come argomento l'id del grafo da cui eliminare gli archi graph-id.
    La funzione scorre tutti gli elementi nella hashtable degli archi. 
    Per ogni elemento, se la chiave è una lista che rappresenta 
    un arco del grafo specificato, allora la voce viene rimossa 
    dalla HashTable degli archi.


(delete-graph graph-id)

    Questa funzione elimina un grafo specifico e tutti i suoi vertici e archi
    dalle rispettive HashTables. 
    Prende come argomento l'id del grafo da eliminare graph-id.


(new-vertex graph-id vertex-id)

    Questa funzione crea un nuovo vertice e lo aggiunge alla HashTable dei
    vertici se non esiste già.
    Prende come argomenti l'id del grafo graph-id in cui inserire 
    il vertice e l'id del vertice da creare vertex-id.
    La funzione aggiunge il nuovo vertice alla hashtable *vertices* con la
    chiave come una lista composta dal simbolo 'vertex, l'id del grafo
    e l'id del vertice. Il valore associato a questa chiave è T.

(is-vertex graph-id vertex-id)

    Questa funzione verifica se un vertice esiste in un grafo specifico. 
    Prende come argomenti l'id del grafo graph-id
    e l'id del vertice da verificare vertex-id.
    La funzione prima verifica se il grafo esiste utilizzando la funzione
    (is-graph). 
    Se il grafo esiste, cerca il vertice nella hash table *vertices*.
    Ritorna T se il vertice esiste nel grafo, altrimenti ritorna NIL.


(graph-vertices graph-id)

    Questa funzione restituisce una lista di tutti i vertici
    in un grafo specifico. 
    Prende come argomento l'id del grafo da cui ottenere i vertici graph-id.
    La funzione inizia creando una lista vuota vertex-list 
    per contenere i vertici. 
    Poi, scorre tutti gli elementi nella hash table *vertices* utilizzando la
    funzione maphash. 
    Per ogni elemento, se la chiave è una lista che rappresenta 
    un vertice del grafo specificato, allora la chiave viene
    aggiunta alla lista vertex-list.
    Ritorna la lista vertex-list che contiene le rappresentazioni 
    di tutti i vertici nel grafo specificato.


(new-edge graph-id vertex-id vertex-id2 &optional weight)

    Questa funzione crea un nuovo arco tra due vertici e lo aggiunge
    alla HashTable degli archi. 
    Prende come argomenti l'id del grafo graph-id, 
    gli id dei due vertici vertex-id e vertex-id2 
    e un peso opzionale weight (impostato ad 1 se omesso).
    La funzione prima verifica se entrambi i vertici esistono nel grafo
    utilizzando la funzione is-vertex.
    Se entrambi i vertici esistono, crea l'arco e lo aggiunge alla hashtable
    *edges* con la chiave come una lista composta dal simbolo 'edge, l'id del
    grafo e gli id dei due vertici. 
    Il valore associato a questa chiave è una lista che rappresenta l'arco,
    composta dal simbolo 'edge, l'id del grafo, gli id dei due vertici
    e il peso dell'arco che li connette.


(graph-edges graph-id)

    Questa funzione restituisce una lista di tutti gli archi in un grafo
    specifico.
    Prende come argomento l'id del grafo graph-id da cui ottenere gli archi.
    La funzione inizia creando una lista vuota vertex-list per contenere
    tutti gli archi.
    Poi, scorre tutti gli elementi nella hash table *edges* utilizzando
    la funzione maphash.
    Per ogni elemento, se la chiave è una lista che rappresenta un bordo 
    del grafo specificato, allora la chiave viene aggiunta a vertex-list.
    Ritorna la lista vertex-list che contiene tutti gli archi nel grafo
    specificato.


(get-weight graph-id vertex-id vertex-id2)

    Questa funzione restituisce il peso dell'arco tra due vertici in un grafo
    specifico. 
    Prende come argomenti l'id del grafo graph-id 
    e gli id dei due vertici vertex-id e vertex-id2.
    La funzione prima verifica se entrambi i vertici esistono nel grafo
    utilizzando la funzione is-vertex. 
    Se uno dei vertici non esiste, la funzione restituirà un errore 
    "Vertex not found".
    Se entrambi i vertici esistono, la funzione cerca l'arco nella hash table
    *edges*.
    Il peso dell'arco è la quinta componente del valore associato 
    a questa chiave.
    Ritorna il peso dell'arco tra i due vertici nel grafo specificato.


(graph-vertex-neighbors graph-id vertex-id)

    Questa funzione restituisce una lista di tutti i nodi adiacenti di un
    vertice specifico in un grafo specifico. 
    Prende come argomenti l'id del grafo graph-id 
    e l'id del vertice da cui prendere i nodi vertex-id.
    La funzione inizia creando una lista vuota neighbors per contenere 
    i nodi adiacenti al vertice. 
    Poi, scorre tutti gli elementi nella hash table *edges* 
    utilizzando la funzione maphash.
    Per ogni elemento, se la chiave è una lista che rappresenta 
    un arco del grafo specificato che parte dal vertice specificato,
    allora il valore (che rappresenta l'arco) viene aggiunto a neighbors.
    Infine ritorna la lista neighbors.


(graph-print graph-id)

    Questa funzione stampa a schermo i vertici e i bordi di un grafo specifico. 


~~~~~~~~~~~~~~~~~~~~~~~~~~~ Implementazione minHeap ~~~~~~~~~~~~~~~~~~~~~~~~~~~


(new-heap heap-id &optional capacity)

    Questa funzione crea un nuovo heap e lo aggiunge al database degli heap. 
    Prende come argomenti l'id dell'heap heap-id
    e una capacità opzionale capacity (impostata a 42 come predefinito).
    La funzione prima verifica se l'heap esiste già nel database degli heap
    utilizzando la funzione gethash con l'id dell'heap come chiave.
    Se l'heap esiste già, la funzione ritorna la rappresentazione 
    dell'heap esistente.
    Se l'heap non esiste, la funzione crea un nuovo heap. 
    Se la capacità è fornita ed è un numero positivo, la funzione 
    crea un array con quella capacità come dimensione. 
    Altrimenti, crea un array con dimensione 42.
    L'array viene inizializzato con nil in tutte le sue posizioni.
    Infine, la funzione aggiunge il nuovo heap al database degli heap 
    con l'id dell'heap come chiave e una lista che rappresenta l'heap 
    come valore. 
    La lista è composta dal simbolo heap, l'id dell'heap, 
    il numero 0 (che rappresenta il numero di elementi nell'heap) e l'array.
    Infine ritorna la lista che rappresenta il nuovo heap.


(heap-delete heap-id)

    Questa funzione elimina un heap specifico dal database degli heap.
    Prende come argomento l'id dell'heap heap-id da eliminare.
    La funzione utilizza la funzione remhash per rimuovere l'heap 
    dalla HashTable degli heap. 


(heap-id heap-id)

    Questa funzione restituisce l'id di un heap specifico se
    esiste nel database degli heap.
    Prende come argomento l'id dell'heap heap-id da verificare.
    La funzione prima verifica se l'heap esiste nella HashTable degli heap
    utilizzando la funzione gethash con l'id dell'heap come chiave.
    Se l'heap non esiste, la funzione ritorna nil.
    Se l'heap esiste, la funzione ritorna l'id dell'heap.

(heap-size heap-id)

    Questa funzione restituisce la dimensione di un heap specifico se esiste
    nel database degli heap.
    Prende come argomento l'id dell'heap heap-id da cui ottenere la dimensione.
    La funzione prima verifica se l'heap esiste nella HashTable degli heap
    utilizzando la funzione gethash con l'id dell'heap come chiave.
    Se l'heap non esiste, la funzione ritorna nil.
    Se l'heap esiste, la funzione ritorna la dimensione dell'heap.


(heap-actual-heap heap-id)

    Questa funzione restituisce l'array di una heap specifica se esiste
    nel database degli heap.
    Prende come argomento l'id dell'heap heap-id da cui ottenere l'array.
    La funzione prima verifica se l'heap esiste nella HashTable degli heap
    utilizzando la funzione gethash con l'id dell'heap come chiave.
    Se l'heap non esiste, la funzione ritorna nil.
    Se l'heap esiste, la funzione ritorna l'array corrispondente alla chiave.


(heap-length heap-id)

    Questa funzione restituisce la dimensione massima di un heap specifico
    se esiste nel database degli heap.
    Prende come argomento l'id dell'heap heap-id da cui ottenere la dimensione.
    La funzione prima verifica se l'heap esiste nella HashTable degli heap
    utilizzando la funzione gethash con l'id dell'heap come chiave.
    Se l'heap non esiste, la funzione ritorna nil.
    Se l'heap esiste, la funzione ritorna la dimensione massima dell'heap.


(set-heap-size heap-id new-size)

    Questa funzione imposta la dimensione di un heap specifico.
    Prende come argomenti l'ID dell'heap heap-id
    e la nuova dimensione new-size.
    La funzione prima verifica se l'heap esiste nella HashTable degli heap
    utilizzando la funzione gethash con l'ID dell'heap come chiave. 
    Se l'heap non esiste, la funzione ritorna nil.
    Se l'heap esiste, la funzione verifica se la nuova dimensione è maggiore
    della capacità attuale dell'heap utilizzando la funzione heap-length.
    Se la nuova dimensione è maggiore, la funzione genera un errore 
    "New size is greater than heap capacity".
    Se la nuova dimensione non è maggiore della capacità attuale, 
    la funzione imposta la dimensione dell'heap alla nuova dimensione.


(heap-array-key heap-id i)

    Questa funzione restituisce la chiave del nodo 
    all'indice i di un heap specifico.
    Prende come argomenti l'ID dell'heap heap-id e l'indice i.
    La funzione prima verifica se l'heap esiste nel database degli heap
    utilizzando la funzione gethash con l'ID dell'heap come chiave.
    Se l'heap non esiste, la funzione ritorna nil.
    Se l'heap esiste, la funzione ottiene l'heap effettivo utilizzando
    la funzione heap-actual-heap. 
    Quindi, ottiene il nodo all'indice i dell'heap effettivo 
    utilizzando la funzione aref. 
    Infine, ottiene la chiave del nodo utilizzando la funzione first.
    Ritorna la chiave del nodo all'indice i dell'heap.


(heap-array-key heap-id i)

    Uguale ad heap-array-key ma utilizza second per ottenere il value


(heap-empty heap-id)

    Questa funzione verifica se un heap specifico è vuoto. 
    Prende come argomento l'ID dell'heap heap-id.
    La funzione prima verifica se l'heap esiste nella HashTable degli heap
    utilizzando la funzione gethash con l'ID dell'heap come chiave.
    Se l'heap non esiste, la funzione ritorna nil.
    Se l'heap esiste, la funzione verifica se la dimensione dell'heap è zero.
    Se la dimensione dell'heap è zero, la funzione ritorna t,
    altrimenti ritorna nil.


(heap-not-empty heap-id)

    Questa funzione verifica se un heap specifico non è vuoto.
    Prende come argomento l'ID dell'heap heap-id.
    La funzione utilizza la funzione heap-empty per verificare se l'heap è
    vuoto, e poi inverte il suo risultato (da T a NIL e da NIL a T).


(heap-head heap-id)

    Questa funzione restituisce l'elemento in cima a un heap specifico.
    Prende come argomento l'ID dell'heap (heap-id).
    La funzione prima verifica se l'heap non è vuoto utilizzando 
    la funzione heap-not-empty.
    Se l'heap è vuoto, la funzione genera un errore "Heap is empty".
    Se l'heap non è vuoto, la funzione ottiene l'heap effettivo utilizzando 
    la funzione heap-actual-heap.
    Quindi, ottiene l'elemento in cima all'heap 
    (l'elemento all'indice 0) utilizzando la funzione aref.
    Ritorna l'elemento in cima all'heap.


(swap heap-id i j)

    Questa funzione scambia due nodi in un heap specifico.
    Prende come argomenti l'ID dell'heap heap-id
    e gli indici dei due nodi da scambiare i e j.
    La funzione prima ottiene l'heap effettivo utilizzando la funzione
    heap-actual-heap. 
    Quindi, utilizza la funzione aref per ottenere i nodi agli indici i e j
    dell'heap effettivo. Infine, utilizza la funzione setf per scambiare i nodi.


(check-duplicate-value arr value)

    Questa funzione verifica se un valore specifico è presente in un array.
    Prende come argomenti l'array arr e il valore da cercare value.
    La funzione prima converte l'array in una lista utilizzando la funzione
    coerce. 
    Quindi, se la lista non è vuota, controlla se il value del primo nodo della
    lista è uguale al value cercato utilizzando la funzione equal. 
    Se è uguale, ritorna il valore. 
    Altrimenti, chiama se stessa ricorsivamente con il resto della lista 
    e il value come argomenti.
    Ritorna il valore se è presente nell'array, altrimenti ritorna nil.


(heapify-up heap-id i)

    Questa funzione riorganizza un heap specifico in modo che mantenga la proprietà di minheap. 
    Prende come argomenti l'ID dell'heap heap-id 
    e l'indice del nodo da riorganizzare i.
    La funzione verifica prima se l'indice i non è zero.
    Se i è zero, la funzione ritorna immediatamente, 
    poiché un nodo all'indice zero non ha genitori.
    Se i non è zero, la funzione calcola l'indice del genitore del nodo
    utilizzando la formula (floor (/ (- i 1) 2)). 
    Quindi, verifica se la chiave del nodo è minore della chiave del suo
    genitore utilizzando la funzione heap-array-key. 
    Se la chiave del nodo è minore, la funzione scambia il nodo con il suo
    genitore utilizzando la funzione swap e poi chiama se stessa ricorsivamente
    con l'ID dell'heap e l'indice del genitore come argomenti.
    Ritorna nil dopo aver riorganizzato l'heap.


(heap-insert heap-id key value)

    Questa funzione inserisce un nuovo nodo in un heap specifico.
    Prende come argomenti l'ID dell'heap heap-id, 
    la chiave del nuovo nodo key 
    e il valore del nuovo nodo value.
    La funzione prima verifica se l'heap esiste utilizzando
    la funzione gethash.
    Se l'heap non esiste, la funzione ritorna nil.
    Poi verifica se il valore non è già presente nell'heap
    utilizzando la funzione check-duplicate-value.
    Se il valore è già presente, la funzione ritorna nil.
    Poi, verifica se l'heap è pieno confrontando la dimensione dell'heap 
    con la sua lunghezza utilizzando le funzioni heap-size e heap-length.
    Se l'heap è pieno, la funzione ritorna nil.
    Inoltre, verifica se la chiave è un numero utilizzando la funzione numberp.
    Se la chiave non è un numero, la funzione ritorna nil.
    Se tutte le verifiche passano, la funzione inserisce il nuovo nodo
    all'ultimo posto dell'heap e incrementa la dimensione dell'heap di uno.
    Quindi, riorganizza l'heap utilizzando la funzione heapify-up.
    Ritorna t dopo aver inserito il nuovo nodo.


(heapify-down heap-id i)

    Questa funzione riorganizza un heap specifico in modo che mantenga 
    proprietà di minheap. 
    Prende come argomenti l'ID dell'heap heap-id 
    e l'indice del nodo da riorganizzare i.
    La funzione verifica prima se la dimensione dell'heap è maggiore di 1.
    Se la dimensione dell'heap è 1 o meno, la funzione ritorna immediatamente,
    poiché un heap di dimensione 1 o meno è già un minheap.
    Se la dimensione dell'heap è maggiore di 1, la funzione calcola gli indici
    dei figli sinistro e destro del nodo utilizzando le formule 
    (+ (* i 2) 1) e (+ (* i 2) 2) rispettivamente.
    Quindi, verifica se entrambi i figli esistono e se la chiave del nodo è
    maggiore della chiave del figlio minore. 
    Se è così, scambia il nodo con il figlio minore e chiama se stessa
    ricorsivamente con l'ID dell'heap e l'indice del figlio minore come
    argomenti.
    Se solo il figlio sinistro esiste e la chiave del nodo è maggiore della
    chiave del figlio sinistro, scambia il nodo con il figlio sinistro e chiama 
    se stessa ricorsivamente con l'ID dell'heap e l'indice del figlio sinistro 
    come argomenti.
    Se solo il figlio destro esiste e la chiave del nodo è maggiore della 
    chiave del figlio destro, scambia il nodo con il figlio destro e chiama se 
    stessa ricorsivamente con l'ID dell'heap e l'indice del figlio destro come 
    argomenti.
    Ritorna nil dopo aver riorganizzato l'heap.


(heap-extract heap-id)

    Questa funzione estrae il nodo con la chiave minima da un heap specifico. 
    Prende come argomento l'ID dell'heap (heap-id).
    La funzione verifica prima se l'heap esiste utilizzando la funzione 
    gethash.
    Se l'heap non esiste, stampa "Heap not found" e ritorna nil.
    Quindi, verifica se l'heap è vuoto utilizzando la funzione heap-empty.
    Se l'heap è vuoto, stampa "Heap is empty" e ritorna nil.
    Se l'heap esiste e non è vuoto, la funzione decrementa la dimensione 
    dell'heap di uno e salva la chiave e il valore del nodo all'indice 0 (il 
    nodo con la chiave minima).
    Poi, sposta l'ultimo nodo dell'heap all'indice 0 e rimuove l'ultimo nodo. 
    Quindi, riorganizza l'heap utilizzando la funzione heapify-down.
    Ritorna una lista contenente la chiave e il valore del nodo estratto.


(modify-key heap-id new-key old-key v)

    Questa funzione modifica la chiave di un nodo in un heap specifico.
    Prende come argomenti l'ID dell'heap heap-id,
    la nuova chiave new-key, la vecchia chiave old-key e il valore del nodo v.
    La funzione verifica prima se l'heap esiste, se l'heap è vuoto, e se sia la 
    nuova che la vecchia chiave sono numeri. 
    Se una di queste verifiche fallisce, la funzione ritorna nil.
    Quindi, cerca il nodo con la vecchia chiave e il valore specificati 
    nell'heap utilizzando la funzione position.
    Se il nodo non viene trovato, la funzione ritorna nil.
    Se il nodo viene trovato, la funzione modifica la chiave del nodo alla 
    nuova chiave utilizzando la funzione setf.
    Poi, verifica se la nuova chiave è minore della vecchia chiave.
    Se è così, riorganizza l'heap verso l'alto utilizzando la funzione 
    heapify-up.
    Altrimenti, riorganizza l'heap verso il basso utilizzando la funzione 
    heapify-down.
    Ritorna t dopo aver modificato la chiave del nodo.


(heap-print heap-id)

    Questa funzione stampa un heap specifico. 
    Prende come argomento l'ID dell'heap (heap-id).
    La funzione verifica prima se l'heap esiste utilizzando la funzione gethash. 
    Se l'heap non esiste, ritorna immediatamente nil.
    Se l'heap esiste, la funzione stampa l'heap utilizzando la funzione format. 


~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Algoritmo di Dijkstra ~~~~~~~~~~~~~~~~~~~~~~~~~~~~


