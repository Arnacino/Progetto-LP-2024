Progetto scritto da: 
    Ferrillo Samuele 
    Antonico Lorenzo 

~~~~~~~~~~~~~~~~~~~~~~~~~~~ Manipolazione di grafi ~~~~~~~~~~~~~~~~~~~~~~~~~~~~


new_graph(G)

    Crea un nuovo grafo se non esiste già un grafo con il medesimo nome


delete_graph(G)

    Cancella un grafo dalla base di dati, eliminando anche tutti 
    i suoi relativi vertici ed archi 


new_vertex(G, V)

    Controlla se il grafo preso in input esiste, in caso positivo controlla 
    che non esista già il vertice e lo inserisce nella base dati 


vertices(G, Vs)

    Ritorna true se tutti i vertici nella lista Vs appartengono al grafo G


list_vertices(G)

    stampa tutti i vertici del grafo G


new_edge(G, U, V)

    crea un arco (dato che il peso non è specificato lo inserisce con peso 1) 
    se non esiste già nella base dati


new_edge(G, U, V, Weight)

    crea un arco con peso Weight 
    (se non esiste già nella base dati un arco che va da U a V qualunque sia il suo peso)


edges(G, Es)

    ritorna true se tutti gli archi nella lista Es appartengono al grafo g


neighbors(G, V, Ns) 

    Ritorna true se Ns è una lista che contiene tutti gli 
    archi uscenti dal vertice V collegati direttamente a V


list_edges(G)

    Stampa tutti gli archi del grafo G


list_graph(G) 

    stampa tutti i vertici e tutti gli archi del grafo


~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Algoritmo di Dijkstra ~~~~~~~~~~~~~~~~~~~~~~~~~~~~


distance(G, V, D) 

    predicato che tiene nella base dati la distanza D dalla sorgente
    (specificata quando si esegue dijkstra_sssp(G, Source)) al vertice V
    (aggiornato durante l'algoritmo)


change_distance(G, V, NewD)

    cambia la distanza nella base dati di V con NewD. 
    per farlo rimuove tutte le istanze di distance e ne asserisce una nuova
    con valore di distanza NewD


previous(G, V, U)

    Predicato che salva nella base dati il vertice U precedente al vertice V
    (aggiornato durante l'algoritmo)


change_previous(G, V, NewU)

    Predicato che cambia previous nella base dati di V con NewU. 
    per farlo rimuove tutte le istanze di previous e ne asserisce una nuova
    con Vertice previous NewU


visited(G, V)

    Predicato che salva nella base di dati quando un vertice V viene visitato.
    (aggiornato durante l'algoritmo)


initialize_distance(G, [V | Rest])

    Questo predicato prende due argomenti: il grafo G e una lista di nodi Vs.
    Il predicato è definito da 3 casi: 
    Caso base. 
    Corrisponde a quando la lista dei nodi è vuota.
    In questo caso, il predicato semplicemente ha successo e non fa niente.
    Caso sorgente. 
    Controlla che il nodo sia la sorgente, 
    guardando se il nodo è stato visitato.
    In caso positivo il predicato imposta la distanza del nodo V
    a 0 e poi inizializza ricorsivamente le distanze per il resto dei nodi.
    Caso generale. 
    In questo caso, il predicato imposta la distanza del nodo V
    a inf (poiché non conosciamo ancora la distanza più breve a V)
    e poi inizializza ricorsivamente le distanze per il resto dei nodi.


initialize_heap(G, [])

    Questo predicato recupera la distanza corrente del nodo V nel grafo G,
    inizializzata con il predicato initialize_distance, poi inserisce V
    nell'heap con la sua distanza come chiave, 
    e infine inizializza ricorsivamente l'heap per il resto dei nodi.


dijkstra_sssp(G, Source)

    Questo predicato prende 2 argomenti: il grafo G e il vertice
    Source, ovvero il nodo da dove far partire il controllo per trovare il
    percorso più breve verso tutti gli altri nodi.

    il predicato inizia controllando che Source sia un vertice valido 
    all'interno del grafo G, dopodichè elimina tutte le distanze,
    i previous e i visited in quel grafo in modo che se viene richiamato due
    volte usando lo stesso grafo le informazioni salvate non si sovrappongano.
    Successivamente salva come visited Source e salva come previous di 
    Source la Source stessa (usato come convenzione per altri metodi).
    Inoltre crea una nuova heap con lo stesso nome del grafo e richiama
    vertices per ottenere tutti i vertici del grafo, initialize_distance
    con i vertici ottenuti dal grafo, in modo da inizializzare tutti i vertici
    diversi dalla sorgente a distanza infinita e initialize_heap per inserire
    tutti i vertici del grafo all'interno della heap creata sopra. 
    Continua richiamando il predicato neighbors con Source e ottiene la lista
    di tutti i nodi adiacenti alla sorgente e utilizza il predicato
    process_neighbors per elaborare le distanze seguendo l'algoritmo,
    infine richiama Dijkstra, un predicato ricorsivo che esegue l'algoritmo per
    tutti gli altri nodi.


Dijkstra(G, Natt)

    predicato ricorsivo per l'implementazione dell'algoritmo di Dijkstra, 
    prima di tutto controlla che il Nodo attuale (Natt) non abbia distanza zero
    (e quindi sia diverso dalla source). 
    Se risulta vero allora si esegue il primo caso, estraendo il nodo dalla
    heap e salvandolo come visitato, 
    altrimenti si esegue il caso del nodo sorgente, che avendolo già salvato
    sopra come visitato non va salvato 2 volte.
    Si calcola poi la distanza dei suoi neighbors attraverso 
    il predicato process_neighbors. 
    Successivamente si richiama il metodo ricorsivamente sulla testa della heap
    che contiene l'elemento con distanza attuale minore 
    che è ancora da controllare.


process_neighbors(G, Np, [(G, Np, Narr, W)| Rest])

    Questo predicato prende tre argomenti: il grafo G, il nodo "precente" Np
    e una lista che contiene i nodi adiacenti a Np.
    Questo predicato calcola ricorsivamente le distanze dei nodi 
    adiacenti a Np, fino a quando la lista non è vuota.
    Ci sono 2 casi: il caso dove il nodo da controllare è già stato 
    visitato e quello dove è ancora da visitare.
    Quando il nodo da controllare (Narr) è da visitare il metodo 
    controlla che la sua nuova distanza NewD 
    (la distanza a Np dalla sorgente + il peso dell'arco da Np a Narr) sia
    minore della distanza conosciuta più breve dalla sorgente a Np.
    Se questo caso risulta vero allora si aggiorna all'interno della base di
    dati i predicati Distance, Previous e si modifica la sua chiave all'interno 
    della heap con la distanza NewD. 
    Infine elabora ricorsivamente gli altri nodi. 
    Il secondo caso è quando il nodo è già stato visitato: all'interno dell'
    algoritmo di Dijkstra quando un nodo è visitato la sua distanza dalla
    sorgente è già la distanza minima, quindi semplicemente si passa al nodo
    successivo, calcolando i rimanenti neighbors ricorsivamente.


shortest_path(G, Source, V, Path)

    Questo predicato prende 4 argomenti: il grafo G, la sorgente 
    dove si è calcolato l'algoritmo di Dijkstra Source, il nodo V verso il quale
    ricaveremo il percorso più breve e una lista di archi Path 
    che corrispondono al percorso più breve verso da Source a V.
    Questo predicato controlla che V e Source siano due vertici, richiama 
    il predicato build_path(G, Source, V, BuiltPath), che ritornerà una lista 
    BuiltPath che è la lista di archi che collegano Source e V 
    ma scritta al contrario, infine la girerà attraverso il predicato reverse
    per renderla Path.


build_path(G, Source, V, [(G, Prev, V, W) | Path1])

    Questo predicato prende 4 argomenti: il grafo G, due nodi Source e V 
    e una lista di archi che corrisponde al percorso tra Source e V.
    Questo predicato ha 2 casi: 
    Caso base. Source e V corrispondono. 
    Quando Source e V corrispondono il predicato non fa niente, 
    vuol dire che abbiamo fatto tutto il percorso da V a Source.
    Caso ricorsivo. Source e V non corrispondono.
    Quando Source e V non corrispondono il predicato aggiunge l'arco
    che collega V e il suo Previous alla lista di archi, 
    per poi richiamare ricorsivamente il predicato build_path 
    sul suo previous.


~~~~~~~~~~~~~~~~~~~~~~~~~~~ Implementazione minHeap ~~~~~~~~~~~~~~~~~~~~~~~~~~~


heap(H, S)

    Predicato che corrisponde nella base di dati ad una heap. 
    Viene aggiornato dinamicamente con la sua dimensione S ogni volta 
    che un elemento viene aggiunto o rimosso ad essa.


heap_entry(H, P, K, V)

    Predicato che corrisponde ad un elemento di una heap H.
    P indica la posizione all'interno della heap dell'elemento, mentre K e V 
    sono rispettivamente la sua chiave e il suo valore. La posizione viene
    aggiornata dinamicamente ogni volta che viene inserito un nuovo elemento
    per far si che rimanga la proprietà di heap.


new_heap(H)

    Predicato che crea una heap. Quando la heap esiste già il predicato non fa 
    niente, mentre quando non esiste crea una nuova heap con dimensione 0.


delete_heap(H) 

    Predicato che elimina una Heap esistente. Elimina tutte le informazioni
    salvate nella base di dati relative ad una heap H


heap_size(H, S)

    Predicato che ritorna True se la dimensione della heap H è esattamente S


empty(H)

    Predicato che ritorna True se la heap controllata è vuota.


not_empty(H)

    Predicato che ritorna True se la heap controllata non è vuota.


head(H, K, V)

    Predicato che ritorna True se la tupla chiave e valore passata 
    è la testa della heap.
    Controlla prima di tutto se la heap non è vuota e poi guarda il primo
    elemento e controlla se corrisponde con i valori passati in input.


heapify_up(H, I)

    Questo predicato ripristina la proprietà di heap dopo l'inserimento 
    di un nuovo elemento all'interno dell'ultima.
    Prende 2 argomenti: la heap H e l'indice I dell'elemento da controllare.
    La prima cosa che controlla è che I sia maggiore di 1, 
    cioè non sia la radice della Heap.
    Se l'indice risulta uguale a 1 la heap 
    non deve essere aggiustata in alcun modo.
    Se l'indice è maggiore di 1 controlla che la sua chiave sia minore
    o uguale della chiave del padre.
    Se è questo il caso allora il padre deve essere scambiato 
    con il figlio e poi deve essere chiamato il metodo ricorsivamente su quello
    che è diventato il padre finché non arriva alla posizione giusta, quindi
    viene richiamato heapify_up con indice quello del padre 
    (ovvero sull'elemento appena spostato)


heapify_down(H, I)

    Questo predicato è praticamente l'opposto di heapify_up. 
    Infatti ripristina la proprietà di heap dopo la rimozione
    di un elemento dalla heap. 
    Prende 2 argomenti: la heap H e l'indice I dell'elemento da controllare.
    heapify_down ha vari casi: 
    Caso base: non succede niente.
    Caso Figlio sinistro maggiore/uguale: 
    In questo caso si controlla la chiave dei 2 figli, se la chiave del figlio
    sinistro è maggiore o uguale rispetto a quella del figlio destro allora si
    guarda la chiave del genitore: se la chiave del genitore è 
    più grande rispetto a quella del figlio allora vanno scambiate e va
    chiamato il metodo heapify_down sull'indice dell'elemento appena scambiato 
    finché non sarà nella posizione giusta.
    Caso solo figlio sinistro: 
    In questo caso si controlla solo la chiave del figlio sinistro come sopra
    dato che il figlio destro non c'è.
    Caso solo figlio destro: 
    Come sopra solo che si controlla la chiave del figlio destro.


insert(H, K, V)

    Questo predicato inserisce un nuovo elemento con chiave K e valore V 
    all'interno di una heap H. aumenta poi la dimensione della heap di 1, 
    inserisce l'elemento all'ultima posizione della heap e infine chiama 
    il predicato heapify_up per ripristinare la proprietà di heap.


extract(H, K, V)

    Questo predicato rimuove un elemento dalla heap se K, V è la tupla che 
    costituisce il primo elemento della heap.
    Il predicato fallisce se non ci sono elementi nella heap, 
    rimuove il primo elemento se c'è un solo elemento, 
    altrimenti riduce la dimensione della heap di 1, 
    scambia il primo elemento con l'ultimo mettendo l'ultimo elemento
    in cima e chiama heapify_down per far ritornare la proprietà di heap.


modify_key(H, NewKey, OldKey, V)

    Questo predicato modifica la chiave di un valore all'interno della heap 
    e la sistema in modo che ritorni la sua proprietà.
    Per farlo sostituisce NewKey con OldKey e controlla se NewKey sia più 
    grande di OldKey. Se NewKey risulta più piccola utilizzerà heapify_up
    altrimenti heapify_down.


list_heap(H)

    predicato che ritorna il nome della heap H e tutti i suoi elementi con 
    relative posizioni all'interno della heap.