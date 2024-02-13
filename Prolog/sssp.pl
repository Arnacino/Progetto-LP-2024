%Ferrillo Samuele 900210
%Antonico Lorenzo 904775


%--------------------- Interfaccia per la gestione dei grafi ------------------%

:- dynamic graph/1, vertex/2, edge/4, distance/3, visited/2, previous/3.

graph(G) :- clause(graph(G), true).
vertex(G, V) :- clause(vertex(G, V), true).
edge(G, U, V, Weight) :- clause(edge(G, U, V, Weight), true).

%Predicato che crea un grafo
%step 1: controllo se esiste già, se esiste ritorna true e basta
%step 2: se non esiste lo creo e lo asserisco
new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

%cancello il grafo
delete_graph(G) :- 
    graph(G),
    retractall(graph(G)),
    retractall(vertex(G, _)),
    retractall(edge(G, _, _, _)).

%Predicato che crea un vertice
%step 1: controllo se esiste già, se esiste ritorna true e basta
%step 2: se non esiste cancello tutti i vertici con lo stesso nome e lo asserisco (previene duplicati)
new_vertex(G, V) :- 
    graph(G),
    vertex(G, V), !.

new_vertex(G, V) :- 
    retractall(vertex(G, V)), 
    assert(vertex(G, V)), !.

%Predicato che ritorna true se tutti i vertici nella lista Vs esistono nel grafo G
vertices(G, Vs) :- findall(V, vertex(G, V), Vs).

%Predicato che stampa tutti i vertici del grafo G
list_vertices(G) :- listing(vertex(G, _)).

%Predicato che crea un arco (se l'arco non ha peso il peso è 1)
new_edge(G, U, V) :- new_edge(G, U, V, 1).
new_edge(G, U, V, Weight) :- edge(G, U, V, Weight), !.
new_edge(G, U, V, Weight) :- 
    retractall(edge(G, U, V, Weight)), 
    assert(edge(G, U, V, Weight)),
    !.

%Predicato che ritorna true se tutti gli archi nella lista Es esistono nel grafo G
edges(G, Es) :- 
    findall((U, V, Weight), edge(G, U, V, Weight), Es).

%Predicato che stampa tutti gli archi uscenti del vertice V
neighbors(G, V, Ns) :- 
    vertex(G, V), 
    findall((G, V, N, Weight), edge(G, V, N, Weight), Ns).

% Predicato che stampa tutti gli archi del grafo G
list_edges(G) :- listing(edge(G, _, _, _)).

%Predicato che stampa il grafo G
list_graph(G) :-
    writeln("Vertici:"),
    list_vertices(G),
    writeln("Archi:"),
    list_edges(G).

%------------------------------ Algoritmo di SSSP (da fare dopo) -----------------------------%

:- dynamic distance/3, previous/3, visited/2.

distance(G, V, D) :- 
    vertex(G, V). %bohhh ? ? ? 

visited(G, V) :- 
    vertex(G, V). %bohhh ? ? ?

previous(G, V, U) :- 
    vertex(G, V), 
    vertex(G, U). %bohhh ? ? ?

%Predicato che cambia la distanza di un vertice
change_distance(G, V, NewDist) :- 
    retractall(distance(G, V, _)), 
    assert(distance(G, V, NewDist)).


change_previous(G, V, U) :- 
    retractall(previous(G, V, _)), 
    assert(previous(G, V, U)).

dijkstra_sssp(G, Source). %TODO

shortest_path(G, Source, V, Path). %TODO

%------------------------------ Algoritmo di MinHeap -----------------------------%

:- dynamic heap/2, heap_entry/4.

heap(H, S) :- clause(heap(H, S), true).

%Predicato che crea un nuovo heap
new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

%Predicato che cancella l'heap
delete_heap(H) :- 
    retractall(heap(H, _S)),
    retractall(heap_entry(H, _, _, _)), !.

%Predicato che ritorna true se la dimensione della heap è S
heap_size(H, S) :- heap(H, S).

%Predicato che ritorna true se l'heap è vuoto
empty(H) :- heap(H, 0).

%Predicato che ritorna true se l'heap non è vuoto
not_empty(H) :- heap(H, S), S > 0.

%Predicato che ritorna true se l'elemento che gli passi è la testa della heap
head(H, K, V) :- heap_entry(H, 1, K, V).

%Predicato che quando inserisci un elemento fa diventare l'heap un minheap (vedi algo)
heapify_up(H, I) :-
    I > 1,  % Continue if the index is not the root
    ParentI is I div 2,  % Compute the index of the parent
    heap_entry(H, I, K, V),  % Get the key and value of the current entry
    heap_entry(H, ParentI, ParentK, ParentV),  % Get the key and value of the parent entry
    K < ParentK,  % Continue if the key of the current entry is less than the key of the parent entry
    % Swap the current entry and the parent entry
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, ParentI, ParentK, ParentV)),
    assert(heap_entry(H, I, ParentK, ParentV)),
    assert(heap_entry(H, ParentI, K, V)),
    heapify_up(H, ParentI).  % Recursively heapify up from the parent index
heapify_up(_, _).  % Base case: do nothing
%COPILOT HA CUCINATO QUESTO PREDICATO

%Predicato che quando rimuovi un elemento fa diventare l'heap un minheap (vedi algo)
heapify_down(H, I) :-
    heap(H, S),  % Get the size of the heap
    LeftI is 2 * I,  % Compute the index of the left child
    RightI is 2 * I + 1,  % Compute the index of the right child
    heap_entry(H, I, K, V),  % Get the key and value of the current entry
    % Find the child with the smallest key
    (heap_entry(H, LeftI, LeftK, _), LeftK < K -> MinI = LeftI, MinK = LeftK; MinI = I, MinK = K),
    (heap_entry(H, RightI, RightK, _), RightK < MinK -> MinI = RightI, MinK = RightK; true),
    MinI \= I,  % Continue if the current entry does not have the smallest key
    % Swap the current entry and the smallest child
    heap_entry(H, MinI, MinK, MinV),
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, MinI, MinK, MinV)),
    assert(heap_entry(H, I, MinK, MinV)),
    assert(heap_entry(H, MinI, K, V)),
    heapify_down(H, MinI).  % Recursively heapify down from the smallest child
heapify_down(_, _).  % Base case: do nothing
%COPILOT HA CUCINATO QUESTO PREDICATO

%Predicato che inserisce un elemento nella heap
insert(H, K, V) :- 
    heap(H, S), 
    NewS is S + 1, 
    retractall(heap(H, S)), 
    assert(heap(H, NewS)), 
    assert(heap_entry(H, NewS, K, V)), 
    heapify_up(H, NewS).

%Predicato che rimuove un elemento dalla heap
extract(H, K, V) :-
    heap(H, S), 
    S > 0, 
    heap_entry(H, 1, K, V), 
    heap_entry(H, S, LastK, LastV), 
    retractall(heap_entry(H, 1, K, V)), 
    retractall(heap_entry(H, S, LastK, LastV)), 
    NewS is S - 1, 
    retractall(heap(H, S)), 
    assert(heap(H, NewS)), 
    assert(heap_entry(H, 1, LastK, LastV)), 
    heapify_down(H, 1).

%Predicato che modfica la chiave di un elemento nella heap
modify_key(H, NewKey, OldKey, V) :- 
    heap_entry(H, I, OldKey, V), 
    retractall(heap_entry(H, I, OldKey, V)), 
    assert(heap_entry(H, I, NewKey, V)),
    (NewKey < OldKey -> heapify_up(H, I) ; heapify_down(H, I)).

%Predicato che restituisce la lista degli elementi della heap
list_heap(H) :- listing(heap(H, _)), listing(heap_entry(H, _, _, _)).

% heap_entry ha H, P, K, V dove H è l'heap, P è la posizione, K è la chiave e V è il valore 