%Ferrillo Samuele 900210
%Antonico Lorenzo 904775


%--------------------- Interfaccia per la gestione dei grafi ------------------%

:- dynamic graph/1, vertex/2, edge/4, distance/3, visited/2, previous/3.

graph(G) :- clause(graph(G), true).
vertex(G, V) :- clause(vertex(G, V), true).
edge(G, U, V, W) :- clause(edge(G, U, V, W), true).

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

delete_graph(G) :- 
    graph(G),
    retractall(graph(G)),
    retractall(vertex(G, _)),
    retractall(edge(G, _, _, _)).

new_vertex(G, V) :- 
    graph(G),
    vertex(G, V), !.

new_vertex(G, V) :- 
    retractall(vertex(G, V)), 
    assert(vertex(G, V)), !.

vertices(G, Vs) :- findall(V, vertex(G, V), Vs).

list_vertices(G) :- listing(vertex(G, _)).

new_edge(G, U, V) :- new_edge(G, U, V, 1).

new_edge(G, U, V, _) :- edge(G, U, V, _), !.

new_edge(G, U, V, Weight) :- 
    retractall(edge(G, U, V, Weight)), 
    assert(edge(G, U, V, Weight)),
    !.

edges(G, Es) :- 
    findall((U, V, Weight), edge(G, U, V, Weight), Es).

neighbors(G, V, Ns) :- 
    vertex(G, V), 
    findall((G, V, N, Weight), edge(G, V, N, Weight), Ns).

list_edges(G) :- listing(edge(G, _, _, _)).

list_graph(G) :-
    writeln("Vertici:"),
    list_vertices(G),
    writeln("Archi:"),
    list_edges(G).

%------------------------------ Algoritmo di SSSP ----------------------------%

:- dynamic distance/3, previous/3, visited/2.

%Predicato che cambia la distanza di un vertice
change_distance(G, V, NewD) :- 
    retractall(distance(G, V, _)), 
    assert(distance(G, V, NewD)).

%Predicato che cambia il vertice precedente di un vertice
change_previous(G, V, NewU) :- 
    retractall(previous(G, V, _)), 
    assert(previous(G, V, NewU)).

%Predicato che ritorna la distanza di un vertice
visited(G, V) :- clause(visited(G, V), true), !.
visited(G, V) :- assert(visited(G, V)).


%Predicato che inizializza la distanza di tutti i vertici
initialize_distance(_, []) :- !.
initialize_distance(G, [V | Rest]) :- 
    visited(G, V),
    assert(distance(G, V, 0)), 
    initialize_distance(G, Rest), !.

initialize_distance(G, [V | Rest]) :- 
    assert(distance(G, V, inf)), 
    initialize_distance(G, Rest), !.

%Predicato che inizializza l'heap
initialize_heap(_, []).
initialize_heap(G, [V | Rest]) :- 
    distance(G, V, D),
    insert(G, D, V),
    initialize_heap(G, Rest), !.

%Predicato che inizializza dijkstra e chiama ricorsivamente dijkstra
dijkstra_sssp(G, Source) :-
    vertex(G, Source),
    retractall(distance(G, _, _)),
    retractall(previous(G, _, _)),
    retractall(visited(G, _)),
    assert(visited(G, Source)),
    assert(previous(G, Source, Source)),
    new_heap(G),
    vertices(G, Vs),
    initialize_distance(G, Vs),
    initialize_heap(G, Vs),
    neighbors(G, Source, Ns),
    process_neighbors(G, Source, Ns),
    dijkstra(G, Source), !.

%Predicato che chiama ricorsivamente dijkstra
dijkstra(G, Natt) :- 
    \+distance(G, Natt, 0),
    not_empty(G),
    extract(G, _, Natt),
    assert(visited(G, Natt)),
    neighbors(G, Natt, Ns),
    process_neighbors(G, Natt, Ns),
    head(G, _, Nsucc),
    dijkstra(G, Nsucc), !.

dijkstra(G, Natt) :- 
    not_empty(G),
    extract(G, _, Natt),
    neighbors(G, Natt, Ns),
    process_neighbors(G, Natt, Ns),
    head(G, _, Nsucc),
    dijkstra(G, Nsucc), !.

dijkstra(G, Natt) :- 
    not_empty(G),
    extract(G, _, Natt),
    assert(visited(G, Natt)),
    neighbors(G, Natt, Ns),
    process_neighbors(G, Natt, Ns),!.

dijkstra(G, _) :- empty(G).

%Predicato che modifica la chiave di un elemento nella heap se distanza minore
process_neighbors(_, _, []) :- !.
process_neighbors(G, Np, [ (G, Np, Narr, W)| Rest]) :-
    \+visited(G, Narr),
    vertex(G, Narr), 
    distance(G, Np, Dp),
    distance(G, Narr, Da),
    NewD is Dp + W,
    NewD < Da,
    change_distance(G, Narr, NewD),
    change_previous(G, Narr, Np),
    modify_key(G, NewD, Da, Narr),
    process_neighbors(G, Np, Rest), !.

process_neighbors(G, Np, [ (G, Np, Narr, W)| Rest]) :-
    \+visited(G, Narr),
    vertex(G, Narr), 
    distance(G, Np, Dp),
    distance(G, Narr, Da),
    NewD is Dp + W,
    NewD >= Da,
    process_neighbors(G, Np, Rest), !.

process_neighbors(G, Np, [ (G, Np, Narr, _)| Rest]) :-
    visited(G, Narr), 
    process_neighbors(G, Np, Rest), !.

shortest_path(G, Source, V, Path) :- 
    vertex(G, Source),
    vertex(G, V),
    build_path(G, Source, V, BuiltPath),
    reverse(BuiltPath, Path).

build_path(G, Source, V, [(G, Prev, V, W) | Path1]) :- 
    previous(G, V, Prev),
    edge(G, Prev, V, W),
    build_path(G, Source, Prev, Path1), !.

build_path(_, Source, V, []) :- 
    V = Source.

%------------------------------ Algoritmo di MinHeap -------------------------%

:- dynamic heap/2, heap_entry/4.

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
head(H, _, _) :-
    heap_size(H, S),
    S =:= 0,
    !,
    fail.

head(H, K, V) :-
    heap_size(H, S),
    S > 0,
    heap_entry(H, 1, K, V).

%Predicato che quando inserisci un elemento fa diventare l'heap un minheap (vedi algo)
heapify_up(H, I) :-
    I > 1,
    ParentI is I div 2,
    heap_entry(H, I, K, V),
    heap_entry(H, ParentI, ParentK, ParentV),
    K < ParentK,
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, ParentI, ParentK, ParentV)),
    assert(heap_entry(H, I, ParentK, ParentV)),
    assert(heap_entry(H, ParentI, K, V)),
    heapify_up(H, ParentI), !.

heapify_up(H, I) :-
    I > 1,
    ParentI is I div 2,
    heap_entry(H, I, K, V),
    heap_entry(H, ParentI, ParentK, ParentV),
    K =:= ParentK,
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, ParentI, ParentK, ParentV)),
    assert(heap_entry(H, I, ParentK, ParentV)),
    assert(heap_entry(H, ParentI, K, V)),
    heapify_up(H, ParentI), !.

heapify_up(_, _).

%Predicato che quando rimuovi 
%un elemento fa diventare l'heap un minheap (vedi algo)
heapify_down(H, I) :-
    heap(H, _),
    LeftI is 2 * I,
    heap_entry(H, I, K, V),
    heap_entry(H, LeftI, LeftK, LeftV),
    RightI is 2 * I + 1,
    heap_entry(H, RightI, RightK, _),
    LeftK =< RightK,
    LeftK =< K,
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, LeftI, LeftK, LeftV)),
    assert(heap_entry(H, I, LeftK, LeftV)),
    assert(heap_entry(H, LeftI, K, V)),
    heapify_down(H, LeftI), !.

heapify_down(H, I) :-
    heap(H, _),
    RightI is 2 * I + 1,
    heap_entry(H, I, K, V),
    LeftI is 2 * I,
    heap_entry(H, RightI, RightK, RightV),
    heap_entry(H, LeftI, LeftK, _),
    RightK =< LeftK,
    RightK =< K,
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, RightI, RightK, RightV)),
    assert(heap_entry(H, I, RightK, RightV)),
    assert(heap_entry(H, RightI, K, V)),
    heapify_down(H, RightI), !.

heapify_down(H, I) :-
    heap(H, _),
    LeftI is 2 * I,
    heap_entry(H, I, K, V),
    heap_entry(H, LeftI, LeftK, LeftV),
    RightI is 2 * I + 1,
    \+ heap_entry(H, RightI, _, _),
    LeftK =< K,
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, LeftI, LeftK, LeftV)),
    assert(heap_entry(H, I, LeftK, LeftV)),
    assert(heap_entry(H, LeftI, K, V)), !.

heapify_down(H, I) :-
    heap(H, _),
    RightI is 2 * I + 1,
    heap_entry(H, I, K, V),
    heap_entry(H, RightI, RightK, RightV),
    LeftI is 2 * I,
    \+ heap_entry(H, LeftI, _, _),
    RightK =< K,
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, RightI, RightK, RightV)),
    assert(heap_entry(H, I, RightK, RightV)),
    assert(heap_entry(H, RightI, K, V)), !.

heapify_down(_,_).

%Predicato che inserisce un elemento nella heap
insert(H, K, V) :- 
    heap(H, S), 
    NewS is S + 1, 
    retractall(heap(H, S)), 
    assert(heap(H, NewS)), 
    assert(heap_entry(H, NewS, K, V)), 
    heapify_up(H, NewS).

%Predicato che rimuove un elemento dalla heap
extract(H, _, _) :- 
    heap(H, S), 
    S =:= 0, 
    !, 
    fail.

extract(H, K, V) :- 
    heap(H, S), 
    S =:= 1, 
    heap_entry(H, 1, K, V),
    retractall(heap(H, S)),
    assert(heap(H, 0)),
    retractall(heap_entry(H, 1, K, V)), !.

extract(H, K, V) :-
    heap(H, S), 
    S > 0, 
    heap_entry(H, 1, K, V), 
    heap_entry(H, S, LastK, LastV), 
    NewS is S - 1, 
    retractall(heap_entry(H, 1, K, V)), 
    retractall(heap_entry(H, S, LastK, LastV)), 
    retractall(heap(H, S)), 
    assert(heap(H, NewS)), 
    assert(heap_entry(H, 1, LastK, LastV)), 
    heapify_down(H, 1).

%Predicato che modfica la chiave di un elemento nella heap
modify_key(H, NewKey, OldKey, V) :-
    NewKey < OldKey,
    heap_entry(H, I, OldKey, V), 
    retractall(heap_entry(H, I, OldKey, V)), 
    assert(heap_entry(H, I, NewKey, V)),
    heapify_up(H, I), !.

modify_key(H, NewKey, OldKey, V) :-
    NewKey >= OldKey,
    heap_entry(H, I, OldKey, V), 
    retractall(heap_entry(H, I, OldKey, V)), 
    assert(heap_entry(H, I, NewKey, V)),
    heapify_down(H, I).

%Predicato che restituisce la lista degli elementi della heap
list_heap(H) :- listing(heap(H, _)), listing(heap_entry(H, _, _, _)).

% heap_entry ha H, P, K, V dove H è l'heap, P è la posizione, K è la chiave e V è il valore 