%Ferrillo Samuele 900210
%Antonico Lorenzo 904775


%--------------------- Interfaccia per la gestione dei grafi ------------------%

:- dynamic graph/1, vertex/2, edge/4, distance/3, visited/2, previous/3.

graph(G) :- clause(graph(G), true).
vertex(G, V) :- clause(vertex(G, V), true).
edge(G, U, V, Weight) :- clause(edge(G, U, V, Weight), true).

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

change_previous(G, V, NewU) :- 
    retractall(previous(G, V, _)), 
    assert(previous(G, V, NewU)).

visited(G, V) :- clause(visited(G, V), true), !.
visited(G, V) :- assert(visited(G, V)).

%Predicato che inizializza le distanze da tutti i vertici a infinito
initialize_distances(_, _, []). % Caso base: la lista è vuota
initialize_distances(G, Source, [Source | Rest]) :-
    assert(distance(G, Source, 0)),
    initialize_distances(G, Source, Rest).

initialize_distances(G, Source, [V | Rest]) :-
    V \= Source,
    assert(distance(G, V, inf)),
    initialize_distances(G, Source, Rest).

initialize_heap(_, []).
initialize_heap(G, [V | Rest]) :- 
    distance(G, V, D),
    insert(G, D, V),
    initialize_heap(G, Rest).

extractV((_, _, V, _), (V)).

extractV_list(Ns, NsList) :- 
    maplist(extractV ,Ns, NsList).

dijkstra_sssp(G, Source) :-
    % cancella tutte le distanze, i predecessori e i visitati perché inizia l'algoritmo
    vertex(G, Source),
    retractall(distance(_, _, _)),
    retractall(previous(_, _, _)),
    retractall(visited(_, _)),
    vertices(G, Vs),
    % inizializza distanze da tutti i vertici a infinito e da Source a 0
    initialize_distances(G, Source, Vs),
    assert(previous(G, Source, Source)),
    new_heap(G),
    % inserisce tutti i nodi nella heap
    initialize_heap(G, Vs),
    dijkstra(G, Source).

dijkstra(G, _) :-
    empty(G), !.

dijkstra(G, Natt) :-
    not_empty(G),
    previous(G, Natt, Nprec),
    extract(G, _, Natt),
    visited(G, Natt),
    assert(previous(G, Natt, Nprec)),
    neighbors(G, Natt, Ns),
    extractV_list(Ns, NsList),
    process_neighbors(G, Natt, NsList),
    not_empty(G),
    head(G, _, Nsucc),
    dijkstra(G, Nsucc), !.

dijkstra(G, Natt) :-
    not_empty(G),
    previous(G, Natt, Nprec),
    extract(G, _, Natt),
    assert(visited(G, Natt)),
    assert(previous(G, Natt, Nprec)),
    neighbors(G, Natt, Ns),
    extractV_list(Ns, NsList),
    process_neighbors(G, Natt, NsList),
    \+ not_empty(G).

% Se un nodo è visitato allora non è nella heap e quindi non è possibile modificarne la chiave 
% quindi la sua distanza si prende da distance e non dalla heap.
process_neighbors(_, _, []) :- !.
% Predicato che processa i vicini di un vertice   
process_neighbors(G, Natt, [ V | Rest]) :-
    visited(G, V),
    distance(G, V, K),
    distance_calc(G, Natt, V, K, Rest), !.

process_neighbors(G, Natt, [ V | Rest]) :-
    \+ visited(G, V),
    heap_entry(G, _, K, V),
    distance_calc(G, Natt, V, K, Rest).

distance_calc(G, Natt, V, K, Rest):-
    edge(G, Natt, V, W),
    distance(G, Natt, Datt),
    NewDistance is W + Datt,
    K > NewDistance,
    change_distance(G, V, NewDistance),
    change_previous(G, V, Natt),
    modify_key(G, NewDistance, K, V),
    process_neighbors(G, Natt, Rest), !.

distance_calc(G, Natt, V, K, Rest):-
    edge(G, Natt, V, W),
    distance(G, Natt, Datt),
    NewDistance is W + Datt,
    K =< NewDistance,
    process_neighbors(G, Natt, Rest).

shortest_path(G, Source, V, Path) :- 
    vertex(G, Source),
    vertex(G, V),
    build_path(G, Source, V, BuiltPath),
    reverse(BuiltPath, Path).

build_path(_, Source, V, []) :- 
    V = Source, !.

build_path(G, Source, V, [(G, Prev, V, W) | Path1]) :- 
    previous(G, V, Prev),
    edge(G, Prev, V, W),
    build_path(G, Source, Prev, Path1).

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

%Predicato che quando rimuovi un elemento fa diventare l'heap un minheap (vedi algo)
heapify_down(H, I) :-
    heap(H, _),
    LeftI is 2 * I,
    RightI is 2 * I + 1,
    heap_entry(H, I, K, V),
    heap_entry(H, LeftI, LeftK, _),
    LeftK < K,
    MinI = LeftI, MinK = LeftK,
    check_right_child(H, RightI, MinI, MinK, I, K, V), !.

heapify_down(H, I) :-
    heap(H, _),
    LeftI is 2 * I,
    RightI is 2 * I + 1,
    heap_entry(H, I, K, V),
    \+ (heap_entry(H, LeftI, LeftK, _), LeftK < K),
    MinI = I, MinK = K,
    check_right_child(H, RightI, MinI, MinK, I, K, V), !.

heapify_down(_, _).

check_right_child(H, RightI, MinI, MinK, I, K, V) :-
    heap_entry(H, RightI, RightK, _),
    RightK < MinK,
    MinI = RightI, MinK = RightK,
    swap_and_heapify(H, MinI, MinK, I, K, V), !.

check_right_child(H, RightI, MinI, MinK, I, K, V) :-
    \+ (heap_entry(H, RightI, RightK, _), RightK < MinK),
    swap_and_heapify(H, MinI, MinK, I, K, V).

swap_and_heapify(H, MinI, MinK, I, K, V) :-
    MinI \= I,
    heap_entry(H, MinI, MinK, MinV),
    retractall(heap_entry(H, I, K, V)),
    retractall(heap_entry(H, MinI, MinK, MinV)),
    assert(heap_entry(H, I, MinK, MinV)),
    assert(heap_entry(H, MinI, K, V)),
    heapify_down(H, MinI), !.

swap_and_heapify(_, _, _, _, _, _).

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

test_1(G) :-
    new_graph(G),
    new_vertex(G, source),
    new_vertex(G, a),
    new_vertex(G, b),
    new_vertex(G, c),
    new_vertex(G, d),
    new_vertex(G, e),
    new_vertex(G, final),
    new_edge(G, a, b, 6),
    new_edge(G, source, a, 2),
    new_edge(G, source, d, 8),
    new_edge(G, a, c, 2),
    new_edge(G, d, c, 2),
    new_edge(G, d, e, 3),
    new_edge(G, c, e, 9),
    new_edge(G, e, final, 1),
    new_edge(G, b, final, 5).


%test inventato a 9 nodi
test_2(G) :-
    new_graph(G),
    new_vertex(G, a),
    new_vertex(G, b),
    new_vertex(G, c),
    new_vertex(G, d),
    new_vertex(G, e),
    new_vertex(G, f),
    new_vertex(G, g),
    new_vertex(G, h),
    new_vertex(G, i),
    new_edge(G, a, b, 1),
    new_edge(G, b, c, 4),
    new_edge(G, d, e, 2),
    new_edge(G, e, f, 5),
    new_edge(G, g, h, 2),
    new_edge(G, h, i, 8),
    new_edge(G, a, d, 3),
    new_edge(G, d, g, 9),
    new_edge(G, b, e, 4),
    new_edge(G, e, h, 9),
    new_edge(G, c, f, 6).

test_3(G) :- 
    new_graph(G),
    new_vertex(G, a),
    new_vertex(G, b),
    new_edge(G, b, a, 1),
    new_edge(G, a, b, 1).

    test_heap(H) :-
        new_heap(H),
        insert(H, 12, a),
        insert(H, 7, b),
        insert(H, 50, c),
        insert(H, 12, d),
        insert(H, 4, e),
        insert(H, 6, f),
        list_heap(H).