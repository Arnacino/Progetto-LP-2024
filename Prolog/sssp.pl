%Ferrillo Samuele 900210
%Antonico Lorenzo 904775

:- dynamic graph/1, vertex/2, edge/4.

graph(G) :- clause(graph(G), true).
vertex(G, V) :- clause(vertex(G, V), true).
edge(G, U, V, Weight) :- clause(edge(G, U, V, Weight), true).

%Predicato che crea un grafo
%step 1: controllo se esiste già, se esiste ritorna true e basta
%step 2: se non esiste lo creo e lo asserisco
new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

%cancello il grafo
delete_graph(G) :- retract(graph(G)).

%Predicato che crea un vertice
%step 1: controllo se esiste già, se esiste ritorna true e basta
%step 2: se non esiste cancello tutti i vertici con lo stesso nome e lo asserisco (previene duplicati)
new_vertex(G, V) :- vertex(G, V), !.
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

neighbors(G, V, Ns) :- 
    vertex(G, V), 
    findall((G, V, N, Weight), edge(G, V, N, Weight), Ns).