Progetto scritto da: 
    Ferrillo Samuele 
    Antonico Lorenzo 

~~~~~~~~~~~~~~~~~~~~~~~~~~~ Manipolazione di grafi ~~~~~~~~~~~~~~~~~~~~~~~~~~~~


new_graph(G).

    Crea un nuovo grafo se non esiste già un grafo con il medesimo nome


delete_graph(G).

    Cancella un grafo dalla base di dati, eliminando anche tutti 
    i suoi relativi vertici ed archi 


new_vertex(G, V).

    Controlla se il grafo preso in input esiste, in caso positivo controlla 
    che non esista già il vertice e lo inserisce nella base dati 


vertices(G, Vs).

    Ritorna true se tutti i vertici nella lista Vs appartengono al grafo G


list_vertices(G).

    stampa tutti i vertici del grafo G


new_edge(G, U, V).

    crea un arco (dato che il peso non è specificato lo inserisce con peso 1) 
    se non esiste già nella base dati


new_edge(G, U, V, Weight).

    crea un arco con peso Weight 
    (se non esiste già nella base dati un arco che va da U a V qualunque sia il suo peso)


edges(G, Es).

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


change_distance(G, V, NewD)

    cambia la distanza nella base dati di V con NewD. 
    per farlo rimuove tutte le istanze di distance e ne asserisce una nuova
    con valore di distanza NewD


previous(G, V, U)

    predicato che tiene nella base dati il vertice U precedente al vertice V
    (aggiornato durante l'algoritmo)


change_previous(G, V, NewU)

    cambia previous nella base dati di V con NewU. 
    per farlo rimuove tutte le istanze di previous e ne asserisce una nuova
    con Vertice previous NewU

visited(G, V)

    predicato che salva nella base di dati quando un vertice V viene visitato