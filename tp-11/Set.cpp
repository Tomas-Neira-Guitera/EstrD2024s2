#include <iostream>
using namespace std;
#include "Set.h"

struct SetSt
//Inv-rep: * cantidad son la cantidad de nodos desde el primero que se pueden recorrest hasta llegar a NULL.
//         * para cada NodoS dentro del SetSt su valor del campo "elem" no se repite en otro NodoS.
{
    int cantidad;
    NodoS* primero;
};


Set emptyS(){
    Set s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
}

bool isEmptyS(Set s){
    return s->cantidad == 0;
}

bool belongsS(int x, Set s){        // Costo: O(n) donde n son la cantidad de elementos del set.
    NodoS* p = s->primero;
    for (int i = 0; i < s->cantidad && p->elem != x; i++)
    {
        p = p->siguiente;
    }
    return p != nullptr;
}

void AddS(int x, Set s){
    if (!belongsS(x, s))
    {
        NodoS* n = new NodoS;
        n->elem = x;
        n->siguiente = s->primero;
        s->primero = n;
        s->cantidad++;
    }
}

void RemoveS(int x, Set s){
    NodoS* p = s->primero;
    NodoS* a;
    for (int i = 0; i < s->cantidad && p->elem != x; i++)
    {
        a = p;
        p = p->siguiente;
    }
    if (p != nullptr)
    {
       a->siguiente = p->siguiente;
       s->cantidad--;
       delete p;
    }
}

int sizeS(Set s){
    return s->cantidad;
}

LinkedList setToList(Set s){
    NodoS* p = s->primero;
    LinkedList l = nil();
    for (int i = 0; i < s->cantidad; i++)
    {
        Cons(p->elem, l);
    }
    return l;
}

void DestroyS(Set s);