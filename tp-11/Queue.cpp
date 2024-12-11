#include <iostream>
using namespace std;
#include "Queue.h";

struct QueueSt      // Inv:rep: * cantidad son los nodos que se pueden recorrer desde primero hasta ultimo.
                    //          * si la cantidad de la cola es == 1 entonces el primer nodo y el ultimo son el mismo. 
                    //          * si la cola esta vacia entonces el primer nodo y el ultimo son NULL.
{
    int cantidad;
    NodoQ* primero;
    NodoQ* ultimo;
};

Queue emptyQ(){
    Queue q = new QueueSt;
    q->cantidad = 0;
    q->primero = nullptr;
    q->ultimo = nullptr;
    return q;
}

bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}

int firstQ(Queue q){
    if (q->primero != nullptr)
    {
        return q->primero->elem;
    }
}

void Enqueue(int x, Queue q){
    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = nullptr;
    
    if (q->primero == nullptr)
    {
        q->primero = n;
        q->ultimo = n;
    }
    else
    {
        q->ultimo->siguiente = n;
    }
    q->cantidad++;
}

void Dequeue(Queue q){
    
    if (q->cantidad == 1)
    {
        delete q->primero;
        q->primero = nullptr;
        q->ultimo = nullptr;
    }
    else
    {
        NodoQ* d = q->primero;
        q->primero = d->siguiente;
        delete d;
    }
    q->cantidad--;
}

int lengthQ(Queue q){
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2){
    if (q1->ultimo == nullptr)
    {
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    else
    {
        q1->ultimo->siguiente = q2->primero;
    }
    
    q1->cantidad += q2->cantidad;
    delete q2;
}

void DestroyQ(Queue q){
    while (!isEmptyQ(q))
    {
        Dequeue(q);
    }
    delete q;
}