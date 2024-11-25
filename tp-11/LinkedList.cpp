#include <iostream>
#include "LinkedList.h"
#include "Iterator.h"
using namespace std;

struct LinkedListSt // Inv.rep: cantidad indica la cantidad de nodos que se pueden recorrerer hasta que primero sea null.
{
    int cantidad;
    NodoL *primero;
};

LinkedList nil()
{
    LinkedListSt *l = new LinkedListSt;
    l->cantidad = 0;
    l->primero = NULL;
    return l;
}

bool isEmpty(LinkedList xs)
{
    return xs->cantidad == 0;
}

int head(LinkedList xs)
{
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs)
{
    NodoL *n = new NodoL;
    n->elem = x;
    n->siguiente = xs->primero;
    xs->primero = n;
    xs->cantidad++;
}

void Tail(LinkedList xs)
{
    NodoL *p = xs->primero;
    xs->primero = p->siguiente;
    delete p;
}

int length(LinkedList xs)
{
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs)
{
    NodoL *n = new NodoL;
    n->elem = x;
    n->siguiente = NULL;

    NodoL *p = xs->primero;

    if (p != NULL)
    {
        while (p->siguiente != NULL)
        {
            p = p->siguiente;
        }
        p->siguiente = n;
    }
    else{
        xs->primero = n;
    }
    xs->cantidad++;
}

void DestroyL(LinkedList xs)
{
    NodoL *p = xs->primero;
    NodoL *e;

    while (p != NULL)
    {
        e = p;
        p = p->siguiente;
        delete e;
    }
    delete xs;
}

void DestroyL2(LinkedList xs)
{
    while (!isEmpty(xs))
    {
        Tail(xs);
    }
    delete xs;
}

// --------- funciones como usuario ---------- //

int sumatoria(LinkedList xs)
{
    int c = 0;
    ListIterator li = getIterator(xs);
    while (!atEnd(li))
    {
        c += current(li);
        Next(li);
    }
    DisposeIterator(li);
    return c;
}

void sucesores(LinkedList xs)
{
    ListIterator li = getIterator(xs);
    while (!atEnd(li))
    {
        setCurrent(current(li)++, li);
        Next(li);
    }
    DisposeIterator(li);
}

bool pertenece(int x, LinkedList xs)
{
    ListIterator li = getIterator(xs);
    while (!atEnd(li))
    {
        if (curret(li) == x)
        {
            DisposeIterator(li);
            return true;
        }
        Next(li);
    }
    DisposeIterator(li);
    return false;
}

bool pertenece2(int x, LinkedList xs)
{
    NodoL p = xs->primero;
    while (p != NULL)
    {
        if (p->elem == x)
        {
            return true;
        }
        p = p->siguiente;
    }
    return false;
}

int apariciones(int x, LinkedList xs)
{
    int c = 0;
    ListIterator li = getIterator(xs);
    while (!atEnd(li))
    {
        if (current(li) == x)
        {
            c++;
        }
        Next(li);
    }
    DisposeIterator(li);
    return c;
}

// precondicion: tiene por lo menos 1 elemento.
int minimo(LinkedList xs)
{
    ListIterator li = getIterator(xs);
    int m = current(li);
    while (!atEnd(li))
    {
        if (current(li) <= m)
        {
            m = current(li);
        }
        Next(li);
    }
    DisposeIterator(li);
    return m;
}

LinkedList copy(LinkedList xs)
{
    LinkedList l = nill();
    ListIterator li = getIterator(xs);
    while (!atEnd(li))
    {
        Snoc(current(li), l);
        Next(li)
    }
    DisposeIterator(li);
    return l;
}
