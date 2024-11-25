#include <iostream>
#include "Iterator.h"
using namespace std;

ListIterator getIterator(LinkedList xs){
    ListIterator l = new ListIteratorSt;
    l->current = xs->primero;
    return l;
}

int current(ListIterator ixs){
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x;
}

void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){
    delete ixs;
}