#include <iostream>
#include "LinkedList.cpp"
using namespace std;


struct ListIteratorSt
{
    NodoL* current;
};

typedef ListIteratorSt* ListIterator;

ListIterator getIterator(LinkedList xs);
int current(ListIterator ixs);
void SetCurrent(int x, ListIterator ixs);
void Next(ListIterator ixs);
bool atEnd(ListIterator ixs);
void DisposeIterator(ListIterator ixs);
void DestroyL(LinkedList xs);