#include <iostream>
using namespace std;

struct NodoL
{
    int elem;
    NodoL* siguiente;
};


struct LinkedListSt;

typedef LinkedListSt* LinkedList;

LinkedList nil();
bool isEmpty(LinkedList xs);
int head(LinkedList xs);
void Cons(int x, LinkedList xs);
void Tail(LinkedList xs);
int length(LinkedList xs);
void Snoc(int x, LinkedList xs);
void DestroyL(LinkedList xs);
