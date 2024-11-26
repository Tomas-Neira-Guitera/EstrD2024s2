#include <iostream>
#include "LinkedList.h"
using namespace std;


struct NodoS
{
    int elem;
    NodoS* siguiente;
};

struct SetSt;

typedef SetSt* Set;

Set emptyS();
bool isEmptyS(Set s);
bool belongsS(int x, Set s);
void AddS(int x, Set s);
void RemoveS(int x, Set s);
int sizeS(Set s);
LinkedList setToList(Set s);
void DestroyS(Set s);

