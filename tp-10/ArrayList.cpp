#include <iostream>
#include "ArrayList.h"
using namespace std;

struct ArrayListSt{
    int cantidad;
    int* elementos;
    int capacidad;
};

ArrayList newArrayList(){
    ArrayListSt* a = new ArrayListSt;
    a->cantidad = 0;
    a->capacidad = 16;
    a->elementos = new int[16];
    return a;
}

ArrayList newArrayListWhit(int c){
    ArrayListSt* a = new ArrayListSt;
    a->cantidad = 0;
    a->capacidad = c;
    a->elementos = new int[c];
    return a; 
}

int lengthAL(ArrayList a){
    return a->cantidad;
}

int get(int i, ArrayList a){
    return a->elementos[i];
}

void set(int i, int x, ArrayList a){
    a->elementos[i] = x;
}

void resize(int s, ArrayList a){
    int* array = new int[s];
    for (int i = 0; i < s && i != a->cantidad; i++)
    {
        array[i] = a->elementos[i];
    }
    if (s < (a->cantidad)){

        a->cantidad = s;
    }
    delete a->elementos;
    a->capacidad = s;
    a->elementos = array;
}

void add(int x, ArrayList a){
    if (a->cantidad == a->capacidad)
    {
        resize(a->capacidad *2, a);
    }
    a->elementos[a->cantidad] = x;
    a->cantidad++;
}

void remove(ArrayList a){
    a->cantidad--;
}