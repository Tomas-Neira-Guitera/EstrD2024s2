#include <iostream>
using namespace std;
#include "Tree.h"
#include "tp-10/ArrayList.cpp"

Tree emptyT(){
    return nullptr;
}

Tree nodeT(int elem, Tree left, Tree right){
    NodeT* t = new NodeT;
    t->elem = elem;
    t->left = left;
    t->right = right;
    return t;
}

bool isEmptyT(Tree t){
    return t == nullptr;
}

int rootT(Tree t){  //Precondicion: debe existir un nodo root.
    return t->elem;
}

Tree left(Tree t){  //Precondicion: debe existir un nodo root.
    return t->left; 
}

Tree right(Tree t){ //Precondicion: debe existir un nodo root.
    return t->right;
}

// ----------------------------- FUNCIONES COMO USUARIO ----------------------------- //
// O(n) donde n son la cantidad de nodos del arbol y tambien son los stack frames que se crean.
int sumarT(Tree t){
    if (!isEmptyT(t))
    {
        return rootT(t) + sumarT(left(t)) + sumarT(right(t));
    }
    else
    {
        return 0;
    }
}

// O(n) donde n son la cantidad de nodos del arbol y tambien son los stack frames que se crean.
int sizeT(Tree t){
    if (!isEmptyT(t))
    {
        return 1 + sizeT(left(t)), sizeT(right(t));
    }
    else
    {
        return 0;
    }
}

// O(n) donde n son la cantidad de nodos del arbol y tambien son los stack frames que se crean.
bool perteneceT(int e, Tree t){
    if (!isEmptyT(t))
    {
        return rootT(t) == e || perteneceT(e, left(t)) || perteneceT(e, right(t));
    }
    else
    {
        return false;
    }
}

int aparicionesT(int e, Tree t){
    if (!isEmptyT(t))
    {
        if (rootT(t) == e)
        {
            return 1 + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
        else
        {
           return aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
    }
    else
    {
        return 0;
    }
}

int heightT(Tree t){
    if (!isEmptyT(t))
    {
        int leftTree = sizeT(left(t));
        int rightTree = sizeT(right(t));

        return 1 + (leftTree > rightTree ? leftTree : rightTree);
    }
    else
    {
        return 0;
    }
}

void agregarElemTree(Tree t, ArrayList ls){
    if (!isEmptyT(t))
    {
        add(rootT(t), ls);
        agregarElemTree(left(t), ls);
        agregarElemTree(right(t), ls);
    }
}

ArrayList toList(Tree t){
    ArrayList list = newArrayList();
    agregarElemTree(t, list);
    return list;
}

void agregarElemLeaves(Tree t, ArrayList ls){
    if (!isEmptyT(t))
    {
        if (isEmptyT(right(t)) && isEmptyT(left(t)))
        {
            add(t, ls);                                     // Falla pq el array es de int, para este caso deberia ser de Nodo*
        }
        else
        {
            agregarElemLeaves(right(t), ls);
            agregarElemLeaves(left(t), ls);
        }
    }
}

ArrayList leaves(Tree t){
    ArrayList leaves = newArrayList();
    agregarElemLeaves(t, leaves);
    return leaves;
}

void agregarElemEnLevelN(Tree t, int n, ArrayList ls){
    if (!isEmptyT(t))
    {
        if (n == 0)
        {
            add(t, ls);
        }
        else
        {
            agregarElemEnLevelN(right(t), n-1, ls);
            agregarElemEnLevelN(left(t), n-1, ls);
        }
    }
}

ArrayList levelN(int n, Tree t){
    ArrayList levelN = newArrayList();
    agregarElemEnLevelN(t, n, levelN);
    return levelN;
}


