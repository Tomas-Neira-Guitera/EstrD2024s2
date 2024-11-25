#include <iostream>
using namespace std;

struct ArrayListSt;

typedef ArrayListSt* ArrayList;

ArrayList newArrayList();

ArrayList newArrayListWhit(int);

int lengthAL(ArrayList);

int get(int, ArrayList);

void set(int, int, ArrayList);

void resize(int, ArrayList);

void add(int, ArrayList);

void remove(ArrayList);