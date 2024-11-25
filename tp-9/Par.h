
#include <iostream>
using namespace std;

struct StructPar {
    int x;
    int y;
};

typedef struct StructPar Par;

Par consPar(int x, int y);

int fst(Par p);

int snd(Par p);

int maxDelPar(Par p);

Par swap(Par p);

Par divisionYResto(int n, int m);
