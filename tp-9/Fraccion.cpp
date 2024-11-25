#include "Fraccion.h"
#include <iostream>
using namespace std;

Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

int numerador(Fraccion f){
    return f.numerador;
}

int denominador(Fraccion f){
    return f.denominador;
}

float division(Fraccion f){
    return f.numerador / f.denominador;
}

Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion f;
    f.numerador = f1.numerador * f2.numerador;
    f.denominador = f1.denominador * f2.denominador;
    return f;
}

int mcd(int n, int m){ 
    while (m != 0) {
        int a = m;
        m = n % m;
        n = a;
    }
    return n;
}

Fraccion simplificada(Fraccion p){
    Fraccion f;
    int maximoComunDivisor = mcd(p.numerador, p.denominador);
    f.numerador = p.numerador / maximoComunDivisor;
    f.denominador = p.denominador / maximoComunDivisor;
    return f;
}

int mcm(int n, int m){ 
    
    return (n * m) / mcd(n,m);
} 

Fraccion sumF(Fraccion f1, Fraccion f2){
    Fraccion f3;
    int minimoComunMultiplo = mcm(f1.denominador, f2.denominador);
    int numeradorF1 = f1.numerador * (minimoComunMultiplo / f1.denominador);
    int numeradorF2 = f2.numerador * (minimoComunMultiplo / f2.denominador);

    f3.numerador = numeradorF1 + numeradorF2;
    f2.denominador = minimoComunMultiplo;
    return f3;
}