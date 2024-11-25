#include "Persona.h"
#include <iostream>
using namespace std;

struct PersonaSt {
    int edad;
    string nombre;
};

Persona consPersona(string nombre, int edad){
    PersonaSt* p = new PersonaSt; 
    p->edad = edad;
    p->nombre = nombre;
    return p;
}

string nombre(Persona p){
    return p->nombre;
}

int edad(Persona p){
    return p->edad;
}

void crecer(Persona p){
    p->edad++;
}

void cambioDeNombre(string nombre, Persona p){
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
    return p1->edad == p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2){
    if (p1->edad > p2->edad)
    {
        return p1;
    }
    return p2;
}