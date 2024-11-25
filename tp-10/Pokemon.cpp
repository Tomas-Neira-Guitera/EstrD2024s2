#include <iostream>
#include "Pokemon.h"
using namespace std;

struct PokemonSt
{
    TipoDePokemon tipo;
    int vida;
};
// Inv de Rep: la vida no puede ser menor a 0

Pokemon consPokemon(TipoDePokemon tipo)
{
    PokemonSt *p = new PokemonSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p)
{
    return p->tipo;
}

int energia(Pokemon p)
{
    return p->vida;
}

void perderEnergia(int energia, Pokemon p)
{
    if ((p->vida - energia) < 0)
    {
        p->vida = 0;
    }
    else
    {
        p->vida = p->vida - energia;
    }
}

bool superaA(Pokemon p1, Pokemon p2)
{
    if (p1->tipo == "Agua" && p2->tipo == "Fuego")
    {
        return true;
    }
    else if (p1->tipo == "Fuego" && p2->tipo == "Planta")
    {
        return true;
    }
    else if (p1->tipo == "Planta" && p2->tipo == "Agua")
    {
        return true;
    }
    false;
}

