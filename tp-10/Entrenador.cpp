#include <iostream>
#include "Entrenador.h"
using namespace std;

struct EntrenadorSt
{
    string nombre;
    Pokemon* pokemones;
    int cantPokemon;
};
// Invariantes: * cantPokemon representa la cantidad de elementos que hay en el array de pokemones.

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemones = pokemon;
    return e;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int c = 0;
    for (size_t i = 0; i < (e->cantPokemon - 1); i++)
    {
        if (e->pokemones[i]->tipo == tipo)
        {
            c++;
        }
    }
    return c;
}

Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemones[i]; 
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    for (size_t i = 0; i < (e2->cantPokemon - 1); i++)
    {
        if (!algunoSuperaA(e1->pokemones, e1->cantPokemon, e2->pokemones[i]))
        {
            return false;
        }
    }
    return true;
}

bool algunoSuperaA(Pokemon* pokemones, int cantidad, Pokemon p){
    for (size_t i = 0; i < (cantidad - 1); i++)
    {
        if (superaA(pokemones[i], p))
        {
            return true;
        } 
    }
    return false;
}