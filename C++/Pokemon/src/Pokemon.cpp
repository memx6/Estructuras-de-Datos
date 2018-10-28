#include "Pokemon.h"

struct PokemonSt {
    string nombre;
    int vida;
};

Pokemon crearP(string nombre, int vida){
    PokemonSt* p = new PokemonSt;
    p->nombre = nombre;
    p->vida = vida;
    return p;
}

string getNombre(Pokemon p){
    return p->nombre;
}

int getVida(Pokemon p){
    return p->vida;
}

void cambiarNombre(Pokemon& p, string nombre){
    p->nombre = nombre;
}
bool estaVivo(Pokemon p){
    return p->vida == 0;
}

void restarUnoAVida(Pokemon& p){
    p->vida--;
}
void lucharN(int n, Pokemon& p1, Pokemon& p2) { ///O(1)
    if ( getVida (p1) >= n && getVida (p2) >= n)
        while (n != 0){
        restarUnoAVida (p1);
        restarUnoAVida (p2);
        n = n - 1;
        };
}
void destruirP(Pokemon& p){
    delete p;
}
