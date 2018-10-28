#include <iostream>
#include "Pokemon.h"

using namespace std;

int main() {
    Pokemon p1 = crearP("bulbasaur", 11);
    Pokemon p2 = crearP("Charmander", 3);
    cout << "  nombre: " << getNombre(p1) << endl;
    cout << "  vida: " << getVida(p1) << endl;
    lucharN ( 4, p1, p2);
    cout << "  nombre: " << getNombre(p1) << endl;
    cout << "  vida: " << getVida(p1) << endl;
    cout << "  nombre: " << getNombre(p2) << endl;
    cout << "  vida: " << getVida(p2) << endl;
}