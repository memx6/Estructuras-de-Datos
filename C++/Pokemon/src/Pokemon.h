#include <iostream>

using namespace std;

struct PokemonSt;

/// INVARIANTE DE REPRESENTACION
/// LA VIDA ES UN NUMERO POSITIVO

typedef PokemonSt* Pokemon;

Pokemon crearP(string nombre, int vida);
string getNombre(Pokemon p);
int getVida(Pokemon p);
void cambiarNombre(Pokemon& p, string nombre);
bool estaVivo(Pokemon p);
void restarUnoAVida(Pokemon& p);
void lucharN(int n, Pokemon& p1, Pokemon& p2); ///O(1)
void destruirP(Pokemon& p);
