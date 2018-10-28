// .h interface
// .cpp implementacion
#include <iostream>
using namespace std;

struct Persona;
// Funciones de construcción
// -------------------------

Persona crearPersona (string nombre);

// funciones de Acceso
string getNombre(Persona p);
int getEdad (Persona p);

// Funciones para darle valor a los campos

Persona setNombre(Persona p, string nombre);
Persona setEdad(Persona p, int);

// Funciones Varias

Persona crecer(Persona p);
void imprimir (Persona p);
