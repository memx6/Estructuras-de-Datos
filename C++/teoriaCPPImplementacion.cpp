#include "teoriaCPPInterface.h"


struct Persona
{
    string nombre;
    int edad;
};

Persona crearPersona(string nombre){
    Persona p;
    p.nombre = nombre;
    p.edad = 0;
    return p;
}
string getNombre (Persona p){
    return p.nombre;
}

int getEdad(Persona p){
    return p.edad;
}

Persona setNombre(Persona p , string nombre){
    p.nombre = nombre;
    return p;
}
Persona setEdad(Persona p , int edad){
    p.edad = edad;
    return p;
}

Persona crecer(Persona p){
    p.edad++;
    return p;
}

void imprimir (Persona p){
    cout << "Nombre: " << p.nombre << endl;
    cout << "Edad: " << p.edad << endl;
}
