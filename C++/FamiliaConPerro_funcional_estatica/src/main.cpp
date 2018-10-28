#include <iostream>
#include "familia.h"
using namespace std;

int main()
{

    cout << "Creo una nueva familia vacía, recuerden que estamos en la implementación ESTÁTICA y en la memoria stack " <<
    "de main se reserva memoria para un struct Familia, cuatro struct Persona y cuatro Struct Perro." << endl << endl;
    Familia f = crearFamilia();

    cout << "Asigno el padre a la familia" << endl << endl;
    Persona padre = crearPersona("Juan", 50);
    f = asignarPadre(f, padre);

    cout << "Muestro los datos del padre" << endl << endl;
    imprimir(getPadre(f));


    cout << "Asigno la madre a la familia" << endl << endl;
    Persona madre = crearPersona("Ana", 48);
    f = asignarMadre(f, madre);

    cout << "Muestro los datos de la madre" << endl << endl;
    imprimir(getMadre(f));


    cout << "Asigno al hijo a la familia" << endl << endl;
    Persona hijo = crearPersona("Lucas", 12);
    f = asignarHijo(f, hijo);

    cout << "Muestro los datos del hijo" << endl << endl;
    imprimir(getHijo(f));

    cout << "Le asigno un perro a la familia. Recuerden que decidimos que la persona tenga un perro a pesar que " <<
    "la mejor solución es asignar el perro a la familia." << endl;
    Perro perro = crearPerro("Lazy", 1);
    f = asignarPerro(f, perro);

    cout << "Muestro los datos de perro accediendo desde el padre" << endl << endl;
    imprimir(getPerro(getPadre(f)));

    cout << "Muestro los datos de perro accediendo desde la madre" << endl << endl;
    imprimir(getPerro(getMadre(f)));

    cout << "Muestro los datos de perro accediendo desde el hijo" << endl << endl;
    imprimir(getPerro(getHijo(f)));

    cout << "IMPORTANTE: Noten que asigné un struct Perro a cada una de las personas. Físicamente los perros son distintos pero" <<
    "modelan al perro de la familia." << endl << endl;

    cout << "Todos crecen un año ..." << endl << endl;
    f = todosCrecenUnAnio(f);

    imprimir(getPadre(f));
    imprimir(getMadre(f));
    imprimir(getHijo(f));
    imprimir(getPerro(getPadre(f)));

    cout << "El perro se pierde y los datos van a apuntar a datos incoherentes ... " << endl << endl;
    f = sePierdeElPerro(f);

    imprimir(getPerro(getPadre(f)));
    imprimir(getPerro(getMadre(f)));
    imprimir(getPerro(getHijo(f)));

    return 0;
}
