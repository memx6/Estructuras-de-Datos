#include <iostream>
#include "Fraccion.h"

using namespace std;

int main(){
    Fraccion f1 = fraccion(1, 2);
    Fraccion f2 = fraccion(4,2);

    Fraccion f3 = sumar(f1, f2);
    cout << "Sumar: " << endl;
    cout << "Numerador: " << numerador(f3) << endl;
    cout << "Denominador: "<< denominador(f3) << "\n" << endl;


    f3 = restar(f1, f2);
    cout << "Restar: " << endl;
    cout << "Numerador: " << numerador(f3) << endl;
    cout << "Denominador: "<< denominador(f3) << "\n" << endl;


    f3 = multiplicar(f1, f2);
    cout << "Multiplicar: " << endl;
    cout << "Numerador: " << numerador(f3) << endl;
    cout << "Denominador: "<< denominador(f3) << "\n" <<  endl;


    f3 = dividir(f1, f2);
    cout << "Dividir: " << endl;
    cout << "Numerador: " << numerador(f3) << endl;
    cout << "Denominador: "<< denominador(f3) << "\n" << endl;


    invertir(f1);
    cout << "Invertir: " << endl;
    cout << "Numerador: " << numerador(f1) << endl;
    cout << "Denominador: "<< denominador(f1) << "\n" << endl;


    simplificar(f2);
    cout << "Simplificado: " << endl;
    cout << "Numerador: " << numerador(f2) << endl;
    cout << "Denominador: "<< denominador(f2) << "\n" << endl;


    destruir(f3);
    cout << "Destruir: " << endl;
    cout << "Numerador: " << numerador(f3) << endl;
    cout << "Denominador: "<< denominador(f3) << "\n" << endl;

    return 0;
}
