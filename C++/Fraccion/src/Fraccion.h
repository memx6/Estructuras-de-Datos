#include <iostream>

using namespace std;

struct FraccionSt;

typedef FraccionSt* Fraccion;

Fraccion fraccion(int x, int y);
int numerador (Fraccion f);
int denominador (Fraccion f);
Fraccion sumaConDiferenteDenominador(Fraccion f1, Fraccion f2);
Fraccion sumarConMismoDenominador(Fraccion f1, Fraccion f2);
Fraccion restarConMismoDenominador(Fraccion f1, Fraccion f2);
Fraccion restarConDiferenteDenominador(Fraccion f1, Fraccion f2);
Fraccion sumar(Fraccion f1, Fraccion f2);
Fraccion restar(Fraccion f1, Fraccion f2);
Fraccion multiplicar(Fraccion f1, Fraccion f2);
Fraccion dividir(Fraccion f1, Fraccion f2);
void invertir(Fraccion& f1);
//Invierte numerador y denominador.
void simplificar(Fraccion& f1);
void destruir(Fraccion& f1);
int mcd(int a, int b);

