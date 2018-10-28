#include "Fraccion.h"

struct FraccionSt {
    int numerador;
    int denominador;
};

Fraccion fraccion(int x, int y){
    FraccionSt* f = new FraccionSt;
    f->numerador = x;
    f->denominador = y;
    return f;
}

Fraccion sumar(Fraccion f1, Fraccion f2){
    if (f1->denominador == f2->denominador)
    {
        return sumarConMismoDenominador(f1 , f2);
    }
    else
    {
        return sumaConDiferenteDenominador (f1,f2);
    }
}


Fraccion restar(Fraccion f1, Fraccion f2){
    if (f1->denominador == f2->denominador)
    {
        return restarConMismoDenominador(f1 , f2);
    }
    else
    {
        return restarConDiferenteDenominador (f1,f2);
    }
}


Fraccion multiplicar(Fraccion f1, Fraccion f2){
    int n = numerador(f1) * numerador(f2);
    int d = denominador(f1) * denominador(f2);
    return fraccion(n , d);
}


Fraccion dividir(Fraccion f1, Fraccion f2){
    int n = numerador(f1) * denominador(f2);
    int d = denominador(f1) * numerador(f2);

    return fraccion(n , d);
}


void invertir(Fraccion& f1){
//Invierte numerador y denominador.
    int n = denominador(f1);
    int d = numerador(f1);
    f1->numerador = n;
    f1-> denominador = d;
}


void simplificar(Fraccion& f1){
    int mcdr = mcd(numerador(f1), denominador(f1));
    f1->numerador /= mcdr;
    f1->denominador /= mcdr;
}

int mcd(int n, int d) {
    if(n < d) return mcd(d,n);
    if(d == 0) return n;
    return mcd(d, n % d);
}

void destruir(Fraccion& f1){
    delete f1;
}

int numerador (Fraccion f){
    return f->numerador;
}

int denominador (Fraccion f){
    return f->denominador;
}

Fraccion sumaConDiferenteDenominador(Fraccion f1, Fraccion f2){
    int n = numerador(f1) * denominador(f2) + numerador(f2) * denominador(f1);
    int d = denominador(f1) * denominador(f2);
    return fraccion (n, d);
}

Fraccion sumarConMismoDenominador(Fraccion f1, Fraccion f2){
    int n = numerador(f1) + numerador(f2);
    return fraccion(n,denominador(f1));
}


Fraccion restarConMismoDenominador(Fraccion f1, Fraccion f2){
     int n = numerador(f1) - numerador(f2);
    return fraccion(n,denominador(f1));
}


Fraccion restarConDiferenteDenominador(Fraccion f1, Fraccion f2){
    int n = numerador(f1) * denominador(f2) - numerador(f2) * denominador(f1);
    int d = denominador(f1) * denominador(f2);
    return fraccion (n, d);
}



