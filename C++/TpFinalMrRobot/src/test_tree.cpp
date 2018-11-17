#include "test_tree.h"

// PROPOSITO: DEVUELVE LA CANTIDAD DE NODOS
int sizeT(GenTree t) {
    int r = 1;
    ArrayList ts = children(t);
    for(int i = 0; i < length(ts); i++) {
        r += sizeT(getAt(ts, i));
    }
    return r;
}

// PROPOSITO: INDICA SI EL ELEMENTO APARECE EN EL ARBOL
bool containsT(string x, GenTree t) {
    bool r = (value(t) == x) ? true : false;
    ArrayList ts = children(t);
    for(int i = 0; i < length(ts); i++) {
        r = r || containsT (x, getAt(children(t), i));
    }
    return r;
}

// PROPOSITO: INDICA LA CANTIDAD DE VECES QUE APARECE EL ELEMENTO
int occurrencesT(string x, GenTree t) {
    int r = (value(t) == x) ? 1 : 0;
    ArrayList ts = children(t);
    for(int i = 0; i < length(ts); i++) {
        r += occurrencesT(x, getAt(ts, i));
    }
    return r;
}

// PROPOSITO: DEVUELVE LA CANTIDAD DE RAMAS
int countLeaves(GenTree t) {
    int r = (isLeaf(t)) ? 1 : 0;
    ArrayList ts = children(t);
    for(int i = 0; i < length(ts); i++) {
        r += countLeaves(getAt(ts, i));
    }
    return r;
}

ArrayList maxChild(ArrayList& xs){


}
// PROPOSITO: DEVUELVE LA ALTURA DEL ARBOL
int heightT(GenTree t) {
    int h = 0;
    ArrayList ts = children(t);
    for(int i = 0; i < length(ts); i++) {
        h = max( h, heightT(getAt(ts, i)));
    }
    h++;
    return h;
}


////
List hijosEnLista (List& lis, GenTree t){
        ArrayList ts = children(t);
        for(int i = 0; i < length(ts); i++){
            hijosEnLista(lis, getAt(ts, i));
        }
        snoc(lis, value(t));
        return lis;
}
// PROPOSITO: DEVUELVE LOS ELEMENTOS DEL ARBOL EN UNA LISTA
// PISTA: HACER UNA SUBTAREA QUE RECIBA UNA LISTA POR PARAMETRO
// PARA PODER MODIFICARLA
List toList(GenTree t) {
    List lis = nil();
    hijosEnLista(lis, t);
    return lis;

}

List hojasEnLista (List& lis, GenTree t){
        ArrayList ts = children(t);
        for(int i = 0; i < length(ts); i++){
                hojasEnLista(lis, getAt(ts, i));
        }
        if (isLeaf(t)){
            snoc(lis, value(t));
        }
        return lis;
}

// PROPOSITO: DEVUELVE LOS ELEMENTOS QUE ESTAN EN LAS HOJAS
List leaves(GenTree t) {
    List lis = nil();
    return hojasEnLista(lis, t);
   
}


// PROPOSITO: AGREGA LOS ELEMENTOS DEL ARRAYLIST AL GENTREE
void addChildren(ArrayList ts, GenTree t) {
    for(int i = 0; i < length(ts); i++){
        addChild(t, getAt(ts, i));
    }
}

void elementosDelNivel(GenTree t, int n, List& xs){
   ArrayList ts = children(t);
   if (n == 0) {
       snoc(xs,value(t));
       return;       
   }
    for(int i = 0; i < length(ts); i++){
        elementosDelNivel(getAt(ts,i), n - 1, xs);
    }
}
// PROPOSITO: DEVUELVE LOS ELEMENTOS DEL NIVEL "n"
List levelN(int n, GenTree t) {
    List lis = nil();
    elementosDelNivel(t,n,lis);
    return lis;
}

// PROPOSITO: DEVUELVE EL STRING CON MAYOR LONGITUD
string stringWithMaxSize(GenTree t) {
    string s = value(t);
    ArrayList ts = children(t);
    for(int i = 0; i < length(ts); i++) {
        string s2 = stringWithMaxSize(getAt(ts, i));
        if (size(s2) > size(s)){
            s = s2;
        }
    }
    return s;
}

//PROPOSITO: Agrega a la lista que recibe por parametro, los elementos que devuelve la 
// busqueda por indice del path recibido.
List elemsInPaths(GenTree t, int path[], int pathSize,int i, List& xs){
   snoc(xs,value(t));
   ArrayList ts = children(t);
   if (i == pathSize){
       return xs;
   }
   elemsInPaths(getAt(ts,path[i]),path,pathSize, i+1, xs);
}
// PROPOSITO: DADO UN ARRAY CON NUMEROS QUE INDICAN UN CAMINO POR EL ARBOL
// DEVUELVE LOS ELEMENTOS QUE SE ENCUENTRAN EN DICHO CAMINO
//Precondicion: El PathSize es la longitud del Path[].
List elemsInPath(GenTree t, int path[], int pathSize) {
    List ls = nil();
    return elemsInPaths(t,path,pathSize,0,ls);
}

// PROPOSITO: DEVUELVE EL ARBOL CON MAYOR ALTURA
// PRECONDICION: LA LISTA NO ESTA VACIA
GenTree maxHeight(ArrayList ts) {
    GenTree h = getAt(ts, 0);
    for(int i = 1; i < length(ts); i++) {
        if (heightT(h) < heightT(getAt(ts, i))){
            h = getAt(ts, i);
        }
        maxHeight(children(getAt(ts, i)));
    }
    return h;
}


List listConcat (List& lis, ArrayList ts){
        for(int i = 0; i < length(ts); i++){
                hijosEnLista(lis, getAt(ts, i));
        }
        return lis;
}
// PROPOSITO: DEVUELVE LA CONCATENACION DE LOS ELEMENTOS DE LOS ARBOLES DE LA LISTA
List concatToList(ArrayList ts) {
    List lis = nil();
    return listConcat(lis, ts);
}

// PROPOSITO: INDICA SI TODOS LOS ELEMENTOS DE LA LISTA ESTAN EN EL ARBOL
bool containsAll(List elems, GenTree t) {
    ListIterator it = initIt(elems);
    bool r = true;
    ArrayList ts = children(t);
    while (not finished(it)){
        r = r && containsT(getCurrent(it),t);
        next(it);
    }
    return r;
}

// PROPOSITO: CONCATENA TODOS LOS STRINGS DEL ARBOL
string concatAll(GenTree t) {
    string r = "";
    ArrayList ts = children(t);
    for(int i = 0; i < length(ts); i++){
        r+=concatAll(getAt(ts, i));
    }
    return r+=value(t);

}

void testTree() {
    // ACA VAN LOS TEST DE LAS FUNCIONES IMPLEMENTADAS

    GenTree t = leaf("");
    for(int i = 0; i < 10; i++) {
        GenTree t2 = leaf("a");
        for(int j = 0; j < 10; j++) {
            addChild(t2, leaf("b"));
        }
        addChild(t, t2);
    }

    cout << "sum: " << sizeT(t) << endl;
    cout << "contains b: " << (containsT("b", t) ? "true" : "false") << endl;
    cout << "occurrences b: " << occurrencesT("b", t) << endl;
    cout << "leaves: " << countLeaves(t) << endl;
    cout << "height: " << heightT(t) << endl;
    cout << "toList: " ; printList(toList(t));
    //cout << "leaves: " ; printList(leaves(t));
    cout << "levelN: " ; printList(levelN(2,t));
    
    // Prueba Funcion addChildren
    // ArrayList ch = crearArrayList();
    // for(int i = 0; i < 10; i++) {
    //     add(ch, leaf(""));
    // }
    // GenTree t5 = leaf("");
    // addChildren(ch, t5);
    // cout << "children: " ; imprimirArrayList(children(t5));
    
    //Prueba Funcion maxHeigthT
    cout << "MaxHeight: " << value(maxHeight(children(t))) << endl;
    cout << "MaxHeight Altura: " << heightT(maxHeight(children(t))) << endl;
    
    //Prueba elemsInPath:
    int path[] = {0,2};
    cout << "elemsInPath: "; printList(elemsInPath(t,path, 2));

    //Prueba stringWithMaxSize
    cout << "stringWithMaxSize: " << stringWithMaxSize(t) <<endl;

    //Prueba concatToList:
    cout << "concatToList: " ; printList(concatToList(children(t)));

    //Prueba containsAll:
    List ls = nil();
    snoc(ls,("a"));
    snoc(ls,("b"));
    cout << "containsALL: " << containsAll(ls, t) <<endl;

    //Prueba concatAll:
    cout << "concatALL: " << concatAll(t) <<endl;
}
