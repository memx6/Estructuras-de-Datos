#ifndef __GENTREE_H
#define __GENTREE_H

#include <iostream>

using namespace std;

#define T_ELEM_TYPE string

struct ArrayListHeader;
typedef ArrayListHeader* ArrayList;

struct GNode;
typedef GNode* GenTree;

// Crea un GenTree
GenTree leaf(T_ELEM_TYPE s);

//Verifica sin el GenTree es una Hoja, en caso de serlo retorna True.
bool isLeaf(GenTree t);

//Retorna el elemento de ese Nodo del GenTree.
T_ELEM_TYPE value(GenTree t);

//Denota el ArrayList con todos sus hijos.
ArrayList children(GenTree t);

// Al GenTree que recibo como parametro le agrega a la lista de hijos un hijo nuevo.
void addChild(GenTree& t, GenTree child);

//Destruye cada uno de los arrayLists que conforman el arbol..
void destroyTree(GenTree& t);

#endif
