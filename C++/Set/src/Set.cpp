#include <iostream>
#include "Set.h"
#include "linked_list.h"

struct NodeSetSt
{
    ELEM_TYPE elem;
    NodeSetSt* next;
};

struct SetST
{
    int size;
    NodeSetSt* first;
};

Set emptyS (){
    SetST* set = new SetST;
    set-> size = 0;
    set-> first = NULL;
    return set;
}

Set singleton(int x){
    Set s = emptyS();
    NodeSetSt* node = new NodeSetSt;
    node -> elem = x;
    node -> next = NULL;
    s -> first = node;
    s -> size = 1;
    return s;
}

bool belongs(int x, Set s){
   NodeSetSt* p = s->first;
   while(p != NULL) {
       if(p->elem == x) return true;
       p = p->next;
   }
   return false;
}


void addS(int x , Set& s){
    if(belongs(x, s)) return;
    NodeSetSt* node = new NodeSetSt;
    node -> elem = x;
    node -> next = NULL;
    node->next = s->first;
    s->size++;
    s->first = node;
}

void removeS(int x, Set& s){
    if (not belongs (x, s)) return;
    NodeSetSt* e = s->first;
    NodeSetSt* prev;
    while (e->elem != x) {
        prev = e;
        e = e -> next;
    }
    if (prev == NULL) {
        s->first = NULL;
    } else {
        prev->next = e->next;
    }
    delete e;
}


int size(Set s){
    return s->size;
}

Set unionS(Set s1, Set s2){
    NodeSetSt* e = s2->first;
    while (e != NULL){
        if (not belongs (e->elem, s1)){
            addS (e->elem,s1);
            e = e->next;
        }
        e = e->next;
    }
    return s1; 
}

Set intersectS(Set s1, Set s2){
    Set setN = emptyS();
    NodeSetSt* e = s2->first;
    while(e != NULL) {
        if (belongs (e->elem, s1)){
             addS (e->elem,setN);
            
        }
        e = e->next;
    }
    return setN;
}

List setToList(Set s){
    List listR = nil();
    NodeSetSt* e = s-> first;
    while (e != NULL){
        cons (e->elem, listR);
        e = e->next;
    }
    return listR;    
}


void destroySet(Set& s){
    NodeSetSt* p = s->first;
    while(p != NULL) {
        NodeSetSt* tmp = p->next;
        delete p;
        p = tmp;
    }
    delete s;
}