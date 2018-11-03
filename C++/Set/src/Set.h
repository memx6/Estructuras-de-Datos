#include <iostream>
#define ELEM_TYPE int

struct NodeSetSt;
struct SetST;
typedef SetST* Set;

Set emptyS();
Set singleton(int x);
bool belongs(int x, Set s);
void addS(int x , Set& s);
void removeS(int x, Set& s);
int size(Set s);
Set unionS(Set s1, Set s2);
Set insersectS(Set s1, Set s2);
List setToList(Set s);
void destroySet(Set& s);