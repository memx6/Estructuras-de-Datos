#include "gentree.h"
#include "array_list.h"

struct GNode
{
  string elem;
  ArrayList children;
};

GenTree leaf(T_ELEM_TYPE x)
{
  GNode* g = new GNode;
  g -> elem = x;
  g ->children = crearArrayList();
  return g;
}

bool isLeaf(GenTree t)
{
  return isEmpty(t->children);
}

T_ELEM_TYPE value(GenTree t)
{
  return t->elem;
}

ArrayList children(GenTree t)
{
  return t->children;
}

void addChild(GenTree& t, GenTree child)
{
  add(t->children, child);
}

void destroyTree(GenTree& t)
{
  ArrayList array = t->children;
  for (int i=0; i < length(array); i++){
    GNode* g = getAt(t->children, i);
    destroyTree(g);
  }
  destroyArrayList(array);
  delete t;
}
