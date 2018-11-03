#include <iostream>

using namespace std;

#define ELEM_TYPE int

struct ListNode;
struct ListHeader;
typedef ListHeader* List;

List nil();                // O(1)
bool isNil(List xs);        // O(1)
void cons(ELEM_TYPE x, List& xs); // O(1)
void snoc(List& xs, ELEM_TYPE x); // O(1)
ELEM_TYPE head(List xs);          // O(1)
void tail(List& xs);        // O(1)
int length(List xs);            // O(1)
void printList(List xs);          // O(n)
void destroyList(List& xs);       // O(n)

typedef ListNode* ListIterator;

ListIterator initIt(List xs); // O(1)
bool finished(ListIterator it); // O(1)
ELEM_TYPE getCurrent(ListIterator it); // O(1)
void setCurrent(ELEM_TYPE x, ListIterator it); // O(1)
void next(ListIterator& it); // O(1)
