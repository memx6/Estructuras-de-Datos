#include <iostream>
#include "linked_list.h"

using namespace std;

int main()
{
    List xs = nil();

    printList(xs);
    cout << endl;
    cout << "vacia? " << (isNil(xs) ? "SI" : "NO") << endl;
    cout << "size: " << length(xs) << endl << endl;

    cout << "agrego elementos 1, 2 y 3" << endl << endl;

    snoc(xs, 1);
    printList(xs);
    snoc(xs, 2);
    printList(xs);
    snoc(xs, 3);
    printList(xs);
    cout << endl;
    cout << "vacia? " << (isNil(xs) ? "SI" : "NO") << endl;
    cout << "size: " << length(xs) << endl << endl;

    cout << "sale el " << head(xs) << endl << endl;

    tail(xs);

    printList(xs);
    cout << endl;
    cout << "vacia? " << (isNil(xs) ? "SI" : "NO") << endl;
    cout << "size: " << length(xs) << endl << endl;

    cout << "sale el " << head(xs) << endl << endl;

    tail(xs);

    printList(xs);
    cout << endl;
    cout << "vacia? " << (isNil(xs) ? "SI" : "NO") << endl;
    cout << "size: " << length(xs) << endl << endl;

    cout << "sale el " << head(xs) << endl << endl;

    tail(xs);

    printList(xs);
    cout << endl;
    cout << "vacia? " << (isNil(xs) ? "SI" : "NO") << endl;
    cout << "size: " << length(xs) << endl << endl;

    cout << "lleno de nuevo y hago una copia" << endl << endl;

    snoc(xs, 1);
    snoc(xs, 2);
    snoc(xs, 3);

    cout << "borro todo" << endl;
    destroyList(xs);

    return 0;
}
