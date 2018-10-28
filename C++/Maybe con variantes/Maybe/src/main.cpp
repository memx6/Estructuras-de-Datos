#include <iostream>
#include "Maybe.h"

using namespace std;

int main()
{
    Maybe m = nothing();
    setJust(m, 2);
    cout << fromJust(m) << endl;

    return 0;
}
