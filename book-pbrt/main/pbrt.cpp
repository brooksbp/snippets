
#include "pbrt.h"
#include "geometry.h"

int main(int argc, char* argv[]) {

    Vector n(1.0f, 2, 3);
    Vector m(1.0f, 2, 3);
    n += m;
    std::cout << n << std::endl;

    return 0;
}
