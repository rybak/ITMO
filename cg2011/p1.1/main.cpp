#include <fstream>
#include "geometry.h"

int main()
{
    std::ifstream in("input.txt");
    std::ofstream out("output.txt");

    double x1, y1, x2, y2;
    double x3, y3, x4, y4;
    while(
        in >> x1 >> y1 >> x2 >> y2
        )
    {
    in >> x3 >> y3 >> x4 >> y4;
    
    point A(x1, y1);
    point B(x2, y2);
    point C(x3, y3);
    point D(x4, y4);
    segment AB(A, B);
    segment CD(C, D);

    if (intersects(AB, CD))
    {
        out << "YES\n";
    }    
    else
    {
        out << "NO\n";
    }
    }
    return 0;
}


