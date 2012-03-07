#include <iostream>
#include <fstream>
#include <vector>
#include <iterator>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel::Point_2 Point_2;


int main(int argc, char* argv[])
{
    using std::vector;
    using std::cin;
    using std::cout;
    using std::cerr;
    using std::ifstream;

    ifstream input(argv[1]);
    double x, y;
    input >> x >> y;
    Point_2 point(x, y); // point to check
    
    size_t n; // amount of vertexes in polygon
    input >> n;
    vector<Point_2> polygon(n);
    for (size_t i = 0; i < n; ++i)
    {
        double x, y;
        input >> x >> y;
        polygon[i] = Point_2(x, y);
    }

    size_t m;
    input >> m;
    vector<size_t> count(m); // amount of vertexes in hole[i]
    vector<vector<Point_2> > holes(m);
    for (size_t i = 0; i < m; ++i)
    {
        input >> count[i];
        holes[i] = vector<Point_2>(count[i]);
        for (size_t j = 0; j < count[i]; ++j)
        {
            double x, y;
            input >> x >> y;
            holes[i][j] = Point_2(x, y);
        }
    }
    input.close();

    bool right_answer = false;

    // TODO solution here

    ifstream output(argv[2]);
    std::string s;
    output >> s;
    output.close();
    bool answer;
    if (s == "YES" || s == "NO")
    {
        answer = (s == "YES");
    }
    else
    {
        // PE
    }

    if (answer == right_answer)
    {
        return 0; // AC
    }
    
    return 1; // WA
}