#include <iostream>
#include <fstream>
#include <vector>
#include <iterator>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/enum.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef K::Point_2 Point;

int main(int argc, char* argv[])
{
    using std::vector;
    using std::cin;
    using std::cout;
    using std::cerr;
    using std::ifstream;

    ifstream input(argv[1]);
    ifstream output(argv[2]);
    
    size_t n; // amount of vertexes in polygon
    input >> n;
    vector<Point> polygon(n);
    for (size_t i = 0; i < n; ++i)
    {
        double x, y;
        input >> x >> y;
        polygon[i] = Point(x, y);
    }

    size_t m;
    input >> m;
    vector<vector<Point> > holes(m);
    for (size_t i = 0; i < m; ++i)
    {
        size_t count;
        input >> count;
        holes[i] = vector<Point>(count);
        for (size_t j = 0; j < count; ++j)
        {
            double x, y;
            input >> x >> y;
            holes[i][j] = Point(x, y);
        }
    }
    size_t L;
    input >> L;
    for (size_t i = 0; i < L; ++i)
    {
        double x, y;
        input >> x >> y;
        Point point(x, y); // point to check
        int right_answer = 0;

        switch (CGAL::bounded_side_2(polygon.begin(), polygon.end(), point, K()))
        {
        case CGAL::ON_BOUNDED_SIDE:
            right_answer = 1; // inside polygon, but maybe in one of holes
            break;
        case CGAL::ON_BOUNDARY:
            right_answer = 0; // on boundary of polygon
            break;
        case CGAL::ON_UNBOUNDED_SIDE:
            right_answer = -1; // outside of polygon
            break;
        }

        for (size_t i = 0; (i < m) && (right_answer == 1); ++i)
        {
            switch (CGAL::bounded_side_2(holes[i].begin(), holes[i].end(), point, K()))
            {
            case CGAL::ON_BOUNDED_SIDE: // inside holes[i]
                right_answer = -1;
                break;
            case CGAL::ON_BOUNDARY: // on boundary of holes[i]
                right_answer = 0;
                break;
            case CGAL::ON_UNBOUNDED_SIDE: // outside of holes[i]
                break;
            }
        }
        int answer;
        output >> answer;
        if ((answer == -1) || (answer == 1) || (answer == 0))
        {
            if (answer == right_answer)
            {
                return 0; // AC
            }
            return 1; // WA   
        }
        else
        {
            return 1; // PE
        }    
    }

    input.close();
    output.close();
}