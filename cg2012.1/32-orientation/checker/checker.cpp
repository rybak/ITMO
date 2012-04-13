#include <iostream>
#include <fstream>
#include <vector>
#include <iterator>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/convex_hull_2.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel::Point_2 Point_2;


int main(int argc, char* argv[]) {
    using std::vector;
    using std::cin;
    using std::cout;
    using std::cerr;
    using std::ifstream;

    if (argc != 3) {
        cerr << "Usage: orientation-checker inputfile outputfile" << std::endl;
        return -1;
    }
    
    ifstream input(argv[1]);
    int n;
    input >> n;
    vector<int> expected(n);
    for (int i = 0; i < n; i++) {
        double ax, ay, bx, by, cx, cy;
        input >> ax >> ay >> bx >> by >> cx >> cy;
        Point_2 a(ax, ay), b(bx, by), c(cx, cy);
        CGAL::Orientation o = CGAL::orientation(a, b, c);
        if (o == CGAL::LEFT_TURN) {
            expected[i] = 1;
        } else if (o == CGAL::COLLINEAR) {
            expected[i] = 0;
        } else {
            expected[i] = -1;
        }
    }

    ifstream output(argv[2]);
    for (int i = 0; i < n; i++) {
        int ans;
        output >> ans;
        if (expected[i] != ans) {
            #ifdef DEBUG
            std::cerr << "WRONG" << std::endl;
            #endif        
            return 1;
        }
    }
    #ifdef DEBUG
    std::cerr << "CORRECT" << std::endl;
    #endif        
    return 0;
}
