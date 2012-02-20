#include <iostream>
#include <fstream>
#include <vector>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel::Point_2 Point_2;


bool isInChull(const Point_2 &p, const std::vector<Point_2> &chull) {
    for (int i = 0; i < chull.size(); i++) {
        if (CGAL::orientation(chull[i], chull[(i + 1) % chull.size()], p) == CGAL::RIGHT_TURN)
            return false;
    }
    return true; 
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: convexhull-checker inputfile outputfile" << std::endl;
        return -1;
    }
    
    std::ifstream input(argv[1]);
    int n;
    input >> n;
    std::vector<Point_2> points(n);
    for (int i = 0; i < n; i++) {
        double x, y;
        input >> x >> y;
        points[i] = Point_2(x, y);
    }
    input.close();


    std::ifstream output(argv[2]);
    int k;
    output >> k;
    std::vector<Point_2> chull(k);
    for (int i = 0; i < k; i++) {
        int index;
        output >> index;
        chull[i] = points[index - 1];
    }
    output.close();

    bool allToLeft = true;
    for (int i = 0; i < n && allToLeft; i++) {
        if (!isInChull(points[i], chull)) {
            allToLeft = false;
        }
    }
    return allToLeft ? 0 : 1;
}
