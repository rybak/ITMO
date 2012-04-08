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
        cerr << "Usage: convexhull-checker inputfile outputfile" << std::endl;
        return -1;
    }
    
    ifstream input(argv[1]);
    int n;
    input >> n;
    vector<Point_2> points(n);
    for (int i = 0; i < n; i++) {
        double x, y;
        input >> x >> y;
        points[i] = Point_2(x, y);
    }
    input.close();


    ifstream output(argv[2]);
    int k;
    output >> k;
    vector<Point_2> chull(k);
    for (int i = 0; i < k; i++) {
        double x, y;
        output >> x >> y;
        chull[i] = Point_2(x, y);
    }
    output.close();

    vector<Point_2> tmp(points.size());
    vector<Point_2>::iterator end = convex_hull_2(points.begin(), points.end(), tmp.begin()); 
    vector<Point_2> result(tmp.begin(), end);
   
    if (chull.size() != result.size()) {
        #ifdef DEBUG
        std::cerr << "WRONG" << std::endl;
        #endif        
        return 1;
    }

    for (int i = 0; i < chull.size(); i++) {
        if (chull[i] == result.front()) {
            std::rotate(chull.begin(), chull.begin() + i, chull.end());
            break;
        }
    }

    for (int i = 0; i < chull.size(); i++) {
        if (chull[i] != result[i]) {
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
