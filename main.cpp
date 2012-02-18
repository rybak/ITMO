#include <iostream>
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

int main() {
    int n;
    std::cin >> n;
    std::vector<Point_2> points(n);
    for (int i = 0; i < n; i++) {
        double x, y;
        std::cin >> x >> y;
        points[i] = Point_2(x, y);
    }
    int k;
    std::cin >> k;
    std::vector<Point_2> chull(k);
    for (int i = 0; i < k; i++) {
        double x, y;
        std::cin >> x >> y;
        chull[i] = Point_2(x, y);
    }

    bool allToLeft = true;
    for (int i = 0; i < n && allToLeft; i++) {
        if (!isInChull(points[i], chull)) {
            allToLeft = false;
        }
    }
    std::cout << (allToLeft ? 1 : 0) << std::endl;

    return 0;
}
