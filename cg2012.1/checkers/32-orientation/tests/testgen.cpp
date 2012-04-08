#include <iostream>
#include <vector>
#include <limits>
#include <random>
#include <functional>
#include <sstream>
#include <cmath>

using namespace std;


const double meps = numeric_limits<double>::epsilon() / 2; //machine epsilon
const int seed = time(NULL);

template<typename A, typename B, typename C>
class Triple {
  public:
    A first;
    B second;
    C third;
    
    Triple()
    {
    }

    Triple(const A& aa, const B &bb, const C& cc):
        first(aa),
        second(bb),
        third(cc)
    {
    }

    bool operator<(const Triple<A, B, C> &other) const {
        if (first != other.first) {
            return first < other.first;
        }
        if (second != other.second) {
            return second < other.second;
        }
        if (third != other.third) {
            return third < other.third;
        }
    }
};

template<typename A, typename B, typename C>
ostream& operator<<(ostream& ofs, const Triple<A, B, C>& tr) {
    ofs << "(" << tr.first<< ", " << tr.second << ", " << tr.third << ")";
    return ofs;
}

template<typename A, typename B, typename C>
Triple<A, B, C> makeTriple(const A& a, const B& b, const C& c) {
    return Triple<A, B, C>(a, b, c);
}



class Point {
  public:
    double x, y;
   
    Point():
        x(0.0),
        y(0.0)
    {
    }

    Point(double x, double y) {
        this->x = x;
        this->y = y;
    }

    Point shifted(double dx, double dy) const {
        return Point(x + dx, y + dy);
    }

    bool operator==(const Point &other) const {
        return x == other.x && y == other.y;
    }

};

ostream & operator<<(ostream &os, const Point &p) {
    os << "(" << "x = " << p.x << ", " << "y = " << p.y << ")";
    return os;
}



/*
 * Simple uniform distributed human readable tests 
 */
vector<Triple<Point, Point, Point> > simpleTests(int n) {
    uniform_int_distribution<int> distribution(-10000, 10000);
    mt19937 engine(seed);
    auto generate = bind(distribution, engine);
    vector<Triple<Point, Point, Point> > ans(n);

    for (int i = 0; i < n; i++) {
        Point a(0, 0), b(0, 0);
        while (a == b) {
            a.x = generate();
            a.y = generate();
            b.x = generate();
            b.y = generate();
        }
        Point c(generate(), generate());
        ans[i] = makeTriple(a, b, c);
    }
    return ans;
}


/*
 * Uniformly generates starting and ending points for the lines.
 * Guarantees that starting and ending points will be distinct.
 */
vector<Triple<Point, Point, Point> > aandbGenerator(int n) {
    double range = 1 << 10;
    uniform_real_distribution<double> distribution(-range, range);
    mt19937 engine(seed ^ 213);
    auto generate = bind(distribution, engine);
    
    vector<Triple<Point, Point, Point> > ans(n);
    for (int i = 0; i < n; i++) {
        Point a(0, 0), b(0, 0);
        while (a == b) {
            a.x = generate();
            a.y = generate();
            b.x = generate();
            b.y = generate();
        }
        ans[i] = makeTriple(a, b, Point(0.0, 0.0));
    }
    return ans;
}

/*
 * Checks if adaptive precision arithmetics should be performed to correctly calculate orientation predicate value.
 */
bool needsAdaptive(const Point &a, const Point &b, const Point &c) {
    double M = abs((b.x - a.x) * (c.y - a.y)) + abs((b.y - a.y) * (c.x - a.x));
    double det = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
    double eps = 4 * M * meps;
    cerr << std::fixed << "Det = " << det << " Eps = " << eps << endl;
    return abs(det) < eps;
}

/*
 * Generates points c such that orientation predicate should be
 * calculated using adaptive precision arithmetics.
 * Uses the following method: point c is the point b shifted by
 * the direction of ab and a little shifted by x and y (independently).
 * This should yield such a point, that the ange between ab and ac
 * is small.
 */
vector<Triple<Point, Point, Point> > adaptiveTests(int n) {
    auto points = aandbGenerator(n);

    mt19937 shiftEngine(seed ^ 543);
    uniform_real_distribution<double> shiftDistribution(0, 10);
    auto generateShift = bind(shiftDistribution, shiftEngine);

    for (int i = 0; i < points.size(); i++) {
        Point a = points[i].first, b = points[i].second, c;
        cerr << "Generating point " << i << endl;
        
        double shift = generateShift();
        Point d = b.shifted((b.x - a.x) * shift, (b.y - a.y) * shift);
        
        double distance = sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
        cerr << "Distance is " << distance << endl;

        uniform_real_distribution<double> moveDistribution(2 * distance, 16 * distance);
        mt19937 moveEngine(seed ^ 123);
        auto generateMove = bind(moveDistribution, moveEngine);
        
        do {
            double dx = generateMove() * meps, dy = generateMove() * meps;
            cerr << std::fixed << "dx = " << dx << " dy = " << dy << endl;
            c = d.shifted(dx, dy);
            //bool needs = needsAdaptive(a, b, c);
            //cerr << (needs ? "Needs adaptive precision" : "Does not need adaptive precision") << endl;
        } while (false && !needsAdaptive(a, b, c));
        points[i].third = c;
    }
    return points;
}

int main() {
    cerr.precision(30);

    auto points = adaptiveTests(10);
    for (int i = 0; i < points.size(); i++) {
        cout.precision(100);
        cout << points[i].first << endl << points[i].second << endl << points[i].third << endl << endl;
    }
        
    return 0;
}




