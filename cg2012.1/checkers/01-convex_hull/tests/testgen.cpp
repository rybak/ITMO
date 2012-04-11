#include <iostream>
#include <vector>
#include <limits>
#include <random>
#include <functional>
#include <sstream>
#include <fstream>
#include <cmath>
#include <algorithm>

using namespace std;


const double meps = numeric_limits<double>::epsilon() / 2; //machine epsilon
const double pi = atan(1.0) * 4;
const int seed = 534;
const int precision = 50;

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


template<typename Distribution>
vector<Point> makeTest(int n, Distribution distribution, int seed)
{
    mt19937 engine(seed);
    auto generate = bind(distribution, engine);
    vector<Point> ans(n);

    for (int i = 0; i < n; i++)
    {
        ans[i].x = generate();
        ans[i].y = generate();
    }
    return ans;
}

/*
 * Uniformly distributed tests.
 */
vector<vector<Point>> randomTests(int smallCount, int mediumCount, int largeCount)
{
    int smallLower = 1, smallUpper = 10;
    int mediumLower = 10, mediumUpper = 50;
    int largeLower = 10000, largeUpper = 50000;

    mt19937 engine(seed ^ 436);
    vector<vector<Point>> tests;
    
    uniform_int_distribution<int> smallDistribution(-10, 10);
    uniform_int_distribution<int> smallSize(smallLower, smallUpper);
    auto generateSize = bind(smallSize, engine);
    for (int i = 0; i < smallCount; i++)
    {
        tests.push_back(makeTest(generateSize(), smallDistribution, engine()));     
    }

    uniform_int_distribution<int> mediumDistribution(-30, 30);
    uniform_int_distribution<int> mediumSize(mediumLower, mediumUpper);
    generateSize = bind(mediumSize, engine);
    for (int i = 0; i < mediumCount; i++)
    {
        tests.push_back(makeTest(generateSize(), mediumDistribution, engine()));     
    }

    uniform_real_distribution<double> largeDistribution(-1000.0, 1000.0);
    uniform_int_distribution<int> largeSize(largeLower, largeUpper);
    generateSize = bind(largeSize, engine);
    for (int i = 0; i < largeCount; i++)
    {
        tests.push_back(makeTest(generateSize(), largeDistribution, engine()));     
    }

    return tests;
}

/*
 * Specific tests
 */
vector<vector<Point>> specificTests()
{
    mt19937 engine(seed ^ 547);
    vector<vector<Point>> tests;
    
    uniform_real_distribution<double> distribution(-10000.0, 10000.0);

    auto test1 = makeTest(1, distribution, seed ^ 735);
    auto test2 = makeTest(2, distribution, seed ^ 2387);
    tests.push_back(test1);
    tests.push_back(test2);

    auto testeq = makeTest(100, distribution, seed ^ 123);
    std::fill(testeq.begin() + 1, testeq.end(), testeq[0]);
    tests.push_back(testeq);

    auto testAllUpper = makeTest(100, distribution, seed ^ 4253);
    testAllUpper.push_back(Point(-10000.0, -20000));
    testAllUpper.push_back(Point(10000.1, -30000));
    std::random_shuffle(testAllUpper.begin(), testAllUpper.end());
    tests.push_back(testAllUpper);

    auto testAllLower = makeTest(100, distribution, seed ^ 7987);
    testAllLower.push_back(Point(-10001.0, 20000));
    testAllLower.push_back(Point(10000.1, 30000));
    std::random_shuffle(testAllLower.begin(), testAllLower.end());
    tests.push_back(testAllLower);

    auto testAllCollinear = makeTest(50, distribution, seed ^ 235);
    for (int i = 1; i < testAllCollinear.size(); i++)
    {
        testAllCollinear[i].x = testAllCollinear[i - 1].x * 2;
        testAllCollinear[i].y = testAllCollinear[i - 1].y * 2;
    }
    std::random_shuffle(testAllCollinear.begin(), testAllCollinear.end());
    tests.push_back(testAllCollinear);
    
    /*
     * cos fails :(
     */
    int points = 50000;
    double R = 1024.0;
    vector<Point> testAllOnConvexHull;
    for (double phi = 0; phi < 2 * pi; phi += (2 * pi) / points)
    {
        testAllOnConvexHull.push_back(Point(R * cos(phi), R * sin(phi)));
    }
    std::random_shuffle(testAllOnConvexHull.begin(), testAllOnConvexHull.end());
    tests.push_back(testAllOnConvexHull);

    points = 1000;
    R = 1024.0;
    vector<Point> testAntiQuickHull;
    double phi = 2 * pi;
    for (int i = 0; i < 1000; i++)
    {
        testAntiQuickHull.push_back(Point(R * cos(phi), R * sin(phi)));
        phi /= 2;
    }
    std::random_shuffle(testAntiQuickHull.begin(), testAntiQuickHull.end());
    tests.push_back(testAntiQuickHull);
   
    return tests;
}


vector<vector<Point>> adaptiveTests(int count, int lowerBound, int upperBound)
{
    double range = 128.0;

    mt19937 engine(seed ^ 493);
    vector<vector<Point>> tests;
    
    uniform_int_distribution<int> testSize(lowerBound, upperBound);
    auto generateSize = bind(testSize, engine);
    uniform_real_distribution<double> distribution(-range, range);

    auto generateShift = bind(distribution, engine);
    for (int i = 0; i < count; i++)
    {
        auto test = makeTest(generateSize(), distribution, engine());
        auto shifted = test;
        for (int j = 0; j < shifted.size(); j++)
        {
            shifted[j] = shifted[j].shifted(generateShift() * meps, generateShift() * meps);
        }
        test.insert(test.end(), shifted.begin(), shifted.end());
        tests.push_back(test);
    }
    return tests;
}

vector<vector<Point>> performanceTests()
{
    vector<vector<Point> > tests;
    
    mt19937 engine(seed ^ 8329);
    
    uniform_real_distribution<double> largeDistribution(-10000.0, 10000.0);
    tests.push_back(makeTest(400000, largeDistribution, engine()));

    auto adaptive = adaptiveTests(1, 100000, 100000);
    tests.push_back(adaptive[0]);

    int points = 100000;
    double R = 1024.0;
    vector<Point> testAllOnConvexHull;
    for (double phi = 0; phi < 2 * pi; phi += (2 * pi) / points)
    {
        testAllOnConvexHull.push_back(Point(R * cos(phi), R * sin(phi)));
    }
    std::random_shuffle(testAllOnConvexHull.begin(), testAllOnConvexHull.end());
    tests.push_back(testAllOnConvexHull);

    return tests;
}

int main()
{
    cerr.precision(precision);
    
    auto simple = randomTests(10, 10, 10);
    auto specific = specificTests();
    auto adaptive = adaptiveTests(10, 5, 50);
    
    auto tests = simple;
    tests.insert(tests.end(), specific.begin(), specific.end());
    tests.insert(tests.end(), adaptive.begin(), adaptive.end());

    for (int i = 0; i < tests.size(); i++)
    {
        string testN = to_string(i);
        testN = string(3 - testN.length(), '0') + testN;
        ofstream testfile(testN + ".test");
        testfile << tests[i].size() << endl;
        testfile.precision(precision);
        for (int j = 0; j < tests[i].size(); j++)
        {
            testfile << tests[i][j].x << " " << tests[i][j].y << endl;
        }
        testfile.close();
    }

    auto performance = performanceTests();
    for (int i = 0; i < performance.size(); i++)
    {
        string testN = to_string(i);
        testN = string(3 - testN.length(), '0') + testN;
        ofstream testfile("./performance_tests/" + testN + ".test");
        testfile << performance[i].size() << endl;
        testfile.precision(precision);
        for (int j = 0; j < performance[i].size(); j++)
        {
            testfile << performance[i][j].x << " " << performance[i][j].y << endl;
        }
        testfile.close();
    }
    return 0;
}




