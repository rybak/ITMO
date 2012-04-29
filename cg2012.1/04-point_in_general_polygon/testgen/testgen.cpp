#include <fstream>
#include <vector>
#include <string>
#include <sstream>

#include <algorithm>
#include <cmath>
#include <random>
#include <functional>

#include "test_case.h"

using std::vector;
using std::bind;
using std::string;
using std::uniform_int_distribution;
using std::uniform_real_distribution;
using std::mt19937;
using namespace std::placeholders;

typedef uniform_real_distribution<double> double_distr;
typedef uniform_int_distribution<int> int_distr;

const int seed = 1993;
const int precision = 100;

vector<test_case> get_hard_coded_tests()
{
	vector<test_case> tests =
	{
		#include "01.hpp"
		,
		#include "02.hpp"
		,
		#include "03.hpp"
		,
		#include "04.hpp"
		,
		#include "05.hpp"
		,
		#include "06.hpp"
		,
		#include "07.hpp"
		,
		#include "08.hpp"
		,
		#include "09.hpp"
	};
	return tests;
}

const double pi = atan(1.0) * 4;
const double min_coord = 10.0;
const vector<double> max_coords({100.0, 1.0e4, 1.0e10});
	
vector<test_case> generate_triangle_tests()
{
	vector<test_case> tests;
	mt19937 engine(seed);
	Point A(0.0, 0.0);
	for (size_t i = 0; i < max_coords.size(); ++i)
	{
		double_distr coord_distr(0, max_coords[i]);
		double_distr triangle_distr(min_coord + 1, max_coords[i]);
		auto point_gen = bind(coord_distr, engine);
		auto triangle_gen = bind(triangle_distr, engine);
		for (size_t j = 0, m = 3; j < m; ++j)
		{
			Point q(point_gen(), point_gen());
			Point B( triangle_gen(), triangle_gen());
			Point C(-triangle_gen(), triangle_gen());
			Point center = (1.0 / 3.0) * (A + B + C);
			Point holeA = 0.5 * (A + center);
			Point holeB = 0.5 * (B + center);
			Point holeC = 0.5 * (C + center);
			tests.push_back({q, {A, B, C}, { {holeA, holeC, holeB} }});
		}
	}
	return tests;
}

vector<test_case> generate_star_tests()
{
	const size_t size = 4;
	const double tiny = 1e-2;
	const double angle_period = pi / size;
	double_distr angle_distr(tiny, angle_period / 2 - tiny);
	
	const double factor_min = 0.3;
	const double factor_max = 0.7;
	
	double_distr factor_distr(factor_min, factor_max);
	
	mt19937 engine(seed ^ 0x91BAC);
	const double radius_min_factor = 0.8;
	vector<test_case> tests;
	for (size_t i = 0; i < max_coords.size(); ++i)
	{
		double_distr radius_distr(radius_min_factor * max_coords[i], max_coords[i]);
		vector<Point> star;
		for (size_t j = 0; j < 2 * size; ++j)
		{
			double angle = angle_period * j + angle_distr(engine);
			double radius = radius_distr(engine) / ((j % 2) + 1);
			double x = radius * cos(angle);
			double y = radius * sin(angle);
			star.push_back(Point(x, y));
		}
		vector<Point> hole(star);
		reverse(hole.begin(), hole.end());
		for (size_t j = 0; j < hole.size(); ++j)
		{
			hole[j] = factor_distr(engine) * hole[j];
		}
		double_distr point_distr(-max_coords[i], max_coords[i]);
		auto point_gen = bind(point_distr, engine);
		Point q(point_gen(), point_gen());
		tests.push_back({q, star, { hole } });
	}
	return tests;
}

vector<test_case> generate_adaptive_tests()
{
	vector<test_case> tests;
	double small_eps = 0.5 * std::numeric_limits<double>::epsilon();
	
	const double small_base = 1.0;
	while (((small_base + small_eps) == small_base)
		|| ((small_base - small_eps) == small_base))
	{
		small_eps *= 2;
	}
	double_distr coord_distr(small_base - small_eps, small_base + small_eps);
	mt19937 engine(seed ^ 0xFE11A5);
	auto coord_gen = bind(coord_distr, engine);
	
	volatile const double big_base = 10e10;
	volatile double big_eps = 1.0e-5;
	while (((big_base + big_eps) == big_base)
		|| ((big_base - big_eps) == big_base))
	{
		big_eps *= 2;
	}
	vector<Point> polygon(
	{// diamond along line y = x
		Point(1, 1),
		Point(big_base, big_base - big_eps),
		Point(big_base, big_base),
		Point(big_base - big_eps, big_base)
	});
	
	const size_t tests_count = 10;
	for (size_t i = 0; i < tests_count; ++i)
	{
		Point q(coord_gen(), coord_gen());
		tests.push_back({q, polygon, {/*no holes*/} });
	}
	return tests;
}

vector<test_case> generate_correctness_tests()
{
	vector<vector<test_case> > test_suit(
	{
		get_hard_coded_tests(),
		generate_triangle_tests(),
		generate_star_tests(),
		generate_adaptive_tests()
	});
	vector<test_case> tests;
	for (auto it = test_suit.begin(); it != test_suit.end(); ++it)
	{
		tests.insert(tests.end(), (*it).begin(), (*it).end());
	}
	
	return tests;
}

string to_string(size_t n)
{
	using std::ios_base;
	std::stringstream stream(ios_base::in | ios_base::out);
	stream << n;
	string s;
	stream >> s;
	return s;
}

string test_num_str(size_t n)
{
	string test_num = to_string(n);
	return string(3 - test_num.length(), '0') + test_num;
}

void output_tests(std::string folder, const std::vector<test_case> &tests)
{
	using std::ofstream;
	for (size_t i = 0; i < tests.size(); ++i)
	{
		ofstream test_file(folder + test_num_str(i + 1) + ".in");
		test_file.precision(precision);
		test_file << tests[i];
		test_file.close();
	}
}

int main()
{
	vector<test_case> correctness_tests(generate_correctness_tests());
	output_tests("./correctness_tests/", correctness_tests);
	output_tests("./performance_tests/", correctness_tests);
	return 0;
}