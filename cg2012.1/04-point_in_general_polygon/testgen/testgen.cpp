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
using std::uniform_real_distribution;
using std::mt19937;
using namespace std::placeholders;

typedef uniform_real_distribution<double> double_distr;
	
const int seed = 1993;
const int precision = 50;

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
const double min_coord = 10;
const vector<double> max_coords({100.0, 10.0e4, 10.0e10});
	
vector<test_case> generate_triangle_tests()
{
	vector<test_case> tests;
	mt19937 engine(seed);
	
	Point A(0.0, 0.0);
	for (size_t i = 0; i < max_coords.size(); ++i)
	{
		double_distr point_distr(0, max_coords[i]);
		double_distr triangle_distr(min_coord + 1, max_coords[i]);
		auto point_gen = bind(point_distr, engine);
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
	vector<test_case> tests;
	
	mt19937 engine(seed ^ 0x91BAC);
	const double tiny = 1e-2;
	const double angle_period = pi / size;
	double_distr angle_distr(tiny, angle_period / 2 - tiny);
	
	double_distr factor_distr(0.3, 0.7);
	
	for (size_t i = 0; i < max_coords.size(); ++i)
	{
		double_distr radius_distr(0.8 * max_coords[i], max_coords[i]);
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


vector<test_case> generate_correctness_tests()
{
	vector<test_case> tests(get_hard_coded_tests());
	vector<test_case> triangle_tests(generate_triangle_tests());
	tests.insert(tests.end(), triangle_tests.begin(), triangle_tests.end());
	vector<test_case> star_tests(generate_star_tests());
	tests.insert(tests.end(), star_tests.begin(), star_tests.end());
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