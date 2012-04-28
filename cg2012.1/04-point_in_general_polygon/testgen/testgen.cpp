#include <fstream>
#include <vector>
#include <string>
#include <sstream>

#include <random>
#include <functional>

#include "test_case.h"

using std::vector;
using std::bind;
using std::string;
using std::uniform_real_distribution;
using std::mt19937;

const int seed = 2012;
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


test_case generate_triange_test()
{
	const double max_coord = 10e10;
	const double min_coord = 10;

	typedef uniform_real_distribution<double> double_distr;

	mt19937 engine(seed);

	double_distr point_distr(0, max_coord);
	auto point_gen = bind(point_distr, engine);
	Point q(point_gen(), point_gen());

	double_distr triange_distr(min_coord, max_coord);
	auto triange_gen = bind(triange_distr, engine);
	Point A;
	Point B( triange_gen(), triange_gen());
	Point C(-triange_gen(), triange_gen());
	Point holeA(min_coord, 0.0);
	Point center = (1.0 / 3.0) * (A + B + C);
	Point holeB  = 0.5 * (B + center);
	Point holeC  = 0.5 * (C + center);
	
	return test_case(q, {A, B, C}, { {holeA, holeB, holeC} });
}

vector<test_case> generate_correctness_tests()
{
	vector<test_case> tests(get_hard_coded_tests());
	const size_t trianges_count = 3;
	for (size_t i = 0; i < trianges_count; ++i)
	{
		tests.push_back(generate_triange_test());
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