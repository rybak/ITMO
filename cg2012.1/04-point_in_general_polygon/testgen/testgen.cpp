#include <iostream>
#include <fstream>
#include <vector>

#include "test_case.h"

vector<test_case> hard_coded_tests()
{
	vector<test_case> tests =
		{
			{
				{5.0, 0.5}, // point
				{ {0.0, 0.0}, {10.0, 0.5}, {0.0, 1.0} },
				{
					{ {0.1, 0.1}, {0.1, 0.9}, {9.0, 0.5} }
				}
			}
		};
	return tests;
}

vector<test_case> generate_correctness_tests()
{
	vector<test_case> tests(hard_coded_tests());
	return tests;
}

int main()
{
	using std::vector;
	using std::cout;
	vector<test_case> correctness_tests(generate_correctness_tests());
	cout << correctness_tests[0];
	return 0;
}