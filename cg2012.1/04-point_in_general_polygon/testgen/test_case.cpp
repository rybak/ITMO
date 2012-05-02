#include <cstddef>
#include "test_case.h"

std::ostream& operator<< (std::ostream& out, const test_case& test)
{
	out << test.q << '\n';
	size_t n = test.p.size();
	out << n << '\n';
	for (size_t i = 0; i < n; ++i)
	{
		out << test.p[i] << '\n';
	}
	n = test.holes.size();
	out << n << '\n';
	for (size_t i = 0; i < n; ++i)
	{
		size_t m = test.holes[i].size();
		out << m << '\n';
		for (size_t j = 0; j < m; ++j)
		{
			out << test.holes[i][j] << '\n';
		}
	}
	return out;
}