#include <cstdlib>
#include <string>
#include <sstream>
#include <fstream>
#include <iostream>
#include <cstdio>

const std::string int2str(int a)
{
	char buffer[10];
	sprintf(buffer, "%d", a);
	return std::string(buffer);
}

int main()
{
	using std::cerr;
	using std::string;
	using std::ofstream;
	const size_t ACtests = 5;
	const size_t PEtests = 3;
	const size_t WAtests = 5;
	for (size_t i = 0; i < ACtests; ++i)
	{
		string num = int2str(i);
		cerr << num << " " << i << "\n";
		ofstream in("test" + num + ".in");
		ofstream out("ACtest" + num + ".out");
		in.close();
		out.close();
	}
	for (size_t i = 0; i < PEtests; ++i)
	{
		string num = int2str(i);
		cerr << num << " " << i << "\n";
		ofstream out("PEtest" + num + ".out");
		out.close();
	}
	for (size_t i = 0; i < WAtests; ++i)
	{
		string num = int2str(i);
		cerr << num << " " << i << "\n";
		ofstream out("WAtest" + num + ".out");
		out.close();
	}

	return 0;
}