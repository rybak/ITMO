#include <cstdlib>
#include <string>
#include <sstream>
#include <iostream>
#include <cstdio>

const std::string int2str(int a)
{
	char buffer[10];
	sprintf(buffer, "%d", a);
	return std::string(buffer);
}

void system(std::string cmd)
{
	system(cmd.c_str());
}

int main()
{
	using std::cerr;
	using std::string;

	const size_t ACtests = 5;
	const size_t PEtests = 3;
	const size_t WAtests = 5;
	string start = "checker.exe test";
	for (size_t i = 0; i < ACtests; ++i)
	{
		string num = int2str(i);
		cerr << "ACtest" + num << ":\n";
		system(start + num + ".in test" + num + ".out");
	}
	for (size_t i = 0; i < PEtests; ++i)
	{
		string num = int2str(i);
		cerr << "PEtest" + num << ":\n";
		system(start + num + ".in PEtest" + num + ".out");
	}
	for (size_t i = 0; i < WAtests; ++i)
	{
		string num = int2str(i);
		cerr << "WAtest" + num << ":\n";
		system(start + num + ".in WAtest" + num + ".out");
	}

	return 0;
}