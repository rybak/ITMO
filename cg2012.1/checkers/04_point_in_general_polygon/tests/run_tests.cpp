#include <cstdlib>
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <cstdio>

const std::string int2str(int a)
{
	char buffer[10];
	sprintf(buffer, "%d", a);
	return std::string(buffer);
}

int str2int(std::string s)
{
    std::stringstream stream(std::stringstream::in | std::stringstream::out);
    stream << s;
    int a;
    stream >> a;
	return a;
}

void system(std::string cmd)
{
	system(cmd.c_str());
}

bool file_exists(std::string filename)
{
	std::ifstream ifile(filename.c_str());
	return ifile;
}

bool check_files(std::string in, std::string out)
{
	bool result = true;
	if (!file_exists(in))
	{
		std::cerr << "File \"" + in + "\" doesn't exists.\n";
		result = false;
	}
	if (!file_exists(out))
	{
		std::cerr << "File \"" + out + "\" doesn't exists.\n";
		result = false;
	}
	return result;
}

int main()
{
	using std::cerr;
	using std::string;

	const size_t ACtests = 9;
	const size_t PEtests = 3;
	const size_t WAtests = 5;
	string checker = "checker.exe ";
	for (size_t i = 0; i < ACtests; ++i)
	{
		string num = int2str(i);
		cerr << "ACtest" + num << ":\n";
		string input = "test" + num + ".in";
		string output = "test" + num + ".out";
		if (check_files(input, output))
		{
			system(checker + input + " " + output);
		}
	}
	for (size_t i = 0; i < PEtests; ++i)
	{
		string num = int2str(i);
		cerr << "PEtest" + num << ":\n";
		string input = "test" + num + ".in";
		string output = "PEtest" + num + ".out";
		if (check_files(input, output))
		{
			system(checker + input + " " + output);
		}
	}
	for (size_t i = 0; i < WAtests; ++i)
	{
		string num = int2str(i);
		cerr << "WAtest" + num << ":\n";
		string input = "test" + num + ".in";
		string output = "WAtest" + num + ".out";
		if (check_files(input, output))
		{
			system(checker + input + " " + output);
		}
	}

	return 0;
}