#include <iostream>
#include <fstream>
#include <vector>
#include <cstdio>
#include <cmath>
#include <ctime>
#include <iostream>
#include <string>
#include <sstream>
#include <cstdlib>

double double_random(){
	double a = (double)((abs(rand())) % 10000)/10000;
	return a;
}

int main(int argc, char* argv[]){
	std::srand(time(0));
	int k = 2;

	for (int i = 0; i < k; ++i){
		std::stringstream ss;
		std::string name;
		ss << "performance_tests/"<< i + 1<< ".in";
		ss >> name;
		std::ofstream out(name.c_str());
		out << 10000 << "\n";
		for (int i = 0; i < 10000; ++i){
			out << double_random() << " " << double_random() << "\n";
		}
		out << double_random() / 10;
	}
}
