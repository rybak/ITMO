#include <iostream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include "math.h"
#include <ctime>
#include <random>
#include <iostream>
#include <string>
#include <sstream>

double random(){
	double a = (double)((abs(rand())) % 10000)/10000;
	return a;
}

int main(int argc, char* argv[]){
	std::srand(time(0));
	std::string n = argv[1];
	int k = atoi(n.c_str());

	for (int i = 0; i < k; ++i){
		std::stringstream ss;
		std::string name;
		ss << "performance_tests\\"<< i + 1<< ".in";
		ss >> name;
		std::ofstream out(name);
		out << 10000 << "\n";
		for (int i = 0; i < 10000; ++i){
			out << random() << " " << random() << "\n";
		}
		out << random() / 10;
	}
}