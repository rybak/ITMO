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
	double a = (double)((abs(rand())) % 10000);
	int n = (abs(rand()) % 5);
	for (int i = 0; i < n; ++i)
		a /= 10;
	return a;
}

int main(int argc, char* argv[]){
	std::srand(time(0));
	std::string n = argv[1];
	int k = atoi(n.c_str());


	for (int i = 0; i < k; ++i){
		std::stringstream ss;
		std::string name;
		ss << "correctness_tests\\" << i + 1 << ".in";
		ss >> name;
		std::ofstream out(name);
		int lngth = abs(rand()) % 1000;
		out << lngth << "\n";
		for (int i = 0; i < lngth; ++i){
			out << random() << " " << random() << "\n";
		}
	}
}