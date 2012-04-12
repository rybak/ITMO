#include <iostream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include "math.h"
#include <ctime>
#include <random>
#include <iostream>

double random(){
	double a = (double)((abs(rand())) % 10000)/10000;
	return a;
}

int main(){
	std::srand(time(0));
	std::ofstream out("performance_test.txt");
	out << 10000 << "\n";
	for (int i = 0; i < 10000; ++i){
		out << random() << " " << random() << "\n";
	}
	out << random() / 10;
}