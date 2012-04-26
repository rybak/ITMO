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
#include <CGAL/convex_hull_2.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/convex_hull_2.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel::Point_2 Point;

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
		int lngth = abs(rand()) % 10000;
		std::vector<Point> points(lngth);
		for (int j = 0; j < lngth; ++j){
			points[j] = Point(random(), random());
		}

		std::vector<Point> tmp(points.size());
		std::vector<Point>::iterator end = convex_hull_2(points.begin(), points.end(), tmp.begin()); 
		std::vector<Point> result(tmp.begin(), end);
		out << result.size() << "\n";
		for (int i = 0; i < result.size(); ++i){
			out << result[i].hx() << " " << result[i].hy() << "\n";
		}
		int size = abs(rand()) % 1000;
		out << size << "\n";
		for (int j = 0; j < size; ++j){
			out << random() << " " << random() << "\n";
		}
	}
}