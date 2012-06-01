#include <iostream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include "math.h"
#include <ctime>
#include <algorithm>
#include <random>
#include <functional>
#include <sstream>
#include <iostream>
#pragma once

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/convex_hull_2.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel::Point_2 Point_2;


double random(){
	double a = (double)((abs(rand())) % 10000)/10000;
	return a;
}

struct dcel{
	int sx;
	int sy;
	int h;
	int count;
	std::vector<Point_2> points;
	dcel(int a, int b, int c, int count_){
		sx = a;
		sy = b;
		h = c;
		count = count_; 
		for(int i = 0; i < count; i++){
			points.push_back(Point_2(sx + h*random(), sy + h*random()));
		}
		points = f();
	}
	std::vector<Point_2> f(){
		std::vector<Point_2> tmp(points.size());
		std::vector<Point_2>::iterator end = convex_hull_2(points.begin(), points.end(), tmp.begin()); 
		std::vector<Point_2> result(tmp.begin(), end);
		return result;
	}
};

struct mesh{
	std::vector<dcel> dcels;
	int h;
	int sx;
	int sy;
	int count;
	mesh(const int& sx1, const int& sy1, const int& h1, const int& count_, const int& pcount){
		sx = sx1;
		sy = sy1;
		h = h1;
		count = count_;
		for(int i = sx; i < (sx + h*count); i = i + h){
			for(int j = sy; j < (sy + h*count); j = j + h){
				dcels.push_back(dcel(i, j, h, pcount));
			}
		}
	}



};


int main(){
	int ptest = 1;
	int cortest = 30;
	std::srand(time(0));
	for(int i = 0; i < ptest; i++){
		std::string testN = std::to_string((long long) i);
		testN = std::string(3 - testN.length(), '0') + testN;
		std::ofstream out("./performance_tests/test" + testN + ".in");
		mesh m(0, 0, 50, 30, 2500);
		out << -100 << " " << -100 << " " << 50*200 << " " << 50 * 200 << "\n";  
		out << m.dcels.size() << "\n";
		for(int i = 0; i < m.dcels.size(); i++){
			out << m.dcels[i].points.size() << "\n";
			for(int j = 0; j < m.dcels[i].points.size(); j++){
				out << m.dcels[i].points[j] << "\n";
				}
			}
		}
	for(int i = 0; i < cortest; i++){
		std::string testN = std::to_string((long long) i);
		testN = std::string(3 - testN.length(), '0') + testN;
		std::ofstream out("./correctness_tests/test" + testN + ".in");
		mesh m(0, 0, 50, 5, 50);
		out << -100 << " " << -100 << " " << 50*200 << " " << 50 * 200 << "\n";  
		out << m.dcels.size() << "\n";
		for(int i = 0; i < m.dcels.size(); i++){
			out << m.dcels[i].points.size() << "\n";
			for(int j = 0; j < m.dcels[i].points.size(); j++){
				out << m.dcels[i].points[j] << "\n";
				}
			}
		}	
}