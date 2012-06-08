#include <iostream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include "math.h"
#include <ctime>
#include <random>
#include <sstream>
#include <iostream>
#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/Cartesian.h>
#include <CGAL/Gmpq.h>
#include <CGAL/Quotient.h>
#include <CGAL/MP_Float.h>
#include <CGAL/intersection_2.h>
#include <CGAL/Polygon_2.h>
#include <CGAL/number_utils.h>
#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/Triangulation_2.h>


typedef CGAL::Gmpq gmp;
typedef CGAL::Cartesian<double> Kernel;
//typedef Kernel::Point_2 Point;
typedef Kernel::Segment_2 Segment;
typedef Kernel::Triangle_2 Triangle;
typedef CGAL::Polygon_2<Kernel> Polygon;
typedef CGAL::Object Object;
typedef Kernel::Vector_2 Vector;
typedef CGAL::Triangulation_2<Kernel>         Triangulation;
typedef Triangulation::Vertex_circulator Vertex_circulator;
typedef Triangulation::Point             Point;


double random(){
	double a = (double)(rand());
	int b = rand();
	if(b % 2 == 0){
		a = -a;
	}
	while(abs(a) > 1){
		a = a/10;
	}
	return a;
}

Point genPoint(const int& h, const int& w){
	return (Point(h * random(), w * random()));
}

std::vector<Point> generatePoints(const double& h, const double& w, const int& count){
	
	std::vector<Point> res;
	res.push_back(Point(-h, -w));
	
	res.push_back(Point(-h, w));
	
	res.push_back(Point(h, -w));
	
	res.push_back(Point(h, w));

	for(int i = 0; i < count; i++){
		res.push_back(Point(h * random(), w * random()));
	}
	return res;
}

std::pair<int, int> findSegment(const Segment& s, const std::vector<Point>& p){
	int a, b;
	for(int i = 0; i < p.size(); i++){
		if(s.source() == p[i]){
			a = i;
		}
		if(s.target() == p[i]){
			b = i;
		}
	}
	return std::make_pair<int, int>(a, b);
}

std::vector<Segment> toSegs(const Triangle& t){
	std::vector<Segment> segs;
	segs.push_back(Segment(t[0], t[1]));
	
	segs.push_back(Segment(t[1], t[2]));
	
	segs.push_back(Segment(t[2], t[0]));
	return segs;
}
std::vector<int> findTriangle(const Triangle& t, const std::vector<Segment>& s){
	std::vector<int> ans;
	std::vector<Segment> segs = toSegs(t);
	for(int i = 0; i < s.size(); i++){
		if((segs[0] == s[i]) || (segs[0] == s[i].opposite())){
			ans.push_back(i);
		}
		if(segs[1] == s[i] || (segs[1] == s[i].opposite())){
			ans.push_back(i);
		}
		if(segs[2] == s[i] || (segs[2] == s[i].opposite())){
			ans.push_back(i);
		}
	}
	return ans;
}



int main(){
	srand(time(0));


	
	for(int ir = 0; ir < 5; ir++){
		std::cout << "cor " << ir << std::endl;
		std::string testN = std::to_string((long long) ir);
		testN = std::string(3 - testN.length(), '0') + testN;
		std::ofstream output("./correctness_tests/test" + testN + ".in");
		std::vector<Point> points = generatePoints(300, 300, 20);
		std::cout << "i did points" << std::endl;
		Triangulation t;
		t.insert(points.begin(),  points.end());
		Triangulation::Finite_edges_iterator it = t.finite_edges_begin();
		std::vector<Segment> segments;
		for(;it != t.finite_edges_end(); it++){
			segments.push_back(t.segment((*it)));
		}

		Triangulation::Finite_faces_iterator iter = t.finite_faces_begin();
		std::vector<Segment> segs;
		std::vector<Triangle> triangles;
		for(;iter != t.finite_faces_end(); iter++){
			triangles.push_back(t.triangle(iter));
		}
	
		std::vector<std::pair<int, int>> segnums;
		for(int i = 0; i < segments.size(); i++){
			segnums.push_back(findSegment(segments[i], points));
		}
	
		std::vector<std::vector<int>> trinums;
		for(int i = 0; i < triangles.size(); i++){
			trinums.push_back(findTriangle(triangles[i], segments));
		}


		output << points.size() << " " << segments.size() << " " << triangles.size() << "\n"; 
		for(int i = 0; i < points.size(); i++){
			output << points[i] << "\n";
		}
		output <<std::endl;

		for(int i = 0; i < segnums.size(); i++){
			output << segnums[i].first << " " << segnums[i].second << "\n";
		}
		output << std::endl;

		for(int i = 0; i < trinums.size(); i++){
			output << trinums[i].size() << " ";
			for(int j = 0; j < trinums[i].size(); j++){
				output << trinums[i][j] << " ";
			} 
			output << "\n";
		}

		int m = 100;
		output << m << std::endl;
		for(int i = 0; i < m; i++){
			output << genPoint(300, 300) << std::endl;
		}
	}








	for(int ir = 0; ir < 1; ir++){
		std::cout << "per " << ir << std::endl;
		
		std::stringstream ss;
		std::string name;
		ss << "./performance_tests/test"<< ir + 1<< ".in";
		std::vector<Point> points = generatePoints(300000, 300000, 1000);
		std::cout << "i did points" << std::endl;
		std::ofstream output(name);
		Triangulation t;
		t.insert(points.begin(),  points.end());
		
		std::cout << "per i did triangulation " << ir << std::endl;

		
		
		Triangulation::Finite_edges_iterator it = t.finite_edges_begin();
		std::vector<Segment> segments;
		for(;it != t.finite_edges_end(); it++){
			segments.push_back(t.segment((*it)));
		}

		std::cout << "i did segments " << segments.size() << std::endl;
		
		Triangulation::Finite_faces_iterator iter = t.finite_faces_begin();
		std::vector<Segment> segs;
		std::vector<Triangle> triangles;
		for(;iter != t.finite_faces_end(); iter++){
			triangles.push_back(t.triangle(iter));
		}

		std::cout << "i did triangles " << triangles.size() <<  std::endl;
		
	
		std::vector<std::pair<int, int>> segnums;
		for(int i = 0; i < segments.size(); i++){
			segnums.push_back(findSegment(segments[i], points));
		}
	
		std::cout << "i did segment to nums" << std::endl;
		

		std::vector<std::vector<int>> trinums;
		for(int i = 0; i < triangles.size(); i++){
			trinums.push_back(findTriangle(triangles[i], segments));
		}


		std::cout << "i did triangles to nums" << std::endl;
		

		output << points.size() << " " << segments.size() << " " << triangles.size() << "\n"; 
		for(int i = 0; i < points.size(); i++){  
			output << points[i] << "\n";
		}
		output <<std::endl;
std::cout << "per  wrote points" << ir << std::endl;
		
		for(int i = 0; i < segnums.size(); i++){
			output << segnums[i].first << " " << segnums[i].second << "\n";
		}
		output << std::endl;

		for(int i = 0; i < trinums.size(); i++){
			output << trinums[i].size() << " ";
			for(int j = 0; j < trinums[i].size(); j++){
				output << trinums[i][j] << " ";
			} 
			output << "\n";
		}

		int m = 500000;
		output << m << std::endl;
		for(int i = 0; i < m; i++){
			output << genPoint(300000, 300000) << std::endl;
		}
	}

}