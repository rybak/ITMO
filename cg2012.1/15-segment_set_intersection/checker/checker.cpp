#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <map>

#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/Cartesian.h>
#include <CGAL/Gmpq.h>
#include <CGAL/intersection_2.h>

typedef CGAL::Gmpq gmp;
typedef CGAL::Cartesian<gmp> Kernel;
typedef Kernel::Point_2 Point;
typedef Kernel::Segment_2 Segment;
typedef CGAL::Object Object;

using std::vector;
using std::cin;
using std::cout;
using std::cerr;
using std::ifstream;
using std::ofstream;

int n;
vector<Point> allintersections;
vector<Segment> all;

bool isSeg(const Object& obj){
	if(const CGAL::Segment_2<Kernel> *ipoint = CGAL::object_cast<CGAL::Segment_2<Kernel> >(&obj)){
		return true;
	}
	return false;
}

bool isPoint(const Object& obj){
	if(const CGAL::Point_2<Kernel> *ipoint = CGAL::object_cast<CGAL::Point_2<Kernel> >(&obj)){
		return true;
	}
	return false;
}

void fill(){
	ofstream output1("seg.txt");
	
	ofstream output2("point.txt");
	for(int i = 0; i < n; i++){
		for(int j = 0; j < i; j++){
			if(i != j){
				if(do_intersect(all[i], all[j])){
					Object result = intersection(all[i], all[j]);
					if(isSeg(result)){
						Segment seg = CGAL::object_cast<CGAL::Segment_2<Kernel> >(result);
						output1 << seg << "\n";
						allintersections.push_back(seg.source());
						allintersections.push_back(seg.target()); 
					}
					if(isPoint(result)){
						Point p = CGAL::object_cast<CGAL::Point_2<Kernel> >(result);
						output2 << p << "\n"; 
						allintersections.push_back(p);
					}
				}
			}
		}
	}
}

vector<std::pair<Point, vector<int>>> generate(){
	vector<std::pair<Point, vector<int>>> result;
	
	for(int i = 0; i < allintersections.size(); i++){
		vector<int> curresult;
		for(int j = 0; j < all.size(); j++){
			if(all[j].has_on(allintersections[i])){
				curresult.push_back(j+1);
			}
		}
		std::sort(curresult.begin(), curresult.end());
		if(!curresult.empty()){
			result.push_back(std::make_pair<Point, vector<int>>(allintersections[i], curresult));
		}
	}

	std::sort(result.begin(), result.end());
	
	int i = 0;
	while(i < result.size()){
		int j = i;
		while(result[i].first == result[j].first){
			j++;
			if(j >= result.size()){
				break;
			}
		}
		j--;
		result.erase(result.begin() + i, result.begin() + j);
		i = i + 1;
	} 
	return result; 
}

bool compare(const vector<int>& a, const vector<int>& b){
	if(a.size() != b.size()){
		return false;
	}
	for(int i = 0; i < a.size(); i++){
		if(a[i] != b[i]){
			return false;
		}
	}
	return true;
}

int main(int argc, char* argv[]){
	ifstream in(argv[1]);
	in >> n;
	double x;
	double y;
	double x1;
	double y1;
	for(int i = 0; i < n; i++){
		in >> x >> y >> x1 >> y1;
		all.push_back(Segment(Point(x, y), Point(x1, y1)));
	}
	
	fill();
	
	vector<std::pair<Point, vector<int>>> trueanswer = generate();
	in.close();

	ifstream input(argv[2]);
	int wronganswer = 0;

	int count = 0;
	input >> count;
	if(count != trueanswer.size()){
		wronganswer++;
	}

	vector<vector<int>> answer(count);
	for(int i = 0; i < count; i++){
		int n = 0;
		input >> n;
		for(int k = 0; k < n; k++){
			int cur;
			input >> cur;
			answer[i].push_back(cur);
		}
		std::sort(answer[i].begin(), answer[i].end());
	}
	
	
	for(int i = 0; i < answer.size(); i++){
		bool flag = true;
		for(int j = 0; (j < trueanswer.size() && flag); j++){
			if(compare(answer[i], trueanswer[j].second)){
				trueanswer.erase(trueanswer.begin() + j);
				flag = false;
			}
		}
		if(flag){
			wronganswer++;
		}
	}

	if((wronganswer == 0) && (trueanswer.size() != 0)){
		wronganswer++;
	}
	  
	if(wronganswer != 0){
		return 1;
	} else {
		return 0;
	}
}