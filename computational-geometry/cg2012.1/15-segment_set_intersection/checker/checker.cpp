#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <map>
#include <set>
#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/Cartesian.h>
#include <CGAL/Gmpq.h>
#include <CGAL/Quotient.h>
#include <CGAL/MP_Float.h>
#include <CGAL/intersection_2.h>

typedef CGAL::Quotient<CGAL::MP_Float> quat;

typedef CGAL::Gmpq gmp;
typedef CGAL::Cartesian<gmp> Kernel;
typedef Kernel::Point_2 Point;
typedef Kernel::Segment_2 Segment;
typedef CGAL::Object Object;

using namespace std;
int n;

set<Point> allpoints;
map<Point, int> gans;

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

//ищем все точки пересечения // looking for every points
vector<Point> fill(){
	for(int i = 0; i < n; i++){
		for(int j = 0; j < i; j++){
			if(i != j){
				if(do_intersect(all[i], all[j])){
					Object result = intersection(all[i], all[j]);
					if(isSeg(result)){
						Segment seg = CGAL::object_cast<CGAL::Segment_2<Kernel> >(result);
						allpoints.insert(seg.source());
						allpoints.insert(seg.target()); 
					}
					if(isPoint(result)){
						Point p = CGAL::object_cast<CGAL::Point_2<Kernel> >(result);
						allpoints.insert(p);
					}
				}
			}
		}
	}
	return vector<Point>(allpoints.begin(), allpoints.end());
}


//для каждой точки находим отрезки, которые через нее проходят
map<Point, vector<int>> generate(){
	vector<Point> allp = fill();
	multimap<Point, int> result;
	
	for(int i = 0; i < allpoints.size(); i++){
		for(int j = 0; j < all.size(); j++){
			if(all[j].has_on(allp[i])){
				result.insert(make_pair<Point, int>(allp[i], j+1));
			}
		}
	}

	map<Point, vector<int>> mres;
	
	multimap<Point,int>::iterator it;
	pair<multimap<Point,int>::iterator,multimap<Point,int>::iterator> ret;
	for(int i = 0; i < allp.size(); i++){
		ret = result.equal_range(allp[i]);
		vector<int> cur;
		for (it=ret.first; it!=ret.second; ++it)
			cur.push_back((*it).second);
		sort(cur.begin(), cur.end());
		mres.insert(make_pair<Point, vector<int>>(allp[i], cur));
	}
	return mres; 
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
	
	
    map<Point, vector<int>> trueanswermap = generate();
	
	in.close();

    ifstream input(argv[2]);
	int wronganswer = 0;

	int count = 0;
	input >> count;
	if(count != trueanswermap.size()){
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
		sort(answer[i].begin(), answer[i].end());
	}
	
	map<Point, vector<int>>::iterator iter;
	map<Point, vector<int>> temp = trueanswermap;
	for(int i = 0; i < answer.size(); i++){
		bool flag = true;
		trueanswermap = temp;
		for(iter = trueanswermap.begin(); (iter != trueanswermap.end() && flag); iter++){
			if(compare(answer[i], (*iter).second)){
				temp.erase((*iter).first);
				flag = false;
			}
		}
		if(flag){
			wronganswer++;
		}
	}

	if((wronganswer == 0) && (temp.size() != 0)){
		wronganswer++;
	}
	  
	if(wronganswer != 0){
		return 1;
	} else {
		return 0;
	}
}