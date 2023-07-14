#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/Cartesian.h>
#include <CGAL/Gmpq.h>
#include <CGAL/Quotient.h>
#include <CGAL/MP_Float.h>
#include <CGAL/intersection_2.h>
#include <CGAL/Polygon_2.h>
#include <CGAL/number_utils.h>
#include <CGAL/Polygon_2_algorithms.h>

#include <fstream>
#include <map>
#include <cmath>
#include <limits.h>

typedef CGAL::Gmpq gmp;
typedef CGAL::Cartesian<gmp> Kernel;
typedef Kernel::Point_2 Point;
typedef Kernel::Segment_2 Segment;
typedef CGAL::Polygon_2<Kernel> Polygon;
typedef CGAL::Object Object;
typedef Kernel::Vector_2 Vector;

std::vector<std::pair<int, int>> answer;
std::vector<Point> points;
std::vector<Segment> segments;
std::vector<std::vector<Point>> faces;

int isPoint(const Point& q){
	for(int i = 0; i < points.size(); i++){
		if(q == points[i]){
			return i;
		}
	}
	return -1;
}

int isSegment(const Point& q){
	for(int i = 0; i < segments.size(); i++){
		if(segments[i].has_on(q)){
			return i;
		}
	}
	return -1;
}

int isFace(const Point& q){
	for(int i = 0; i < faces.size(); i++){
		if(CGAL::bounded_side_2(faces[i].begin(), faces[i].end(), q, Kernel())){
			return i;
		}
	}
	return -1;
}

std::pair<int, int> gen(const Point& q){
	int dim = -1;
	int ans = -1;
	int isp = isPoint(q);
	if(isp != -1){
		dim = 0;
		ans = isp; 
	} else {
		isp = isSegment(q);
		if(isp != -1){
			dim = 1;
			ans = isp;
		} else {
			isp = isFace(q);
			if(isp != -1){
				dim = 2;
				ans = isp;
			} 
		}
	}
	return std::make_pair<int, int>(dim, ans);
}


int main(int argc, char* argv[]){
	std::ifstream in(argv[1]);
	int np = 0;
	int ns = 0;
	int nf = 0;
	in >> np >> ns >> nf;
	for(int i = 0; i < np; i++){
		Point p;
		in >> p;
		points.push_back(p);
	}
	for(int i = 0; i < ns; i++){
		int a;
		int b;
		in >> a >> b;
		Segment s(points[a], points[b]);
		segments.push_back(s);
	}
	for(int i = 0; i < nf; i++){
		std::vector<int> segs(3);
		in >> segs[0] >> segs[1] >> segs[2];
		faces.push_back(segs);
	}
		
		
	std::vector<Point> queries;
	int m;
	in >> m;
	for(int i = 0; i < m; i++){
		Point p;
		in >> p;
		queries.push_back(p);
	}

	for(int i = 0; i < m; i++){
		answer.insert(std::make_pair<int, int>(gen(queries[i])));
	}

	std::ifstream input(argv[2]);

	std::vector<std::pair<int, int>> wer;
	for(int i = 0; i < m; i++){
		int d, a;
		input >> d >> a;
		wer.insert(std::make_pair<int, int>(d, a)));
	} 

	std::vector<std::pair<int, int>>::iterator iter1, iter2;
	for(iter1 = answer.begin(); iter1 != answer.end(); iter1++){
		for(iter2 = wer.begin() ;iter2 != wer.end(); iter2++){
			if((*iter1).first() == (*iter2).first() ){
				return 1;
			}
			if((*iter1).second() != (*iter2).second() ){
				return 1;
			}
		}
	}
	return 0;
}