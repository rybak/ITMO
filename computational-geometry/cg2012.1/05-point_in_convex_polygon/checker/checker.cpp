#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Polygon_2_algorithms.h>
#include <iostream>
#include <fstream>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef K::Point_2 Point;
using std::cout; using std::endl;

bool check_outside(Point pt, Point *pgn_begin, Point *pgn_end, K traits)
{
	switch(CGAL::bounded_side_2(pgn_begin, pgn_end,pt, traits)) {
	case CGAL::ON_BOUNDED_SIDE :
		return false;
		break;
	case CGAL::ON_BOUNDARY:
		return false;
		break;
	case CGAL::ON_UNBOUNDED_SIDE:
		return true;
		break;
	}
}

int main(int argc, char* argv[])
{
	if (argc != 3){
		std::cerr << "Usage: pointinconvexpolygon-checker inputfile outputfile" << std::endl;
		return -1;
	}
	std::ifstream in(argv[1]);
	std::ifstream out(argv[2]);
	int n;
	in >> n;

	Point* polygon;
	polygon = new Point[n];
	for (int i = 0; i < n; ++i){
		in >> polygon[i];
	}
	int k;
	in >> k;
	for (int i = 0; i < k; ++i){
		Point cur_point;
		in >> cur_point;
		std::string cur;
		bool ans;
		out >> cur;
		if (cur == "YES"){
			ans = false;
		} else if (cur == "NO"){
			ans = true;
		} else{
			return 1;
		}
		if (ans != check_outside(cur_point, polygon, polygon+n, K())){
			return 1;
		}
	}
	in.close();
	out.close();

	return 0;
}