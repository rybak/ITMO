#include <CGAL/Polygon_2_algorithms.h>
#include <CGAL/Cartesian.h>
#include <CGAL/Gmpq.h>
#include <CGAL/Quotient.h>
#include <CGAL/MP_Float.h>
#include <CGAL/intersection_2.h>
#include <CGAL/Polygon_2.h>
#include <CGAL/number_utils.h>
#include <CGAL/CORE_Expr.h>

#include <cmath>
#include <limits.h>


typedef CGAL::Gmpq gmp;
typedef CGAL::Cartesian<CORE::Expr> Kernel;
typedef Kernel::Point_2 Point;
typedef Kernel::Segment_2 Segment;
typedef CGAL::Polygon_2<Kernel> Polygon;
typedef CGAL::Object Object;


using std::vector;


struct Obstacle{
	vector<Point> points;
	vector<Segment> segments;
	size_t size;
	Obstacle(const vector<std::pair<double, double>>& point){
		size = point.size();
		for(int i = 0; i < point.size(); i++){
			points.push_back(Point(point[i].first, point[i].second));
		}
		for(int j = 0; j < size; j++){
			segments.push_back(Segment(points[(j + size - 1)%(size)] , points[j%(size)]));
			segments.push_back(Segment(points[(j + 1)%(size)] , points[j%(size)]));
		}
	}
	bool contain(const Point& ex){
		for(int i  = 0; i < size; i++){
			if(points[i] == ex){
				return true;
			}
		}
		return false;
	}
	bool contain(const Segment& seg){
		for(int i  = 0; i < segments.size(); i++){
			if(segments[i] == seg || segments[i].opposite() == seg){
				return true;
			}
		}
		return false;
	}
	bool contain(const Point& first, const Point& second){
		return (contain(first) && contain(second));
	}
	int intersects(const Point& first, const Point& second){
		Segment seg(first, second);
		if(contain(first, second)){
			if(contain(seg)){
				return 0;
			} else {
				return 1;
			}
		}
		if(contain(first) || contain(second)){
			int count = 0;
			for(int i = 0; i < segments.size(); i++){
				if((do_intersect(seg, segments[i]))){
					count++;
				} 
			}
			if(count == 4){
				return 0;
			} else {
				return 1;
			}
		}
		if(!contain(first) && !contain(second)){
			for(int i = 0; i < segments.size(); i++){
				if((do_intersect(seg, segments[i]))){
					return 1;
				} 
			}
		}	
		return 0;
	}
};

CORE::Expr length(const Point& a, const Point& b){
	Segment seg(a, b);
	return sqrt(seg.squared_length());
}
bool full_intersect(const Point& first, const Point& second, vector<Obstacle> obstacles){
	int sum = 0;
	for(int i = 0; i < obstacles.size(); i++){
		sum += obstacles[i].intersects(first, second);
	}
	return (sum == 0);
}

int main(int argc, char* argv[]){
	
	vector<Point> points;

	std::ifstream in(argv[1]);
	double xstart = 0;
	double ystart = 0;
	double xfin = 0;
	double yfin = 0;

	in >> xstart >> ystart >> xfin >> yfin;

	points.push_back(Point(xstart, ystart));
	points.push_back(Point(xfin, yfin));

	 int n = 0;
	 in >> n;

	 vector<Obstacle> obstacles;
	 for(int i = 0; i < n; i++){
		 int m = 0;
		 in >> m;
		 vector<std::pair<double, double>> currentpoints;
		 for(int j = 0; j < m; j++){
			 double x = 0;
			 double y = 0;
			 in >> x >> y;
			 points.push_back(Point(x, y));
			 currentpoints.push_back(std::make_pair<double, double>(x, y));
		 }
		 Obstacle current(currentpoints);
		 obstacles.push_back(current);
	 }
	 in.close();

	 vector<vector<int>> graph_edge(points.size());
	 vector<std::pair<double, CORE::Expr>> graph_vertex(points.size());
	 vector<int> parent(points.size());
	 for(int i = 0; i < points.size(); i++){
		 for(int j = 0; j < points.size(); j++){
			 if(i != j){
				 if(full_intersect(points[i], points[j], obstacles)){
					 graph_edge[i].push_back(j);
				 }
			 }
		 }
	 }

	 graph_vertex[0].first = 0;
	 graph_vertex[0].second = 0;
 
	 for(int i = 1; i < graph_vertex.size(); i++){
		 graph_vertex[i].first = (std::numeric_limits<double>::max());
	 
		 graph_vertex[i].second = (CORE::Expr)(std::numeric_limits<double>::max());
		 parent[i] = -1;
	 }

	for(int i = 0; i < graph_vertex.size(); i++){
		for(int j = 0; j < graph_edge[i].size(); j++){
			if(graph_vertex[i].second + length(points[i], points[graph_edge[i][j]]) < graph_vertex[graph_edge[i][j]].second){
				graph_vertex[graph_edge[i][j]].second = graph_vertex[i].second + length(points[i], points[graph_edge[i][j]]);
				graph_vertex[graph_edge[i][j]].first = graph_vertex[graph_edge[i][j]].second.doubleValue();
				parent[graph_edge[i][j]] = i;
			}
		}
	} 

	int wrongdistance = 1;
	int wrongpath = 1;


	std::ifstream input(argv[2]);
	int count;
	input >> count;
	double p;
	double q;
	vector<Point> path;

	path.push_back(Point(xstart, ystart));

	for(int i = 0; i < n; i++){
		input >> p >> q;
		path.push_back(Point(p, q));
	}

	input.close();

	path.push_back(Point(xfin, yfin));
	CORE::Expr distance = 0;

	for(int i = 0; ((i < path.size() - 1) && (wrongpath == 1)); i++){
		if(!full_intersect(path[i], path[i + 1], obstacles)){
			wrongpath--;
		} 
	
		distance += length(path[i], path[i + 1]);
	
	}

	if(distance != graph_vertex[1].second){
		wrongdistance--;
	}
	

	if((wrongdistance + wrongpath) != 2){
		return 1;
	} else {
		return 0;
	}	 

}