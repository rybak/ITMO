#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Min_circle_2.h>
#include <CGAL/Min_circle_2_traits_2.h>
#include <iostream>
#include <fstream>
#include <vector>

typedef  CGAL::Exact_predicates_exact_constructions_kernel K;
typedef  CGAL::Min_circle_2_traits_2<K>  Traits;
typedef  CGAL::Min_circle_2<Traits>      Min_circle;

typedef  K::Point_2                      Point;

int
main(int argc, char* argv[])
{
	if (argc != 3) 
    {
        std::cerr << "Usage: minimumcircumscribedcircle-checker inputfile outputfile" << std::endl;
        return -1;
    }

	std::ifstream input(argv[1]);
	std::ifstream output(argv[2]);
	int n;
	input >> n;
	Point* P = new Point[n];

    for ( int i = 0; i < n; ++i)
        input >> P[i];
	input.close();

    Min_circle  mc_input( P, P+n, true);

	int s;
	output  >> s;

	if (s != 2 && s != 3){
		return 1;
	}

	Min_circle mc_output;

	std::vector<Point> check;

	for (int i = 0; i < s; ++i){
		int k;
		output >> k;
		if (k < 1 || k > n){
			return 1;
		}
		mc_output.insert(P[k - 1]);
		check.push_back(P[k - 1]);
	}
	output.close();

	for (int i = 0; i < s; ++i){
		if ((check[i].hx()-mc_output.circle().center().hx())*(check[i].hx()-mc_output.circle().center().hx()) + (check[i].hy()-mc_output.circle().center().hy())*(check[i].hy()-mc_output.circle().center().hy()) != mc_output.circle().squared_radius()){
			return 1;
		}
	}

	return mc_input.circle() != mc_output.circle();
}