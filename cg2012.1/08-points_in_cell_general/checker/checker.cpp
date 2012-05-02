#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>

#include <CGAL/Gmpq.h>
typedef CGAL::Gmpq gmp;

void onedim(std::vector<std::vector<gmp>>& points, gmp val1, gmp val2){
	std::sort(points.begin(), points.end());
	std::vector<gmp> current(points.size());
	for(int i = 0; i < points.size(); i++){
		current[i] = points[i][0];
	}
	int a = std::lower_bound(current.begin(), current.end(), val1) - current.begin();
	int b = std::upper_bound(current.begin(), current.end(), val2) - current.begin();
	for(int i = 0; i < points.size(); i++){
		std::rotate(points[i].begin(), points[i].begin() + 1, points[i].end());
	}
	
	points.erase(points.begin() + b, points.end());
	points.erase(points.begin(), points.begin() + a);

}  

int main(int argc, char* argv[]){
	std::ifstream in(argv[1]);
	int d;
	in >> d;

	std::vector<std::pair<gmp, gmp>> dcels(d);
	for(int i = 0; i < d; i++){
		double a;
		double b;
		in >> a;
		dcels[i].first = gmp(a);
	}

	for(int i = 0; i < d; i++){
		double a;
		double b;
		in >> a;
		dcels[i].second = dcels[i].first + gmp(a);
	}


	int n;
	in >> n;
	std::vector<std::vector<gmp>> points;
	for(int i = 0; i < n; i++){
		std::vector<gmp> current(d);
		double val;
		for(int j = 0; j < d; j++){
			in >> val;
			current[j] = gmp(val);
		}
		points.push_back(current);
	}
	
	std::vector<std::vector<gmp>> pointsstart = points;
	
	int dim = d;
	int i = 0;
	while((dim > 0) && points.size() != 0){
		onedim(points, dcels[i].first, dcels[i].second);
		i++;
		dim--;
	}

	
	std::vector<int> answer;
	if(points.size() == 0){
		answer.push_back(-1);
	} else {
		for(int i = 0; i < points.size(); i++){
			for(int j = 0; j < pointsstart.size(); j++){
				if(points[i] == pointsstart[j]){
					answer.push_back(j + 1);
				}
			}
		}
	}
	std::sort(answer.begin(), answer.end());
	in.close();
	
	
	
	std::ifstream input(argv[2]);

	int nans;
	input >> nans;
	std::vector<int> ans;
	int cur = -1;
	for(int i = 0; i < nans; i++){
		input >> cur;
		ans.push_back(cur);
	}

	input.close();
	
	
	if(ans.size() != answer.size()){
		return 1;
	}

	std::sort(ans.begin(), ans.end());
	
	
	for(int i = 0; i < nans; i++){
		if(ans[i] != answer[i]){
			return 1;
		}
	}	
	
	return 0;
}