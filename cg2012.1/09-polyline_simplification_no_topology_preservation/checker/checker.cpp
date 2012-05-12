#include <cmath>
#include <algorithm>
#include <fstream>
#include <vector>
#include <iostream>
#include <CGAL/Gmpq.h>

typedef CGAL::Gmpq gmp;


struct Point
{
	gmp x, y;

	Point (gmp _x = 0, gmp _y = 0):x(_x),y(_y){};

	Point operator-(const Point& a) const
	{
		return Point(x - a.x, y - a.y);
	}

	Point operator+ (const Point& a) const
	{
		return Point(x + a.x, y + a.y);
	}
};

struct Segment
{
	Point a, b;
	Segment (gmp _ax = 0, gmp _ay = 0, gmp _bx = 0, gmp _by = 0):a(_ax, _ay),b(_bx, _by){};
	Segment (Point _a, Point _b):a(_a),b(_b){};
};

gmp pow(gmp x, int z)
{
	gmp ans = 1;
	for (int i = 0; i < z; ++i)
		ans *= x;
	return ans;
}

gmp distancePointPoint(const Point& a, const Point& b)
{
	return (pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
}

// ќпредел€ем попадает ли перпендикул€р к пр€мой через точку непосредственно в отрезок и в зависимости от
// этого находим рассто€ние от точки до пр€мой или от точки до концов отрезка и берем минимум
gmp distancePointSegment(const Point& point, const Segment& segment)
{	
	gmp r1 = distancePointPoint(point, segment.a);
	gmp r2 = distancePointPoint(point, segment.b);
	gmp r12 = distancePointPoint(segment.a, segment.b);

	if ( r1 >= distancePointPoint(Point(r2,r12), Point(0,0)) || r2 >= distancePointPoint(Point(r1,r12), Point(0,0)))
		return std::min(distancePointPoint(point, segment.a),distancePointPoint(point, segment.b));

	return pow((segment.a.y - segment.b.y) * point.x + (segment.b.x - segment.a.x) * point.y + (segment.a.x * segment.b.y - segment.b.x * segment.a.y), 2) / (pow(segment.b.x - segment.a.x,2) + pow(segment.b.y - segment.a.y,2));
}

void DouglasPeucker(const int& begin, const int& end, const std::vector<Point>& points, std::vector<bool>& usage, const gmp& eps)
{
	gmp max = -1;
	int num = -1;
	Segment segment = Segment(points[begin], points[end]);

	for (int i = begin + 1; i < end; i++)
	{
		gmp dist = distancePointSegment(points[i], segment);
		if (max < dist && dist > eps)
		{
			max = dist;
			num = i;
		}
	}

	if (max != -1)
	{
		usage[num] = true;
		DouglasPeucker(begin, num, points, usage, eps);
		DouglasPeucker(num, end, points, usage, eps);
	}
}

bool checkOnWrongPoints(const int& begin, const int& end, const std::vector<Point>& points, const gmp& eps)
{
	Segment segment = Segment(points[begin], points[end]);

	for(int i = begin + 1; i < end; i++)
		if ( distancePointSegment(points[i], segment) > eps)
			return true;

	return false;
}

int main(int argc, char* argv[])
{
	if (argc != 3) 
	{
        std::cerr << "Usage: polygonalchain-checker inputfile outputfile" << std::endl;
        return -1;
    }
	
	std::ifstream in(argv[1]);
	int n;
	in >> n;
	std::vector<Point> points(n);
	for (int i = 0; i < n; i++)
	{
		gmp x, y;
		in >> x >> y;
		points[i] = Point(x, y);
	}
	gmp eps;
	in >> eps;
	in.close();

	std::vector<bool> usage(n);
	usage.assign(n, false);
	usage[0] = true;
	usage[n - 1] = true;
	
	//—читаем оптимальное количество вершин после применени€ алгоритма DoiglasPeucked
	DouglasPeucker(0, n - 1, points, usage, eps);

	int ans = 0;
	for (int i = 0; i < n; i++)
	{
		if (usage[i])
			ans++;
	}

	//—равниваем с ответом пользовател€(на лекции мы узнали, что возможно этот алгоритм не самый оптимальный, 
	//поэтому допускаетс€ решение с меньшим количеством вершин)
	std::ifstream out(argv[2]);
	int user_ans;
	out >> user_ans;
	if (user_ans < 2 || user_ans > ans)
		return 1;

	std::vector<int> user_points(user_ans);
	for (int i = 0; i < user_ans; i++)
	{
		out >> user_points[i];
		user_points[i]--;
	}
	out.close();

	//≈сли исходные концы цепи не сохранились, то €вно что-то не так
	if (user_points[0] != 0 || user_points[user_ans - 1] != n - 1)
		return 1;

	//ѕровер€ем, действительно ли все вершины не дальше eps
	for (int i = 0; i < user_ans - 1; i++)
	{
		if (checkOnWrongPoints(user_points[i], user_points[i + 1], points, eps))
			return 1;
	}

	return 0;
}
