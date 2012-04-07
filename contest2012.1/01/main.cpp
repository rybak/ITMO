#include <vector>
#include <iostream>
#include "geometry.h"

void quick_hull(const std::vector<point> &points, std::vector<size_t> &hull, size_t left, size_t right, const std::vector<size_t> &new_points)
{
    using std::vector;
    size_t max_pos = new_points[0];
    size_t m = new_points.size();
    for (size_t i = 0; i < m; ++i)
    {
        if (quickhull_predicate(points[left], points[right], points[new_points[i]], points[max_pos]) == 1)
        {
            max_pos = new_points[i];
        }
    }
    vector<size_t> left_points;
    vector<size_t> right_points;
    
    for (size_t i = 0, tmp; i < m; ++i)
    {
        tmp = new_points[i];
        if (tmp != max_pos)
        {
            if (left_turn(points[left], points[max_pos], points[tmp]) == 1)
            {
                left_points.push_back(tmp);
            }
            else if (left_turn(points[max_pos], points[right], points[tmp]) == 1)
            {
                right_points.push_back(tmp);
            }
        }
    }
    if (right_points.size() > 0)
    {
        quick_hull(points, hull, max_pos, right, right_points);
    }
    hull.push_back(max_pos);
    if (left_points.size() > 0)
    {
        quick_hull(points, hull, left, max_pos, left_points);
    }
}

void quick_hull(const std::vector<point> &points, std::vector<size_t> &hull)
{
    using std::vector;
    size_t n = points.size();
    switch (n)
    {
    case 2:
        if (points[0] != points[1])
        {
            hull.push_back(1);
        }
    case 1:
        hull.push_back(0);
    case 0:
        return;
    }
    
    size_t left = 0, right = 0;
    for (size_t i = 1; i < n; ++i)
    {
        if ((points[i].x < points[left].x) || ((points[i].x == points[left].x) && (points[i].y < points[left].y)))
        {
            left = i;
        }
        else if ((points[i].x > points[right].x) || ((points[i].x == points[right].x) && (points[i].y > points[right].y)))
        {
            right = i;
        }
    }
    vector<size_t> above;
    vector<size_t> below;
    for (size_t i = 0; i < n; ++i)
    {
        if (i == left || i == right)
        {
            continue;
        }
        int t = left_turn(points[left], points[right], points[i]);
        if (t == 1)
        {
            above.push_back(i);
        }
        else if (t == -1)
        {
            below.push_back(i);
        }
    }

    if (above.size() > 0)
    {
        quick_hull(points, hull, left, right, above);
    }
    hull.push_back(left);
    if (below.size() > 0)
    {
        quick_hull(points, hull, right, left, below);
    }
    hull.push_back(right);
}

int main()
{
    using std::cin;
    using std::cout;
    using std::vector;
    //freopen("input.txt", "r", stdin);
    //freopen("output.txt", "w", stdout);
    
    size_t n;
    cin >> n;
    
    vector<point> points;
    double x, y;
    for (size_t i = 0; i < n; ++i)
    {
        cin >> x >> y;
        points.push_back(point(x, y));    
    }

    vector<size_t> hull;
    quick_hull(points, hull);

    size_t m = hull.size();
    cout << m << '\n';
    for (size_t i = 0; i < m; ++i)
    {
		cout << points[hull[i]].x << ' ' << points[hull[i]].y << '\n';
    }
}