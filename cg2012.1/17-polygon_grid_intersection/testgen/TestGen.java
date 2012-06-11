import java.io.*;
import java.util.*;

class PointComparator implements Comparator<Point> {
	private final Point center;
	
	PointComparator(Point center) {
		this.center = center;
	}
		
	public boolean equals(Object arg0, Object arg1) {		
		Point a = new Point(((Point) arg0).x - center.x, ((Point) arg0).y - center.y);
		Point b = new Point(((Point) arg1).x - center.x, ((Point) arg1).y - center.y);
		
		return a.equals(b);
	}
	
	public int compare(Point arg0, Point arg1) {
		Point a = new Point(arg0.x - center.x, arg0.y - center.y);
		Point b = new Point(arg1.x - center.x, arg1.y - center.y);
		
		return a.compareTo(b);
	}
}

class Point implements Comparable<Point> {
	final double x, y;	

	public Point(double x, double y) {
		this.x = x;
		this.y = y;
	}
	
	public double getX() {
		return x;
	}
	
	public double getY() {
		return y;
	}
	
	double abs() {
		return Math.sqrt(y * y + x * x);
	}
	
	double arg() {
		return Math.atan2(y, x);
	}
	
	public String toString() {
		return x + " " + y;
	}
	
	public boolean equals(Object o) {
		Point other = (Point) o;
		return x == other.x && y == other.y;
	}
	
	public int compareTo(Point other) {
		if (!equals(other)) {
			if (arg() != other.arg()) {
				return arg() > other.arg() ? 1 : -1;
			} else {			
				return abs() > other.abs() ? 1 : -1;
			}
		} else {	
			return 0;
		}
	}
}

class RandomPoint extends Random {	
	
	public Point nextPoint() {
		return new Point(nextInt() * nextDouble(), nextInt() * nextDouble());
	}
}

class PointArrayList extends ArrayList<Point> {
	
	public Point getCenter() {
		double x = 0, y = 0;
		
		if (!this.isEmpty()) {
			for (Point point : this) {
				x += point.getX();
				y += point.getY();
			}
			
			x /= this.size();
			y /= this.size();
		}
		
		return new Point(x, y);
	}
	
	public String toString() {
		String result = "";
		
		for (Iterator<Point> i = this.iterator(); i.hasNext(); ) {
			result += i.next();
			
			if (i.hasNext()) {
				result += "\n";
			}
		}
			
		return result;
	}
}

public class TestGen {
	private static RandomPoint rp;
	
	private static PointArrayList generatePoints(int count) {
		PointArrayList result = new PointArrayList();
		
		while (result.size() < count) {
			Point point = rp.nextPoint();
			
			if (!result.contains(point)) {
				result.add(point);				
			}
		}
		
		return result;
	}
	
	private static void generateTests(int task, int count) throws FileNotFoundException {
		PrintWriter pw;
		String fileName, stringFormat;
		int n;
		
		if (task == 0) {
			fileName = "correctness_tests/";
			stringFormat = "%02d";			
			n = rp.nextInt(50);
			
		} else {
			fileName = "performance_tests/";
			n = rp.nextInt(1000);
			stringFormat = "%03d";
		}
		
		for (int i = 0; i < count; i++) {			
			pw = new PrintWriter(fileName + String.format(stringFormat, i + 1) + ".in");
						
			pw.println(n);
			PointArrayList vertices = generatePoints(n);
			 
			Collections.sort(vertices, new PointComparator(vertices.getCenter()));		
			pw.println(vertices);
			
			pw.close();
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		rp = new RandomPoint();
		generateTests(0, 20);
		generateTests(1, 5);
	}
}
