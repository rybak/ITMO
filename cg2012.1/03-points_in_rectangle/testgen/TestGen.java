
import java.io.*;
import java.util.*;

class Point {
	final double x, y;	

	public Point(double x, double y) {
		this.x = x;
		this.y = y;
	}
	
	public String toString() {
		return x + " " + y;
	}
	
	public boolean equals(Object o) {
		Point other = (Point) o;
		return x == other.x || y == other.y;
	}
}

class Rectangle {
	final Point a, b;
	
	public Rectangle(Point a, Point b) {
		this.a = new Point(Math.min(a.x, b.x), Math.min(a.y, b.y));
		this.b = new Point(Math.max(a.x, b.x), Math.max(a.y, b.y));
	}
	
	public String toSting() {
		return a + " " + b;
	}
}

class RandomPoint extends Random {
	
	public Point nextPoint() {
		return new Point(nextInt() * nextDouble(), nextInt() * nextDouble());
	}
}

class PointArrayList extends ArrayList<Point> {
	
	public String toString() {
		String result = "";
		
		for (Iterator<Point> i = this.iterator(); i.hasNext(); ) {
			result += i.next().toString();
			
			if (i.hasNext()) {
				result += "\n";
			}
		}
			
		return result;
	}
}

class RectangleArrayList extends ArrayList<Rectangle> {
	
	public String toString() {
		String result = "";
		
		for (Iterator<Rectangle> i = this.iterator(); i.hasNext(); ) {
			result += i.next().toSting();			
			
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
	
	private static RectangleArrayList generateRectangles(int count) {
		RectangleArrayList result = new RectangleArrayList();
		
		while (result.size() < count) {
			Point a = rp.nextPoint();
			Point b = rp.nextPoint();
			
			while (a.equals(b)) {
				b = rp.nextPoint();
			}
			
			Rectangle rectangle = new Rectangle(a, b);
			
			if (!result.contains(rectangle)) {
				result.add(rectangle);				
			}
		}
		
		return result;
	}
	
	private static void generateTests(int task, int count) throws FileNotFoundException {
		PrintWriter pw;
		String fileName, stringFormat;
		int n, m;
		
		if (task == 0) {
			fileName = "correctness_tests/";
			stringFormat = "%02d";			
			n = rp.nextInt(200);
			m = rp.nextInt(50);
			
		} else {
			fileName = "performance_tests/";
			n = rp.nextInt(1000);
			m = rp.nextInt(300);
			stringFormat = "%03d";
		}
		
		for (int i = 0; i < count; i++) {
			String s = fileName + String.format(stringFormat, i + 1) + ".in";
			pw = new PrintWriter(s);
						
			pw.println(n);
			pw.println(generatePoints(n));
				
			pw.println(m);
			pw.println(generateRectangles(m));
			
			pw.close();
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		rp = new RandomPoint();
		generateTests(0, 20);
		generateTests(1, 100);
	}
}
