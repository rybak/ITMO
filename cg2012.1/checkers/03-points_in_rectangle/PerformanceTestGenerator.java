
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
	
	private final static double MAX_VALUE = 1e5;	
	
	public Point nextPoint() {
		return new Point(nextInt((int) MAX_VALUE) * nextDouble(), nextInt((int) MAX_VALUE) * nextDouble());
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

public class PerformanceTestGenerator {
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
	
	public static void main(String[] args) throws FileNotFoundException {
		if (args.length != 2) {
			System.out.println("unexpected parameters");
		}
		
		PrintWriter pw = new PrintWriter("test.in");
		rp = new RandomPoint();
		
		int n = Integer.parseInt(args[0]);
		pw.println(n);
		PointArrayList s = generatePoints(n);
		pw.println(s);
		
		int m = Integer.parseInt(args[1]);
		pw.println(m);
		RectangleArrayList q = generateRectangles(m);
		pw.println(q);			
		
		pw.close();
	}
}
