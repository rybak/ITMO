
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
			result += i.next();
			
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
		PointArrayList q = generatePoints(m);
		pw.println(q);			
		
		pw.close();
	}

}
