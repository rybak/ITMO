
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
	
	public Point nextPoint() {
		return new Point(nextInt() * nextDouble(), nextInt() * nextDouble());
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
		int n, m;
		
		if (task == 0) {
			fileName = "correctness_tests/";
			stringFormat = "%02d";			
			n = rp.nextInt(50);
			m = rp.nextInt(200);
			
		} else {
			fileName = "performance_tests/";
			n = rp.nextInt(300);
			m = rp.nextInt(1000);
			stringFormat = "%03d";
		}
		
		for (int i = 0; i < count; i++) {			
			pw = new PrintWriter(fileName + String.format(stringFormat, i + 1) + ".in");
						
			pw.println(n);
			pw.println(generatePoints(n));
				
			pw.println(m);
			pw.println(generatePoints(m));
			
			pw.close();
		}
	}
	
	public static void main(String[] args) throws FileNotFoundException {
		rp = new RandomPoint();
		generateTests(0, 20);
		generateTests(1, 100);		
	}
}
