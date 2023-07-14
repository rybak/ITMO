package ru.ifmo.ctddev.rybak.simpleiter;
import java.awt.Color;
import java.awt.Graphics2D;
import java.util.*;

public class Calculator {
	private double x0;

	private double leftX;
	private double rightX;
	private double bottomY;
	private double topY;
	private int iter;

	private int width = 0;
	private int height = 0;

	public Calculator(int w, int h, double minX, double maxX, double minY,
			double maxY, double x0, int iter) {
		this.width = w;
		this.height = h;
		this.leftX = minX;
		this.rightX = maxX;
		this.bottomY = minY;
		this.topY = maxY;
		this.x0 = x0;
		this.iter = iter;
	}

	private final static double eps = 1e-9;

	private final static Color X_COLOR = Color.GREEN;
	private final static Color BIF_COLOR = Color.MAGENTA;

	public void draw(Graphics2D g) {
		drawAxis(g);
		for (int scrnX = 0; scrnX < width; ++scrnX) {
			double r = getRealX(scrnX);
			double x2 = 1 - 1 / r;
			Collection<Double> ys = buildXs(r, x0);
			// if (1 <= r && r <= 3)
			draw2(g, r, x0, ys);
			for (double y : ys) {
				drawRealPoint(g, r, y, BIF_COLOR);
			}
			drawRealPoint(g, r, x2, X_COLOR);
		}
	}

	private void draw2(Graphics2D g, double r, double x02, Collection<Double> ys) {
		ArrayList<Double> ys2 = buildXs2(r, x0);
		for (int i = 0; i < ys2.size(); ++i) {
			double x = ys2.get(i);
			if (!ys.contains(x)) {
				drawRealPoint(g, r, x, (i % 2 == 0) ? ((i % 4 == 0) ? Color.RED
						: Color.ORANGE) : ((i % 4 == 1) ? Color.BLUE
						: Color.CYAN));
			}
		}
		System.err.println(r + " " + ys.size());
	}

	private Set<Double> buildXs(double r, double x0) {
		Set<Double> xs = new HashSet<Double>();
		double x = x0;
		for (int i = 0; i < iter; ++i) {
			x = nextX(x, r);
		}
		for (int i = 0; i < iter; ++i) {
			x = nextX(x, r);
			xs.add(x);
		}
		return xs;
	}

	private ArrayList<Double> buildXs2(double r, double x0) {
		ArrayList<Double> xs = new ArrayList<Double>();
		double x = x0;
		for (int i = 0; i < iter; ++i) {
			x = nextX(x, r);
			xs.add(x);
		}
		return xs;

	}

	private double nextX(double x, double r) {
		return r * x * (1 - x);
	}

	private final static int NOTCH_SIZE = 2;

	private static Color AXIS_COLOR = Color.BLACK;

	private void drawAxis(Graphics2D g) {
		g.setColor(AXIS_COLOR);
		int h = g.getFontMetrics().getAscent();
		int x0 = getSCRNX(0);
		int y0 = getSCRNY(0);
		g.drawLine(x0, 0, x0, height - 1);
		g.drawLine(0, y0, width - 1, y0);
		for (double x = Math.round(leftX - 1); x <= rightX; x += 1.0) {
			int xx = getSCRNX(x);
			g.drawLine(xx, y0 - NOTCH_SIZE, xx, y0 + NOTCH_SIZE);
			g.drawLine(xx, 0, xx, height);
			g.drawString(Visualizer.showDouble(x), xx + NOTCH_SIZE, y0 + h);
		}
		for (double y = Math.round(bottomY - 1); y <= topY; y += 1.0) {
			if (!doubleEq(y, 0, eps)) {
				int yy = getSCRNY(y);
				g.drawLine(x0 - NOTCH_SIZE, yy, x0 + NOTCH_SIZE, yy);

				g.drawString(Visualizer.showDouble(y), x0 + NOTCH_SIZE, yy + h
						/ 2);
			}
		}
	}

	public double getRealX(int scrnX) {
		return ((double) scrnX) / width * getWidth() + leftX;
	}

	public double getRealY(int scrnY) {
		return topY - ((double) scrnY) / height * getHeight();
	}

	public int getSCRNX(double x) {
		return (int) Math.round((x - leftX) / getWidth() * width);
	}

	public int getSCRNY(double y) {
		return (int) Math.round((topY - y) / getHeight() * height);
	}

	private void drawRealPoint(Graphics2D g, double x, double y, Color c) {
		g.setColor(c);
		drawPointGraphics(g, getSCRNX(x), getSCRNY(y));
	}

	private void drawPointGraphics(Graphics2D g, int x, int y) {
		if (x < 0 || x > width || y >= height || y < 0) {
			return;
		}
		g.drawLine(x, y, x, y);
	}

	private boolean doubleEq(double a, double b, double eps) {
		return Math.abs(a - b) < eps;
	}

	private double getWidth() {
		return Math.abs(rightX - leftX);
	}

	private double getHeight() {
		return Math.abs(topY - bottomY);
	}

}
