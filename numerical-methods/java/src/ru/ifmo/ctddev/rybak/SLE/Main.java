package ru.ifmo.ctddev.rybak.SLE;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Locale;
import java.util.Random;
import java.util.StringTokenizer;

public class Main {

	private Random random = new Random(42);

	private static final String doubleFormat = "%10.5f  ";

	public static void main(String[] args) {
		new Main().run();
	}

	private void run() {
		Locale.setDefault(Locale.US);
		in = new MyScanner();
		try {
			out = new PrintWriter(new FileWriter(filename + ".out"));
			solve();
		} catch (IOException e) {
			e.printStackTrace();
		}
		in.close();
		out.close();
	}

	private void solve() {
		int n = in.nextInt();
		double[][] good = generateGoodMatrix(n);
		double[][] bad = generateBadMatrix(n);
		ArrayList<SLESolver> solvers = new ArrayList<SLESolver>();
		solvers.add(new GaussSLESolver());
		solvers.add(new JacobiSolver());
		solvers.add(new SeidelSolver());
		solvers.add(new SeidelRelaxationSolver());
		solvers.add(new ConjugateGradientSolver());
		out.println("Хорошая обусловленность:");
		showMatrix(n, good);
		for (SLESolver solver : solvers) {
			runTest(solver, good);
		}
		out.println("Плохая обусловленность (матрица Гильберта):");
		showMatrix(n, bad);
		for (SLESolver solver : solvers) {
			runTest(solver, bad);
		}
		out.println("целай матрица:");
		double[][] integer = generateMatrix();
		showMatrix(2, integer);
		for (SLESolver solver : solvers) {
			runTest(solver, integer);
		}

	}

	private void printDelimeter() {
		printDelimeter(80);
	}

	private void printDelimeter(int n) {
		for (int i = 0; i < n; ++i) {
			out.print('=');
		}
		out.println();
	}

	private void showMatrix(int n, double[][] a) {
		for (int i = 0; i < n; ++i) {
			showVector(a[i], n);
			out.print("  |  ");
			out.printf(doubleFormat + "\n", a[i][n]);
		}
		Matrix A = new Matrix(Common.getA(a));
		out.printf("cond = %10.5f\n", A.cond());
		printDelimeter();
		out.println();
	}

	@SuppressWarnings("unused")
	private void debugMatrix(double[][] a) {
		for (int i = 0; i < a.length; ++i) {
			out.println(Arrays.toString(a[i]));
		}
	}

	private void runTest(SLESolver s, double[][] a) {
		double[] x = null;
		try {
			x = s.solve(a);
		} catch (ArithmeticException e) {
		}
		out.println(s.getName());
		showSolution(a, x);
		if (s instanceof IterativeSLESolver) {
			out.println("Количество итераций: "
					+ ((IterativeSLESolver) s).numberOfIterations());
		}
		out.println();
	}

	private void showSolution(double[][] a, double[] x) {
		if (x != null) {
			double[][] A = Common.getA(a);
			double[] b = Common.getb(a);
			double[] b1 = new Matrix(A).mult(x);
			out.println(Arrays.toString(b));
			out.println(Arrays.toString(b1));
			
			Matrix.vectorMinus(b1, b);
			out.println("решение:");
			showVector(x);
			out.println();
			out.printf("невязка: %20.15f\n", Matrix.vectorNorm2(b1));
		} else {
			out.println("решение не найдено");
		}
	}

	private double[][] generateMatrix() {
		double[][] res = new double[2][3];
		res[0][0] = 2;
		res[0][1] = 7;
		res[0][2] = 3;

		res[1][0] = -4;
		res[1][1] = -5;
		res[1][2] = 1;
		return res;
	}

	private double[][] generateBadMatrix(int n) {
		double[][] res = new double[n][n + 1];
		for (int i = 0; i < n; ++i) {
			for (int j = 0; j < n; ++j) {
				res[i][j] = 1.0 / (i + j + 3);
			}
			res[i][n] = random.nextDouble();
		}

		return res;
	}

	private double[][] generateGoodMatrix(int n) {
		double[][] res = new double[n][n + 1];
		for (int i = 0; i < n; ++i) {
			double s = 0;
			for (int j = 0; j <= n; ++j) {
				res[i][j] = random.nextDouble();
				s += Math.abs(res[i][j]);
			}
			s -= res[i][i];
			while (Math.abs(res[i][i]) < s) {
				res[i][i] *= 10;
			}
		}
		for (int i = 0; i < n; ++i) {
			for (int j = i + 1; j < n; ++j) {
				double r = res[i][j] + res[j][i];
				res[i][j] = r * 0.5;
				res[j][i] = r * 0.5;
			}
		}

		return res;
	}

	private void showVector(double[] a) {
		if (a != null) {
			showVector(a, a.length);
		}
	}

	private void showVector(double[] a, int size) {
		if (a != null) {
			for (int i = 0; i < size; ++i) {
				out.printf(doubleFormat, a[i]);
			}
		}
	}

	@SuppressWarnings("unused")
	private void testMatrixInverted() {
		double[][] a = new double[2][2];
		a[0][0] = 2;
		a[0][1] = 3;
		a[1][0] = -1.0;
		a[1][1] = 0.0;
		Matrix A = new Matrix(a);
		Matrix B = A.inverted();
		out.println(A.toString());
		out.println(B.toString());
	}

	private BufferedReader br;
	private StringTokenizer st;
	private MyScanner in;
	private PrintWriter out;

	private final String filename = "SLE";

	private class MyScanner {
		public MyScanner() {
			try {
				br = new BufferedReader(new FileReader(filename + ".in"));
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}
		}

		public String next() {
			try {
				while (st == null || !st.hasMoreTokens()) {
					st = new StringTokenizer(br.readLine());
				}
				return st.nextToken();
			} catch (IOException e) {
				e.printStackTrace();
				return null;
			}
		}

		public int nextInt() {
			return Integer.parseInt(next());
		}

		// public char nextChar() {
		// return next().charAt(0);
		// }
		//
		// public long nextLong() {
		// return Long.parseLong(next());
		// }
		//
		// public double nextDouble() {
		// return Double.parseDouble(next());
		// }

		public void close() {
			try {
				br.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
