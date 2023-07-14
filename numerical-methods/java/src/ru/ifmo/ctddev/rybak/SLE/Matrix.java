package ru.ifmo.ctddev.rybak.SLE;

import java.util.Arrays;

public class Matrix {

	public static void copyMatrix(double[][] from, double[][] to) {
		for (int i = 0; i < from.length; ++i) {
			to[i] = Arrays.copyOf(from[i], from[i].length);
		}
	}

	public static void vectorPlus(double[] a, double[] b) {
		if (a.length != b.length) {
			throw new IllegalArgumentException(
					"vectorPlus: Sizes don't match : " + a.length + " != "
							+ b.length);
		}
		for (int i = 0; i < a.length; ++i) {
			a[i] += b[i];
		}
	}

	public static void vectorMult(double[] a, double b) {
		for (int i = 0; i < a.length; ++i) {
			a[i] *= b;
		}
	}

	public static void vectorMinus(double[] a, double[] b) {
		if (a.length != b.length) {
			throw new IllegalArgumentException(
					"vectorMinus: Sizes don't match : " + a.length + " != "
							+ b.length);
		}
		for (int i = 0; i < a.length; ++i) {
			a[i] -= b[i];
		}
	}

	public static double vectorNorm(double[] a) {
		// double res = 0.0;
		// int n = a.length;
		// for (int i = 0; i < n; ++i) {
		// res += Math.abs(Math.pow(a[i], n));
		// }
		// res = Math.pow(res, 1.0 / n);

		double res = Math.abs(a[0]);
		for (int i = 1; i < a.length; ++i) {
			res = Math.max(res, Math.abs(a[i]));
		}

		return res;
	}

	public static double vectorNorm2(double[] a) {
		double res = 0.0;
		int n = a.length;
		for (int i = 0; i < n; ++i) {
			res += Math.abs(Math.pow(a[i], n));
		}
		return Math.pow(res, 1.0 / n);
	}

	public static double scalarProduct(double[] a, double[] b) {
		double res = 0.0;
		for (int i = 0; i < a.length; ++i) {
			res += a[i] * b[i];
		}
		return res;
	}

	private int n, m;

	double[][] a;

	public Matrix(double[][] a) {
		n = a.length;
		m = a[0].length;
		this.a = Arrays.copyOf(a, n);
	}

	public Matrix(int n, int m) {
		this.n = n;
		this.m = m;
		a = new double[n][m];
	}

	public double cond() {
		return this.norm() * this.inverted().norm();
	}

	public Matrix inverted() {
		if (n != m) {
			throw new NoSuchMethodError("Cannot inverse not square matrix");
		}
		double[][] res = new double[n][n];
		for (int i = 0; i < n; ++i) {
			res[i][i] = 1;
		}
		double[][] ga = new double[n][];
		copyMatrix(a, ga);
		int[] where = new int[n];
		Arrays.fill(where, -1);
		for (int j = 0, i = 0; j < n && i < n; ++j) {
			{
				int maxPos = i;
				for (int k = i; k < n; ++k) {
					if (Math.abs(ga[k][j]) > Math
							.abs(ga[maxPos][j])) {
						maxPos = k;
					}
				}
				if (Math.abs(ga[maxPos][j]) < 1e-9) {
					continue;
				}
				for (int k = 0; k < n; ++k) {
					double tmp = ga[maxPos][k];
					ga[maxPos][k] = ga[i][k];
					ga[i][k] = tmp;
					tmp = res[maxPos][k];
					res[maxPos][k] = res[i][k];
					res[i][k] = tmp;
				}
			}
			
			where[j] = i;
			for (int ii = 0; ii < n; ++ii) {
				if (ii != i) {
					double c = ga[ii][j] / ga[i][j];
					for (int jj = 0; jj < n; ++jj) {
						ga[ii][jj] -= (ga[i][jj] * c);
						res[ii][jj] -= (res[i][jj] * c);
					}
				}
			}
			++i;
		}
		for (int j = n - 1; j > 0; --j) {
			for (int i = 0; i < j; ++i) {
				double c = ga[i][j] / ga[j][j];
				ga[i][j] = 0;
				for (int k = 0; k < n; ++k) {
					res[i][k] -= res[j][k] * c;	
				}
			}
		}
		double[][] answer = new double[n][n];
		for (int i = 0; i < n; ++i) {
			if (where[i] != -1) {
				for (int j = 0; j < n; ++j) {
					answer[i][j] = res[where[i]][j] / ga[where[i]][i];
				}
			} else {
				for (int j = 0; j < n; ++j) {
					answer[i][j] = 0;
				}
			}
		}
		return new Matrix(answer);
	}

	public double[] mult(double[] b) {
		if (m != b.length) {
			throw new IllegalArgumentException("Matrix mult");
		}
		double[] res = new double[b.length];
		Arrays.fill(res, 0.0);
		for (int i = 0; i < n; ++i) {
			for (int j = 0; j < m; ++j) {
				res[i] += a[i][j] * b[j];
			}
		}
		return res;
	}

	public double norm() {
		// l-norma
		double[] s = new double[a.length];
		Arrays.fill(s, 0);
		for (int j = 0; j < n; ++j) {
			for (int i = 0; i < n; ++i) {
				s[j] += Math.abs(a[i][j]);
			}
		}
		double ans = s[0];
		for (int i = 1; i < n; ++i) {
			if (ans < s[i]) {
				ans = s[i];
			}
		}
		return ans;
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < n; ++i) {
			sb.append(Arrays.toString(a[i]));
			sb.append('\n');
		}
		return sb.toString();
	}
}
