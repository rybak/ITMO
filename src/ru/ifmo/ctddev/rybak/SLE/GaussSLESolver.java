package ru.ifmo.ctddev.rybak.SLE;

import java.util.Arrays;

public class GaussSLESolver extends SLESolver {

	private static final double EPS = 1e-9;

	@Override
	public double[] solve(double[][] input) {
		double[][] a = new double[input.length][];
		Matrix.copyMatrix(input, a);
		int m = a.length;
		int n = a[0].length - 1;
		int[] where = new int[m];
		Arrays.fill(where, -1);
		for (int j = 0, i = 0; j < n && i < m; ++j, ++i) {
			{
				int maxPos = i;
				for (int k = i; k < m; ++k) {
					if (Math.abs(a[k][j]) > Math.abs(a[maxPos][j])) {
						maxPos = k;
					}
				}
				if (Math.abs(a[maxPos][j]) < EPS) {
					continue;
				}
				for (int k = j; k <= n; ++k) {
					double tmp = a[maxPos][k];
					a[maxPos][k] = a[i][k];
					a[i][k] = tmp;
				}
			}
			where[j] = i;
			for (int k = i + 1; k < m; ++k) {
				if (k != i) {
					double c = a[k][j] / a[i][j];
					for (int h = j; h <= n; ++h) {
						a[k][h] -= (a[i][h] * c);
					}
				}
			}
		}
		double[] answer = new double[n];

		for (int j = n - 1; j > 0; --j) {
			for (int i = 0; i < j; ++i) {
				double c = a[i][j] / a[j][j];
				a[i][j] = 0;
				a[i][n] -= a[j][n] * c;
			}
		}

		for (int i = 0; i < n; ++i) {
			if (where[i] != -1) {
				answer[i] = a[where[i]][n] / a[where[i]][i];
			} else {
				answer[i] = 0;
			}
		}
		return answer;
	}

	@Override
	protected String getMethodName() {
		return "Гаусса";
	}

}
