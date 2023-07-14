package ru.ifmo.ctddev.rybak.SLE;

import java.util.Arrays;

public class SeidelSolver extends JacobiSolver {
	@Override
	protected String getMethodName() {
		return "Зейделя";
	}

	public double[] solve(double[][] a, double eps) {
		return super.solve(a, eps);
	}

	protected double eps1(double eps) {
		double[][] c2 = new double[n][n];
		for (int i = 0; i < n; ++i) {
			for (int j = i + 1; j < n; ++j) {
				c2[i][j] = c[i][j];
			}
		}
		Matrix C2 = new Matrix(c2);
		double cNorm = C.norm();
		double c2Norm = C2.norm();
		return Math.abs(eps * (1.0 - cNorm) / c2Norm);
	}

	protected double[] jacobiInternalNext(double[] x) {
		double[] res = Arrays.copyOf(x, n);
		for (int i = 0; i < n; ++i) {
			double s = 0.0;
			for (int j = 0; j < n; ++j) {
				if (i != j) {
					s += c[i][j] * res[j];
				}
			}
			res[i] = s + d[i];
		}
		return res;
	}
}
