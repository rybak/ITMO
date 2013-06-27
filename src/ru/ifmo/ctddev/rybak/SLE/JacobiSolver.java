package ru.ifmo.ctddev.rybak.SLE;

public class JacobiSolver extends IterativeSLESolver {

	protected double[] d;
	protected double[][] c;
	protected Matrix C;

	@Override
	protected String getMethodName() {
		return "Якоби";
	}
	
	protected double eps1(double eps) {
		double cNorm = C.norm();
		return Math.abs(eps * (1.0 - cNorm) / cNorm);
	}

	protected void makeEquivalentForm(double[][] a) {
		c = new double[n][n];
		d = new double[n];
		for (int i = 0; i < n; ++i) {
			for (int j = 0; j < n; ++j) {
				c[i][j] = i == j ? 0 : -a[i][j] / a[i][i];
			}
		}
		for (int i = 0; i < n; ++i) {
			d[i] = a[i][n] / a[i][i];
		}
		C = new Matrix(c);
	}

	protected double[] jacobiInternalNext(double[] x) {
		double[] res = C.mult(x);
		Matrix.vectorPlus(res, d);
		if (!Common.checkNaN(res)) {
			System.err.println("NaN error");
			return null;
		}
		return res;
	}

	protected double[] next(double[] x) {
		double[] res = jacobiInternalNext(x);
		if (!Common.checkNaN(res)) {
			System.err.println("NaN error");
			return null;
		}
		return res;
	}

	@Override
	protected void preparations(double[][] a, double[] solution) {
		makeEquivalentForm(a);
	}


}
