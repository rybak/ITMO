package ru.ifmo.ctddev.rybak.SLE;

import java.util.Arrays;

public abstract class IterativeSLESolver extends SLESolver {

	protected long iterations;
	protected int n;
	protected static final double DEFAULT_EPS = 1e-9;
	protected static final long MAX_ITER = 10000;

	public long numberOfIterations() {
		return iterations;
	}

	public double[] solve(double[][] a) {
		return solve(a, DEFAULT_EPS);
	}

	public double[] solve(double[][] a, double eps) {
		n = a.length;
		double[] solution = Common.generateStart(n);
		iterations = 0;
		double[] diff;
		preparations(a, solution);
		eps = eps1(eps);
		do {
			double[] candidate = next(solution);
			if (candidate == null) {
				return null;
			}
			diff = Arrays.copyOf(candidate, n);
			Matrix.vectorMinus(diff, solution);
			++iterations;
			solution = candidate;
		} while (iterations < MAX_ITER && Matrix.vectorNorm(diff) > eps);
		if (iterations == MAX_ITER) {
			return null;
		}
		return solution;
	}

	protected abstract double[] next(double[] solution);

	protected abstract double eps1(double eps);

	protected abstract void preparations(double[][] a, double[] solution);

}
