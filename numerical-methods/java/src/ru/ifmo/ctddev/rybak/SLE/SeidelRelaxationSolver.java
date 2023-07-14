package ru.ifmo.ctddev.rybak.SLE;

public class SeidelRelaxationSolver extends SeidelSolver {

	@Override
	protected String getMethodName() {
		return super.getMethodName() + " (с релаксациями)";
	}

	protected double[] jacobiInternalNext(double[] x) {
		double[] half = super.jacobiInternalNext(x);
		final double omega = 1.6;
		double[] res = new double[n];
		for (int i = 0; i < n; ++i) {
			res[i] = omega * half[i] + (1.0 - omega) * x[i];
		}
		return res;
	}
}
