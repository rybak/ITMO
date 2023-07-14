package ru.ifmo.ctddev.rybak.SLE;

import java.util.Arrays;

public class ConjugateGradientSolver extends IterativeSLESolver {

	private Matrix A;
	private double[] b;
	private double[] g;
	private double[] p;

	@Override
	public String getMethodName() {
		return "сопряженных градиентов";
	}
	
	@Override
	protected void preparations(double[][] a, double[] solution) {
		A = new Matrix(Common.getA(a));
		b = Common.getb(a);
		g = calcG(solution);
		p = calcP0(g);
	}

	@Override
	protected double eps1(double eps) {
		return eps;
	}

	private double[] calcP0(double[] g) {
		double[] res = Arrays.copyOf(g, g.length);
		Matrix.vectorMult(res, -1.0);
		return res;
	}

	@Override
	protected double[] next(double[] solution) {
		double[] res = Arrays.copyOf(solution, n);
		double[] pk = Arrays.copyOf(p, n);
		double alpha = calcAlpha(g, p);
		Matrix.vectorMult(p, alpha);
		Matrix.vectorPlus(res, p);
		g = calcG(res);
		p = calcP(g, pk);
		return res;
	}

	private double[] calcG(double[] x) {
		double[] g;
		g = A.mult(x);
		Matrix.vectorMinus(g, b);
		return g;
	}

	private double calcAlpha(double[] g, double[] p) {
		return -Matrix.scalarProduct(g, p) / Matrix.scalarProduct(A.mult(p), p);
	}

	private double[] calcP(double g[], double[] pk) {
		double[] res = calcP0(g);
		double[] Apk = A.mult(pk);
		double beta = -Matrix.scalarProduct(Apk, g)
				/ Matrix.scalarProduct(Apk, pk);
		Matrix.vectorMult(pk, beta);
		Matrix.vectorPlus(res, pk);
		return res;
	}

}
