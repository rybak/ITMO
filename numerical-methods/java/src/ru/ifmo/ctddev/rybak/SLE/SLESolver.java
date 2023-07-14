package ru.ifmo.ctddev.rybak.SLE;

public abstract class SLESolver {
	public String getName() {
		return "Метод " + getMethodName();
	}

	protected abstract String getMethodName();

	public abstract double[] solve(double[][] a);
}
