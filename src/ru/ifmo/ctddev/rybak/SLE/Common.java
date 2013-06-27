package ru.ifmo.ctddev.rybak.SLE;

import java.util.Arrays;

public class Common {

	public static double[][] getA(double[][] a) {
		double[][] res = new double[a.length][a[0].length - 1];
		for (int i = 0; i < res.length; ++i) {
			for (int j = 0; j < res[0].length; ++j) {
				res[i][j] = a[i][j];
			}
		}
		return res;
	}

	public static double[] getb(double[][] a) {
		double[] res = new double[a.length];
		for (int i = 0; i < res.length; ++i) {
			res[i] = a[i][a[0].length - 1];
		}
		return res;
	}

	public static double[] generateStart(int n) {
		double[] res = new double[n];
		Arrays.fill(res, 1.0);
		return res;
	}

	public static boolean checkNaN(double[] a) {
		if (a == null) {
			return false;
		}
		for (int i = 0; i < a.length; ++i) {
			if (Double.isNaN(a[i])) {
				return false;
			}
		}
		return true;
	}

	public static String showSize(int n, int m) {
		return n + " x " + m;
	}
}
