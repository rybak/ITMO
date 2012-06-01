import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static java.lang.Math.abs;

/**
 * Created by IntelliJ IDEA.
 * User: ррр
 * Date: 26.04.12
 * Time: 20:09
 * To change this template use File | Settings | File Templates.
 */

public class TestGen8 {

    public static void main(String[] args) throws FileNotFoundException {
        generate(0, 20);
        generate(1, 3);
    }

    private static void generate(int task, int count) throws FileNotFoundException {
        Random random = new Random();

        PrintWriter printWriter;
        String fileName, stringFormat;
        int n, m;

        if (task == 0) {
            fileName = "correctness_tests/";
            stringFormat = "%02d";
            n = random.nextInt(50);
            m = random.nextInt(200);

        } else {
            fileName = "performance_tests/";
            n = 10;
            m = 1000;
            stringFormat = "%03d";
        }

        for (int i = 0; i < count; i++) {
            String s = fileName + String.format(stringFormat, i + 1) + ".in";
            printWriter = new PrintWriter(s);

            write(printWriter, n, m);
        }
    }

    static void write(PrintWriter out, int dmax, int nmax){
        Random random = new Random();

        int d = abs(random.nextInt(dmax) + 1);
        List<Double> a = new ArrayList<Double>();
        out.println(d);
        for(int i = 0; i < d; i++){
            a.add((abs(random.nextInt(150)) + 1) * nextDouble());
            out.print(a.get(a.size() - 1) + " ");
        }
        out.println();

        for(int i = 0; i < d; i++){
            out.print(nextDouble(a.get(i), a.get(i) + 100) + " ");
        }

        int n = random.nextInt(nmax) + 10;

        out.println();
        out.println(n);
        for(int i = 0; i < n; i++){
            out.println();
            for(int j = 0; j < d; j++){
                out.print(nextDouble(0, random.nextInt(150) + 100) + " ");
            }
        }
        out.close();
    }
    static double nextDouble(double start, double finish){
        Random random = new Random();

        if(finish < start){
            double tmp = start;
            start = finish;
            finish = tmp;
        }
        return start + random.nextDouble()*(finish - start);
    }
    static double nextDouble(){
        return nextDouble(0, 1);
    }
}
