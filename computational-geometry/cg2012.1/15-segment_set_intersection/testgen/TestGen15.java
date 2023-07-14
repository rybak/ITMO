import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Random;

public class TestGen15 {
     public static void main(String[] args) throws FileNotFoundException {
        generate(0, 50);
        generate(1, 1);
    }

    private static void generate(int task, int count) throws FileNotFoundException {
        PrintWriter pw;
        String fileName, stringFormat;
        int n, m;
        Random random = new Random();

        if (task == 0) {
            fileName = "correctness_tests/";
            stringFormat = "%02d";
            //n = random.nextInt(50) + 1;
        } else {
            fileName = "performance_tests/";
            n = 400000;
            stringFormat = "%03d";
        }

        for (int i = 0; i < count; i++) {
            String s = fileName + String.format(stringFormat, i + 1) + ".in";
            pw = new PrintWriter(s);

            if(task == 0){
                write(pw, random.nextInt(50) + 1);
            } else {
                write(pw, 400000);
            }
        }
    }
    static void write(PrintWriter out, int n){
        Random random = new Random();

        out.println(n);
        for(int i = 0; i < n; i++){
            for(int j = 0; j < 4; j++){
                out.print(random.nextDouble() * (1 + random.nextInt(1000)) + " ");
            }
            out.println();
        }
        out.close();
    }
}
