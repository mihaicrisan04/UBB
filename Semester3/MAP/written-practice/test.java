
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class test {
    public static void main(String[] args) {


        // List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8,9,10,11,12,14,15);
        // String str = numbers.stream()
        //         .filter(n -> n % 5 == 0 || n % 2 == 0)  
        //         .map(n -> "N" + n + "R")
        //         .reduce("", (a, b) -> a + b);

        // List<String> list = numbers.stream()
        //         .filter(n -> n % 5 == 0 || n % 2 == 0)  
        //         .map(n -> "N" + n + "R")
        //         .collect(Collectors.toList());

        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8,9,10,11,12,14,15);
        int sum = numbers.stream()
                .filter(n -> n % 3 == 0 || n % 7 == 0)
                .map(n -> n - 1)
                .reduce(0, (a, b) -> (a + b) % 5);

        System.out.println(sum);



    }
}