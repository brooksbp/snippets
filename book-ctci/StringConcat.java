import java.util.Date;
import java.util.UUID;


public class StringConcat {

  public static String joinWords(String[] words) {
    String all = "";
    for (String w : words) {
      all = all + w;
    }
    return all;
  }

  public static String joinWords2(String[] words) {
    StringBuffer all = new StringBuffer();
    for (String w : words) {
      all.append(w);
    }
    return all.toString();
  }
  
  public static void main(String[] argv) {

    // n = 10000
    // joinWords  = 2252
    // joinWords2 = 7
    
    String[] a1 = new String[10000];
    for (int i = 0; i < 10000; i = i + 1) {
      a1[i] = UUID.randomUUID().toString();
    }

    String tmp;
    long start, end;
    
    start = new Date().getTime();
    tmp = joinWords(a1);
    end = new Date().getTime();

    System.out.println("joinWords  = " + (end - start));
    
    start = new Date().getTime();
    tmp = joinWords2(a1);
    end = new Date().getTime();
    
    System.out.println("joinWords2 = " + (end - start));
  }
  
}
