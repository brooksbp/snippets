// javac ArrayList.java ; java ArrayList

import java.util.*;

public class ArrayList {

  // hg.openjdk.java.net/jdk7/jdk7-gate/jdk/file/tip/src/share/classes/java/util/ArrayList.java
  
  public ArrayList<String> merge(String[] words, String[] more) {
    ArrayList<String> sentence = new ArrayList<String>();
    for (String w : words) sentence.add(w);
    for (String w : more) sentence.add(w);
    return sentence;
  }
  
}
