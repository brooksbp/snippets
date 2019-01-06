// javac HashTable.java
// java HashTable

import java.util.*;

public class HashTable {

  public static void main(String[] args) {
    System.out.println("Hello, world");
  }

  public HashMap<Integer, Student> buildMap(Student[] students) {
    HashMap<Integer, Student> map = new HashMap<Integer, Student>();
    for (Student s : students) map.put(s.getId(), s);
    return map;
  }

}

class Student {

  Integer id = 0;
  
  Integer getId() {
    return id;
  }
  
}
