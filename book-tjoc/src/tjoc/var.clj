(def x 42)

(.start (Thread. #(println "Answer: " x)))
