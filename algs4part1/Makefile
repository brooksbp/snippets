JFLAGS = -g -cp .:stdlib.jar
JC = javac
.SUFFIXES: .java .class
.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
	HelloWorld.java

default: classes

classes: $(CLASSES:.java=.class)

clean:
	$(RM) *.class
