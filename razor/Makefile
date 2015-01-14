all: HuttonsRazor.class buisness/HelloWorld.class Test.class

HuttonsRazor.class: HuttonsRazor.java
	javac -classpath .:../kawa/kawa-2.0.1.jar HuttonsRazor.java

buisness/HelloWorld.class: buisness HuttonsRazor.class
	java -classpath .:../kawa/kawa-2.0.1.jar HuttonsRazor

buisness:
	mkdir buisness

Test.class: buisness/HelloWorld.class
	javac Test.java

test: Test.class
	java Test
