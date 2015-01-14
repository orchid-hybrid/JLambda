import gnu.bytecode.*;

interface Node { public void visit(CodeAttr code); }

class HaltNode implements Node {
    public void visit(CodeAttr code) {
	code.emitReturn();
    }
}

class PushNode implements Node {
    int i;
    Node c;

    public PushNode(int i, Node c) {
	this.i = i;
	this.c = c;
    }
    
    public void visit(CodeAttr code) {
	code.emitPushInt(i);
	c.visit(code);
    }
}

class AddNode implements Node {
    Node c;

    public AddNode(Node c) {
	this.c = c;
    }
    
    public void visit(CodeAttr code) {
	code.emitAdd();
	c.visit(code);
    }
}

public class HuttonsRazor {
    public static void main(String[] args) throws Exception {
	// "public class HelloWorld extends java.lang.Object".
	ClassType c = new ClassType("buisness/HelloWorld");
	c.setSuper("java.lang.Object");
	c.setModifiers(Access.PUBLIC);

	Node t1 = new PushNode(17,new HaltNode());
	Node t2 = new PushNode(1,new PushNode(1,new AddNode(new HaltNode())));
	Node t3 = new PushNode(1,new PushNode(2,new AddNode(new PushNode(3,new AddNode(new PushNode(4,new AddNode(new PushNode(5,new AddNode(new HaltNode())))))))));
	Node t4 = new PushNode(1,new PushNode(2,new PushNode(3,new PushNode(4,new PushNode(5,new AddNode(new AddNode(new AddNode(new AddNode(new HaltNode())))))))));
	Node t5 = new PushNode(5,new PushNode(-7,new AddNode(new PushNode(-5,new PushNode(7,new AddNode(new AddNode(new HaltNode())))))));
	
	// "public static int exec()".
	Method m = c.addMethod("exec", "()I", Access.PUBLIC | Access.STATIC);
	CodeAttr code = m.startCode();
	code.pushScope();
	t5.visit(code);
	code.popScope();
	
	c.writeToFile("buisness/HelloWorld.class");
    }
}
