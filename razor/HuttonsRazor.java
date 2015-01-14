import gnu.bytecode.*;

public class HuttonsRazor {
    public static void t1(CodeAttr code) {
	// PUSH 17 HALT
	code.emitPushInt(17);
	code.emitReturn();
    }
    
    public static void t2(CodeAttr code) {
	// PUSH 1 (PUSH 1 (ADD HALT))
	code.emitPushInt(1);
	code.emitPushInt(1);
	code.emitAdd();
	code.emitReturn();
    }

    public static void t3(CodeAttr code) {
	// PUSH 1 (PUSH 2 (ADD (PUSH 3 (ADD (PUSH 4 (ADD (PUSH 5 (ADD HALT))))))))
	code.emitPushInt(1);
	code.emitPushInt(2);
	code.emitAdd();
	code.emitPushInt(3);
	code.emitAdd();
	code.emitPushInt(4);
	code.emitAdd();
	code.emitPushInt(5);
	code.emitAdd();
	code.emitReturn();
    }

    public static void t4(CodeAttr code) {
	// PUSH 1 (PUSH 2 (PUSH 3 (PUSH 4 (PUSH 5 (ADD (ADD (ADD (ADD HALT))))))))
	code.emitPushInt(1);
	code.emitPushInt(2);
	code.emitPushInt(3);
	code.emitPushInt(4);
	code.emitPushInt(5);
	code.emitAdd();
	code.emitAdd();
	code.emitAdd();
	code.emitAdd();
	code.emitReturn();
    }
    
    public static void t5(CodeAttr code) {
	// PUSH 5 (PUSH (-7) (ADD (PUSH (-5) (PUSH 7 (ADD (ADD HALT))))))
	code.emitPushInt(5);
	code.emitPushInt(-7);
	code.emitAdd();
	code.emitPushInt(-5);
	code.emitPushInt(7);
	code.emitAdd();
	code.emitAdd();
	code.emitReturn();
    }
    
    public static void main(String[] args) throws Exception {
	// "public class HelloWorld extends java.lang.Object".
	ClassType c = new ClassType("buisness/HelloWorld");
	c.setSuper("java.lang.Object");
	c.setModifiers(Access.PUBLIC);
	
	// "public static int exec()".
	Method m = c.addMethod("exec", "()I", Access.PUBLIC | Access.STATIC);
	CodeAttr code = m.startCode();
	code.pushScope();
	t5(code);
	code.popScope();
	
	c.writeToFile("buisness/HelloWorld.class");
    }
}
