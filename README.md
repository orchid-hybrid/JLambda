# JLambda

We use gnu.bytecode from the kawa scheme compiler for this. http://www.gnu.org/software/kawa/gnu.bytecode/

Run this command to download it then `ant` to compile it. This gives you kawa.jar which includes gnu.bytecode. It's needed when assembling .class files but the files we create are freestanding.

```
svn -q checkout svn://sourceware.org/svn/kawa/trunk kawa
```
