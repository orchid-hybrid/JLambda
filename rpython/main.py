import os
import sys
from rpython.rlib.rarithmetic import r_uint32, intmask

# _______ ENtry Ppoint ________-


## portable endian handling thanks to http://commandcenter.blogspot.co.uk/2012/04/byte-order-fallacy.html
def from_endian_little(a,b,c,d):
    return (a<<0)|(b<<8)|(c<<16)|(d<<24)
def from_endian_big(a,b,c,d):
    return (a<<24)|(b<<16)|(c<<8)|(d<<0)

def program_words(b):
    s = int(len(b)/4)
    a = [r_uint32(0)] * s
    for i in xrange(0, s): #xrange?
        a[i] = r_uint32(from_endian_little(ord(b[4*i+0]),
                                           ord(b[4*i+1]),
                                           ord(b[4*i+2]),
                                           ord(b[4*i+3])))
    return a

class Value:
    def __init__(self, value):
        self.value = value

class Closure(Value):
    def __init__(self, code, env):
        self.code = code
        self.env = env
    def __repr__(self):
        return "clo<" + str(self.code) + ">"

class Num(Value):
    def __init__(self, n):
        self.n = n
    def __repr__(self):
        return str(self.n)

def mainloop(program):
    pc = 0
    s = [] # return stack
    e = [] # argument stack
    while True:
        op = intmask(program[pc])
        if op == 0: # HALT
            return [s, e]
        elif op == 1: # PUSH
            s.append(Num(intmask(program[pc+1])))
            pc += 2
        elif op == 2: # ADD
            s.append(Num(s.pop().n + s.pop().n))
            pc += 1
        elif op == 3: # LOOKUP
            s.append(e[intmask(program[pc+1])])
            pc += 2
        elif op == 4: # ABS
            s.append(Closure(intmask(program[pc+1]), e))
            pc += 2
        elif op == 5: # RET
            v = s.pop() # pop return value
            k = s.pop() # pop continuation
            s.append(v) # push return value
            e = list(k.env) # set contination environment
            pc = k.code # enter continuation
        elif op == 6: # APP
            v = s.pop() # get the value being applied
            c = s.pop() # get the closure being applied
            s.append(Closure(pc+1, e)) # push a closure for the continuation
            e = list(c.env) # set the environment to the closure environment
            e.append(v) # append the argument to the environment
            pc = c.code # set the program counter inside the closure body
        else:
            assert False

## following two functions taken from pypy-tutorial
def run(fp):
    program_code = ""
    while True:
        read = os.read(fp, 4096)
        if len(read) == 0:
            break
        program_code += read
    os.close(fp)
    #print( ":".join(['%0X' % ord(b) for b in program_code]))
    program = program_words(program_code)
    print(mainloop(program))

def entry_point(argv):
    try:
        filename = argv[1]
    except IndexError:
        print "You must supply a filename"
        return 1
    run(os.open(filename, os.O_RDONLY, 0777))
    return 0

# ____ Defune and set up rtarget ___

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    entry_point(sys.argv)
