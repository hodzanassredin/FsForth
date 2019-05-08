module Forth 

open System
open System.Text
open Hardware

module ForthVM =
(*      A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth
This is PUBLIC DOMAIN (see public domain release statement below).
$Id: jonesforth.S,v 1.47 2009-09-11 08:33:13 rich Exp $

*)
    let JONES_VERSION = 47

    open Memory

    type FnPointer = Int32
    type PredefinedWords = {
        NEXT : FnPointer
        DOCOL : FnPointer //NEST
        EXIT : FnPointer//UNNEST
    }
    type Fn = ForthVM -> FnPointer
(*
INTRODUCTION ----------------------------------------------------------------------

FORTH is one of those alien languages which most working programmers regard in the same
way as Haskell, LISP, and so on.  Something so strange that they'd rather any thoughts
of it just go away so they can get on with writing this paying code.  But that's wrong
and if you care at all about programming then you should at least understand all these
languages, even if you will never use them.

LISP is the ultimate high-level language, and features from LISP are being added every
decade to the more common languages.  But FORTH is in some ways the ultimate in low level
programming.  Out of the box it lacks features like dynamic memory management and even
strings.  In fact, at its primitive level it lacks even basic concepts like IF-statements
and loops.

Why then would you want to learn FORTH?  There are several very good reasons.  First
and foremost, FORTH is minimal.  You really can write a complete FORTH in, say, 2000
lines of code.  I don't just mean a FORTH program, I mean a complete FORTH operating
system, environment and language.  You could boot such a FORTH on a bare PC and it would
come up with a prompt where you could start doing useful work.  The FORTH you have here
isn't minimal and uses a Linux process as its 'base PC' (both for the purposes of making
it a good tutorial). It's possible to completely understand the system.  Who can say they
completely understand how Linux works, or gcc?

Secondly FORTH has a peculiar bootstrapping property.  By that I mean that after writing
a little bit of assembly to talk to the hardware and implement a few primitives, all the
rest of the language and compiler is written in FORTH itself.  Remember I said before
that FORTH lacked IF-statements and loops?  Well of course it doesn't really because
such a lanuage would be useless, but my point was rather that IF-statements and loops are
written in FORTH itself.

Now of course this is common in other languages as well, and in those languages we call
them 'libraries'.  For example in C, 'printf' is a library function written in C.  But
in FORTH this goes way beyond mere libraries.  Can you imagine writing C's 'if' in C?
And that brings me to my third reason: If you can write 'if' in FORTH, then why restrict
yourself to the usual if/while/for/switch constructs?  You want a construct that iterates
over every other element in a list of numbers?  You can add it to the language.  What
about an operator which pulls in variables directly from a configuration file and makes
them available as FORTH variables?  Or how about adding Makefile-like dependencies to
the language?  No problem in FORTH.  How about modifying the FORTH compiler to allow
complex inlining strategies -- simple.  This concept isn't common in programming languages,
but it has a name (in fact two names): "macros" (by which I mean LISP-style macros, not
the lame C preprocessor) and "domain specific languages" (DSLs).

This tutorial isn't about learning FORTH as the language.  I'll point you to some references
you should read if you're not familiar with using FORTH.  This tutorial is about how to
write FORTH.  In fact, until you understand how FORTH is written, you'll have only a very
superficial understanding of how to use it.

So if you're not familiar with FORTH or want to refresh your memory here are some online
references to read:

http://en.wikipedia.org/wiki/Forth_%28programming_language%29

http://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm

http://wiki.laptop.org/go/Forth_Lessons

http://www.albany.net/~hello/simple.htm

Here is another "Why FORTH?" essay: http://www.jwdt.com/~paysan/why-forth.html

Discussion and criticism of this FORTH here: http://lambda-the-ultimate.org/node/2452

ACKNOWLEDGEMENTS ----------------------------------------------------------------------

This code draws heavily on the design of LINA FORTH (http://home.hccnet.nl/a.w.m.van.der.horst/lina.html)
by Albert van der Horst.  Any similarities in the code are probably not accidental.

Some parts of this FORTH are also based on this IOCCC entry from 1992:
http://ftp.funet.fi/pub/doc/IOCCC/1992/buzzard.2.design.
I was very proud when Sean Barrett, the original author of the IOCCC entry, commented in the LtU thread
http://lambda-the-ultimate.org/node/2452#comment-36818 about this FORTH.

And finally I'd like to acknowledge the (possibly forgotten?) authors of ARTIC FORTH because their
original program which I still have on original cassette tape kept nagging away at me all these years.
http://en.wikipedia.org/wiki/Artic_Software

PUBLIC DOMAIN ----------------------------------------------------------------------

I, the copyright holder of this work, hereby release it into the public domain. This applies worldwide.

In case this is not legally possible, I grant any entity the right to use this work for any purpose,
without any conditions, unless such conditions are required by law.

SETTING UP ----------------------------------------------------------------------

Let's get a few housekeeping things out of the way.  Firstly because I need to draw lots of
ASCII-art diagrams to explain concepts, the best way to look at this is using a window which
uses a fixed width font and is at least this wide:

 <------------------------------------------------------------------------------------------------------------------------>

Secondly make sure TABS are set to 8 characters.  The following should be a vertical
line.  If not, sort out your tabs.

        |
        |
        |

Thirdly I assume that your screen is at least 50 characters high.

ASSEMBLING ----------------------------------------------------------------------

If you want to actually run this FORTH, rather than just read it, you will need Linux on an
i386.  Linux because instead of programming directly to the hardware on a bare PC which I
could have done, I went for a simpler tutorial by assuming that the 'hardware' is a Linux
process with a few basic system calls (read, write and exit and that's about all).  i386
is needed because I had to write the assembly for a processor, and i386 is by far the most
common.  (Of course when I say 'i386', any 32- or 64-bit x86 processor will do.  I'm compiling
this on a 64 bit AMD Opteron).

Again, to assemble this you will need gcc and gas (the GNU assembler).  The commands to
assemble and run the code (save this file as 'jonesforth.S') are:

gcc -m32 -nostdlib -static -Wl,-Ttext,0 -Wl,--build-id=none -o jonesforth jonesforth.S
cat jonesforth.f - | ./jonesforth

If you want to run your own FORTH programs you can do:

cat jonesforth.f myprog.f | ./jonesforth

If you want to load your own FORTH code and then continue reading user commands, you can do:

cat jonesforth.f myfunctions.f - | ./jonesforth

ASSEMBLER ----------------------------------------------------------------------

(You can just skip to the next section -- you don't need to be able to read assembler to
follow this tutorial).

However if you do want to read the assembly code here are a few notes about gas (the GNU assembler):

(1) Register names are prefixed with '%', so %eax is the 32 bit i386 accumulator.  The registers
    available on i386 are: %eax, %ebx, %ecx, %edx, %esi, %edi, %ebp and %esp, and most of them
    have special purposes.

(2) Add, mov, etc. take arguments in the form SRC,DEST.  So mov %eax,%ecx moves %eax -> %ecx

(3) Constants are prefixed with '$', and you mustn't forget it!  If you forget it then it
    causes a read from memory instead, so:
    mov $2,%eax         moves number 2 into %eax
    mov 2,%eax          reads the 32 bit word from address 2 into %eax (ie. most likely a mistake)

(4) gas has a funky syntax for local labels, where '1f' (etc.) means label '1:' "forwards"
    and '1b' (etc.) means label '1:' "backwards".  Notice that these labels might be mistaken
    for hex numbers (eg. you might confuse 1b with $0x1b).

(5) 'ja' is "jump if above", 'jb' for "jump if below", 'je' "jump if equal" etc.

(6) gas has a reasonably nice .macro syntax, and I use them a lot to make the code shorter and
    less repetitive.

For more help reading the assembler, do "info gas" at the Linux prompt.

Now the tutorial starts in earnest.

THE DICTIONARY ----------------------------------------------------------------------

In FORTH as you will know, functions are called "words", and just as in other languages they
have a name and a definition.  Here are two FORTH words:

: DOUBLE DUP + ;                \ name is "DOUBLE", definition is "DUP +"
: QUADRUPLE DOUBLE DOUBLE ;     \ name is "QUADRUPLE", definition is "DOUBLE DOUBLE"

Words, both built-in ones and ones which the programmer defines later, are stored in a dictionary
which is just a linked list of dictionary entries.

<--- DICTIONARY ENTRY (HEADER) ----------------------->
+------------------------+--------+---------- - - - - +----------- - - - -
| LINK POINTER           | LENGTH/| NAME              | DEFINITION
|                        | FLAGS  |                   |
+--- (4 bytes) ----------+- byte -+- n bytes  - - - - +----------- - - - -

I'll come to the definition of the word later.  For now just look at the header.  The first
4 bytes are the link pointer.  This points back to the previous word in the dictionary, or, for
the first word in the dictionary it is just a NULL pointer.  Then comes a length/flags byte.
The length of the word can be up to 31 characters (5 bits used) and the top three bits are used
for various flags which I'll come to later.  This is followed by the name itself, and in this
implementation the name is rounded up to a multiple of 4 bytes by padding it with zero bytes.
That's just to ensure that the definition starts on a 32 bit boundary.

A FORTH variable called LATEST contains a pointer to the most recently defined word, in
other words, the head of this linked list.

DOUBLE and QUADRUPLE might look like this:

  pointer to previous word
   ^
   |
+--|------+---+---+---+---+---+---+---+---+------------- - - - -
| LINK    | 6 | D | O | U | B | L | E | 0 | (definition ...)
+---------+---+---+---+---+---+---+---+---+------------- - - - -
   ^       len                         padding
   |
+--|------+---+---+---+---+---+---+---+---+---+---+---+---+------------- - - - -
| LINK    | 9 | Q | U | A | D | R | U | P | L | E | 0 | 0 | (definition ...)
+---------+---+---+---+---+---+---+---+---+---+---+---+---+------------- - - - -
   ^       len                                     padding
   |
   |
  LATEST

You should be able to see from this how you might implement functions to find a word in
the dictionary (just walk along the dictionary entries starting at LATEST and matching
the names until you either find a match or hit the NULL pointer at the end of the dictionary);
and add a word to the dictionary (create a new definition, set its LINK to LATEST, and set
LATEST to point to the new word).  We'll see precisely these functions implemented in
assembly code later on.

One interesting consequence of using a linked list is that you can redefine words, and
a newer definition of a word overrides an older one.  This is an important concept in
FORTH because it means that any word (even "built-in" or "standard" words) can be
overridden with a new definition, either to enhance it, to make it faster or even to
disable it.  However because of the way that FORTH words get compiled, which you'll
understand below, words defined using the old definition of a word continue to use
the old definition.  Only words defined after the new definition use the new definition.

DIRECT THREADED CODE ----------------------------------------------------------------------

Now we'll get to the really crucial bit in understanding FORTH, so go and get a cup of tea
or coffee and settle down.  It's fair to say that if you don't understand this section, then you
won't "get" how FORTH works, and that would be a failure on my part for not explaining it well.
So if after reading this section a few times you don't understand it, please email me
(rich@annexia.org).

Let's talk first about what "threaded code" means.  Imagine a peculiar version of C where
you are only allowed to call functions without arguments.  (Don't worry for now that such a
language would be completely useless!)  So in our peculiar C, code would look like this:

f ()
{
  a ();
  b ();
  c ();
}

and so on.  How would a function, say 'f' above, be compiled by a standard C compiler?
Probably into assembly code like this.  On the right hand side I've written the actual
i386 machine code.

f:
  CALL a                        E8 08 00 00 00
  CALL b                        E8 1C 00 00 00
  CALL c                        E8 2C 00 00 00
  ; ignore the return from the function for now

"E8" is the x86 machine code to "CALL" a function.  In the first 20 years of computing
memory was hideously expensive and we might have worried about the wasted space being used
by the repeated "E8" bytes.  We can save 20% in code size (and therefore, in expensive memory)
by compressing this into just:

08 00 00 00             Just the function addresses, without
1C 00 00 00             the CALL prefix.
2C 00 00 00

On a 16-bit machine like the ones which originally ran FORTH the savings are even greater - 33%.

[Historical note: If the execution model that FORTH uses looks strange from the following
paragraphs, then it was motivated entirely by the need to save memory on early computers.
This code compression isn't so important now when our machines have more memory in their L1
caches than those early computers had in total, but the execution model still has some
useful properties].

Of course this code won't run directly on the CPU any more.  Instead we need to write an
interpreter which takes each set of bytes and calls it.

On an i386 machine it turns out that we can write this interpreter rather easily, in just
two assembly instructions which turn into just 3 bytes of machine code.  Let's store the
pointer to the next word to execute in the %esi register:

        08 00 00 00     <- We're executing this one now.  %esi is the _next_ one to execute.
%esi -> 1C 00 00 00
        2C 00 00 00

The all-important i386 instruction is called LODSL (or in Intel manuals, LODSW).  It does
two things.  Firstly it reads the memory at %esi into the accumulator (%eax).  Secondly it
increments %esi by 4 bytes.  So after LODSL, the situation now looks like this:

        08 00 00 00     <- We're still executing this one
        1C 00 00 00     <- %eax now contains this address (0x0000001C)
%esi -> 2C 00 00 00

Now we just need to jump to the address in %eax.  This is again just a single x86 instruction
written JMP *(%eax).  And after doing the jump, the situation looks like:

        08 00 00 00
        1C 00 00 00     <- Now we're executing this subroutine.
%esi -> 2C 00 00 00

To make this work, each subroutine is followed by the two instructions 'LODSL; JMP *(%eax)'
which literally make the jump to the next subroutine.

And that brings us to our first piece of actual code!  Well, it's a macro.
*)

    and CodeMemory()=
        //native funcs addressable storage
        let nativeFuncs : Fn[] = Array.zeroCreate (1<<<8) 
        let mutable nextFnPointer = 0
        let addFn f = 
            nativeFuncs.[nextFnPointer] <- f
            nextFnPointer <- nextFnPointer + 1
            nextFnPointer - 1
    
        let nextF (vm:ForthVM) = 
            vm.W <- getInt vm.memory vm.IP//current codeword
            vm.IP <- vm.IP + baseSize
            getInt vm.memory vm.W//native fn address
        let next = addFn nextF
(*      The macro is called NEXT.  That's a FORTH-ism.  It expands to those two instructions.

        Every FORTH primitive that we write has to be ended by NEXT.  Think of it kind of like
        a return.

        The above describes what is known as direct threaded code.

        To sum up: We compress our function calls down to a list of addresses and use a somewhat
        magical macro to act as a "jump to next function in the list".  We also use one register (%esi)
        to act as a kind of instruction pointer, pointing to the next function in the list.

        I'll just give you a hint of what is to come by saying that a FORTH definition such as:

        : QUADRUPLE DOUBLE DOUBLE ;

        actually compiles (almost, not precisely but we'll see why in a moment) to a list of
        function addresses for DOUBLE, DOUBLE and a special function called EXIT to finish off.

        At this point, REALLY EAGLE-EYED ASSEMBLY EXPERTS are saying "JONES, YOU'VE MADE A MISTAKE!".

        I lied about JMP *(%eax).  

        INDIRECT THREADED CODE ----------------------------------------------------------------------

        It turns out that direct threaded code is interesting but only if you want to just execute
        a list of functions written in assembly language.  So QUADRUPLE would work only if DOUBLE
        was an assembly language function.  In the direct threaded code, QUADRUPLE would look like:

                +------------------+
                | addr of DOUBLE  --------------------> (assembly code to do the double)
                +------------------+                    NEXT
        %esi -> | addr of DOUBLE   |
                +------------------+

        We can add an extra indirection to allow us to run both words written in assembly language
        (primitives written for speed) and words written in FORTH themselves as lists of addresses.

        The extra indirection is the reason for the brackets in JMP *(%eax).

        Let's have a look at how QUADRUPLE and DOUBLE really look in FORTH:

                : QUADRUPLE DOUBLE DOUBLE ;

                +------------------+
                | codeword         |               : DOUBLE DUP + ;
                +------------------+
                | addr of DOUBLE  ---------------> +------------------+
                +------------------+               | codeword         |
                | addr of DOUBLE   |               +------------------+
                +------------------+               | addr of DUP   --------------> +------------------+
                | addr of EXIT     |               +------------------+            | codeword      -------+
                +------------------+       %esi -> | addr of +     --------+       +------------------+   |
                                                   +------------------+    |       | assembly to    <-----+
                                                   | addr of EXIT     |    |       | implement DUP    |
                                                   +------------------+    |       |    ..            |
                                                                           |       |    ..            |
                                                                           |       | NEXT             |
                                                                           |       +------------------+
                                                                           |
                                                                           +-----> +------------------+
                                                                                   | codeword      -------+
                                                                                   +------------------+   |
                                                                                   | assembly to   <------+
                                                                                   | implement +      |
                                                                                   |    ..            |
                                                                                   |    ..            |
                                                                                   | NEXT             |
                                                                                   +------------------+

        This is the part where you may need an extra cup of tea/coffee/favourite caffeinated
        beverage.  What has changed is that I've added an extra pointer to the beginning of
        the definitions.  In FORTH this is sometimes called the "codeword".  The codeword is
        a pointer to the interpreter to run the function.  For primitives written in
        assembly language, the "interpreter" just points to the actual assembly code itself.
        They don't need interpreting, they just run.

        In words written in FORTH (like QUADRUPLE and DOUBLE), the codeword points to an interpreter
        function.

        I'll show you the interpreter function shortly, but let's recall our indirect
        JMP *(%eax) with the "extra" brackets.  Take the case where we're executing DOUBLE
        as shown, and DUP has been called.  Note that %esi is pointing to the address of +

        The assembly code for DUP eventually does a NEXT.  That:

        (1) reads the address of + into %eax            %eax points to the codeword of +
        (2) increments %esi by 4
        (3) jumps to the indirect %eax                  jumps to the address in the codeword of +,
                                                        ie. the assembly code to implement +

                +------------------+
                | codeword         |
                +------------------+
                | addr of DOUBLE  ---------------> +------------------+
                +------------------+               | codeword         |
                | addr of DOUBLE   |               +------------------+
                +------------------+               | addr of DUP   --------------> +------------------+
                | addr of EXIT     |               +------------------+            | codeword      -------+
                +------------------+               | addr of +     --------+       +------------------+   |
                                                   +------------------+    |       | assembly to    <-----+
                                           %esi -> | addr of EXIT     |    |       | implement DUP    |
                                                   +------------------+    |       |    ..            |
                                                                           |       |    ..            |
                                                                           |       | NEXT             |
                                                                           |       +------------------+
                                                                           |
                                                                           +-----> +------------------+
                                                                                   | codeword      -------+
                                                                                   +------------------+   |
                                                                        now we're  | assembly to    <-----+
                                                                        executing  | implement +      |
                                                                        this       |    ..            |
                                                                        function   |    ..            |
                                                                                   | NEXT             |
                                                                                   +------------------+

        So I hope that I've convinced you that NEXT does roughly what you'd expect.  This is
        indirect threaded code.

        I've glossed over four things.  I wonder if you can guess without reading on what they are?

        .
        .
        .

        My list of four things are: (1) What does "EXIT" do?  (2) which is related to (1) is how do
        you call into a function, ie. how does %esi start off pointing at part of QUADRUPLE, but
        then point at part of DOUBLE.  (3) What goes in the codeword for the words which are written
        in FORTH?  (4) How do you compile a function which does anything except call other functions
        ie. a function which contains a number like : DOUBLE 2 * ; ?

        THE INTERPRETER AND RETURN STACK ------------------------------------------------------------

        Going at these in no particular order, let's talk about issues (3) and (2), the interpreter
        and the return stack.

        Words which are defined in FORTH need a codeword which points to a little bit of code to
        give them a "helping hand" in life.  They don't need much, but they do need what is known
        as an "interpreter", although it doesn't really "interpret" in the same way that, say,
        Java bytecode used to be interpreted (ie. slowly).  This interpreter just sets up a few
        machine registers so that the word can then execute at full speed using the indirect
        threaded model above.

        One of the things that needs to happen when QUADRUPLE calls DOUBLE is that we save the old
        %esi ("instruction pointer") and create a new one pointing to the first word in DOUBLE.
        Because we will need to restore the old %esi at the end of DOUBLE (this is, after all, like
        a function call), we will need a stack to store these "return addresses" (old values of %esi).

        As you will have seen in the background documentation, FORTH has two stacks, an ordinary
        stack for parameters, and a return stack which is a bit more mysterious.  But our return
        stack is just the stack I talked about in the previous paragraph, used to save %esi when
        calling from a FORTH word into another FORTH word.

        In this FORTH, we are using the normal stack pointer (%esp) for the parameter stack.
        We will use the i386's "other" stack pointer (%ebp, usually called the "frame pointer")
        for our return stack.

        I've got two macros which just wrap up the details of using %ebp for the return stack.
        You use them as for example "PUSHRSP %eax" (push %eax on the return stack) or "POPRSP %ebx"
        (pop top of return stack into %ebx).

        And with that we can now talk about the interpreter.

        In FORTH the interpreter function is often called DOCOL (I think it means "DO COLON" because
        all FORTH definitions start with a colon, as in : DOUBLE DUP + ;

        The "interpreter" (it's not really "interpreting") just needs to push the old %esi on the
        stack and set %esi to the first word in the definition.  Remember that we jumped to the
        function using JMP *(%eax)?  Well a consequence of that is that conveniently %eax contains
        the address of this codeword, so just by adding 4 to it we get the address of the first
        data word.  Finally after setting up %esi, it just does NEXT which causes that first word
        to run.
*)

(* DOCOL - the interpreter! *)
        //alternative name: enter
        let docol (vm:ForthVM) = 
            vm.RSP.push vm.IP
            vm.IP <- vm.W
            vm.IP <- vm.IP + baseSize
            next
        let docol = addFn docol
(*
Just to make this absolutely clear, let's see how DOCOL works when jumping from QUADRUPLE
into DOUBLE:

        QUADRUPLE:
        +------------------+
        | codeword         |
        +------------------+               DOUBLE:
        | addr of DOUBLE  ---------------> +------------------+
        +------------------+       %eax -> | addr of DOCOL    |
%esi -> | addr of DOUBLE   |               +------------------+
        +------------------+               | addr of DUP      |
        | addr of EXIT     |               +------------------+
        +------------------+               | etc.             |

First, the call to DOUBLE calls DOCOL (the codeword of DOUBLE).  DOCOL does this:  It
pushes the old %esi on the return stack.  %eax points to the codeword of DOUBLE, so we
just add 4 on to it to get our new %esi:

        QUADRUPLE:
        +------------------+
        | codeword         |
        +------------------+               DOUBLE:
        | addr of DOUBLE  ---------------> +------------------+
top of return   +------------------+       %eax -> | addr of DOCOL    |
stack points -> | addr of DOUBLE   |       + 4 =   +------------------+
        +------------------+       %esi -> | addr of DUP      |
        | addr of EXIT     |               +------------------+
        +------------------+               | etc.             |

Then we do NEXT, and because of the magic of threaded code that increments %esi again
and calls DUP.

Well, it seems to work.

One minor point here.  Because DOCOL is the first bit of assembly actually to be defined
in this file (the others were just macros), and because I usually compile this code with the
text segment starting at address 0, DOCOL has address 0.  So if you are disassembling the
code and see a word with a codeword of 0, you will immediately know that the word is
written in FORTH (it's not an assembler primitive) and so uses DOCOL as the interpreter.

STARTING UP ----------------------------------------------------------------------

Now let's get down to nuts and bolts.  When we start the program we need to set up
a few things like the return stack.  But as soon as we can, we want to jump into FORTH
code (albeit much of the "early" FORTH code will still need to be written as
assembly language primitives).

This is what the set up code does.  Does a tiny bit of house-keeping, sets up the
separate return stack (NB: Linux gives us the ordinary parameter stack already), then
immediately jumps to a FORTH word called QUIT.  Despite its name, QUIT doesn't quit
anything.  It resets some internal state and starts reading and interpreting commands.
(The reason it is called QUIT is because you can call QUIT from your own FORTH code
to "quit" your program and go back to interpreting).
*)
        //alternative names: exit
        let exit (vm:ForthVM) =
            vm.IP <- vm.RSP.pop() 
            next
    
        let exit = addFn exit

        let constFn (vm:ForthVM) =
            let value = getInt vm.memory (vm.W + baseSize)
            vm.SP.push value 
            next

        member x.CONST = addFn constFn
    
        member x.DirectPredefinedWords = {
            NEXT = next
            DOCOL = docol
            EXIT = exit
        }

        member x.addFunction = addFn
    
        member x.get idx = 
            if next = idx then nextF else nativeFuncs.[idx]

    and ForthVM = {
        memory : Memory
        allocated : Cursor
        data : Cursor
        SP : Stack
        RSP : Stack
        input_buffer : InputBuffer
        out_buffer : OutputBuffer
        word_buffer : StringBuffer
        STATE : Variable
        LATEST : Variable
        HERE : Variable
        BASE : Variable
        errmsg : Span
        errmsgnl : Span
        mutable IP : int //istruction pointer in bytes esi
        mutable W : int //work eax

        //indirect fn calls
        NEXT : Variable
        DOCOL : Variable
        EXIT : Variable
        QUIT : Variable//cold start
    }

    let create size config  = 
        let memory = createMemory size
        let allocated = createCursor 0 size 
        let allocate bytes init = 
            let address = inc allocated bytes
            let c = createCursor address bytes
            init (memory,c)

        let var init (memory, cursor) = Variable(memory, cursor, init)

        let reserveString (str:string) = 
            let arr = Encoding.ASCII.GetBytes str
            let init (m,c) = copyFromBytes m c.span.address arr
                             c.span
            allocate arr.Length init
        {
            memory = memory
            allocated = allocated
            data = allocate config.INITIAL_DATA_SEGMENT_SIZE snd
            SP = allocate config.DATA_STACK_SIZE Stack
            RSP = allocate config.RETURN_STACK_SIZE Stack
            input_buffer = allocate config.BUFFER_SIZE InputBuffer
            out_buffer= allocate config.BUFFER_SIZE OutputBuffer
            word_buffer = allocate 32 StringBuffer
            STATE = allocate baseSize  <| var 0
            LATEST = allocate baseSize  <| var 0
            HERE = allocate baseSize  <| var 1//0 is not defined word
            BASE = allocate baseSize  <| var 10
            errmsg = reserveString "PARSE ERROR: "
            errmsgnl = reserveString Environment.NewLine
            IP = 0
            W = 0
            NEXT = allocate baseSize  <| var 0
            DOCOL = allocate baseSize  <| var 0
            EXIT = allocate baseSize  <| var 0
            QUIT = allocate baseSize  <| var 0
        }
(*
BUILT-IN WORDS ----------------------------------------------------------------------

Remember our dictionary entries (headers)?  Let's bring those together with the codeword
and data words to see how : DOUBLE DUP + ; really looks in memory.

  pointer to previous word
   ^
   |
+--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
+---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
   ^       len                         pad  codeword      |
   |                                                      V
  LINK in next word                             points to codeword of DUP

Initially we can't just write ": DOUBLE DUP + ;" (ie. that literal string) here because we
don't yet have anything to read the string, break it up at spaces, parse each word, etc. etc.
So instead we will have to define built-in words using the GNU assembler data constructors
(like .int, .byte, .string, .ascii and so on -- look them up in the gas info page if you are
unsure of them).

The long way would be:

.int <link to previous word>
.byte 6                 // len
.ascii "DOUBLE"         // string
.byte 0                 // padding
DOUBLE: .int DOCOL              // codeword
.int DUP                // pointer to codeword of DUP
.int PLUS               // pointer to codeword of +
.int EXIT               // pointer to codeword of EXIT

That's going to get quite tedious rather quickly, so here I define an assembler macro
so that I can just write:

defword "DOUBLE",6,,DOUBLE
.int DUP,PLUS,EXIT

and I'll get exactly the same effect.

Don't worry too much about the exact implementation details of this macro - it's complicated!
*)
module Words = 
    open ForthVM
    [<Flags>]
    type Flags =
        | NONE = 0
        | IMMEDIATE = 0x80
        | HIDDEN = 0x20

    let LENMASK = 0x1f
    type ReadMode = SKIP|WORD|COMMENT//word parser modes

    type Writer(vm:ForthVM, code:CodeMemory) =
        let writeByte (v:byte) = 
            let address = vm.HERE.value
            vm.memory.[address] <- v
            vm.HERE.value <- address + 1

        let write (v:Int32) = 
            let address = vm.HERE.value
            Memory.setInt vm.memory address v
            vm.HERE.value <- address + Memory.baseSize
        
        let writeString (str:String) = Encoding.ASCII.GetBytes str |> Array.iter writeByte 

        let align () = vm.HERE.value <- Memory.pad vm.HERE.value  
        
        member x.setIndirectPredefinedWords () =
            vm.NEXT.value <- code.DirectPredefinedWords.NEXT
            vm.DOCOL.value <- code.DirectPredefinedWords.DOCOL
            vm.EXIT.value <- code.DirectPredefinedWords.EXIT
            {
                NEXT = vm.NEXT.address
                DOCOL = vm.DOCOL.address
                EXIT = vm.EXIT.address
            }
        member x.setQUIT quit = vm.QUIT.value <- quit
        member x.create (name: string) (flags:Flags) =
            let nameSize = byte name.Length
            assert (nameSize <= 31uy)
            let flags = byte flags

            let curlink = vm.LATEST.value
            vm.LATEST.value <- vm.HERE.value//set link
            write(curlink)
            nameSize + flags |> writeByte
            writeString name
            align()

        member x.writeCodewordPayloadRetCodeword (name: string) (flags:Flags) (codeword:FnPointer) (payload:Int32[]) = 
            x.create name flags 
            let codewordAddr = vm.HERE.value
            write codeword
            for d in payload do
                write d 
            codewordAddr

        member x.defwordRetCodeword (name: string) (flags:Flags) (program:Int32[]) = 
            x.writeCodewordPayloadRetCodeword name flags code.DirectPredefinedWords.DOCOL program

        member x.defword (name: string) (flags:Flags) (program:Int32[]) = 
            x.defwordRetCodeword name flags program |>ignore

        //add native word
        member x.defcodeRetCodeword (name: string) (flags:Flags) (f:Fn) = 
            let fAddr = code.addFunction f
            x.writeCodewordPayloadRetCodeword name flags fAddr Array.empty

        member x.defcode (name: string) (flags:Flags) (f:Fn) = x.defcodeRetCodeword name flags f |> ignore

        member x.defconstRetCodeword (name: string) (flags:Flags) (value:ForthVM ->Int32) = 
            let value = value vm
            //x.writeCodewordPayloadRetCodeword name flags code.CONST [|value|]
            x.defcodeRetCodeword name flags (fun vm -> 
                vm.SP.push value
                code.DirectPredefinedWords.NEXT
            )

        member x.defconst (name: string) (flags:Flags) (value:ForthVM ->Int32) = 
            x.defconstRetCodeword name flags value |> ignore
        //add variable
        member x.defvarRetCodeword (name: string) (flags:Flags) (varAddress:ForthVM -> Int32) (initial:Int32 option) = 
            let varAddress = varAddress vm
            match initial with  
                | Some(initial) -> Memory.setInt vm.memory varAddress initial
                | _ -> ()
            x.defconstRetCodeword name flags (fun vm -> varAddress)
        member x.defvar (name: string) (flags:Flags) (varAddress:ForthVM -> Int32) (initial:Int32 option) = x.defvarRetCodeword name flags varAddress initial |> ignore
        
        
    let init (x: Writer) (words:PredefinedWords) = 
        let codewords = x.setIndirectPredefinedWords ()

        x.defcode "EXIT" Flags.NONE (fun vm -> 
            words.EXIT
        ) 

        x.defcode "DROP" Flags.NONE (fun vm -> 
            vm.SP.pop() |> ignore
            words.NEXT
        ) 

        x.defcode "SWAP" Flags.NONE (fun vm -> 
            let x = vm.SP.pop()
            let y = vm.SP.pop()
            vm.SP.push(x)
            vm.SP.push(y)
            words.NEXT
        ) 
        x.defcode "DUP" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 0)// duplicate top of stack
            words.NEXT
        ) 
        x.defcode "OVER" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 1)//get the second element of stack and push it on top
            words.NEXT
        ) 
        //( a b c -- b c a )
        x.defcode "ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push ebx
            vm.SP.push eax
            vm.SP.push ecx
            words.NEXT 
        ) 
        //( a b c -- c a b ) rot rot 
        x.defcode "-ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push eax
            vm.SP.push ecx
            vm.SP.push ebx
            words.NEXT 
        ) 
        //( a b -- ) drop drop ;
        x.defcode "2DROP" Flags.NONE (fun vm ->  // drop top two elements of stack
            vm.SP.pop () |> ignore
            vm.SP.pop () |> ignore
            words.NEXT 
        ) 
        //( a b -- a b a b ) over over ;
        x.defcode "2DUP" Flags.NONE (fun vm ->  // duplicate top two elements of stack
            let b = vm.SP.peek 0
            let a = vm.SP.peek 1
            vm.SP.push a
            vm.SP.push b
            words.NEXT 
        ) 
        //( d1 d2 — d2 d1 )
        x.defcode "2SWAP" Flags.NONE (fun vm ->  // swap top two pairs of elements of stack
            let eax = vm.SP.pop ()
            let ebx = vm.SP.pop ()
            let ecx = vm.SP.pop ()
            let edx = vm.SP.pop ()
            vm.SP.push ebx
            vm.SP.push eax
            vm.SP.push edx
            vm.SP.push ecx
            words.NEXT 
        ) 

        let apply (vm:ForthVM) = vm.SP.apply
        let apply2 (vm:ForthVM) (f: Int32->Int32->Int32)  = vm.SP.apply (f <| vm.SP.pop())
        let boolToBase b = if b then 1 else 0
    
        let applyBool vm f  = apply vm (fun a -> f a |> boolToBase) 
        let applyBool2 vm f  = apply2 vm (fun a b -> f a b |> boolToBase)

        let def applier name f= x.defcode name Flags.NONE (fun vm -> 
            applier vm f 
            words.NEXT  
        ) 

        //( a -- a a | 0 ) dup if dup then 
        //Duplicate x if it is non-zero. 
        x.defcode "?DUP" Flags.NONE (fun vm -> // duplicate top of stack if non-zero
            let eax = vm.SP.peek 0
            if eax <> 0 then vm.SP.push eax
            words.NEXT 
        ) 

        let flip f a b = f b a 

        def apply "1+" <| (+) 1
        def apply "1-" <| flip (-) 1
        def apply "4+" <| (+) 4
        def apply "4-" <| flip (-) 4
        def apply2 "+" <| (+)
        def apply2 "-" <| flip (-)
        def apply2 "*" <| (*)
    
        //( n1 n2 -- n3 n4 ) Divide n1 by n2, giving the single-cell remainder n3 and the single-cell quotient n4
        x.defcode "/MOD" Flags.NONE (fun vm -> 
            let divisor = vm.SP.pop()
            let dividend = vm.SP.pop()
            let quotient, remainder = Math.DivRem(dividend, divisor)
            vm.SP.push remainder// push remainder
            vm.SP.push quotient// push quotient
            words.NEXT 
        ) 

        ////Lots of comparison operations like =, <, >, etc..
        ////ANS FORTH says that the comparison words should return all (binary) 1's for
        ////TRUE and all 0's for FALSE.  

        //n1 n2 – f  
        def applyBool2 "=" (=)
        def applyBool2 "<>" (<>)
        def applyBool2 "<" <| flip (<)
        def applyBool2 ">" <| flip (>)
        def applyBool2 "<=" <| flip (<=)
        def applyBool2 ">=" <| flip (>=)
        def applyBool2 ">" <| flip (>)
    
        def applyBool "0=" <| flip (=) 0
        def applyBool "0<>" <| flip (<>) 0
        def applyBool "0<" <| flip (<) 0
        def applyBool "0>" <| flip (>) 0
        def applyBool "0<=" <| flip (<=) 0
        def applyBool "0>=" <| flip (>=) 0
        //bitwise
        def apply2 "AND" (&&&) 
        def apply2 "OR" (|||) 
        def apply2 "XOR" (^^^) 
        def apply "INVERT" (~~~) 
    
        let LIT = x.defcodeRetCodeword "LIT" Flags.NONE (fun vm ->
            vm.W <- Memory.getInt vm.memory vm.IP//current codeword
            vm.IP <- vm.IP + Memory.baseSize
            //let l = Memory.getInt vm.memory vm.W
            vm.SP.push vm.W//todo fix
            words.NEXT
        )
        x.defcode "!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            Memory.setInt vm.memory address data
            words.NEXT 
        )
        let FETCH = x.defcodeRetCodeword "@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = Memory.getInt vm.memory address 
            vm.SP.push data
            words.NEXT 
        )
        x.defcode "+!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()
            Memory.setInt vm.memory address (Memory.getInt vm.memory address + amount)
            words.NEXT 
        )
        x.defcode "-!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()
            Memory.setInt vm.memory address (Memory.getInt vm.memory address - amount)
            words.NEXT 
        )

        //! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes
        //so we also define standard words C@ and C!.
        //Byte-oriented operations only work on architectures which permit them (i386 is one of those).

        x.defcode "C!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            vm.memory.[address] <- byte data
            words.NEXT 
        )
        x.defcode "C@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = int vm.memory.[address]
            vm.SP.push <| data
            words.NEXT 
        )

        // C@C! is a useful byte copy primitive. 
        x.defcode "C@C!" Flags.NONE (fun vm -> 
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            vm.memory.[destination] <- vm.memory.[source]
            vm.SP.push (source + 1)// increment source address
            vm.SP.push (destination + 1)// increment destination address
            words.NEXT
        )

        // and CMOVE is a block copy operation. 
        x.defcode "CMOVE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            for i in 0..length do
                vm.memory.[destination + i] <- vm.memory.[source + i]
            words.NEXT 
        )

        x.defvar "STATE" Flags.NONE (fun vm -> vm.STATE.address) Option.None
        x.defvar "HERE" Flags.NONE (fun vm -> vm.HERE.address) Option.None
        let LATEST = x.defvarRetCodeword "LATEST" Flags.NONE (fun vm -> vm.LATEST.address) Option.None //name_SYSCALL0 // SYSCALL0 must be last in built-in dictionary
        x.defvar "BASE" Flags.NONE (fun vm -> vm.BASE.address) Option.None
        
        x.defconst "S0" Flags.NONE (fun vm -> vm.SP.S0)
        x.defconst "VERSION" Flags.NONE (fun vm -> JONES_VERSION)
        let RZ = x.defconstRetCodeword "R0" Flags.NONE (fun vm -> vm.RSP.S0)
        x.defconst "DOCOL" Flags.NONE (fun vm -> words.DOCOL)//probaly words -> codewords
        x.defconst "F_IMMED" Flags.NONE (fun vm ->  int Flags.IMMEDIATE)
        x.defconst "F_HIDDEN" Flags.NONE (fun vm ->  int Flags.HIDDEN)
        x.defconst "F_LENMASK" Flags.NONE (fun vm -> LENMASK)

        //RETURN STACK ----------------------------------------------------------------------
        x.defcode ">R" Flags.NONE (fun vm -> 
            vm.SP.pop() |> vm.RSP.push
            words.NEXT 
        )
        x.defcode "R>" Flags.NONE (fun vm -> 
            vm.RSP.pop() |> vm.SP.push
            words.NEXT 
        )
        //get return stack pointer
        x.defcode "RSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.RSP.top
            words.NEXT 
        )
        //set return stack pointer
        let RSPSTORE = x.defcodeRetCodeword "RSP!" Flags.NONE (fun vm -> 
            vm.RSP.top <- vm.SP.pop()
            words.NEXT 
        )
        x.defcode "RDROP" Flags.NONE (fun vm -> 
            vm.RSP.pop()|> ignore
            words.NEXT 
        )
        //PARAMETER (DATA) STACK ----------------------------------------------------------------------
        //get data stack pointer
        x.defcode "DSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.SP.top
            words.NEXT 
        )
        //set data stack pointer
        x.defcode "DSP!" Flags.NONE (fun vm -> 
            vm.SP.top <- vm.SP.pop()
            words.NEXT 
        )

        //io
        x.defcode "KEY" Flags.NONE (fun vm -> 
            vm.input_buffer.get() |> int |> vm.SP.push
            words.NEXT 
        )
        x.defcode "EMIT" Flags.NONE (fun vm -> 
            vm.SP.pop() |> byte |> vm.out_buffer.set
            words.NEXT 
        )
         
        let rec readWord vm mode : Memory.Span =
            let c = vm.input_buffer.get() 
            match (char c,mode) with
                | ('\n', ReadMode.COMMENT) -> readWord vm ReadMode.SKIP
                | (_, ReadMode.COMMENT) -> readWord vm ReadMode.COMMENT
                | (c, ReadMode.SKIP) when c = ' ' || c = '\t' || c = '\r' || c = '\n' -> readWord vm ReadMode.SKIP 
                | (c, ReadMode.WORD) when c = ' ' || c = '\t' || c = '\r' || c = '\n' -> vm.word_buffer.getStrAndReset()
                | ('\\', ReadMode.SKIP) -> readWord vm ReadMode.COMMENT
                | (_, _) -> vm.word_buffer.write c
                            readWord vm ReadMode.WORD

        let _WORD vm = 
            let str = readWord vm ReadMode.SKIP
            str

        let WORD = x.defcodeRetCodeword "WORD" Flags.NONE (fun vm -> 
            let str = _WORD vm
            vm.SP.push str.address
            vm.SP.push str.size
            words.NEXT 
        )

        let _NUMBER vm (str:Memory.Span) = 
            let radix = vm.BASE.value
            let mutable n = 0
            let mutable idx = 0

            let current() = int vm.memory.[str.address + idx]

            let mutable c = current ()
            let isNegative = '-' = char c
            if isNegative then idx <- 1
            let mutable cnt = true
            while cnt && idx < str.size do
                n <- n * radix
                let d = current() - (int '0')
                if d > radix || d < 0 then cnt <- false
                else n <- n + d
                     idx <- idx + 1
            
            let count = if str.size = 0 then 0 else str.size - idx

            if isNegative then -n, count else n, count

        x.defcode "NUMBER" Flags.NONE (fun vm -> 
            let len, addr = vm.SP.pop(), vm.SP.pop()
            let number, numberOfUnparsedChars = _NUMBER vm {address = addr ; size = len}
            vm.SP.push number
            vm.SP.push numberOfUnparsedChars // 0 if no error
            words.NEXT 
        )

        let rec eqStrings (vm:ForthVM) address address2 length = 
            if length = 0 
            then true
            else if vm.memory.[address] = vm.memory.[address2]
                    then eqStrings vm (address+1) (address2+1) (length - 1)
                    else false

        let _FIND vm (str:Memory.Span) = 
            let mutable wordAddr = vm.LATEST.value
            let mutable cnt = true
            while cnt do
                if wordAddr = 0 
                then cnt <- false
                else let flagsLen = vm.memory.[wordAddr + Memory.baseSize] |> int
                     let flagsLen = (int Flags.HIDDEN ||| LENMASK) &&& flagsLen
                     if flagsLen = str.size && eqStrings vm str.address (wordAddr + Memory.baseSize + 1) str.size
                     then cnt <- false
                     else wordAddr <- Memory.getInt vm.memory wordAddr
            wordAddr
                     

        let FIND = x.defcodeRetCodeword "FIND" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let addrOfDictEntry = _FIND vm {address = address; size = length }
            vm.SP.push addrOfDictEntry
            words.NEXT 
        )


        let _TCFA (vm:ForthVM) linkAddress = 
            let flagsLenAddress = linkAddress + Memory.baseSize
            let length = vm.memory.[flagsLenAddress] |> int &&& LENMASK
            let cfaAddress = flagsLenAddress + 1 + length |> Memory.pad
            cfaAddress

        x.defcode ">CFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress
            vm.SP.push cfaAddress
            words.NEXT 
        )
        x.defcode ">DFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress
            vm.SP.push (cfaAddress + Memory.baseSize)
            words.NEXT 
        )
        let readString (vm:ForthVM) address length = 
            let arr = Array.create length ' '
            for i in 0..(length - 1) do
                arr.[i] <- char (vm.memory.[address + i])
            new String(arr)

        let CREATE = x.defcodeRetCodeword "CREATE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let name = readString vm address length
            x.create name Flags.NONE
            words.NEXT 
        )
        let _COMMA (vm:ForthVM) v =
            let address = vm.HERE.value
            Memory.setInt vm.memory address v
            vm.HERE.value <-address + Memory.baseSize

        let COMMA = x.defcodeRetCodeword "," Flags.NONE (fun vm -> 
            let v = vm.SP.pop()
            _COMMA vm v
            words.NEXT 
        )

        let LBRAC = x.defcodeRetCodeword "[" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.value <- 0
            words.NEXT 
        )
        let RBRAC = x.defcodeRetCodeword "]" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.value <- 1
            words.NEXT 
        )

        let toggleWordFlag wordAddress (flag:Flags) (vm:ForthVM)  = 
            let flagsAddress = wordAddress + Memory.baseSize
            let flagsLen = vm.memory.[flagsAddress]|> int
            let flagsLen = flagsLen ^^^ int flag //toggle flag
            vm.memory.[flagsAddress] <- byte flagsLen
            words.NEXT 

        let immediate (vm:ForthVM) = toggleWordFlag vm.LATEST.value Flags.IMMEDIATE vm

        let IMMEDIATE = x.defcodeRetCodeword "IMMEDIATE" Flags.IMMEDIATE immediate

        let hidden (vm:ForthVM) = 
            let addr = vm.SP.pop()
            toggleWordFlag addr Flags.HIDDEN vm

        let HIDDEN = x.defcodeRetCodeword "HIDDEN" Flags.IMMEDIATE hidden

        x.defword ":" Flags.NONE 
            [|
                WORD;// Get the name of the new word
                CREATE;// CREATE the dictionary entry / header
                LIT; words.DOCOL; COMMA;// Append DOCOL  (the codeword).
                LATEST; FETCH; HIDDEN // Make the word hidden (see below for definition).
                RBRAC;// Go into compile mode.
                codewords.EXIT// Return from the function.
            |]
        
        x.defword ";" Flags.IMMEDIATE 
            [|
                LIT; codewords.EXIT; COMMA;// Append EXIT (so the word will return).
                LATEST; FETCH; HIDDEN; // Toggle hidden flag -- unhide the word (see below for definition).
                LBRAC;// Go back to IMMEDIATE mode.
                codewords.EXIT;// Return from the function.
            |]
        x.defword "HIDE" Flags.NONE [|WORD;FIND;HIDDEN;codewords.EXIT|]

        let TICK = x.defcodeRetCodeword "'" Flags.NONE (fun vm ->
            vm.SP.push <| Memory.getInt vm.memory vm.IP//todo check
            vm.IP <- vm.IP + Memory.baseSize
            words.NEXT 
        )

        let BRANCH = x.defcodeRetCodeword "BRANCH" Flags.NONE (fun vm ->
            vm.IP <- vm.IP + Memory.getInt vm.memory vm.IP // add the offset to the instruction pointer
            words.NEXT 
        )
        x.defcode "0BRANCH" Flags.NONE (fun vm ->
            if vm.SP.pop () = 0 // top of stack is zero?
            then vm.IP <- vm.IP + Memory.getInt vm.memory vm.IP // add the offset to the instruction pointer
            else vm.IP <- vm.IP + Memory.baseSize// otherwise we need to skip the offset
            words.NEXT 
        )

        x.defcode "LITSTRING" Flags.NONE (fun vm ->
            let length = Memory.getInt vm.memory vm.IP
            vm.IP <- vm.IP + Memory.baseSize
            vm.SP.push vm.IP     // push the address of the start of the string
            vm.SP.push length    // push length on the stack
            vm.IP <- vm.IP + length |> Memory.pad  // skip past the string and round up to next BaseSize byte boundary
            words.NEXT 
        )
        x.defcode "TELL" Flags.NONE (fun vm ->
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            vm.out_buffer.setString {address = address; size = length}
            words.NEXT 
        )


        let INTERPRET = x.defcodeRetCodeword "INTERPRET" Flags.NONE (fun vm ->
            let exec cfa = 
                vm.W<-cfa
                Memory.getInt vm.memory cfa

            let word = _WORD vm // Returns %ecx = length, %edi = pointer to word.
            
            let pointer = _FIND vm word//pointer to header or 0 if not found.
            if pointer <> 0 //found word
            then
                let nameFlags = vm.memory.[pointer + Memory.baseSize] |> int
                let codeword = _TCFA vm pointer
                let isImmediate = (nameFlags &&& int Flags.IMMEDIATE) <> 0
                if isImmediate
                then exec codeword// If IMMED, jump straight to executing.
                else if vm.STATE.value = 0// Are we compiling or executing?
                        then exec codeword// Jump if executing.
                        else // Compiling - just append the word to the current dictionary definition.
                            //printfn "compiling %s" debug
                            _COMMA vm codeword
                            words.NEXT
            else // Not in the dictionary (not a word) so assume it's a literal number.
                    let number, numberOfUnparsedChars = _NUMBER vm word
                    if numberOfUnparsedChars > 0
                    then vm.out_buffer.setString vm.errmsg
                         vm.out_buffer.setString <| vm.input_buffer.GetLast 40
                         vm.out_buffer.setString vm.errmsgnl
                         words.NEXT
                    else if vm.STATE.value = 0// Are we compiling or executing?vm.SP.push number
                        then vm.SP.push number// Jump if executing.
                             words.NEXT
                        else // Compiling - just append the word to the current dictionary definition.
                            _COMMA vm LIT
                            _COMMA vm number
                            words.NEXT
        )
        
        let QUIT = x.defwordRetCodeword "QUIT" Flags.NONE
                    [|
                        RZ;RSPSTORE;// R0 RSP!, clear the return stack
                        INTERPRET;// interpret the next word
                        BRANCH;-(Memory.baseSize * 2)// and loop (indefinitely)
                    |]
        x.defcode "CHAR" Flags.NONE (fun vm ->
            let str = _WORD vm
            //printfn "read word %s" <| Memory.toString vm.memory str
            let c = int vm.memory.[str.address]
            vm.SP.push c
            words.NEXT
        )
        x.defcode "EXECUTE" Flags.NONE (fun vm ->
            let addr = vm.SP.pop()// Get xt into %eax and jump to it. After xt runs its NEXT will continue executing the current word.
            addr
        )
        x.setQUIT QUIT //cold start

