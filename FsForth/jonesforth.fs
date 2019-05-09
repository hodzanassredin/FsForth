(*      
A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth
This is PUBLIC DOMAIN (see public domain release statement below).
$Id: jonesforth.S,v 1.47 2009-09-11 08:33:13 rich Exp $
*)
(* Notes from HodzaNassredin
I decided not to change all the tutorial from assembly to fshap.
Main things to know about differences:
Instead of registers eax, esi, etc we use variables and fields of ForthVM record.
Main fields: W - work register(%eax in assembly), IP - instruction pointer (%esi in assembly).
Memory representeed as a byte array. Memory has separate regions for different purposes.
Every region described by a Span record. Also we have Cursor record, it is just a Span with mutable cursor.
All that infrastructure classes defined in Hardware.fs.
You don't have to understand now all of that. It will be more clear from the tutorial later.
*)
module Forth 

open System
open System.Text
open Hardware


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

 I assume that your screen is at least 50 characters high.

ASSEMBLING ----------------------------------------------------------------------

If you want to actually run this FORTH, rather than just read it, you will need dot net core.  

Again, to assemble this you will need dot net core installed.  The commands to
assemble and run the code are:

cat jonesforth.f - | dotnet run -p FsForth/FsForth.fsproj

If you want to run your own FORTH programs you can do:
cat jonesforth.f myprog.f | dotnet run -p FsForth/FsForth.fsproj

If you want to load your own FORTH code and then continue reading user commands, you can do:
cat jonesforth.f myfunctions.f - | dotnet run -p FsForth/FsForth.fsproj

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
module Forth =
    let JONES_VERSION = 47
    open Hardware.ForthVM
    open CodeMemory
    let createBaseWords (code : CodeMemory<Fn>) = 
        let next = code.addFunction ( fun (vm:ForthVM) ->
            vm.W <- Memory.getInt vm.memory vm.IP//current codeword
            vm.IP <- vm.IP + Memory.baseSize
            Memory.getInt vm.memory vm.W//native fn address
        )
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
        let docol = code.addFunction ( fun (vm:ForthVM) ->
            vm.RSP.push vm.IP
            vm.IP <- vm.W
            vm.IP <- vm.IP + Memory.baseSize
            next
        )
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
        //alternative names: unnest
        let exit = code.addFunction ( fun (vm:ForthVM) ->
            vm.IP <- vm.RSP.pop() 
            next
        )
        {
            NEXT = next
            DOCOL = docol
            EXIT = exit
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

*)
(* Flags - these are discussed later. *)
    [<Flags>]
    type Flags =
        | NONE = 0
        | IMMEDIATE = 0x80
        | HIDDEN = 0x20

    let LENMASK = 0x1f// length mask
    type ReadMode = SKIP|WORD|COMMENT//word parser modes

    type Writer(vm:ForthVM) =
        let baseWords = createBaseWords vm.CodeMemory
        let writeByte (v:byte) = 
            let address = vm.HERE.value//get current address
            vm.memory.[address] <- v//write byte
            vm.HERE.value <- address + 1//update current address

        let write (v:Int32) = 
            let address = vm.HERE.value//get current address
            Memory.setInt vm.memory address v//write int
            vm.HERE.value <- address + Memory.baseSize//update current address
        
        let writeString (str:String) = Encoding.ASCII.GetBytes str |> Array.iter writeByte 

        let align () = vm.HERE.value <- Memory.pad vm.HERE.value  
        do
            //init inderect base words addresses
            vm.NEXT.value <- baseWords.NEXT
            vm.DOCOL.value <- baseWords.DOCOL
            vm.EXIT.value <- baseWords.EXIT

        member x.setEntryPoint codeword = vm.EntryPoint.value <- codeword
        
        member x.PredefinedWords = baseWords
        member x.IndirectPredefinedWords =
            {
                NEXT = vm.NEXT.address
                DOCOL = vm.DOCOL.address
                EXIT = vm.EXIT.address
            }
        //write word header
        member x.create (name: string) (flags:Flags) =
            let nameSize = byte name.Length
            assert (nameSize <= 31uy)
            let flags = byte flags

            let curlink = vm.LATEST.value
            vm.LATEST.value <- vm.HERE.value//link
            write(curlink)
            nameSize + flags |> writeByte// flags + length byte
            writeString name// the name
            align()// padding to next 4 byte boundary

        member x.writeCodewordAndPayload (name: string) (flags:Flags) (codeword:CodeMemory.FnPointer) (payload:Int32[]) = 
            x.create name flags 
            let codewordAddr = vm.HERE.value
            write codeword
            // list of word pointers follow
            for d in payload do
                write d 
            codewordAddr

        member x.defword (name: string) (flags:Flags) (program:Int32[]) = 
            x.writeCodewordAndPayload name flags baseWords.DOCOL program // codeword - the interpreter
(*
Similarly I want a way to write words written in assembly language.  There will quite a few
of these to start with because, well, everything has to start in assembly before there's
enough "infrastructure" to be able to start writing FORTH words, but also I want to define
some common FORTH words in assembly language for speed, even though I could write them in FORTH.

This is what DUP looks like in memory:

  pointer to previous word
   ^
   |
+--|------+---+---+---+---+------------+
| LINK    | 3 | D | U | P | code_DUP ---------------------> points to the assembly
+---------+---+---+---+---+------------+                    code used to write DUP,
   ^       len              codeword                        which ends with NEXT.
   |
  LINK in next word

Again, for brevity in writing the header I'm going to write an assembler macro called defcode.
As with defword above, don't worry about the complicated details of the macro.
*)
        member x.defcode (name: string) (flags:Flags) (f:Fn) = 
            let fAddr = vm.CodeMemory.addFunction f
            x.writeCodewordAndPayload name flags fAddr Array.empty

        member x.defconst (name: string) (flags:Flags) (value:ForthVM ->Int32) = 
            let value = value vm
            //x.writeCodewordPayloadRetCodeword name flags code.CONST [|value|]
            x.defcode name flags (fun vm -> 
                vm.SP.push value
                baseWords.NEXT
            )

        //add variable
        member x.defvar (name: string) (flags:Flags) (varAddress:ForthVM -> Int32) (initial:Int32 option) = 
            let varAddress = varAddress vm
            match initial with  
                | Some(initial) -> Memory.setInt vm.memory varAddress initial
                | _ -> ()
            x.defconst name flags (fun vm -> varAddress)
        
        
    let init (x: Writer) = 
        let words = x.PredefinedWords
        let codewords = x.IndirectPredefinedWords 
(*
Now some easy FORTH primitives.  These are written in assembly for speed.  If you understand
i386 assembly language then it is worth reading these.  However if you don't understand assembly
you can skip the details.
*)


        let DROP = x.defcode "DROP" Flags.NONE (fun vm -> 
            vm.SP.pop() |> ignore// drop top of stack
            words.NEXT
        ) 

        let SWAP = x.defcode "SWAP" Flags.NONE (fun vm -> 
            let x = vm.SP.pop()// swap top two elements on stack
            let y = vm.SP.pop()
            vm.SP.push(x)
            vm.SP.push(y)
            words.NEXT
        ) 
        let DUP = x.defcode "DUP" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 0)// duplicate top of stack
            words.NEXT
        ) 
        let OVER = x.defcode "OVER" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 1)//get the second element of stack and push it on top
            words.NEXT
        ) 
        //( a b c -- b c a )
        let ROT = x.defcode "ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push ebx
            vm.SP.push eax
            vm.SP.push ecx
            words.NEXT 
        ) 
        //( a b c -- c a b ) rot rot 
        let NROT = x.defcode "-ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push eax
            vm.SP.push ecx
            vm.SP.push ebx
            words.NEXT 
        ) 
        //( a b -- ) drop drop ;
        let TWODROP = x.defcode "2DROP" Flags.NONE (fun vm ->  // drop top two elements of stack
            vm.SP.pop () |> ignore
            vm.SP.pop () |> ignore
            words.NEXT 
        ) 
        //( a b -- a b a b ) over over ;
        let TWODUP = x.defcode "2DUP" Flags.NONE (fun vm ->  // duplicate top two elements of stack
            let b = vm.SP.peek 0
            let a = vm.SP.peek 1
            vm.SP.push a
            vm.SP.push b
            words.NEXT 
        ) 
        //( d1 d2 — d2 d1 )
        let TWOSWAP = x.defcode "2SWAP" Flags.NONE (fun vm ->  // swap top two pairs of elements of stack
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
        let QDUP = x.defcode "?DUP" Flags.NONE (fun vm -> // duplicate top of stack if non-zero
            let eax = vm.SP.peek 0
            if eax <> 0 then vm.SP.push eax
            words.NEXT 
        ) 

        let flip f a b = f b a 

        let INCR = def apply "1+" <| (+) 1// increment top of stack
        let DECR = def apply "1-" <| flip (-) 1// decrement top of stack
        let INCR4 = def apply "4+" <| (+) 4// add 4 to top of stack
        let DECR4 = def apply "4-" <| flip (-) 4// subtract 4 from top of stack
        let ADD = def apply2 "+" <| (+)// get top of stack and add it to next word on stack
        let SUB = def apply2 "-" <| flip (-)// get top of stack and subtract it from next word on stack
        let MUL = def apply2 "*" <| (*)
(*
In this FORTH, only /MOD is primitive.  Later we will define the / and MOD words in
terms of the primitive /MOD.  The design of the i386 assembly instruction idiv which
leaves both quotient and remainder makes this the obvious choice.
*)
        //( n1 n2 -- n3 n4 ) Divide n1 by n2, giving the single-cell remainder n3 and the single-cell quotient n4
        let DIVMOD = x.defcode "/MOD" Flags.NONE (fun vm -> 
            let divisor = vm.SP.pop()
            let dividend = vm.SP.pop()
            let quotient, remainder = Math.DivRem(dividend, divisor)
            vm.SP.push remainder// push remainder
            vm.SP.push quotient// push quotient
            words.NEXT 
        ) 

(*
Lots of comparison operations like =, <, >, etc..

ANS FORTH says that the comparison words should return all (binary) 1's for
TRUE and all 0's for FALSE.  However this is a bit of a strange convention
so this FORTH breaks it and returns the more normal (for C programmers ...)
1 meaning TRUE and 0 meaning FALSE.
*)

        //n1 n2 – f  
        let EQU = def applyBool2 "=" (=)// top two words are equal?
        let NEQU = def applyBool2 "<>" (<>)// top two words are not equal?
        let LT = def applyBool2 "<" <| flip (<)
        let GT = def applyBool2 ">" <| flip (>)
        let LE = def applyBool2 "<=" <| flip (<=)
        let GE = def applyBool2 ">=" <| flip (>=)
        //def applyBool2 ">" <| flip (>)
    
        let ZEQU = def applyBool "0=" <| flip (=) 0// top of stack equals 0?
        let ZNEQU = def applyBool "0<>" <| flip (<>) 0// top of stack not 0?
        let ZLT = def applyBool "0<" <| flip (<) 0// comparisons with 0
        let ZGT = def applyBool "0>" <| flip (>) 0
        let ZLE = def applyBool "0<=" <| flip (<=) 0
        let ZGE = def applyBool "0>=" <| flip (>=) 0
        
        let AND = def apply2 "AND" (&&&) // bitwise AND
        let OR = def apply2 "OR" (|||) // bitwise OR
        let XOR = def apply2 "XOR" (^^^) // bitwise XOR
        let INVERT = def apply "INVERT" (~~~) // this is the FORTH bitwise "NOT" function (cf. NEGATE and NOT)

(*
        RETURNING FROM FORTH WORDS ----------------------------------------------------------------------

        Time to talk about what happens when we EXIT a function.  In this diagram QUADRUPLE has called
        DOUBLE, and DOUBLE is about to exit (look at where %esi is pointing):

                QUADRUPLE
                +------------------+
                | codeword         |
                +------------------+               DOUBLE
                | addr of DOUBLE  ---------------> +------------------+
                +------------------+               | codeword         |
                | addr of DOUBLE   |               +------------------+
                +------------------+               | addr of DUP      |
                | addr of EXIT     |               +------------------+
                +------------------+               | addr of +        |
                                                   +------------------+
                                           %esi -> | addr of EXIT     |
                                                   +------------------+

        What happens when the + function does NEXT?  Well, the following code is executed.
*)
        let EXIT = x.defcode "EXIT" Flags.NONE (fun vm -> 
            words.EXIT
        ) 

(*
        EXIT gets the old %esi which we saved from before on the return stack, and puts it in %esi.
        So after this (but just before NEXT) we get:

                QUADRUPLE
                +------------------+
                | codeword         |
                +------------------+               DOUBLE
                | addr of DOUBLE  ---------------> +------------------+
                +------------------+               | codeword         |
        %esi -> | addr of DOUBLE   |               +------------------+
                +------------------+               | addr of DUP      |
                | addr of EXIT     |               +------------------+
                +------------------+               | addr of +        |
                                                   +------------------+
                                                   | addr of EXIT     |
                                                   +------------------+

        And NEXT just completes the job by, well, in this case just by calling DOUBLE again :-)

        LITERALS ----------------------------------------------------------------------

        The final point I "glossed over" before was how to deal with functions that do anything
        apart from calling other functions.  For example, suppose that DOUBLE was defined like this:

        : DOUBLE 2 * ;

        It does the same thing, but how do we compile it since it contains the literal 2?  One way
        would be to have a function called "2" (which you'd have to write in assembler), but you'd need
        a function for every single literal that you wanted to use.

        FORTH solves this by compiling the function using a special word called LIT:

        +---------------------------+-------+-------+-------+-------+-------+
        | (usual header of DOUBLE)  | DOCOL | LIT   | 2     | *     | EXIT  |
        +---------------------------+-------+-------+-------+-------+-------+

        LIT is executed in the normal way, but what it does next is definitely not normal.  It
        looks at %esi (which now points to the number 2), grabs it, pushes it on the stack, then
        manipulates %esi in order to skip the number as if it had never been there.

        What's neat is that the whole grab/manipulate can be done using a single byte single
        i386 instruction, our old friend LODSL.  Rather than me drawing more ASCII-art diagrams,
        see if you can find out how LIT works:
*)
        let LIT = x.defcode "LIT" Flags.NONE (fun vm ->
            vm.W <- Memory.getInt vm.memory vm.IP//current literal
            vm.IP <- vm.IP + Memory.baseSize
            vm.SP.push vm.W// push the literal number on to stack
            words.NEXT
        )
(*
MEMORY ----------------------------------------------------------------------

As important point about FORTH is that it gives you direct access to the lowest levels
of the machine.  Manipulating memory directly is done frequently in FORTH, and these are
the primitive words for doing it.
*)
        let STORE = x.defcode "!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            Memory.setInt vm.memory address data
            words.NEXT 
        )
        let FETCH = x.defcode "@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = Memory.getInt vm.memory address 
            vm.SP.push data
            words.NEXT 
        )
        let ADDSTORE = x.defcode "+!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()// the amount to add
            Memory.setInt vm.memory address (Memory.getInt vm.memory address + amount)// add it
            words.NEXT 
        )
        let SUBSTORE = x.defcode "-!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()// the amount to subtract
            Memory.setInt vm.memory address (Memory.getInt vm.memory address - amount)
            words.NEXT 
        )
(*
! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes
so we also define standard words C@ and C!.

Byte-oriented operations only work on architectures which permit them (i386 is one of those).
 *)
        let STOREBYTE = x.defcode "C!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            vm.memory.[address] <- byte data
            words.NEXT 
        )
        let FETCHBYTE = x.defcode "C@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = int vm.memory.[address]
            vm.SP.push <| data
            words.NEXT 
        )

        // C@C! is a useful byte copy primitive. 
        let CCOPY = x.defcode "C@C!" Flags.NONE (fun vm -> 
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            vm.memory.[destination] <- vm.memory.[source]// get source character copy to destination
            vm.SP.push (source + 1)// increment source address
            vm.SP.push (destination + 1)// increment destination address
            words.NEXT
        )

        // and CMOVE is a block copy operation. 
        let CMOVE = x.defcode "CMOVE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            for i in 0..length do
                vm.memory.[destination + i] <- vm.memory.[source + i]
            words.NEXT 
        )

(*
        BUILT-IN VARIABLES ----------------------------------------------------------------------

        These are some built-in variables and related standard FORTH words.  Of these, the only one that we
        have discussed so far was LATEST, which points to the last (most recently defined) word in the
        FORTH dictionary.  LATEST is also a FORTH word which pushes the address of LATEST (the variable)
        on to the stack, so you can read or write it using @ and ! operators.  For example, to print
        the current value of LATEST (and this can apply to any FORTH variable) you would do:

        LATEST @ . CR

        To make defining variables shorter, I'm using a macro called defvar, similar to defword and
        defcode above.  (In fact the defvar macro uses defcode to do the dictionary header).
*)
(*
The built-in variables are:

STATE           Is the interpreter executing code (0) or compiling a word (non-zero)?
LATEST          Points to the latest (most recently defined) word in the dictionary.
HERE            Points to the next free byte of memory.  When compiling, compiled words go here.

BASE            The current base for printing and reading numbers.

*)
      
        let STATE = x.defvar "STATE" Flags.NONE (fun vm -> vm.STATE.address) Option.None
        let HERE = x.defvar "HERE" Flags.NONE (fun vm -> vm.HERE.address) Option.None
        let LATEST = x.defvar "LATEST" Flags.NONE (fun vm -> vm.LATEST.address) Option.None //name_SYSCALL0 // SYSCALL0 must be last in built-in dictionary
        let BASE = x.defvar "BASE" Flags.NONE (fun vm -> vm.BASE.address) Option.None
(*
BUILT-IN CONSTANTS ----------------------------------------------------------------------

It's also useful to expose a few constants to FORTH.  When the word is executed it pushes a
constant value on the stack.

The built-in constants are:

VERSION         Is the current version of this FORTH.
S0              Stores the address of the top of the parameter stack.
R0              The address of the top of the return stack.
DOCOL           Pointer to DOCOL.
F_IMMED         The IMMEDIATE flag's actual value.
F_HIDDEN        The HIDDEN flag's actual value.
F_LENMASK       The length mask in the flags/len byte.
*)
        let SZ = x.defconst "S0" Flags.NONE (fun vm -> vm.SP.S0)
        let VERSION = x.defconst "VERSION" Flags.NONE (fun vm -> JONES_VERSION)
        let RZ = x.defconst "R0" Flags.NONE (fun vm -> vm.RSP.S0)
        let __DOCOL = x.defconst "DOCOL" Flags.NONE (fun vm -> words.DOCOL)//probaly words -> codewords
        let __F_IMMED = x.defconst "F_IMMED" Flags.NONE (fun vm ->  int Flags.IMMEDIATE)
        let __F_HIDDEN = x.defconst "F_HIDDEN" Flags.NONE (fun vm ->  int Flags.HIDDEN)
        let __F_LENMASK = x.defconst "F_LENMASK" Flags.NONE (fun vm -> LENMASK)
(*
RETURN STACK ----------------------------------------------------------------------

These words allow you to access the return stack.  Recall that the register %ebp always points to
the top of the return stack.
*)
        let TOR = x.defcode ">R" Flags.NONE (fun vm -> 
            vm.SP.pop() |> vm.RSP.push
            words.NEXT 
        )
        let FROMR = x.defcode "R>" Flags.NONE (fun vm -> 
            vm.RSP.pop() |> vm.SP.push
            words.NEXT 
        )
        //get return stack pointer
        let RSPFETCH = x.defcode "RSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.RSP.top
            words.NEXT 
        )
        //set return stack pointer
        let RSPSTORE = x.defcode "RSP!" Flags.NONE (fun vm -> 
            vm.RSP.top <- vm.SP.pop()
            words.NEXT 
        )
        let RDROP = x.defcode "RDROP" Flags.NONE (fun vm -> 
            vm.RSP.pop()|> ignore// pop return stack and throw away
            words.NEXT 
        )
(*
PARAMETER (DATA) STACK ----------------------------------------------------------------------

These functions allow you to manipulate the parameter stack.  Recall that Linux sets up the parameter
stack for us, and it is accessed through %esp.
*)
        let DSPFETCH = x.defcode "DSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.SP.top
            words.NEXT 
        )
        //set data stack pointer
        let DSPSTORE = x.defcode "DSP!" Flags.NONE (fun vm -> 
            vm.SP.top <- vm.SP.pop()
            words.NEXT 
        )
(*
INPUT AND OUTPUT ----------------------------------------------------------------------

These are our first really meaty/complicated FORTH primitives.  I have chosen to write them in
assembler, but surprisingly in "real" FORTH implementations these are often written in terms
of more fundamental FORTH primitives.  I chose to avoid that because I think that just obscures
the implementation.  After all, you may not understand assembler but you can just think of it
as an opaque block of code that does what it says.

Let's discuss input first.

The FORTH word KEY reads the next byte from stdin (and pushes it on the parameter stack).
So if KEY is called and someone hits the space key, then the number 32 (ASCII code of space)
is pushed on the stack.

In FORTH there is no distinction between reading code and reading input.  We might be reading
and compiling code, we might be reading words to execute, we might be asking for the user
to type their name -- ultimately it all comes in through KEY.

The implementation of KEY uses an input buffer of a certain size (defined at the end of this
file).  It calls the Linux read(2) system call to fill this buffer and tracks its position
in the buffer using a couple of variables, and if it runs out of input buffer then it refills
it automatically.  The other thing that KEY does is if it detects that stdin has closed, it
exits the program, which is why when you hit ^D the FORTH system cleanly exits.

     buffer                           bufftop
|                                |
V                                V
+-------------------------------+--------------------------------------+
| INPUT READ FROM STDIN ....... | unused part of the buffer            |
+-------------------------------+--------------------------------------+
                  ^
                  |
               currkey (next character to read)

<---------------------- BUFFER_SIZE (4096 bytes) ---------------------->
*)
        let KEY = x.defcode "KEY" Flags.NONE (fun vm -> 
            vm.input_buffer.get() |> int |> vm.SP.push// push return value on stack
            words.NEXT 
        )
(*
By contrast, output is much simpler.  The FORTH word EMIT writes out a single byte to stdout.
This implementation just uses the write system call.  No attempt is made to buffer output, but
it would be a good exercise to add it.
*)
        let EMIT = x.defcode "EMIT" Flags.NONE (fun vm -> 
            vm.SP.pop() |> byte |> vm.out_buffer.set
            words.NEXT 
        )
(*
Back to input, WORD is a FORTH word which reads the next full word of input.

What it does in detail is that it first skips any blanks (spaces, tabs, newlines and so on).
Then it calls KEY to read characters into an internal buffer until it hits a blank.  Then it
calculates the length of the word it read and returns the address and the length as
two words on the stack (with the length at the top of stack).

Notice that WORD has a single internal buffer which it overwrites each time (rather like
a static C string).  Also notice that WORD's internal buffer is just 32 bytes long and
there is NO checking for overflow.  31 bytes happens to be the maximum length of a
FORTH word that we support, and that is what WORD is used for: to read FORTH words when
we are compiling and executing code.  The returned strings are not NUL-terminated.

Start address+length is the normal way to represent strings in FORTH (not ending in an
ASCII NUL character as in C), and so FORTH strings can contain any character including NULs
and can be any length.

WORD is not suitable for just reading strings (eg. user input) because of all the above
peculiarities and limitations.

Note that when executing, you'll see:
WORD FOO
which puts "FOO" and length 3 on the stack, but when compiling:
: BAR WORD FOO ;
is an error (or at least it doesn't do what you might expect).  Later we'll talk about compiling
and immediate mode, and you'll understand why.
*)
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

        let WORD = x.defcode "WORD" Flags.NONE (fun vm -> 
            let str = _WORD vm
            vm.SP.push str.address// push base address
            vm.SP.push str.size// push length
            words.NEXT 
        )
(*
As well as reading in words we'll need to read in numbers and for that we are using a function
called NUMBER.  This parses a numeric string such as one returned by WORD and pushes the
number on the parameter stack.

The function uses the variable BASE as the base (radix) for conversion, so for example if
BASE is 2 then we expect a binary number.  Normally BASE is 10.

If the word starts with a '-' character then the returned value is negative.

If the string can't be parsed as a number (or contains characters outside the current BASE)
then we need to return an error indication.  So NUMBER actually returns two items on the stack.
At the top of stack we return the number of unconverted characters (ie. if 0 then all characters
were converted, so there is no error).  Second from top of stack is the parsed number or a
partial value if there was an error.
*)
        let _NUMBER vm (str:Memory.Span) = 
            let radix = vm.BASE.value
            let mutable n = 0
            let mutable idx = 0

            let current() = int vm.memory.[str.address + idx]

            let mutable c = current ()
            let isNegative = '-' = char c // negative number?
            if isNegative then idx <- 1
            let mutable cnt = true
            // Loop reading digits.
            while cnt && idx < str.size do
                n <- n * radix
                // Convert 0-9, A-Z to a number 0-35.
                let d = current() - (int '0')
                if d > radix || d < 0 then cnt <- false// >= BASE?
                else n <- n + d
                     idx <- idx + 1
            
            let count = if str.size = 0 then 0 else str.size - idx

            if isNegative then -n, count else n, count// Negate the result if first character was '-' 

        let NUMBER= x.defcode "NUMBER" Flags.NONE (fun vm -> 
            let len, addr = vm.SP.pop(), vm.SP.pop()
            let number, numberOfUnparsedChars = _NUMBER vm {address = addr ; size = len}
            vm.SP.push number
            vm.SP.push numberOfUnparsedChars // 0 if no error
            words.NEXT 
        )


(*
        DICTIONARY LOOK UPS ----------------------------------------------------------------------

        We're building up to our prelude on how FORTH code is compiled, but first we need yet more infrastructure.

        The FORTH word FIND takes a string (a word as parsed by WORD -- see above) and looks it up in the
        dictionary.  What it actually returns is the address of the dictionary header, if it finds it,
        or 0 if it didn't.

        So if DOUBLE is defined in the dictionary, then WORD DOUBLE FIND returns the following pointer:

    pointer to this
        |
        |
        V
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+

        See also >CFA and >DFA.

        FIND doesn't find dictionary entries which are flagged as HIDDEN.  See below for why.
*)
        let rec eqStrings (vm:ForthVM) address address2 length = 
            if length = 0 
            then true
            else if vm.memory.[address] = vm.memory.[address2]
                    then eqStrings vm (address+1) (address2+1) (length - 1)
                    else false
        
        let _FIND vm (str:Memory.Span) = 
            // Now we start searching backwards through the dictionary for this word.
            // LATEST points to name header of the latest word in the dictionary
            let mutable wordAddr = vm.LATEST.value
            let mutable cnt = true
            while cnt do
                if wordAddr = 0 
                then cnt <- false// NULL pointer?  (end of the linked list)
                else let flagsLen = vm.memory.[wordAddr + Memory.baseSize] |> int
                     let flagsLen = (int Flags.HIDDEN ||| LENMASK) &&& flagsLen
                     // Compare the length expected and the length of the word.
                     // Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
                     // this won't pick the word (the length will appear to be wrong).
                     if flagsLen = str.size && eqStrings vm str.address (wordAddr + Memory.baseSize + 1) str.size
                     then cnt <- false// The strings are the same - return the header pointer
                     else wordAddr <- Memory.getInt vm.memory wordAddr// Move back through the link field to the previous word
            wordAddr
                     

        let FIND = x.defcode "FIND" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let addrOfDictEntry = _FIND vm {address = address; size = length }
            vm.SP.push addrOfDictEntry
            words.NEXT 
        )


(*
        FIND returns the dictionary pointer, but when compiling we need the codeword pointer (recall
        that FORTH definitions are compiled into lists of codeword pointers).  The standard FORTH
        word >CFA turns a dictionary pointer into a codeword pointer.

        The example below shows the result of:

                WORD DOUBLE FIND >CFA

        FIND returns a pointer to this
        |                               >CFA converts it to a pointer to this
        |                                          |
        V                                          V
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
                                                   codeword

        Notes:

        Because names vary in length, this isn't just a simple increment.

        In this FORTH you cannot easily turn a codeword pointer back into a dictionary entry pointer, but
        that is not true in most FORTH implementations where they store a back pointer in the definition
        (with an obvious memory/complexity cost).  The reason they do this is that it is useful to be
        able to go backwards (codeword -> dictionary entry) in order to decompile FORTH definitions
        quickly.

        What does CFA stand for?  My best guess is "Code Field Address".
*)

        let _TCFA (vm:ForthVM) linkAddress = 
            let flagsLenAddress = linkAddress + Memory.baseSize// Skip link pointer.
            let length = vm.memory.[flagsLenAddress] |> int &&& LENMASK// Just the length, not the flags.
            let cfaAddress = flagsLenAddress + 1 + length |> Memory.pad// Skip the name. The codeword is 4-byte aligned.
            cfaAddress

        let TCFA = x.defcode ">CFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress
            vm.SP.push cfaAddress
            words.NEXT 
        )

(*
        Related to >CFA is >DFA which takes a dictionary entry address as returned by FIND and
        returns a pointer to the first data field.

        FIND returns a pointer to this
        |                               >CFA converts it to a pointer to this
        |                                          |
        |                                          |    >DFA converts it to a pointer to this
        |                                          |             |
        V                                          V             V
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
                                                   codeword

        (Note to those following the source of FIG-FORTH / ciforth: My >DFA definition is
        different from theirs, because they have an extra indirection).

        You can see that >DFA is easily defined in FORTH just by adding 4 to the result of >CFA.
*)
        let TDFA = x.defcode ">DFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress// >CFA         (get code field address)
            vm.SP.push (cfaAddress + Memory.baseSize)// 4+ (add 4 to it to get to next word)
            words.NEXT // EXIT         (return from FORTH word)
        )

(*
        COMPILING ----------------------------------------------------------------------

        Now we'll talk about how FORTH compiles words.  Recall that a word definition looks like this:

                : DOUBLE DUP + ;

        and we have to turn this into:

          pointer to previous word
           ^
           |
        +--|------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
        +---------+---+---+---+---+---+---+---+---+------------+--|---------+------------+------------+
           ^       len                         pad  codeword      |
           |                                                      V
          LATEST points here                            points to codeword of DUP

        There are several problems to solve.  Where to put the new word?  How do we read words?  How
        do we define the words : (COLON) and ; (SEMICOLON)?

        FORTH solves this rather elegantly and as you might expect in a very low-level way which
        allows you to change how the compiler works on your own code.

        FORTH has an INTERPRET function (a true interpreter this time, not DOCOL) which runs in a
        loop, reading words (using WORD), looking them up (using FIND), turning them into codeword
        pointers (using >CFA) and deciding what to do with them.

        What it does depends on the mode of the interpreter (in variable STATE).

        When STATE is zero, the interpreter just runs each word as it looks them up.  This is known as
        immediate mode.

        The interesting stuff happens when STATE is non-zero -- compiling mode.  In this mode the
        interpreter appends the codeword pointer to user memory (the HERE variable points to the next
        free byte of user memory -- see DATA SEGMENT section below).

        So you may be able to see how we could define : (COLON).  The general plan is:

        (1) Use WORD to read the name of the function being defined.

        (2) Construct the dictionary entry -- just the header part -- in user memory:

    pointer to previous word (from LATEST)                      +-- Afterwards, HERE points here, where
           ^                                                    |   the interpreter will start appending
           |                                                    V   codewords.
        +--|------+---+---+---+---+---+---+---+---+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      |
        +---------+---+---+---+---+---+---+---+---+------------+
                   len                         pad  codeword

        (3) Set LATEST to point to the newly defined word, ...

        (4) .. and most importantly leave HERE pointing just after the new codeword.  This is where
            the interpreter will append codewords.

        (5) Set STATE to 1.  This goes into compile mode so the interpreter starts appending codewords to
            our partially-formed header.

        After : has run, our input is here:

        : DOUBLE DUP + ;
                 ^
                 |
                Next byte returned by KEY will be the 'D' character of DUP

        so the interpreter (now it's in compile mode, so I guess it's really the compiler) reads "DUP",
        looks it up in the dictionary, gets its codeword pointer, and appends it:

                                                                             +-- HERE updated to point here.
                                                                             |
                                                                             V
        +---------+---+---+---+---+---+---+---+---+------------+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        |
        +---------+---+---+---+---+---+---+---+---+------------+------------+
                   len                         pad  codeword

        Next we read +, get the codeword pointer, and append it:

                                                                                          +-- HERE updated to point here.
                                                                                          |
                                                                                          V
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          |
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+
                   len                         pad  codeword

        The issue is what happens next.  Obviously what we _don't_ want to happen is that we
        read ";" and compile it and go on compiling everything afterwards.

        At this point, FORTH uses a trick.  Remember the length byte in the dictionary definition
        isn't just a plain length byte, but can also contain flags.  One flag is called the
        IMMEDIATE flag (F_IMMED in this code).  If a word in the dictionary is flagged as
        IMMEDIATE then the interpreter runs it immediately _even if it's in compile mode_.

        This is how the word ; (SEMICOLON) works -- as a word flagged in the dictionary as IMMEDIATE.

        And all it does is append the codeword for EXIT on to the current definition and switch
        back to immediate mode (set STATE back to 0).  Shortly we'll see the actual definition
        of ; and we'll see that it's really a very simple definition, declared IMMEDIATE.

        After the interpreter reads ; and executes it 'immediately', we get this:

        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
        +---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
                   len                         pad  codeword                                           ^
                                                                                                       |
                                                                                                      HERE
        STATE is set to 0.

        And that's it, job done, our new definition is compiled, and we're back in immediate mode
        just reading and executing words, perhaps including a call to test our new word DOUBLE.

        The only last wrinkle in this is that while our word was being compiled, it was in a
        half-finished state.  We certainly wouldn't want DOUBLE to be called somehow during
        this time.  There are several ways to stop this from happening, but in FORTH what we
        do is flag the word with the HIDDEN flag (F_HIDDEN in this code) just while it is
        being compiled.  This prevents FIND from finding it, and thus in theory stops any
        chance of it being called.

        The above explains how compiling, : (COLON) and ; (SEMICOLON) works and in a moment I'm
        going to define them.  The : (COLON) function can be made a little bit more general by writing
        it in two parts.  The first part, called CREATE, makes just the header:

                                                   +-- Afterwards, HERE points here.
                                                   |
                                                   V
        +---------+---+---+---+---+---+---+---+---+
        | LINK    | 6 | D | O | U | B | L | E | 0 |
        +---------+---+---+---+---+---+---+---+---+
                   len                         pad

        and the second part, the actual definition of : (COLON), calls CREATE and appends the
        DOCOL codeword, so leaving:

                                                                +-- Afterwards, HERE points here.
                                                                |
                                                                V
        +---------+---+---+---+---+---+---+---+---+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      |
        +---------+---+---+---+---+---+---+---+---+------------+
                   len                         pad  codeword

        CREATE is a standard FORTH word and the advantage of this split is that we can reuse it to
        create other types of words (not just ones which contain code, but words which contain variables,
        constants and other data).
*)
        let readString (vm:ForthVM) address length = 
            let arr = Array.create length ' '
            for i in 0..(length - 1) do
                arr.[i] <- char (vm.memory.[address + i])
            new String(arr)

        let CREATE = x.defcode "CREATE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let name = readString vm address length
            x.create name Flags.NONE
            words.NEXT 
        )

(*
        Because I want to define : (COLON) in FORTH, not assembler, we need a few more FORTH words
        to use.

        The first is , (COMMA) which is a standard FORTH word which appends a 32 bit integer to the user
        memory pointed to by HERE, and adds 4 to HERE.  So the action of , (COMMA) is:

                                                        previous value of HERE
                                                                 |
                                                                 V
        +---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
        | LINK    | 6 | D | O | U | B | L | E | 0 |             |  <data>    |
        +---------+---+---+---+---+---+---+---+---+-- - - - - --+------------+
                   len                         pad                            ^
                                                                              |
                                                                        new value of HERE

        and <data> is whatever 32 bit integer was at the top of the stack.

        , (COMMA) is quite a fundamental operation when compiling.  It is used to append codewords
        to the current word that is being compiled.
*)

        let _COMMA (vm:ForthVM) v =
            let address = vm.HERE.value
            Memory.setInt vm.memory address v// Store it.
            vm.HERE.value <-address + Memory.baseSize// Update HERE (incremented)

        let COMMA = x.defcode "," Flags.NONE (fun vm -> 
            let v = vm.SP.pop()// Code pointer to store.
            _COMMA vm v
            words.NEXT 
        )

(*
        Our definitions of : (COLON) and ; (SEMICOLON) will need to switch to and from compile mode.

        Immediate mode vs. compile mode is stored in the global variable STATE, and by updating this
        variable we can switch between the two modes.

        For various reasons which may become apparent later, FORTH defines two standard words called
        [ and ] (LBRAC and RBRAC) which switch between modes:

        Word    Assembler       Action          Effect
        [       LBRAC           STATE := 0      Switch to immediate mode.
        ]       RBRAC           STATE := 1      Switch to compile mode.

        [ (LBRAC) is an IMMEDIATE word.  The reason is as follows: If we are in compile mode and the
        interpreter saw [ then it would compile it rather than running it.  We would never be able to
        switch back to immediate mode!  So we flag the word as IMMEDIATE so that even in compile mode
        the word runs immediately, switching us back to immediate mode.
*)
        let LBRAC = x.defcode "[" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.value <- 0
            words.NEXT 
        )
        let RBRAC = x.defcode "]" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.value <- 1
            words.NEXT 
        )
(*
        EXTENDING THE COMPILER ----------------------------------------------------------------------

        Words flagged with IMMEDIATE (F_IMMED) aren't just for the FORTH compiler to use.  You can define
        your own IMMEDIATE words too, and this is a crucial aspect when extending basic FORTH, because
        it allows you in effect to extend the compiler itself.  Does gcc let you do that?

        Standard FORTH words like IF, WHILE, .''
        and so on are all written as extensions to the basic
        compiler, and are all IMMEDIATE words.

        The IMMEDIATE word toggles the F_IMMED (IMMEDIATE flag) on the most recently defined word,
        or on the current word if you call it in the middle of a definition.

        Typical usage is:

        : MYIMMEDWORD IMMEDIATE
                ...definition...
        ;

        but some FORTH programmers write this instead:

        : MYIMMEDWORD
                ...definition...
        ; IMMEDIATE

        The two usages are equivalent, to a first approximation.
*)
        let toggleWordFlag wordAddress (flag:Flags) (vm:ForthVM)  = 
            let flagsAddress = wordAddress + Memory.baseSize
            let flagsLen = vm.memory.[flagsAddress]|> int
            let flagsLen = flagsLen ^^^ int flag //toggle flag
            vm.memory.[flagsAddress] <- byte flagsLen
            words.NEXT 

        let immediate (vm:ForthVM) = toggleWordFlag vm.LATEST.value Flags.IMMEDIATE vm

        let IMMEDIATE = x.defcode "IMMEDIATE" Flags.IMMEDIATE immediate
(*
'addr HIDDEN' toggles the hidden flag (F_HIDDEN) of the word defined at addr.  To hide the
most recently defined word (used above in : and ; definitions) you would do:

        LATEST @ HIDDEN

'HIDE word' toggles the flag on a named 'word'.

Setting this flag stops the word from being found by FIND, and so can be used to make 'private'
words.  For example, to break up a large word into smaller parts you might do:

        : SUB1 ... subword ... ;
        : SUB2 ... subword ... ;
        : SUB3 ... subword ... ;
        : MAIN ... defined in terms of SUB1, SUB2, SUB3 ... ;
        HIDE SUB1
        HIDE SUB2
        HIDE SUB3

After this, only MAIN is 'exported' or seen by the rest of the program.
*)
        let hidden (vm:ForthVM) = 
            let addr = vm.SP.pop()
            toggleWordFlag addr Flags.HIDDEN vm

        let HIDDEN = x.defcode "HIDDEN" Flags.IMMEDIATE hidden
(*
Now we can define : (COLON) using CREATE.  It just calls CREATE, appends DOCOL (the codeword), sets
the word HIDDEN and goes into compile mode.
*)
        let COLON = x.defword ":" Flags.NONE 
                        [|
                            WORD;// Get the name of the new word
                            CREATE;// CREATE the dictionary entry / header
                            LIT; words.DOCOL; COMMA;// Append DOCOL  (the codeword).
                            LATEST; FETCH; HIDDEN // Make the word hidden (see below for definition).
                            RBRAC;// Go into compile mode.
                            codewords.EXIT// Return from the function.
                        |]
(*
; (SEMICOLON) is also elegantly simple.  Notice the F_IMMED flag.
*)
        let COSEMICOLONLON = x.defword ";" Flags.IMMEDIATE 
                                [|
                                    LIT; codewords.EXIT; COMMA;// Append EXIT (so the word will return).
                                    LATEST; FETCH; HIDDEN; // Toggle hidden flag -- unhide the word (see below for definition).
                                    LBRAC;// Go back to IMMEDIATE mode.
                                    codewords.EXIT;// Return from the function.
                                |]
        let HIDE = x.defword "HIDE" Flags.NONE [|WORD;FIND;HIDDEN;codewords.EXIT|]

(*
        ' (TICK) is a standard FORTH word which returns the codeword pointer of the next word.

        The common usage is:

        ' FOO ,

        which appends the codeword of FOO to the current word we are defining (this only works in compiled code).

        You tend to use ' in IMMEDIATE words.  For example an alternate (and rather useless) way to define
        a literal 2 might be:

        : LIT2 IMMEDIATE
                ' LIT ,         \ Appends LIT to the currently-being-defined word
                2 ,             \ Appends the number 2 to the currently-being-defined word
        ;

        So you could do:

        : DOUBLE LIT2 * ;

        (If you don't understand how LIT2 works, then you should review the material about compiling words
        and immediate mode).

        This definition of ' uses a cheat which I copied from buzzard92.  As a result it only works in
        compiled code.  It is possible to write a version of ' based on WORD, FIND, >CFA which works in
        immediate mode too.
*)
        let TICK = x.defcode "'" Flags.NONE (fun vm ->
            vm.SP.push <| Memory.getInt vm.memory vm.IP // Get the address of the next word. Push it on the stack.
            vm.IP <- vm.IP + Memory.baseSize//and skip it.
            words.NEXT 
        )

(*
        BRANCHING ----------------------------------------------------------------------

        It turns out that all you need in order to define looping constructs, IF-statements, etc.
        are two primitives.

        BRANCH is an unconditional branch. 0BRANCH is a conditional branch (it only branches if the
        top of stack is zero).

        The diagram below shows how BRANCH works in some imaginary compiled word.  When BRANCH executes,
        %esi starts by pointing to the offset field (compare to LIT above):

        +---------------------+-------+---- - - ---+------------+------------+---- - - - ----+------------+
        | (Dictionary header) | DOCOL |            | BRANCH     | offset     | (skipped)     | word       |
        +---------------------+-------+---- - - ---+------------+-----|------+---- - - - ----+------------+
                                                                   ^  |                       ^
                                                                   |  |                       |
                                                                   |  +-----------------------+
                                                                  %esi added to offset

        The offset is added to %esi to make the new %esi, and the result is that when NEXT runs, execution
        continues at the branch target.  Negative offsets work as expected.

        0BRANCH is the same except the branch happens conditionally.

        Now standard FORTH words such as IF, THEN, ELSE, WHILE, REPEAT, etc. can be implemented entirely
        in FORTH.  They are IMMEDIATE words which append various combinations of BRANCH or 0BRANCH
        into the word currently being compiled.

        As an example, code written like this:

                condition-code IF true-part THEN rest-code

        compiles to:

                condition-code 0BRANCH OFFSET true-part rest-code
                                          |             ^
                                          |             |
                                          +-------------+
*)
        let BRANCH = x.defcode "BRANCH" Flags.NONE (fun vm ->
            vm.IP <- vm.IP + Memory.getInt vm.memory vm.IP // add the offset to the instruction pointer
            words.NEXT 
        )
        let ZBRANCH = x.defcode "0BRANCH" Flags.NONE (fun vm ->
            if vm.SP.pop () = 0 // top of stack is zero?
            then vm.IP <- vm.IP + Memory.getInt vm.memory vm.IP // add the offset to the instruction pointer
            else vm.IP <- vm.IP + Memory.baseSize// otherwise we need to skip the offset
            words.NEXT 
        )
(*
        LITERAL STRINGS ----------------------------------------------------------------------

        LITSTRING is a primitive used to implement the ." and S" operators (which are written in
        FORTH).  See the definition of those operators later.

        TELL just prints a string.  It's more efficient to define this in assembly because we
        can make it a single Linux syscall.
*)
        let LITSTRING = x.defcode "LITSTRING" Flags.NONE (fun vm ->
            let length = Memory.getInt vm.memory vm.IP
            vm.IP <- vm.IP + Memory.baseSize
            vm.SP.push vm.IP     // push the address of the start of the string
            vm.SP.push length    // push length on the stack
            vm.IP <- vm.IP + length |> Memory.pad  // skip past the string and round up to next BaseSize byte boundary
            words.NEXT 
        )
        let TELL = x.defcode "TELL" Flags.NONE (fun vm ->
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            vm.out_buffer.setString {address = address; size = length}
            words.NEXT 
        )

(*
QUIT AND INTERPRET ----------------------------------------------------------------------

QUIT is the first FORTH function called, almost immediately after the FORTH system "boots".
As explained before, QUIT doesn't "quit" anything.  It does some initialisation (in particular
it clears the return stack) and it calls INTERPRET in a loop to interpret commands.  The
reason it is called QUIT is because you can call it from your own FORTH words in order to
"quit" your program and start again at the user prompt.

INTERPRET is the FORTH interpreter ("toploop", "toplevel" or "REPL" might be a more accurate
description -- see: http://en.wikipedia.org/wiki/REPL).
*)

(*
        This interpreter is pretty simple, but remember that in FORTH you can always override
        it later with a more powerful one!
 *)

        let INTERPRET = x.defcode "INTERPRET" Flags.NONE (fun vm ->
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
        // QUIT must not return (ie. must not call EXIT).
        let QUIT = x.defword "QUIT" Flags.NONE
                    [|
                        RZ;RSPSTORE;// R0 RSP!, clear the return stack
                        INTERPRET;// interpret the next word
                        BRANCH;-(Memory.baseSize * 2)// and loop (indefinitely)
                    |]

(*
        ODDS AND ENDS ----------------------------------------------------------------------

        CHAR puts the ASCII code of the first character of the following word on the stack.  For example
        CHAR A puts 65 on the stack.

        EXECUTE is used to run execution tokens.  See the discussion of execution tokens in the
        FORTH code for more details.

        SYSCALL0, SYSCALL1, SYSCALL2, SYSCALL3 make a standard Linux system call.  (See <asm/unistd.h>
        for a list of system call numbers).  As their name suggests these forms take between 0 and 3
        syscall parameters, plus the system call number.

        In this FORTH, SYSCALL0 must be the last word in the built-in (assembler) dictionary because we
        initialise the LATEST variable to point to it.  This means that if you want to extend the assembler
        part, you must put new words before SYSCALL0, or else change how LATEST is initialised.
*)
        let CHAR = x.defcode "CHAR" Flags.NONE (fun vm ->
            let str = _WORD vm
            //printfn "read word %s" <| Memory.toString vm.memory str
            let c = int vm.memory.[str.address]
            vm.SP.push c
            words.NEXT
        )
        let EXECUTE = x.defcode "EXECUTE" Flags.NONE (fun vm ->
            let addr = vm.SP.pop()// Get xt into %eax and jump to it. After xt runs its NEXT will continue executing the current word.
            addr
        )
        x.setEntryPoint QUIT //cold start

(*
        START OF FORTH CODE ----------------------------------------------------------------------

        We've now reached the stage where the FORTH system is running and self-hosting.  All further
        words can be written as FORTH itself, including words like IF, THEN, .'', etc which in most
        languages would be considered rather fundamental.

        I used to append this here in the assembly file, but I got sick of fighting against gas's
        crack-smoking (lack of) multiline string syntax.  So now that is in a separate file called
        jonesforth.f

        If you don't already have that file, download it from http://annexia.org/forth in order
        to continue the tutorial.
*)

(* END OF jonesforth.fs *)
