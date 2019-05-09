This is Richard WM Jones's literate x86 assembly implementation of Forth reimplemented in F#.

All features exclude linux syscals are implemented. 

When you are starting forth learning the most interesting part is how to build forth vm from scratch.
Jonesforth is one of the best implementations and tutorial at the same time.
Unfortunately, it is hard sometimes to understand assembler code so fsForth is an attempt to made it clearer for the developer without deep assembler knowledge.

You just need to open [jonesforth.fs](https://github.com/hodzanassredin/FsForth/blob/master/FsForth/jonesforth.fs) and start reading.

Second part [jonesforth.f](https://github.com/hodzanassredin/FsForth/blob/master/jonesforth.f) is optional and it is more about how to work with Forth and how to use the full power of the forth programming language including metaprogramming features.

if you want to play with forth then install dotnet core and exec run.cmd (on windows) or ./run.sh on linux.

["Thinking Forth" by Leo Brodie](http://thinking-forth.sourceforge.net/)

["Starting forth" by Leo Brodie](https://www.forth.com/starting-forth/0-starting-forth/)

[forth words](https://github.com/jdavidberger/pijFORTHos/blob/master/doc/forth.md)

[jonesforth](http://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/)

[jonesforth github mirror](https://github.com/nornagon/jonesforth)
