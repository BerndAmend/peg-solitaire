# Getting Started #

Download peg-solitaire v7 from https://github.com/tptb/peg-solitaire/releases/download/v7/peg-solitaire-v7.jar.

Usage example
  * Solve the english board for
    * the center position: `java -Xmx550M -jar peg-solitaire-v7.jar` (requires ~550MB memory and ~9 seconds on a Intel i7-3770 with the parameter -threads 64)
    * for all start positions: `java -Xmx7G -jar peg-solitaire-v7.jar -full` (requires ~7GB memory 2 minutes ~20 seconds on a Intel i7-3770 with the parameter -threads 64)
  * Solve the european board for
    * the center position: `java -Xmx12G -jar peg-solitaire-v7.jar euro` (requires ~12GB memory and 4 minutes ~15 seconds on a Intel i7-3770 with the parameter -threads 64)
  * Define a board to solve
```
. o o o o .
o o o o o o
o o . . o o
o o . . o o
o o o o o o
. o o o o .
```
> or
```
. . . . . . . o o
. . . . . . o o o
o o . . . o o o .
o o o . o o o . .
. o o o o o . . .
. . o o o . . . .
. . . o . . . . .
```

> with `java -Xmx500M -jar peg-solitaire-v7.jar user` or `java -Xmx500M -jar peg-solitaire-v7.jar user -full`

Allowing the application to use more memory dramatically decreases the time required to solve the selected board.
The memory usage can be reduced using the parameter "-threads 1".

## Compile ##
To compile the application yourself download the Scala IDE from http://scala-ide.org/.
