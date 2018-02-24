# Contract bridge in Haskell

I don't know how to play bridge. After being invited by a friend
to participate, and reading an introductory book by Charles Goren,
I thought the rules might stick if I coded them into a program.

First, I wasn't planning to go as far as an AI-based computer
player. I just wanted to play with the rules, and, for me,
writing something down is half the battle in remembering.

Second, the part of the book where I got bogged down was the
scoring. Sadly, I didn't succeed in coding the scoring rules
in time for a visit to the friend mentioned above.

In the end, I observed expert players for a few hours, and
that was good for confirming the parts of the book I understood,
and filling my head with some banter about the scoring that
some day I may get.

The code in this repository doesn't compile into a running
program. You can play with it by running `stack ghci` and
interactively applying the functions.

It lacks some basic functions to make it a pleasant experience,
such as randomly shuffling a deck, an interactive game loop,
nice display of a player's hand, score keeping.

It would probably be a good opportunity for practicing property-based
testing.

