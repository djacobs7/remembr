## remembr

Learn new r methods and remember them.  Uses spaced repetition to help you learn to R language and it's millions of packages.


### Installation

`install.packages('devtools'); devtools::install_github( "djacobs7/remembr");`

### Usage

Call `library(remembr)`, and then just use R like normal. It will automatically parse your code and make a note of any functions that you are using. It only annotates code that you enter in the console( also known as the REPL), so it won't count anything that you source, and it also should be pretty fast.

remembr treats your code like a deck of flashcards. Every time you enter a piece of code, it creates a new flashcard for all the functions in that code. If you have used that function before, it puts the flashcard in a pile of flashcards to be reviewed later.

You have several piles of flashcards. Initially, new functions go into a 10 minute pile. When you review a card in the 10 minute pile, it goes into the 1 hour pile. Then into a 1 day pile, and so on. This gives you lots of practice early, and less and less as you get more familiar with the function.

In order to get suggestions based of what functions to study, just call `remindMe()`, and it will give you a list of functions to study. You can study them by simply calling them in the REPL. remembr will make a note that you used the function, similar to what would happen if you studied a flashcard and put it away for a while.

You can also use `remindPackage(packageName)` to see all the flashcards for a particular package. This can be helpful if you know you used a method in a package once, but cannot remember it's name.
