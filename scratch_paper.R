# this is a comment
5%%2 == 1 # two percent signs is how R represents modulo
die <- 1:6
a <- 4
ls # just returning a function causes the interpreter to print out its
   # it's source code, if available, or the location of it's bytecode
factorial(4) # built-in, for some reason
args(ls) # super useful, really shows that r was intended to be a line by
         # line interpreted language
sample(die, size = 2, replace = TRUE) # a TRUE die rolling simulation!

roll <- function() { # my first function!
  # returns the sum of rolling two six sided die
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

roll2 <- function(bones = 1:6) { # my second function, first with arguments!
  # returns the sum of two rolls from the provided vector
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}
