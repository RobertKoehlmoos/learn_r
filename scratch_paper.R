library("ggplot2")
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

# c is a function that concatenates values into a vector
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
y <- x**2
qplot(x, y) # my first r plot!

rolls <- replicate(1000, roll())
qplot(rolls, binwidth = 1)
mean(rolls)

weighted_roll <- function(weights) {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE, prob = weights)
  sum(dice)
}
weighted_rolls <- replicate(1000, weighted_roll(c(1/8,1/8,1/8,1/8,1/8,3/8)))
qplot(weighted_rolls, binwidth = 1)
