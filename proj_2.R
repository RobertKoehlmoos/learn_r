DECK <- read.csv("C:/Users/17033/Desktop/Drive/Dev/learn_r/r_examples/deck.csv")
deck <- DECK

deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1,], envir = globalenv())
  card
}
shuffle <- function() {
  assign("deck", DECK[sample(1:52, size = 52), ], envir = globalenv())
}
deal()
shuffle()

whatisenv <- function() {
  # just figuring some stuff out  
  list(globalenv(), environment())
}
whatisenv()

show_env <- function(){
  temp <- "temp"
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}
show_env()

setup <- function(deck) {
  # this entire thing seems horrible dumb, convoluted, and goes against
  # every instinct I have, but it's what the book recommends, so I'm going
  # with it for now because maybe the author knows a little more than me
  # and this is just how things are done in R
  DECK <- deck
  
  deal <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  
  shuffle <- function() {
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  list(deal = deal, shuffle = shuffle)
}
cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle
shuffle()

