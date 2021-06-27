get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols) {
  same <- length(unique(symbols)) == 1
  if(same) {#all the same
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- payouts[symbols[1]]
  }else if (all(symbols %in% c("B", "BB", "BBB"))) {#all bars
    prize <- 5
  } else { # count cherries
    payouts <- c(0, 2, 5)
    prize <- payouts[sum(symbols == "C") + 1]
  }
  
  #adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize * 2**diamonds
}

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}

slot_display <- function(prize){
  
  # extract symbols
  symbols <- attr(prize, "symbols")
  
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  
  # combine symbol with prize as a character string
  # \n is special escape sequence for a new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  
  # display character string in console without quotes
  cat(string)
}
