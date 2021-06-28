get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  slots <- symbols[symbols != "DD"]
  
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  
  same <- length(unique(slots)) == 1
  
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {#all the same
    prize <- payouts[slots[1]]
  } else if (all(slots %in% c("B", "BB", "BBB"))) {#all bars
    prize <- 5
  } else if (cherries > 0){ # count cherries
    payouts <- c(0, 2, 5)
    prize <- payouts[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  
  #adjust for diamonds
  prize * 2**diamonds
}

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  structure(prize, symbols = symbols, class = "slots")
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

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) { # while's are much less common than for's in R
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}

print.slots <- function(x, ...) {
  slot_display(x)
}

# finding the expected value of our slot machine
combos <- expand.grid(wheel, wheel, wheel)
probs <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1,
           "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos$prob1 <- probs[combos$Var1]
combos$prob2 <- probs[combos$Var2]
combos$prob3 <- probs[combos$Var3]
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
combos$prize <- NA
for (i in 1:nrow(combos)) {
  combos$prize[i] = score(c(combos$Var1[i], combos$Var2[i], combos$Var3[i]))
}
sum(combos$prob * combos$prize) # 0.934356
 