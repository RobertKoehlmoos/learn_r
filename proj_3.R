wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
get_symbols <- function() {
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

# Playing with speed
abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}

abs_sets <- function(vec){
  negs <- vec < 0
  vec[negs] <- vec[negs] * -1
  vec
}

long <- rep(c(-1, 1), 5000000)
system.time(abs_loop(long))
#user  system elapsed 
#0.49    0.01    0.50 

system.time(abs_sets(long))
# user  system elapsed
# 0.20    0.02    0.21

system.time(abs(long))
# user  system elapsed 
# 0.03    0.00    0.03 

# vectorizing functions practice
change_symbols_old <- function(vec){
  for (i in 1:length(vec)){
    if (vec[i] == "DD") {
      vec[i] <- "joker"
    } else if (vec[i] == "C") {
      vec[i] <- "ace"
    } else if (vec[i] == "7") {
      vec[i] <- "king"
    }else if (vec[i] == "B") {
      vec[i] <- "queen"
    } else if (vec[i] == "BB") {
      vec[i] <- "jack"
    } else if (vec[i] == "BBB") {
      vec[i] <- "ten"
    } else {
      vec[i] <- "nine"
    } 
  }
  vec
}
# taking advantage of vectorization
change_symbols <- function(vec){
  symbol_map <- c("DD" = "joker", "C" = "ace", "7" = "king", "B" = "queen",
                  "BB" = "jack", "BBB" = "ten", "0" = "nine")
  for (symbol in attr(symbol_map, "names")) {
    new_symbol <- symbol_map[symbol]
    vec[vec == symbol] <- new_symbol
  }
  vec
}
# taking advantage of ALL the vectorization
change_symbols_best <- function(vec) {
  symbol_map <- c("DD" = "joker", "C" = "ace", "7" = "king", "B" = "queen",
                  "BB" = "jack", "BBB" = "ten", "0" = "nine")
  symbol_map[vec]
}
vec <- c("DD", "C", "7", "B", "BB", "BBB", "0")

change_symbols(vec)
##  "joker" "ace"   "king"  "queen" "jack"  "ten"   "nine"

many <- rep(vec, 1000000)

system.time(change_symbols_old(many))
# user  system elapsed 
# 8.43    0.00    8.45 

system.time(change_symbols(many))
# user  system elapsed 
# 0.30    0.03    0.33 

system.time(change_symbols_best(many))
# user  system elapsed 
# 0.11    0.02    0.13 


# the wild card handling logic below is the result of hours of tears and pain,
# mostly the result of my own failure to check for logical mistakes
# But in the end it is short, clear, and correct. May it stand for eternity
score_many <- function(syms){
  # plays_symbols is a n x 3 matrix where each row consists of the three
  # slots for that play
  prize <- rep(0, nrow(syms))
  
  ## Count the number of cherries and diamonds in each combination
  cherries <- rowSums(syms == "C")
  diamonds <- rowSums(syms == "DD")
  
  # any cherries means diamonds must be cherries
  syms[cherries > 0 & syms[, 1] == "DD", 1] <- "C"
  syms[cherries > 0 & syms[, 2] == "DD", 2] <- "C"
  syms[cherries > 0 & syms[, 3] == "DD", 3] <- "C"
  
  # without cherry rows handles, now diamonds can take the value
  # of any other non-diamond value in their spin because
  # either they will all be the same, all will be bars,
  # or nothing will be give out
  one_with_two <- syms[, 1] == "DD" & syms[, 2] != "DD"
  syms[one_with_two, 1] <- syms[one_with_two, 2]
  one_with_three <- syms[, 1] == "DD" & syms[, 3] != "DD"
  syms[one_with_three, 1] <- syms[one_with_three, 3]
  syms[syms[, 2] == "DD", 2] <- syms[syms[, 2] == "DD", 1]
  syms[syms[, 3] == "DD", 3] <- syms[syms[, 3] == "DD", 1]
  
  # case where cherries are present
  prize <- c(0, 2, 5, 10)[cherries + diamonds + 1]
  prize[cherries == 0] <- 0
  
  # case where all are the same
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  same <- syms[, 1] == syms[, 2] & syms[, 2] == syms[, 3]
  prize[same] <- payouts[syms[same, 1]]
  
  # change prize for combinations that contain all bars
  bars <- syms == "B" | syms ==  "BB" | syms == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars] <- 5
  
  # not need to handle wilds, because there are none left!
  prize <- prize * (2**diamonds)
}

get_many_symbols <- function(n) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  vec <- sample(wheel, size = 3 * n, replace = TRUE,
                prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol = 3)
}

play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2],
             w3 = symb_mat[,3], prize = score_many(symb_mat))
}

system.time(play_many(100000))
