#Start
library(tidyverse)
library(dplyr)
library(gtools)
library(spatialEco)

#Set-up
cash_info <- "C:/Users/royce/OneDrive/Desktop/Data Science Materials/Scripts/Practice/Blackjack Simulator/Cash_Record.txt"
game_info <- "C:/Users/royce/OneDrive/Desktop/Data Science Materials/Scripts/Practice/Blackjack Simulator/Output.txt"
cat("", file = game_info, append = FALSE)
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
val <- c("Ace", "Pair", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(suits, val)
deck_named <- paste(deck$Var2, "of", deck$Var1)
static_deck_named <- deck_named

#Functions
tailfile <- function(file, n) {
  bufferSize <- 1024L
  size <- file.info(file)$size
  
  if (size < bufferSize) {
    bufferSize <- size
  }
  
  pos <- size - bufferSize
  text <- character()
  k <- 0L
  
  f <- file(file, "rb")
  on.exit(close(f))
  
  while(TRUE) {
    seek(f, where=pos)
    chars <- readChar(f, nchars=bufferSize)
    k <- k + length(gregexpr(pattern="\\n", text=chars)[[1L]])
    text <- paste0(text, chars)
    if (k > n || pos == 0L) {
      break
    }
    pos <- max(pos-bufferSize, 0L)
  }
  tail(strsplit(text, "\\n")[[1L]], n)
}

adjust_cash <- function(x, mode=c("set", "add", "multiply")) {
  if (mode == "set") {
    cat(paste("\n", x, sep=""), file = cash_info, append = TRUE)
  } else if (mode == "add") {
    cat(paste("\n", current_cash+x, sep=""), file = cash_info, append = TRUE)
  } else if (mode == "multiply") {
    cat(paste("\n", current_cash*x, sep=""), file = cash_info, append = TRUE)
  }
}

find_value <- function(x){
  a <- match(x,static_deck_named)
  b <- (floor((a/4)-0.25)+1)
  for (i in  1:length(x)) {
    if(b[i] %in% c(10, 11, 12, 13)) {
      b[i] <- 10
    }
  }
  for (i in 1:length(x)) {
    if (b[i] == 1) {
      b <- b[-i]
      if (length(x) == 2){
        b <- insert.values(b, 11, i)
      } else if (sum(b) <= 11) {
        b <- insert.values(b, 10, i)
      } else {
        b <- insert.values(b, 1, i)
      }
      if (sum(b) == 22 & length(b) == 2){
        b[1] <- 10
      }
    }
  }
  return(sum(b))
}

check_bust <- function(x) {
  if (find_value(x) > 21) {
    cat("You have busted, try again.", file = game_info, append = TRUE)
    cat(paste("\n", x, sep = " ", collapse = " "), file = game_info, append = TRUE)
    cat(paste("\n", "You have lost (in dollars): ", Bet), file = game_info, append = TRUE)
    adjust_cash(-Bet, mode = "add")
    stop()
  }
}

draw_one <- function(x) {
  holder <- sample(deck_named, 1)
  deck_named <- deck_named[-match(holder,static_deck_named)]
  return(append(x, holder, after=length(x)))
}

cards_loeq <- function(x, y) {
  a <- match(x, static_deck_named)
  b <- (floor((a/4)-0.25)+1)
  for (i in  1:length(x)) {
    if(b[i] %in% c(10, 11, 12, 13)) {
      b[i] <- 10
    }
  }
  return(sum(b <= y))
}

#Betting
Bet <-  0
current_cash <- as.numeric(tailfile(cash_info, 1))
cat(paste("You currently have (in dollars): ", current_cash, "\n"), file = game_info, append = TRUE)
while (TRUE) {
  Bet <- as.numeric(readline(prompt="How much would you like to bet?"))
  if (class(Bet) != "numeric") {
    cat("Please enter a number.")
  } else if (Bet > current_cash) {
    cat("You cannot bet with more money that you have.")
  } else if(Bet < 10) {
    cat("You must bet at least 10 dollars.")
  } else {
    break
  }
}

# Execution
both_hands <- sample(deck_named, 4)
deck_named <- deck_named[-match(both_hands,deck_named)]
player_hand <- both_hands[c(1,3)]
com_hand <- both_hands[c(2,4)]
cat(paste(player_hand, "\n"), file = game_info, append = TRUE)
cat(paste(find_value(player_hand), "\n"), file = game_info, append = TRUE)

#Player Turn
if (find_value(player_hand) == 21) {
  cat(paste("Congratulations! You got a Blackjack!", "\n"), file = game_info, append = TRUE)
  cat(paste(player_hand, sep = "\n", collapse = " "), file = game_info, append = TRUE)
  cat(paste("\n", "You have earned (in dollars): ", Bet*2), file = game_info, append = TRUE)
  adjust_cash(Bet*2, mode = "add")
  stop()
}
for (i in 1:3) {
  player_call <- readline(prompt="Hit?")
  if (player_call == "p"){
    player_hand <- draw_one(player_hand)
    check_bust(player_hand)
    cat(paste(player_hand, "\n"), file = game_info, append = TRUE)
    cat(paste(find_value(player_hand), "\n"), file = game_info, append = TRUE)
  } else if (find_value(player_hand) < 16) {
    cat("You must meet the minimum bet of 16 points.")
  } else {
    break
  }
}
if (length(player_hand) == 3 & length(intersect(player_hand, c("Seven of Diamonds", "Seven of Clubs", "Seven of Hearts", "Seven of Spades"))) == 3) {
  cat(paste("Congratulations! You have TRIPLE SEVENS!", "\n"), file = game_info, append = TRUE)
  cat(paste(player_hand, sep = " ", collapse = " "), file = game_info, append = TRUE)
  cat(paste("\n", "You have earned (in dollars): ", Bet*7), file = game_info, append = TRUE)
  adjust_cash(Bet*7, mode = "add")
}
if (length(player_hand) >= 5) {
  cat(paste("Congratulations! You have a 5 Dragon!", "\n"), file = game_info, append = TRUE)
  cat(paste(player_hand, sep = " ", collapse = " "), file = game_info, append = TRUE)
  cat(paste("\n", "You have earned (in dollars): ", Bet*2), file = game_info, append = TRUE)
  adjust_cash(Bet*2, mode = "add")
  stop()
}

#Computer Turn
if (find_value(com_hand) == 21) {
  cat(paste("The Dealer got a Blackjack! You Lost.", "\n", "Dealer:", paste(com_hand, sep = " ", collapse = " "), "\n", "You:", paste(player_hand, collapse = "")), file = game_info, append = TRUE)
  cat(paste("\n", "You have lost (in dollars): ", Bet*2), file = game_info, append = TRUE)
  adjust_cash(-Bet*2, mode = "add")
  stop()
}
for (i in 1:3) {
 deck_to_com <- append(deck_named, player_hand, after=length(deck_named))
 com_good_draws_left <- cards_loeq(deck_to_com, 21-find_value(com_hand))
  if (find_value(com_hand) < 16){
    com_hand <- draw_one(com_hand)
  } else if (find_value(com_hand) <= 17 & length(com_hand) == 2 & length(intersect(com_hand, c("Ace of Diamonds", "Ace of Clubs", "Ace of Hearts", "Ace of Spades"))) == 1){
    com_hand <- draw_one(com_hand)
  } else if (find_value(com_hand) >= 17 & length(com_hand) == 2) {
    break
  } else if (find_value(com_hand) == 16 & length(player_hand) > 2 & length(com_hand) == 2){
    break
  } else if (com_good_draws_left/length(deck_to_com) > (0.284*1.3^(length(com_hand) - 2) + 0.0013)) {
    com_hand <- draw_one(com_hand)
  } else {
    break
  }
}
if (find_value(com_hand) > 21) {
  cat(paste("The Dealer busted! You won!",  "\n" ,"Dealer: ", paste(com_hand, sep = " ", collapse = " "), "\n", "You: ", paste(player_hand, collapse = "")), file = game_info, append = TRUE)
  cat(paste("\n", "You have earned (in dollars): ", Bet), file = game_info, append = TRUE)
  adjust_cash(Bet, mode = "add")
  stop()
}
if (length(com_hand) == 3 & length(intersect(com_hand, c("Seven of Diamonds", "Seven of Clubs", "Seven of Hearts", "Seven of Spades"))) == 3) {
  cat(paste("The Dealer has TRIPLE SEVENS! You Lost.", "\n"), file = game_info, append = TRUE)
  cat(paste(player_hand, sep = " ", collapse = " "), file = game_info, append = TRUE)
  cat(paste("\n", "You have earned (in dollars): ", Bet*7), file = game_info, append = TRUE)
  adjust_cash(-Bet*7, mode = "add")
}
if (length(com_hand) >= 5) {
  cat(paste("The Dealer has a 5 Dragon! You Lost.", "\n"), file = game_info, append = TRUE)
  cat(paste(player_hand, sep = " ", collapse = " "), file = game_info, append = TRUE)
  cat(paste("\n", "You have lost (in dollars): ", Bet*2), file = game_info, append = TRUE)
  adjust_cash(-Bet*2, mode = "add")
  stop()
}

#Decision on Winner under Normal Circumstances
decision <- find_value(player_hand) - find_value(com_hand)
com_hand <- paste(com_hand, sep = " ", collapse = " ")
player_hand <- paste(player_hand, sep = " ", collapse = " ")
if (decision > 0) {
  cat(paste("Congratulations! You beat the Dealer!",  "\n" ,"Dealer: ", com_hand, "\n", "You: ", player_hand), file = game_info, append = TRUE)
  cat(paste("\n", "You have earned (in dollars): ", Bet), file = game_info, append = TRUE)
  adjust_cash(Bet, mode = "add")
} else if (decision < 0) {
  cat(paste("You Lost. Try Again.", "\n" ,"Dealer: ", com_hand, "\n", "You: ", player_hand), file = game_info, append = TRUE)
  cat(paste("\n", "You have lost (in dollars): ", Bet), file = game_info, append = TRUE)
  adjust_cash(-Bet, mode = "add")
} else if (decision == 0) {
  cat(paste("\n", "It's a tie!", "\n" ,"Dealer: ", com_hand, "\n", "You: ", player_hand), file = game_info, append = TRUE)
}
stop()
#End

