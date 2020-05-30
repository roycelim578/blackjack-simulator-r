#Start
library(tidyverse)
library(dplyr)
library(gtools)
library(spatialEco)

#Set-up
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
val <- c("Ace", "Pair", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(suits, val)
deck_named <- paste(deck$Var2, "of", deck$Var1)
static_deck_named <- deck_named

#Functions
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

draw_one <- function(x) {
  holder <- sample(deck_named, 1)
  deck_named <- deck_named[-match(holder,static_deck_named)]
  return(append(x, holder, after=length(x)))
}

#Execute
n = 100000
sixteen_over_only <- replicate(n, {
  deck_named <- static_deck_named
  while (TRUE) {
    hand <- sample(deck_named, 2)
    if (find_value(hand) != 21) {
      break
    } 
  }
  while (find_value(hand) < 16) {
     hand <- draw_one(hand)
  }
  if (find_value(hand) > 21) {
    return(NA)
  }
  for (i in 1:3) {
    hand <- draw_one(hand)
    if (find_value(hand) > 21) {
      hand <- hand[-length(hand)]
      break
    }
  }
  return(length(hand))
 }
)
Rateof_hands_past_sixteen <- sum(sixteen_over_only != NA)/length(sixteen_over_only)
cards_drawn_bef_bust <- vector(mode = "numeric", length=max(na.omit(sixteen_over_only)))
for (i in 1:(max(na.omit(sixteen_over_only)) - 1)) {
  cards_drawn_bef_bust[i] <- mean(na.omit(sixteen_over_only) == i+1)
}  
cards_drawn_bef_bust <- cards_drawn_bef_bust
cards_drawn_bef_bust[!is.finite(cards_drawn_bef_bust)] <- NA
cards_drawn_bef_bust <- na.omit(cards_drawn_bef_bust)
cards_drawn_bef_bust