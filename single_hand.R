#!/usr/bin/env Rscript

# Load required library
library(combinat)

# Read configuration
source("texas_holdem_tournament.R")

# Read player names
player_names <- read_player_names()
selected_names <- sample(player_names, 10, replace = FALSE)

# Initialize players for one hand
players <- data.frame(
  player = 1:10,
  name = selected_names,
  chips = rep(1000, 10),
  hands_won = rep(0, 10),
  active = rep(TRUE, 10),
  stringsAsFactors = FALSE
)

cat("🃏 SINGLE HAND - TEXAS HOLD'EM 🃏\n")
cat("===============================\n")
cat("Players:", paste(players$name, collapse = ", "), "\n")
cat("Starting chips: 1000 each\n")
cat("Blinds: 5 / 10\n")
cat("===============================\n\n")

# Play one hand
result <- play_tournament_hand(players, 5, 10, "balanced", 1)

cat("\n💰 FINAL CHIP STANDINGS AFTER ONE HAND:\n")
for(i in order(-result$players$chips)) {
  player <- result$players[i,]
  cat(sprintf("%s: %d chips\n", player$name, player$chips))
}

cat("\nHand completed successfully! 🎉\n")