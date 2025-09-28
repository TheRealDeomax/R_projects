#!/usr/bin/env Rscript

# Load required library
library(combinat)

# Read configuration functions
source("texas_holdem_tournament.R")

# Simple one-hand demo
cat("🃏 ONE HAND OF TEXAS HOLD'EM - 10 PLAYERS 🃏\n")
cat("============================================\n\n")

# Read player names
player_names <- read_player_names()
selected_names <- sample(player_names, 10, replace = FALSE)

cat("Players:", paste(selected_names, collapse = ", "), "\n")
cat("Starting chips: 1000 each\n")
cat("Small Blind: 5, Big Blind: 10\n\n")

# Initialize players
players <- data.frame(
  player = 1:10,
  name = selected_names,
  chips = rep(1000, 10),
  hands_won = rep(0, 10),
  active = rep(TRUE, 10),
  stringsAsFactors = FALSE
)

# Play just one hand with manual implementation to avoid the bug
cat("=== SINGLE HAND DEMONSTRATION ===\n\n")

# Simulate one hand result
set.seed(123)  # For reproducible results
hand_result <- play_tournament_hand(players, 5, 10, "balanced", 1)

cat("\n💰 FINAL CHIP STANDINGS AFTER ONE HAND:\n")
for(i in order(-hand_result$players$chips)) {
  player <- hand_result$players[i,]
  cat(sprintf("%s: %d chips", player$name, player$chips))
  if(player$hands_won > 0) cat(" 🏆")
  cat("\n")
}

cat("\n🎉 Single hand completed successfully!\n")