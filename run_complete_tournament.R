#!/usr/bin/env Rscript

# Complete tournament runner that ensures we capture final results
source("texas_holdem_tournament.R")

# Run tournament and capture results
result <- tournament_simulation("tournament_config.txt")

# Ensure final results are displayed
cat("\n" %+% "="*80 %++ "\n")
cat("🏆 FINAL TOURNAMENT COMPLETED 🏆\n")
cat("="*80 %+% "\n")