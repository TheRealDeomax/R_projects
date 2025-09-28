# Texas Hold'em Poker Simulator
# Simulates multiple hands with 4 players showing probabilities and decisions

library(combinat)

# Card and deck management
create_deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  rank_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
  
  deck <- data.frame(
    suit = rep(suits, each = 13),
    rank = rep(ranks, 4),
    value = rep(rank_values, 4),
    stringsAsFactors = FALSE
  )
  return(deck)
}

shuffle_deck <- function(deck) {
  return(deck[sample(nrow(deck)), ])
}

# Hand evaluation functions
get_hand_rank <- function(cards) {
  # cards should be a data frame with suit, rank, value columns
  values <- sort(cards$value, decreasing = TRUE)
  suits <- cards$suit
  
  # Check for flush
  is_flush <- length(unique(suits)) == 1
  
  # Check for straight
  is_straight <- FALSE
  if (length(unique(values)) == 5) {
    if (max(values) - min(values) == 4) {
      is_straight <- TRUE
    }
    # Special case: A-2-3-4-5 straight (wheel)
    if (all(values == c(14, 5, 4, 3, 2))) {
      is_straight <- TRUE
      values <- c(5, 4, 3, 2, 1)  # Ace low
    }
  }
  
  # Count occurrences of each value
  value_counts <- table(values)
  counts <- sort(as.numeric(value_counts), decreasing = TRUE)
  unique_values <- as.numeric(names(sort(value_counts, decreasing = TRUE)))
  
  # Determine hand ranking (higher number = better hand)
  if (is_straight && is_flush) {
    if (all(values == c(14, 13, 12, 11, 10))) {
      return(list(rank = 10, description = "Royal Flush", kickers = values))
    } else {
      return(list(rank = 9, description = "Straight Flush", kickers = values))
    }
  } else if (counts[1] == 4) {
    return(list(rank = 8, description = "Four of a Kind", kickers = unique_values))
  } else if (counts[1] == 3 && counts[2] == 2) {
    return(list(rank = 7, description = "Full House", kickers = unique_values))
  } else if (is_flush) {
    return(list(rank = 6, description = "Flush", kickers = values))
  } else if (is_straight) {
    return(list(rank = 5, description = "Straight", kickers = values))
  } else if (counts[1] == 3) {
    return(list(rank = 4, description = "Three of a Kind", kickers = unique_values))
  } else if (counts[1] == 2 && counts[2] == 2) {
    return(list(rank = 3, description = "Two Pair", kickers = unique_values))
  } else if (counts[1] == 2) {
    return(list(rank = 2, description = "One Pair", kickers = unique_values))
  } else {
    return(list(rank = 1, description = "High Card", kickers = values))
  }
}

get_best_hand <- function(hole_cards, community_cards) {
  all_cards <- rbind(hole_cards, community_cards)
  
  if (nrow(all_cards) < 5) {
    # Not enough cards for a complete hand yet
    return(get_hand_rank(all_cards))
  }
  
  # Generate all combinations of 5 cards from available cards
  best_rank <- 0
  best_hand <- NULL
  
  if (nrow(all_cards) == 5) {
    return(get_hand_rank(all_cards))
  }
  
  # Try all combinations of 5 cards
  card_indices <- 1:nrow(all_cards)
  combinations <- combn(card_indices, 5)
  
  for (i in 1:ncol(combinations)) {
    combo_cards <- all_cards[combinations[, i], ]
    hand_rank <- get_hand_rank(combo_cards)
    
    if (hand_rank$rank > best_rank) {
      best_rank <- hand_rank$rank
      best_hand <- hand_rank
    }
  }
  
  return(best_hand)
}

# Probability calculation
calculate_win_probability <- function(hole_cards, community_cards, num_opponents = 3, num_simulations = 1000) {
  wins <- 0
  
  # Get remaining cards in deck
  deck <- create_deck()
  used_cards <- rbind(hole_cards, community_cards)
  remaining_deck <- deck[!paste(deck$suit, deck$rank) %in% paste(used_cards$suit, used_cards$rank), ]
  
  for (sim in 1:num_simulations) {
    # Shuffle remaining deck
    sim_deck <- remaining_deck[sample(nrow(remaining_deck)), ]
    
    # Deal remaining community cards if needed
    cards_needed <- 5 - nrow(community_cards)
    if (cards_needed > 0) {
      sim_community <- rbind(community_cards, sim_deck[1:cards_needed, ])
      sim_deck <- sim_deck[-(1:cards_needed), ]
    } else {
      sim_community <- community_cards
    }
    
    # Get our best hand
    our_hand <- get_best_hand(hole_cards, sim_community)
    
    # Simulate opponent hands
    opponent_beats_us <- FALSE
    for (opp in 1:num_opponents) {
      opp_hole <- sim_deck[(2*opp-1):(2*opp), ]
      opp_hand <- get_best_hand(opp_hole, sim_community)
      
      # Compare hands
      if (opp_hand$rank > our_hand$rank) {
        opponent_beats_us <- TRUE
        break
      } else if (opp_hand$rank == our_hand$rank) {
        # Compare kickers
        for (k in 1:length(our_hand$kickers)) {
          if (opp_hand$kickers[k] > our_hand$kickers[k]) {
            opponent_beats_us <- TRUE
            break
          } else if (opp_hand$kickers[k] < our_hand$kickers[k]) {
            break
          }
        }
        if (opponent_beats_us) break
      }
    }
    
    if (!opponent_beats_us) {
      wins <- wins + 1
    }
  }
  
  return(wins / num_simulations)
}

# Player decision making
make_decision <- function(win_prob, pot_size, bet_to_call, stack_size) {
  pot_odds <- bet_to_call / (pot_size + bet_to_call)
  
  if (bet_to_call == 0) {
    # Free to see next card
    if (win_prob > 0.15) {
      return("check")
    } else {
      return("check")  # Always check when free
    }
  }
  
  if (win_prob > pot_odds + 0.1) {  # Need 10% edge over pot odds
    if (win_prob > 0.7) {
      bet_size <- min(pot_size * 0.8, stack_size)
      return(paste("raise", bet_size))
    } else if (win_prob > 0.5) {
      bet_size <- min(pot_size * 0.5, stack_size)
      return(paste("raise", bet_size))
    } else {
      return("call")
    }
  } else {
    return("fold")
  }
}

# Game simulation
simulate_hand <- function(num_players = 4) {
  cat("\n=== NEW HAND ===\n")
  
  # Initialize game state
  deck <- shuffle_deck(create_deck())
  pot <- 0
  small_blind <- 5
  big_blind <- 10
  
  # Player stacks
  stacks <- rep(1000, num_players)
  player_names <- paste("Player", 1:num_players)
  
  # Deal hole cards
  hole_cards <- list()
  for (i in 1:num_players) {
    hole_cards[[i]] <- deck[(2*i-1):(2*i), ]
    deck <- deck[-(1:2), ]
  }
  
  # Post blinds
  stacks[1] <- stacks[1] - small_blind
  stacks[2] <- stacks[2] - big_blind
  pot <- small_blind + big_blind
  
  cat("Blinds posted: Small blind =", small_blind, ", Big blind =", big_blind, "\n")
  cat("Starting pot:", pot, "\n\n")
  
  # Show hole cards
  for (i in 1:num_players) {
    cat(player_names[i], "hole cards:", 
        paste(hole_cards[[i]]$rank, "of", hole_cards[[i]]$suit, collapse = ", "), "\n")
  }
  cat("\n")
  
  community_cards <- data.frame(suit = character(0), rank = character(0), value = numeric(0))
  active_players <- rep(TRUE, num_players)
  
  # Pre-flop
  cat("=== PRE-FLOP ===\n")
  for (i in 1:num_players) {
    if (active_players[i]) {
      win_prob <- calculate_win_probability(hole_cards[[i]], community_cards, 
                                          sum(active_players) - 1, 500)
      bet_to_call <- if (i <= 2) 0 else big_blind  # Blinds already posted
      
      decision <- make_decision(win_prob, pot, bet_to_call, stacks[i])
      
      cat(sprintf("%s: Win Probability = %.2f%%, Decision = %s\n", 
                  player_names[i], win_prob * 100, decision))
      
      if (grepl("fold", decision)) {
        active_players[i] <- FALSE
      } else if (grepl("call", decision)) {
        call_amount <- big_blind
        stacks[i] <- stacks[i] - call_amount
        pot <- pot + call_amount
      } else if (grepl("raise", decision)) {
        raise_amount <- as.numeric(strsplit(decision, " ")[[1]][2])
        stacks[i] <- stacks[i] - raise_amount
        pot <- pot + raise_amount
      }
    }
  }
  cat("Pot after pre-flop:", pot, "\n\n")
  
  if (sum(active_players) <= 1) {
    cat("Hand ends pre-flop\n")
    return()
  }
  
  # Flop
  cat("=== FLOP ===\n")
  community_cards <- deck[1:3, ]
  deck <- deck[-(1:3), ]
  
  cat("Community cards:", paste(community_cards$rank, "of", community_cards$suit, collapse = ", "), "\n\n")
  
  for (i in 1:num_players) {
    if (active_players[i]) {
      win_prob <- calculate_win_probability(hole_cards[[i]], community_cards, 
                                          sum(active_players) - 1, 500)
      current_hand <- get_best_hand(hole_cards[[i]], community_cards)
      
      decision <- make_decision(win_prob, pot, 0, stacks[i])  # Assume checking round
      
      cat(sprintf("%s: Current hand = %s, Win Probability = %.2f%%, Decision = %s\n", 
                  player_names[i], current_hand$description, win_prob * 100, decision))
      
      if (grepl("fold", decision)) {
        active_players[i] <- FALSE
      } else if (grepl("raise", decision)) {
        raise_amount <- as.numeric(strsplit(decision, " ")[[1]][2])
        stacks[i] <- stacks[i] - raise_amount
        pot <- pot + raise_amount
      }
    }
  }
  cat("Pot after flop:", pot, "\n\n")
  
  if (sum(active_players) <= 1) {
    cat("Hand ends at flop\n")
    return()
  }
  
  # Turn
  cat("=== TURN ===\n")
  community_cards <- rbind(community_cards, deck[1, ])
  deck <- deck[-1, ]
  
  cat("Community cards:", paste(community_cards$rank, "of", community_cards$suit, collapse = ", "), "\n\n")
  
  for (i in 1:num_players) {
    if (active_players[i]) {
      win_prob <- calculate_win_probability(hole_cards[[i]], community_cards, 
                                          sum(active_players) - 1, 500)
      current_hand <- get_best_hand(hole_cards[[i]], community_cards)
      
      decision <- make_decision(win_prob, pot, 0, stacks[i])
      
      cat(sprintf("%s: Current hand = %s, Win Probability = %.2f%%, Decision = %s\n", 
                  player_names[i], current_hand$description, win_prob * 100, decision))
      
      if (grepl("fold", decision)) {
        active_players[i] <- FALSE
      } else if (grepl("raise", decision)) {
        raise_amount <- as.numeric(strsplit(decision, " ")[[1]][2])
        stacks[i] <- stacks[i] - raise_amount
        pot <- pot + raise_amount
      }
    }
  }
  cat("Pot after turn:", pot, "\n\n")
  
  if (sum(active_players) <= 1) {
    cat("Hand ends at turn\n")
    return()
  }
  
  # River
  cat("=== RIVER ===\n")
  community_cards <- rbind(community_cards, deck[1, ])
  
  cat("Final community cards:", paste(community_cards$rank, "of", community_cards$suit, collapse = ", "), "\n\n")
  
  # Final showdown
  best_rank <- 0
  winner <- 0
  
  for (i in 1:num_players) {
    if (active_players[i]) {
      final_hand <- get_best_hand(hole_cards[[i]], community_cards)
      
      cat(sprintf("%s: Final hand = %s\n", player_names[i], final_hand$description))
      
      if (final_hand$rank > best_rank) {
        best_rank <- final_hand$rank
        winner <- i
      }
    }
  }
  
  cat(sprintf("\nWinner: %s wins pot of %d!\n", player_names[winner], pot))
}

# Run multiple hands
run_simulation <- function(num_hands = 3) {
  cat("Texas Hold'em Simulation with 4 Players\n")
  cat("========================================\n")
  
  for (hand in 1:num_hands) {
    cat("\n--- HAND", hand, "---")
    simulate_hand(4)
    cat("\n" , rep("=", 50), "\n")
  }
}

# Run the simulation
run_simulation(3)