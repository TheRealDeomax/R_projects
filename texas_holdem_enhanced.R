# Texas Hold'em Poker Simulator with Configuration Support
# Reads parameters from game_config.txt and provides comprehensive game tracking

library(combinat)

# Enhanced Texas Hold'em Poker Tournament Simulator
# Plays until one winner remains (tournament style)
# Reads configuration from external file for customizable gameplay

library(combinat)

# Function to read configuration from file
read_config <- function(config_file = "game_config.txt") {
  if (!file.exists(config_file)) {
    cat("Configuration file not found. Using default settings.\n")
    return(list(
      num_players = 4,
      max_hands = 100,  # Maximum hands to prevent infinite games
      starting_chips = 1000,
      small_blind = 5,
      big_blind = 10,
      strategy = "conservative",
      tournament_mode = TRUE
    ))
  }
  
  config <- list()
  lines <- readLines(config_file)
  
  for(line in lines) {
    if(grepl("^#", line) || line == "") next  # Skip comments and empty lines
    
    parts <- strsplit(line, "=")[[1]]
    if(length(parts) == 2) {
      key <- trimws(parts[1])
      value <- trimws(parts[2])
      
      # Convert to appropriate type
      if(key %in% c("num_players", "max_hands", "starting_chips", "small_blind", "big_blind")) {
        config[[key]] <- as.numeric(value)
      } else if(key == "tournament_mode") {
        config[[key]] <- as.logical(value)
      } else {
        config[[key]] <- value
      }
    }
  }
  
  return(config)
}
read_config <- function(config_file = "game_config.txt") {
  if (!file.exists(config_file)) {
    stop("Configuration file not found: ", config_file)
  }
  
  config <- list()
  lines <- readLines(config_file)
  
  for (line in lines) {
    # Skip comments and empty lines
    if (grepl("^#", line) || grepl("^\\s*$", line)) next
    
    # Parse key=value pairs
    if (grepl("=", line)) {
      parts <- strsplit(line, "=")[[1]]
      key <- trimws(parts[1])
      value <- trimws(parts[2])
      
      # Convert to appropriate type
      if (key %in% c("num_players", "num_hands", "small_blind", "big_blind")) {
        config[[key]] <- as.numeric(value)
      } else if (key %in% c("show_hole_cards", "show_probabilities", "show_detailed_decisions", "show_final_summary")) {
        config[[key]] <- as.logical(value)
      } else if (key == "player_strategies") {
        config[[key]] <- trimws(strsplit(value, ",")[[1]])
      } else {
        config[[key]] <- as.numeric(value)
      }
    }
  }
  
  return(config)
}

# Initialize deck
create_deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  rank_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
  
  deck <- expand.grid(suit = suits, rank = ranks, stringsAsFactors = FALSE)
  deck$value <- rep(rank_values, 4)
  return(deck[sample(nrow(deck)), ])
}

# Deal hole cards to players
deal_hole_cards <- function(deck, num_players) {
  hole_cards <- list()
  for (i in 1:num_players) {
    hole_cards[[i]] <- matrix(c(deck$rank[i*2-1], deck$suit[i*2-1],
                               deck$rank[i*2], deck$suit[i*2]), 
                             nrow = 2, ncol = 2, byrow = TRUE)
    colnames(hole_cards[[i]]) <- c("rank", "suit")
  }
  return(hole_cards)
}

# Deal community cards
deal_community_cards <- function(deck, num_cards, start_index = 9) {
  cards <- matrix(nrow = num_cards, ncol = 2)
  for (i in 1:num_cards) {
    cards[i, ] <- c(deck$rank[start_index + i - 1], deck$suit[start_index + i - 1])
  }
  colnames(cards) <- c("rank", "suit")
  return(cards)
}

# Hand evaluation
evaluate_hand <- function(cards) {
  # Convert to data frame for easier processing
  if (is.matrix(cards)) {
    df <- data.frame(
      rank = cards[, "rank"],
      suit = cards[, "suit"],
      stringsAsFactors = FALSE
    )
  } else {
    df <- cards
  }
  
  # Add numeric values
  rank_values <- c("2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, 
                   "10"=10, "J"=11, "Q"=12, "K"=13, "A"=14)
  df$value <- rank_values[df$rank]
  
  # Find best 5-card hand
  if (nrow(df) == 7) {
    # Try all combinations of 5 cards
    best_hand <- list(rank = 0, name = "High Card")
    for (combo in combn(1:7, 5, simplify = FALSE)) {
      hand <- evaluate_5_cards(df[combo, ])
      if (hand$rank > best_hand$rank) {
        best_hand <- hand
      }
    }
    return(best_hand)
  } else {
    return(evaluate_5_cards(df))
  }
}

evaluate_5_cards <- function(cards) {
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
    if (all(sort(values) == c(2, 3, 4, 5, 14))) {
      is_straight <- TRUE
    }
  }
  
  # Count occurrences
  value_counts <- table(values)
  counts <- sort(as.numeric(value_counts), decreasing = TRUE)
  
  # Determine hand rank
  if (is_straight && is_flush) {
    return(list(rank = 8, name = "Straight Flush"))
  } else if (counts[1] == 4) {
    return(list(rank = 7, name = "Four of a Kind"))
  } else if (counts[1] == 3 && counts[2] == 2) {
    return(list(rank = 6, name = "Full House"))
  } else if (is_flush) {
    return(list(rank = 5, name = "Flush"))
  } else if (is_straight) {
    return(list(rank = 4, name = "Straight"))
  } else if (counts[1] == 3) {
    return(list(rank = 3, name = "Three of a Kind"))
  } else if (counts[1] == 2 && counts[2] == 2) {
    return(list(rank = 2, name = "Two Pair"))
  } else if (counts[1] == 2) {
    return(list(rank = 1, name = "One Pair"))
  } else {
    return(list(rank = 0, name = "High Card"))
  }
}

# Calculate pre-flop win probability (Monte Carlo simulation)
calculate_preflop_probability <- function(hole_cards, num_players, simulations = 1000) {
  wins <- 0
  
  for (sim in 1:simulations) {
    # Create fresh deck without hole cards
    deck <- create_deck()
    hole_ranks <- hole_cards[, "rank"]
    hole_suits <- hole_cards[, "suit"]
    
    # Remove known cards from deck
    for (i in 1:2) {
      deck <- deck[!(deck$rank == hole_ranks[i] & deck$suit == hole_suits[i]), ]
    }
    
    # Deal community cards
    community <- deck[1:5, ]
    
    # Evaluate our hand
    our_cards <- rbind(
      data.frame(rank = hole_ranks, suit = hole_suits, value = c(0, 0)),
      community
    )
    our_hand <- evaluate_hand(our_cards)
    
    # Simulate other players' hands
    better_hands <- 0
    remaining_deck <- deck[6:nrow(deck), ]
    
    for (player in 2:num_players) {
      if (nrow(remaining_deck) >= 2) {
        opp_hole <- remaining_deck[1:2, ]
        remaining_deck <- remaining_deck[3:nrow(remaining_deck), ]
        
        opp_cards <- rbind(opp_hole, community)
        opp_hand <- evaluate_hand(opp_cards)
        
        if (opp_hand$rank > our_hand$rank) {
          better_hands <- better_hands + 1
        }
      }
    }
    
    if (better_hands == 0) {
      wins <- wins + 1
    }
  }
  
  return(wins / simulations)
}

# Calculate post-flop win probability
calculate_win_probability <- function(hole_cards, community_cards, num_opponents, simulations = 1000) {
  wins <- 0
  
  # Create deck without known cards
  deck <- create_deck()
  hole_ranks <- hole_cards[, "rank"]
  hole_suits <- hole_cards[, "suit"]
  
  # Remove hole cards from deck
  for (i in 1:2) {
    deck <- deck[!(deck$rank == hole_ranks[i] & deck$suit == hole_suits[i]), ]
  }
  
  # Remove community cards from deck
  for (i in 1:nrow(community_cards)) {
    deck <- deck[!(deck$rank == community_cards[i, "rank"] & 
                  deck$suit == community_cards[i, "suit"]), ]
  }
  
  cards_needed <- 5 - nrow(community_cards)
  
  for (sim in 1:simulations) {
    # Complete the community cards if needed
    remaining_community <- community_cards
    if (cards_needed > 0) {
      new_cards <- deck[sample(nrow(deck), cards_needed), ]
      remaining_community <- rbind(community_cards, 
                                  matrix(c(new_cards$rank, new_cards$suit), 
                                        nrow = cards_needed, ncol = 2))
    }
    
    # Evaluate our hand
    our_cards <- rbind(
      matrix(c(hole_ranks, hole_suits), nrow = 2, ncol = 2),
      remaining_community
    )
    our_hand <- evaluate_hand(our_cards)
    
    # Simulate opponents
    better_hands <- 0
    available_cards <- deck
    if (cards_needed > 0) {
      available_cards <- available_cards[!available_cards$rank %in% new_cards$rank | 
                                        !available_cards$suit %in% new_cards$suit, ]
    }
    
    for (opp in 1:num_opponents) {
      if (nrow(available_cards) >= 2) {
        opp_indices <- sample(nrow(available_cards), 2)
        opp_hole <- available_cards[opp_indices, ]
        available_cards <- available_cards[-opp_indices, ]
        
        opp_cards <- rbind(
          matrix(c(opp_hole$rank, opp_hole$suit), nrow = 2, ncol = 2),
          remaining_community
        )
        opp_hand <- evaluate_hand(opp_cards)
        
        if (opp_hand$rank > our_hand$rank) {
          better_hands <- better_hands + 1
        }
      }
    }
    
    if (better_hands == 0) {
      wins <- wins + 1
    }
  }
  
  return(wins / simulations)
}

# Player decision making based on strategy
make_decision <- function(win_prob, current_bet, pot_size, player_chips, hand_strength, strategy = "moderate", config) {
  # Get strategy-specific thresholds from config
  fold_threshold <- config[[paste0(strategy, "_fold_threshold")]]
  raise_threshold <- config[[paste0(strategy, "_raise_threshold")]]
  
  # Calculate pot odds
  pot_odds <- ifelse(current_bet > 0, current_bet / (pot_size + current_bet), 0)
  
  if (win_prob < fold_threshold) {
    return("fold")
  } else if (win_prob > raise_threshold) {
    return("raise")
  } else if (win_prob > pot_odds && current_bet > 0) {
    return("call")
  } else {
    return("check")
  }
}

# Simulate a complete hand
simulate_hand <- function(config, hand_number) {
  deck <- create_deck()
  num_players <- config$num_players
  
  # Deal hole cards
  hole_cards <- deal_hole_cards(deck, num_players)
  
  # Post blinds
  small_blind <- config$small_blind
  big_blind <- config$big_blind
  pot <- small_blind + big_blind
  
  if (config$show_detailed_decisions) {
    cat("=== NEW HAND", hand_number, "===", "\n")
    cat("Blinds posted: Small blind =", small_blind, ", Big blind =", big_blind, "\n")
    cat("Starting pot:", pot, "\n\n")
  }
  
  # Show hole cards if configured
  if (config$show_hole_cards) {
    for (i in 1:num_players) {
      cat("Player", i, "(", config$player_strategies[i], "strategy) hole cards:", 
          paste(hole_cards[[i]][1,], collapse = " of "), ",", 
          paste(hole_cards[[i]][2,], collapse = " of "), "\n")
    }
    cat("\n")
  }
  
  active_players <- 1:num_players
  
  # Pre-flop betting
  if (config$show_detailed_decisions) cat("=== PRE-FLOP ===", "\n")
  for (i in active_players) {
    win_prob <- calculate_preflop_probability(hole_cards[[i]], num_players)
    strategy <- config$player_strategies[i]
    decision <- make_decision(win_prob, 0, pot, 1000, 0, strategy, config)
    
    if (config$show_probabilities) {
      cat("Player", i, "(", strategy, "): Win Probability =", sprintf("%.2f%%", win_prob * 100), 
          ", Decision =", decision, "\n")
    }
    
    if (decision == "fold") {
      active_players <- active_players[active_players != i]
    }
  }
  
  # Flop
  if (length(active_players) > 1) {
    community_cards <- deal_community_cards(deck, 3)
    if (config$show_detailed_decisions) {
      cat("Pot after pre-flop:", pot, "\n\n")
      cat("=== FLOP ===", "\n")
      cat("Community cards:", paste(apply(community_cards, 1, function(x) paste(x, collapse = " of ")), collapse = ", "), "\n\n")
    }
    
    for (i in active_players) {
      combined_cards <- rbind(hole_cards[[i]], community_cards)
      hand_strength <- evaluate_hand(combined_cards)
      win_prob <- calculate_win_probability(hole_cards[[i]], community_cards, length(active_players) - 1)
      strategy <- config$player_strategies[i]
      decision <- make_decision(win_prob, 0, pot, 1000, hand_strength$rank, strategy, config)
      
      if (config$show_probabilities) {
        cat("Player", i, "(", strategy, "): Current hand =", hand_strength$name, 
            ", Win Probability =", sprintf("%.2f%%", win_prob * 100), 
            ", Decision =", decision, "\n")
      }
      
      if (decision == "fold") {
        active_players <- active_players[active_players != i]
      }
    }
  }
  
  # Turn
  if (length(active_players) > 1) {
    turn_card <- deal_community_cards(deck, 1, 12)
    community_cards <- rbind(community_cards, turn_card)
    if (config$show_detailed_decisions) {
      cat("Pot after flop:", pot, "\n\n")
      cat("=== TURN ===", "\n")
      cat("Community cards:", paste(apply(community_cards, 1, function(x) paste(x, collapse = " of ")), collapse = ", "), "\n\n")
    }
    
    for (i in active_players) {
      combined_cards <- rbind(hole_cards[[i]], community_cards)
      hand_strength <- evaluate_hand(combined_cards)
      win_prob <- calculate_win_probability(hole_cards[[i]], community_cards, length(active_players) - 1)
      strategy <- config$player_strategies[i]
      decision <- make_decision(win_prob, 0, pot, 1000, hand_strength$rank, strategy, config)
      
      if (config$show_probabilities) {
        cat("Player", i, "(", strategy, "): Current hand =", hand_strength$name, 
            ", Win Probability =", sprintf("%.2f%%", win_prob * 100), 
            ", Decision =", decision, "\n")
      }
      
      if (decision == "fold") {
        active_players <- active_players[active_players != i]
      }
    }
  }
  
  # River
  if (length(active_players) > 1) {
    river_card <- deal_community_cards(deck, 1, 13)
    community_cards <- rbind(community_cards, river_card)
    if (config$show_detailed_decisions) {
      cat("Pot after turn:", pot, "\n\n")
      cat("=== RIVER ===", "\n")
      cat("Final community cards:", paste(apply(community_cards, 1, function(x) paste(x, collapse = " of ")), collapse = ", "), "\n\n")
    }
  }
  
  # Final showdown
  winner <- NA
  if (length(active_players) > 1) {
    best_hands <- list()
    for (i in active_players) {
      combined_cards <- rbind(hole_cards[[i]], community_cards)
      best_hands[[as.character(i)]] <- evaluate_hand(combined_cards)
      if (config$show_detailed_decisions) {
        cat("Player", i, ": Final hand =", best_hands[[as.character(i)]]$name, "\n")
      }
    }
    
    # Determine winner
    winner <- active_players[1]
    best_rank <- best_hands[[as.character(winner)]]$rank
    
    for (i in active_players[-1]) {
      if (best_hands[[as.character(i)]]$rank > best_rank) {
        winner <- i
        best_rank <- best_hands[[as.character(i)]]$rank
      }
    }
    
    cat("Winner: Player", winner, "wins pot of", pot, "!\n")
  } else if (length(active_players) == 1) {
    winner <- active_players[1]
    cat("Winner: Player", winner, "wins pot of", pot, "by default!\n")
  }
  
  return(list(winner = winner, pot = pot))
}

# Main simulation with comprehensive tracking
main_simulation <- function(config_file = "game_config.txt") {
  tryCatch({
    # Read configuration
    config <- read_config(config_file)
    
    # Validate configuration
    if (length(config$player_strategies) != config$num_players) {
      stop("Number of player strategies must match number of players")
    }
    
    # Initialize game statistics
    game_stats <- list(
      player_wins = rep(0, config$num_players),
      player_earnings = rep(0, config$num_players),
      total_pots = c()
    )
    names(game_stats$player_wins) <- paste("Player", 1:config$num_players)
    names(game_stats$player_earnings) <- paste("Player", 1:config$num_players)
    
    cat("Texas Hold'em Simulation", "\n")
    cat("========================", "\n")
    cat("Players:", config$num_players, "\n")
    cat("Hands:", config$num_hands, "\n")
    cat("Strategies:", paste(config$player_strategies, collapse = ", "), "\n")
    cat("Blinds:", config$small_blind, "/", config$big_blind, "\n\n")
    
    # Simulate multiple hands
    for (hand in 1:config$num_hands) {
      if (config$show_detailed_decisions) {
        cat("--- HAND", hand, "---", "\n")
      }
      
      result <- simulate_hand(config, hand)
      
      # Update statistics
      if (!is.na(result$winner)) {
        game_stats$player_wins[result$winner] <- game_stats$player_wins[result$winner] + 1
        game_stats$player_earnings[result$winner] <- game_stats$player_earnings[result$winner] + result$pot
        game_stats$total_pots <- c(game_stats$total_pots, result$pot)
      }
      
      if (config$show_detailed_decisions && hand < config$num_hands) {
        cat("\n", paste(rep("=", 80), collapse = ""), "\n\n")
      }
    }
    
    # Final summary
    if (config$show_final_summary) {
      cat("\n", paste(rep("*", 50), collapse = ""), "\n")
      cat("FINAL RESULTS", "\n")
      cat(paste(rep("*", 50), collapse = ""), "\n\n")
      
      cat("WINS BY PLAYER:", "\n")
      for (i in 1:config$num_players) {
        win_pct <- round(game_stats$player_wins[i] / config$num_hands * 100, 1)
        cat(sprintf("Player %d (%s): %d wins (%.1f%%)\n", 
                    i, config$player_strategies[i], game_stats$player_wins[i], win_pct))
      }
      
      cat("\nEARNIGS BY PLAYER:", "\n")
      for (i in 1:config$num_players) {
        cat(sprintf("Player %d (%s): %d chips\n", 
                    i, config$player_strategies[i], game_stats$player_earnings[i]))
      }
      
      cat("\nGAME STATISTICS:", "\n")
      cat("Total hands played:", config$num_hands, "\n")
      if (length(game_stats$total_pots) > 0) {
        cat("Average pot size:", round(mean(game_stats$total_pots), 1), "\n")
        cat("Total chips in play:", sum(game_stats$player_earnings), "\n")
      }
      
      # Strategy performance
      strategies <- unique(config$player_strategies)
      cat("\nSTRATEGY PERFORMANCE:", "\n")
      for (strategy in strategies) {
        strategy_players <- which(config$player_strategies == strategy)
        strategy_wins <- sum(game_stats$player_wins[strategy_players])
        strategy_earnings <- sum(game_stats$player_earnings[strategy_players])
        total_strategy_hands <- length(strategy_players) * config$num_hands
        win_rate <- round(strategy_wins / total_strategy_hands * 100, 1)
        
        cat(sprintf("%s strategy: %d wins, %d earnings (%.1f%% win rate)\n", 
                    tools::toTitleCase(strategy), strategy_wins, strategy_earnings, win_rate))
      }
    }
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    cat("Make sure game_config.txt exists and is properly formatted.\n")
  })
}

# Run the simulation with command line argument support
args <- commandArgs(trailingOnly = TRUE)
config_file <- if (length(args) > 0) args[1] else "game_config.txt"
main_simulation(config_file)