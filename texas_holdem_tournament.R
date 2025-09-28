# Texas Hold'em Tournament Simulator
# Plays until one winner remains (elimination tournament)
# Reads configuration from external file for customizable gameplay

library(combinat)

# Function to read player names
read_player_names <- function(names_file = "texas_holdem_names.txt") {
  if (!file.exists(names_file)) {
    cat("Names file not found. Using default player names.\n")
    return(c("Alice", "Bob", "Charlie", "Diana", "Eve", "Frank", "Grace", "Henry", "Ivy", "Jack"))
  }
  
  lines <- readLines(names_file)
  names <- c()
  
  for(line in lines) {
    line <- trimws(line)
    if(!grepl("^#", line) && line != "") {
      names <- c(names, line)
    }
  }
  
  return(names)
}

# Function to read tournament configuration
read_tournament_config <- function(config_file = "tournament_config.txt") {
  if (!file.exists(config_file)) {
    cat("Tournament configuration file not found. Using default settings.\n")
    return(list(
      num_players = 4,
      starting_chips = 1000,
      small_blind = 5,
      big_blind = 10,
      blind_increase_hands = 10,  # Increase blinds every N hands
      blind_multiplier = 1.5,     # Multiply blinds by this factor
      max_hands = 200,            # Safety limit to prevent infinite games
      strategy = "balanced",
      show_detailed_decisions = TRUE,
      show_probabilities = TRUE,
      show_elimination = TRUE
    ))
  }
  
  config <- list()
  lines <- readLines(config_file)
  
  for(line in lines) {
    line <- trimws(line)
    if(grepl("^#", line) || line == "") next
    
    parts <- strsplit(line, "=")[[1]]
    if(length(parts) == 2) {
      key <- trimws(parts[1])
      value <- trimws(parts[2])
      
      # Remove any inline comments
      value <- trimws(strsplit(value, "#")[[1]][1])
      
      # Convert to appropriate type
      if(key %in% c("num_players", "starting_chips", "small_blind", "big_blind", 
                    "blind_increase_hands", "max_hands")) {
        config[[key]] <- as.numeric(value)
      } else if(key == "blind_multiplier") {
        config[[key]] <- as.numeric(value)
      } else if(key %in% c("show_detailed_decisions", "show_probabilities", "show_elimination")) {
        config[[key]] <- toupper(value) == "TRUE"
      } else {
        config[[key]] <- value
      }
    }
  }
  
  return(config)
}

# Create a deck of cards
create_deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  deck <- expand.grid(Rank = ranks, Suit = suits, stringsAsFactors = FALSE)
  deck$Value <- match(deck$Rank, ranks)
  return(deck[sample(nrow(deck)), ])
}

# Deal hole cards to active players
deal_hole_cards <- function(deck, active_players) {
  hole_cards <- list()
  card_index <- 1
  
  for(player in active_players) {
    hole_cards[[player]] <- deck[card_index:(card_index + 1), ]
    card_index <- card_index + 2
  }
  
  return(list(hole_cards = hole_cards, remaining_deck = deck[(card_index):nrow(deck), ]))
}

# Deal community cards
deal_community_cards <- function(deck, num_cards, start_index = 1) {
  return(deck[start_index:(start_index + num_cards - 1), ])
}

# Evaluate poker hand strength
evaluate_hand <- function(cards) {
  if (nrow(cards) < 5) return(list(rank = 0, name = "Incomplete"))
  
  # Convert to numeric values for easier comparison
  values <- cards$Value
  suits <- cards$Suit
  
  # Sort values in descending order
  sorted_values <- sort(values, decreasing = TRUE)
  
  # Count occurrences of each value
  value_counts <- table(values)
  sorted_counts <- sort(value_counts, decreasing = TRUE)
  
  # Check for flush
  suit_counts <- table(suits)
  is_flush <- any(suit_counts >= 5)
  
  # Check for straight
  unique_values <- sort(unique(values))
  is_straight <- FALSE
  straight_high <- 0
  
  if(length(unique_values) >= 5) {
    for(i in 1:(length(unique_values) - 4)) {
      if(all(diff(unique_values[i:(i+4)]) == 1)) {
        is_straight <- TRUE
        straight_high <- unique_values[i+4]
      }
    }
  }
  
  # Special case: A-2-3-4-5 straight (wheel)
  if(all(c(14, 2, 3, 4, 5) %in% unique_values)) {
    is_straight <- TRUE
    straight_high <- 5
  }
  
  # Determine hand ranking
  if(is_straight && is_flush) {
    return(list(rank = 8, name = "Straight Flush", high_card = straight_high))
  } else if(sorted_counts[1] == 4) {
    return(list(rank = 7, name = "Four of a Kind", high_card = as.numeric(names(sorted_counts)[1])))
  } else if(sorted_counts[1] == 3 && sorted_counts[2] == 2) {
    return(list(rank = 6, name = "Full House", high_card = as.numeric(names(sorted_counts)[1])))
  } else if(is_flush) {
    return(list(rank = 5, name = "Flush", high_card = max(values)))
  } else if(is_straight) {
    return(list(rank = 4, name = "Straight", high_card = straight_high))
  } else if(sorted_counts[1] == 3) {
    return(list(rank = 3, name = "Three of a Kind", high_card = as.numeric(names(sorted_counts)[1])))
  } else if(sorted_counts[1] == 2 && sorted_counts[2] == 2) {
    return(list(rank = 2, name = "Two Pair", high_card = max(as.numeric(names(sorted_counts)[1:2]))))
  } else if(sorted_counts[1] == 2) {
    return(list(rank = 1, name = "One Pair", high_card = as.numeric(names(sorted_counts)[1])))
  } else {
    return(list(rank = 0, name = "High Card", high_card = max(values)))
  }
}

# Calculate win probability using Monte Carlo simulation
calculate_win_probability <- function(hole_cards, community_cards, num_opponents, trials = 1000) {
  if(is.null(community_cards) || nrow(community_cards) == 0) {
    # Pre-flop calculation (simplified)
    high_card <- max(hole_cards$Value)
    is_pair <- hole_cards$Value[1] == hole_cards$Value[2]
    is_suited <- hole_cards$Suit[1] == hole_cards$Suit[2]
    
    base_prob <- 1 / (num_opponents + 1)
    
    if(is_pair) {
      if(high_card >= 10) return(min(0.8, base_prob * 3))
      else return(min(0.6, base_prob * 2))
    } else if(high_card >= 12) {
      return(min(0.7, base_prob * 2.5))
    } else if(is_suited && high_card >= 10) {
      return(min(0.6, base_prob * 2))
    } else {
      return(max(0.1, base_prob))
    }
  }
  
  # Post-flop exact calculation (simplified for performance)
  my_hand <- evaluate_hand(rbind(hole_cards, community_cards))
  
  # Estimate based on hand strength
  base_prob <- switch(as.character(my_hand$rank),
                     "0" = 0.1, "1" = 0.3, "2" = 0.5, "3" = 0.7,
                     "4" = 0.8, "5" = 0.85, "6" = 0.9, "7" = 0.95, "8" = 0.98)
  
  # Adjust for number of opponents
  adjusted_prob <- base_prob / (1 + num_opponents * 0.2)
  return(min(0.98, max(0.02, adjusted_prob)))
}

# Make betting decision based on hand strength and strategy
make_decision <- function(win_prob, current_bet, pot_size, chips, hand_rank, strategy) {
  pot_odds <- if(current_bet > 0) current_bet / (pot_size + current_bet) else 0
  
  # Strategy-based thresholds
  thresholds <- switch(strategy,
    "tight" = list(fold = 0.3, call = 0.6, raise = 0.8),
    "loose" = list(fold = 0.15, call = 0.4, raise = 0.7),
    "aggressive" = list(fold = 0.2, call = 0.45, raise = 0.6),
    "conservative" = list(fold = 0.35, call = 0.65, raise = 0.85),
    "balanced" = list(fold = 0.25, call = 0.5, raise = 0.75)
  )
  
  # All-in threshold for tournament play
  chip_ratio <- chips / (pot_size + current_bet)
  
  if(win_prob < thresholds$fold || chip_ratio < 3) {
    return("fold")
  } else if(win_prob < thresholds$call) {
    return("call")
  } else if(win_prob < thresholds$raise) {
    return("call")
  } else {
    if(chip_ratio < 5) return("all_in")
    return("raise")
  }
}

# Play one tournament hand
play_tournament_hand <- function(players, small_blind, big_blind, strategy, dealer_pos) {
  active_players <- which(players$active & players$chips > 0)
  
  if(length(active_players) < 2) {
    return(list(players = players, next_dealer = dealer_pos))
  }
  
  # Create deck and deal cards
  deck <- create_deck()
  card_deal <- deal_hole_cards(deck, active_players)
  hole_cards <- card_deal$hole_cards
  deck <- card_deal$remaining_deck
  
  # Initialize pot and bets
  pot <- 0
  player_bets <- rep(0, nrow(players))
  
  # Post blinds
  sb_player <- active_players[(which(active_players == dealer_pos) %% length(active_players)) + 1]
  bb_player <- active_players[(which(active_players == dealer_pos) + 1) %% length(active_players) + 1]
  
  # Ensure blinds don't exceed chip stacks
  sb_amount <- min(small_blind, players$chips[sb_player])
  bb_amount <- min(big_blind, players$chips[bb_player])
  
  players$chips[sb_player] <- players$chips[sb_player] - sb_amount
  players$chips[bb_player] <- players$chips[bb_player] - bb_amount
  player_bets[sb_player] <- sb_amount
  player_bets[bb_player] <- bb_amount
  pot <- sb_amount + bb_amount
  
  cat(sprintf("Blinds posted: %s (SB) = %d, %s (BB) = %d\n", 
              players$name[sb_player], sb_amount, players$name[bb_player], bb_amount))
  cat("Starting pot:", pot, "\n\n")
  
  # Show hole cards
  for(player in active_players) {
    cards_str <- paste(apply(hole_cards[[player]], 1, function(x) paste(x[1], "of", x[2])), collapse = ", ")
    cat(sprintf("%s hole cards: %s\n", players$name[player], cards_str))
  }
  cat("\n")
  
  # Pre-flop betting
  cat("=== PRE-FLOP ===\n")
  for(player in active_players) {
    if(players$chips[player] > 0) {
      win_prob <- calculate_win_probability(hole_cards[[player]], NULL, length(active_players) - 1)
      hand_eval <- evaluate_hand(hole_cards[[player]])
      decision <- make_decision(win_prob, 0, pot, players$chips[player], hand_eval$rank, strategy)
      
      cat(sprintf("%s: Win Probability = %.2f%%, Decision = %s\n", 
                  players$name[player], win_prob * 100, decision))
      
      if(decision == "fold") {
        active_players <- active_players[active_players != player]
      }
    }
  }
  
  if(length(active_players) <= 1) {
    winner <- if(length(active_players) == 1) active_players[1] else bb_player
    players$chips[winner] <- players$chips[winner] + pot
    players$hands_won[winner] <- players$hands_won[winner] + 1
    cat(sprintf("\nWinner: Player %d wins pot of %d!\n", winner, pot))
    
    next_dealer <- (dealer_pos %% nrow(players)) + 1
    return(list(players = players, next_dealer = next_dealer))
  }
  
  # Flop
  if(length(active_players) > 1) {
    community_cards <- deal_community_cards(deck, 3)
    cat(sprintf("Pot after pre-flop: %d\n\n", pot))
    cat("=== FLOP ===\n")
    cat("Community cards:", paste(apply(community_cards, 1, function(x) paste(x[1], "of", x[2])), collapse = ", "), "\n\n")
    
    for(player in active_players) {
      combined_cards <- rbind(hole_cards[[player]], community_cards)
      hand_strength <- evaluate_hand(combined_cards)
      win_prob <- calculate_win_probability(hole_cards[[player]], community_cards, length(active_players) - 1)
      decision <- make_decision(win_prob, 0, pot, players$chips[player], hand_strength$rank, strategy)
      
      cat(sprintf("Player %d: Current hand = %s, Win Probability = %.2f%%, Decision = %s\n", 
                  player, hand_strength$name, win_prob * 100, decision))
      
      if(decision == "fold") {
        active_players <- active_players[active_players != player]
      }
    }
  }
  
  # Turn
  if(length(active_players) > 1) {
    turn_card <- deal_community_cards(deck, 1, 4)
    community_cards <- rbind(community_cards, turn_card)
    cat(sprintf("Pot after flop: %d\n\n", pot))
    cat("=== TURN ===\n")
    cat("Community cards:", paste(apply(community_cards, 1, function(x) paste(x[1], "of", x[2])), collapse = ", "), "\n\n")
    
    for(player in active_players) {
      combined_cards <- rbind(hole_cards[[player]], community_cards)
      hand_strength <- evaluate_hand(combined_cards)
      win_prob <- calculate_win_probability(hole_cards[[player]], community_cards, length(active_players) - 1)
      decision <- make_decision(win_prob, 0, pot, players$chips[player], hand_strength$rank, strategy)
      
      cat(sprintf("Player %d: Current hand = %s, Win Probability = %.2f%%, Decision = %s\n", 
                  player, hand_strength$name, win_prob * 100, decision))
      
      if(decision == "fold") {
        active_players <- active_players[active_players != player]
      }
    }
  }
  
  # Determine winner
  winner <- 0
  
  # River
  if(length(active_players) > 1) {
    river_card <- deal_community_cards(deck, 1, 5)
    community_cards <- rbind(community_cards, river_card)
    cat(sprintf("Pot after turn: %d\n\n", pot))
    cat("=== RIVER ===\n")
    cat("Final community cards:", paste(apply(community_cards, 1, function(x) paste(x[1], "of", x[2])), collapse = ", "), "\n\n")
    
    # Final showdown
    best_hand_rank <- -1
    best_high_card <- 0
    
    for(player in active_players) {
      combined_cards <- rbind(hole_cards[[player]], community_cards)
      hand_strength <- evaluate_hand(combined_cards)
      
      cat(sprintf("%s: Final hand = %s\n", players$name[player], hand_strength$name))
      
      if(hand_strength$rank > best_hand_rank || 
         (hand_strength$rank == best_hand_rank && hand_strength$high_card > best_high_card)) {
        best_hand_rank <- hand_strength$rank
        best_high_card <- hand_strength$high_card
        winner <- player
      }
    }
  } else if(length(active_players) == 1) {
    winner <- active_players[1]
  } else {
    # All players folded, big blind wins
    winner <- bb_player
  }
  
  # Award pot to winner
  if(winner > 0) {
    players$chips[winner] <- players$chips[winner] + pot
    players$hands_won[winner] <- players$hands_won[winner] + 1
    cat(sprintf("\nWinner: Player %d wins pot of %d!\n", winner, pot))
  }
  
  next_dealer <- (dealer_pos %% nrow(players)) + 1
  return(list(players = players, next_dealer = next_dealer))
}

# Main tournament simulation
tournament_simulation <- function(config_file = "tournament_config.txt") {
  config <- read_tournament_config(config_file)
  player_names <- read_player_names()
  
  # Randomly select names for this tournament
  selected_names <- sample(player_names, config$num_players, replace = FALSE)
  
  cat("🏆 TEXAS HOLD'EM TOURNAMENT 🏆\n")
  cat("===============================\n")
  cat("Players:", config$num_players, "\n")
  cat("Contestants:", paste(selected_names, collapse = ", "), "\n")
  cat("Starting chips:", config$starting_chips, "each\n")
  cat("Starting blinds:", config$small_blind, "/", config$big_blind, "\n")
  cat("Strategy:", config$strategy, "\n")
  cat("===============================\n\n")
  
  # Initialize players with names
  players <- data.frame(
    player = 1:config$num_players,
    name = selected_names,
    chips = rep(config$starting_chips, config$num_players),
    hands_won = rep(0, config$num_players),
    active = rep(TRUE, config$num_players),
    stringsAsFactors = FALSE
  )
  
  hand_num <- 0
  dealer_position <- 1
  current_sb <- config$small_blind
  current_bb <- config$big_blind
  
  # Tournament loop - continue until one player remains
  while(sum(players$active) > 1 && hand_num < config$max_hands) {
    hand_num <- hand_num + 1
    
    # Increase blinds periodically
    if(hand_num %% config$blind_increase_hands == 0 && hand_num > 0) {
      current_sb <- ceiling(current_sb * config$blind_multiplier)
      current_bb <- ceiling(current_bb * config$blind_multiplier)
      cat(sprintf("🔔 BLINDS INCREASED: %d/%d (Hand %d)\n\n", current_sb, current_bb, hand_num))
    }
    
    # Remove eliminated players
    players$active[players$chips <= 0] <- FALSE
    active_count <- sum(players$active)
    
    if(active_count <= 1) break
    
    cat(sprintf("--- TOURNAMENT HAND %d ---\n", hand_num))
    cat(sprintf("Active players: %d\n", active_count))
    cat(sprintf("Blinds: %d/%d\n", current_sb, current_bb))
    
    # Find next active dealer
    while(!players$active[dealer_position]) {
      dealer_position <- (dealer_position %% nrow(players)) + 1
    }
    
    # Play the hand
    result <- play_tournament_hand(players, current_sb, current_bb, config$strategy, dealer_position)
    players <- result$players
    dealer_position <- result$next_dealer
    
    # Show chip standings
    active_players <- players[players$active, ]
    if(nrow(active_players) > 1) {
      cat("\n💰 CHIP STANDINGS:\n")
      active_players <- active_players[order(-active_players$chips), ]
      for(i in 1:nrow(active_players)) {
        cat(sprintf("%s: %d chips\n", active_players$name[i], active_players$chips[i]))
      }
      
      # Show eliminated players
      eliminated <- players[!players$active, ]
      if(nrow(eliminated) > 0) {
        cat("\n❌ ELIMINATED:\n")
        for(i in 1:nrow(eliminated)) {
          cat(sprintf("%s: ELIMINATED (Hand %d)\n", eliminated$name[i], hand_num))
        }
      }
    }
    
    cat("\n", paste(rep("=", 80), collapse = ""), "\n\n")
  }
  
  # Tournament results
  final_active <- players[players$active, ]
  
  if(nrow(final_active) == 1) {
    winner <- final_active[1, ]
    cat("🏆🏆🏆 TOURNAMENT CHAMPION! 🏆🏆🏆\n")
    cat("=====================================\n")
    cat(sprintf("🥇 %s WINS THE TOURNAMENT! 🥇\n", winner$name))
    cat(sprintf("Final chips: %d\n", winner$chips))
    cat(sprintf("Hands won: %d\n", winner$hands_won))
    cat(sprintf("Total hands played: %d\n", hand_num))
    
    if(hand_num < config$max_hands) {
      cat("Tournament completed by elimination!\n")
    } else {
      cat("Tournament ended at hand limit.\n")
    }
  } else {
    cat("TOURNAMENT INCOMPLETE\n")
    cat("=====================\n")
    cat("Multiple players still active at hand limit.\n")
  }
  
  cat("\n🏅 FINAL TOURNAMENT STANDINGS 🏅\n")
  cat("=================================\n")
  
  # Sort all players by chips (winners first, then by chips)
  all_players <- players[order(-players$active, -players$chips), ]
  
  position <- 1
  for(i in 1:nrow(all_players)) {
    player <- all_players[i, ]
    status <- if(player$active) "🏆 WINNER" else "❌ ELIMINATED"
    
    cat(sprintf("%d. %s: %d chips, %d hands won (%s)\n", 
                position, player$name, player$chips, 
                player$hands_won, status))
    position <- position + 1
  }
  
  cat(sprintf("\nTournament Duration: %d hands\n", hand_num))
  cat(sprintf("Total Prize Pool: %s chips\n", 
              format(config$num_players * config$starting_chips, big.mark = ",")))
  
  return(all_players)
}

# Run the tournament
args <- commandArgs(trailingOnly = TRUE)
config_file <- if(length(args) > 0) args[1] else "tournament_config.txt"
tournament_simulation(config_file)