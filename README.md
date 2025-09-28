# 🎰 Texas Hold'em Tournament Simulator

A comprehensive R-based Texas Hold'em poker tournament simulator with realistic probability calculations, strategic decision-making, and elimination-style gameplay.

## 🏆 Features

### 🎮 Tournament Mode
- **Elimination-style gameplay** that continues until one winner remains
- **Progressive blind increases** to maintain tournament pressure
- **Real-time chip tracking** and player elimination
- **Configurable tournament parameters** via external configuration files

### 🃏 Poker Mechanics
- **Complete Texas Hold'em implementation** with proper hand evaluation
- **Monte Carlo probability calculations** for pre-flop scenarios
- **Exact probability calculations** for post-flop situations
- **Strategic decision making** based on hand strength, pot odds, and player strategies

### 🎯 Player Strategies
- **Multiple strategy types**: tight, loose, aggressive, conservative, balanced
- **Realistic folding behavior** based on probability thresholds
- **Dynamic betting decisions** considering chip stack and tournament position

### 👥 Player Personalization
- **Named players** with a pool of 50+ diverse first names
- **Random name assignment** for each tournament
- **Comprehensive game statistics** tracking wins, eliminations, and performance

## 🚀 Quick Start

### Prerequisites
- R (version 3.6 or higher)
- `combinat` package

### Installation
```bash
# Clone the repository
git clone https://github.com/TheRealDeomax/R_projects.git
cd R_projects

# Install required R package
R -e "install.packages('combinat', repos='https://cran.r-project.org')"
```

### Running a Tournament
```bash
# Standard 4-player tournament
Rscript texas_holdem_tournament.R

# High-stakes 6-player tournament
Rscript texas_holdem_tournament.R high_stakes_tournament.txt
```

## ⚙️ Configuration

### Tournament Settings (`tournament_config.txt`)
```
num_players = 4                    # Number of players (2-10)
starting_chips = 1000              # Starting chip stack
small_blind = 5                    # Initial small blind
big_blind = 10                     # Initial big blind
blind_increase_hands = 10          # Hands between blind increases
blind_multiplier = 1.5             # Blind increase factor
max_hands = 200                    # Safety limit
strategy = balanced                # Player strategy
show_detailed_decisions = TRUE     # Display detailed gameplay
show_probabilities = TRUE          # Show win probabilities
show_elimination = TRUE            # Show elimination messages
```

### Player Names (`texas_holdem_names.txt`)
- Pool of 50+ diverse first names
- Automatically assigned to players each tournament
- Easy to customize by editing the names file

## 📊 Sample Output

```
🏆 TEXAS HOLD'EM TOURNAMENT 🏆
===============================
Players: 4
Contestants: Grace, Quinn, Ethan, Ivan
Starting chips: 1000 each
Starting blinds: 5 / 10
Strategy: balanced
===============================

--- TOURNAMENT HAND 1 ---
Active players: 4
Blinds: 5/10
Blinds posted: Quinn (SB) = 5, Ethan (BB) = 10

Grace hole cards: 7 of Diamonds, 9 of Clubs
Quinn hole cards: Q of Hearts, A of Spades

=== PRE-FLOP ===
Grace: Win Probability = 25.00%, Decision = call
Quinn: Win Probability = 62.50%, Decision = call

🏅 FINAL TOURNAMENT STANDINGS 🏅
=================================
1. Alex: 4000 chips, 42 hands won (🏆 WINNER)
2. Winter: 0 chips, 7 hands won (❌ ELIMINATED)
3. Hunter: 0 chips, 10 hands won (❌ ELIMINATED)
4. Ivan: 0 chips, 37 hands won (❌ ELIMINATED)
```

## 🎲 Game Mechanics

### Hand Evaluation
- **Complete poker hand rankings**: High Card through Straight Flush
- **Accurate tie-breaking** based on high cards and kickers
- **Proper handling** of community cards and hole cards

### Probability Calculations
- **Pre-flop**: Monte Carlo simulation considering hole cards
- **Post-flop**: Exact calculations using remaining deck combinations
- **Dynamic updates** as community cards are revealed

### Decision Making
- **Strategy-based thresholds** for fold/call/raise decisions
- **Pot odds consideration** in betting decisions
- **Tournament dynamics** including all-in scenarios and short stacks

## 📁 File Structure

```
R_projects/
├── texas_holdem_tournament.R      # Main tournament simulator
├── texas_holdem_enhanced.R        # Alternative version with detailed stats
├── texas_holdem_simulator.R       # Original basic version
├── tournament_config.txt          # Standard tournament configuration
├── high_stakes_tournament.txt     # High-stakes tournament configuration
├── texas_holdem_names.txt         # Player names pool
└── README.md                      # This file
```

## 🛠️ Customization

### Adding New Strategies
Modify the `make_decision()` function to add new strategic behaviors:

```r
thresholds <- switch(strategy,
  "your_strategy" = list(fold = 0.2, call = 0.5, raise = 0.8),
  # ... existing strategies
)
```

### Tournament Variants
Create new configuration files for different tournament styles:
- **Turbo tournaments**: Faster blind increases
- **Deep stack**: Higher starting chips
- **Heads-up**: 2-player tournaments

## 🧪 Testing

The simulator has been tested with:
- Various player counts (2-10 players)
- Different blind structures and increase rates
- Multiple strategy combinations
- Edge cases (all-fold scenarios, tie hands)

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## ⚠️ Disclaimer

**FOR EDUCATIONAL AND ENTERTAINMENT PURPOSES ONLY**

This software is provided for educational, research, and entertainment purposes only. It is intended to demonstrate programming concepts, probability calculations, and game theory applications.

### Important Notes:

1. **No Real Money**: This simulator does not involve real money gambling or wagering of any kind.

2. **Educational Tool**: The primary purpose is to teach poker mechanics, probability theory, and strategic decision-making concepts.

3. **Not Gambling Software**: This is not intended to be used for actual gambling, betting, or any real-money poker activities.

4. **Probability Simulation**: While the probability calculations are mathematically sound, they are simplified for demonstration purposes and may not reflect all nuances of professional poker play.

5. **No Warranty**: The software is provided "as is" without warranty of any kind. The authors are not responsible for any misuse of this software.

6. **Legal Compliance**: Users are responsible for ensuring their use of this software complies with all applicable local, state, and federal laws regarding gambling and gaming simulations.

7. **Age Restriction**: Intended for users 18+ years of age, as it simulates gambling mechanics even without real money involved.

### Responsible Use:
- Use only for learning programming, mathematics, and game theory
- Do not use as a basis for real gambling decisions
- Be aware of local laws regarding gambling simulations
- If you have gambling addiction concerns, please seek appropriate help

---

**Author**: prajuk nusbaum (deomax@yahoo.com)  
**Created**: September 2025  
**Language**: R  
**License**: MIT