# Optimizing-MLB-Lineups-Using-Monte-Carlo-Simulations
This is Monte Carlo simulation built in R that models plate appearances for a 9-player batting lineup across 100,000 simulated games. Seven different batting orders are compared to determine which arrangement statistically produces the most runs per game.

# Files
Simulation.r - The actual R code
stats.csv - Per-player outcome probabilities

# Packages
Make sure these R packages are installed before running:

# Prerequisites
install.packages(c("tidyverse", "furrr", "plyr", "RColorBrewer"))

Ensure stats.csv and Simulation.r are in the same directory before running

# How to run
Should be ran in RStudio or another R environment

By default, the code simulates 100,000 games per lineup. This can be changed by modifying the value of n_games on line 482.

The goal is to identify whether lineup order has a statistically significant effect on run production

