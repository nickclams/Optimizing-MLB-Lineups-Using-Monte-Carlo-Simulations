#Loading packages
library(plyr)
library(RColorBrewer)
library(furrr)
library(tidyverse)

stats <- read.csv("stats.csv")
names(stats)


sample_pa <- function(row) {
  results <- c("BB", "X1B", "X2B", "X3B", "HR", "HBP", "K", "OUT")
  probability <- as.numeric(row[results])
  sample(results, size = 1, prob = probability)
  
}

lineups <- list (
  L1 = c("Brett Baty", "Francisco Alvarez", "Bo Bichette", "Francisco Lindor", "Marcus Semien", "Luis Robert Jr.", "Carson Benge", "Juan Soto", "Jorge Polanco"),
  L2 = c("Marcus Semien", "Brett Baty", "Luis Robert Jr.", "Carson Benge", "Francisco Alvarez", "Jorge Polanco", "Bo Bichette", "Francisco Lindor", "Juan Soto"),
  L3 = c("Francisco Lindor", "Juan Soto", "Bo Bichette", "Francisco Alvarez", "Jorge Polanco", "Brett Baty", "Marcus Semien", "Carson Benge", "Luis Robert Jr."),
  L4 = c("Juan Soto", "Francisco Lindor", "Bo Bichette", "Francisco Alvarez", "Brett Baty", "Marcus Semien", "Luis Robert Jr.", "Carson Benge", "Jorge Polanco"),
  L5 = c("Bo Bichette", "Juan Soto", "Francisco Lindor", "Jorge Polanco", "Francisco Alvarez", "Brett Baty", "Marcus Semien", "Luis Robert Jr.", "Carson Benge"),
  L6 = c("Francisco Lindor", "Bo Bichette", "Jorge Polanco", "Juan Soto", "Francisco Alvarez", "Brett Baty", "Marcus Semien", "Luis Robert Jr.", "Carson Benge"),
  L7 = c("Juan Soto", "Francisco Lindor", "Bo Bichette", "Jorge Polanco", "Francisco Alvarez", "Brett Baty", "Marcus Semien", "Carson Benge", "Luis Robert Jr.")
)

create_lineup <- function(stats, lineup_vector) {
  stats %>% slice(match(lineup_vector, Player))
}

####advance game function####
adv_game <- function(state, result, batter, dp_prob = 0.22, fc_prob = 0.2, adv_out_prob = 0.35, rbi_out_prob = 0.59, logs = TRUE) {
    runner1st <- state$on1st
    runner2nd <- state$on2nd
    runner3rd <- state$on3rd
    outs <- state$outs
    runs <- 0
    log <- ""
    
    occupied1st <- !is.na(runner1st)
    occupied2nd <- !is.na(runner2nd)
    occupied3rd <- !is.na(runner3rd)
    
    ####hit by pitch result####
    if (result %in% c("HBP")) {
      if (occupied1st && occupied2nd && occupied3rd) {
        runs <- runs + 1
        log <- paste0(batter, " is hit by a pitch. Run scores.")
        runner3rd <- runner2nd
        runner2nd <- runner1st
        runner1st <- batter
      }
      else {
        log <- paste0(batter, " is hit by a pitch.")
        
        if (occupied1st && occupied2nd && !occupied3rd) {
          runner3rd <- runner2nd
          runner2nd <- runner1st
          runner1st <- batter
        } else if (occupied1st && !occupied2nd) {
          runner2nd <- runner1st
          runner1st <- batter
        } else if (!occupied1st) {
          runner1st <- batter
        }
      }
      
    ####walk result####
    } else if (result %in% c("BB")) {
      if (occupied1st && occupied2nd && occupied3rd) {
        runs <- runs + 1
        log <- paste0(batter, " walks. Run scores.")
        runner3rd <- runner2nd
        runner2nd <- runner1st
        runner1st <- batter
      }
      else {
        log <- paste0(batter, " walks.")
        if (occupied1st && occupied2nd && !occupied3rd) {
          runner3rd <- runner2nd
          runner2nd <- runner1st
          runner1st <- batter
        } else if (occupied1st && !occupied2nd) {
          runner2nd <- runner1st
          runner1st <- batter
        } else if (!occupied1st) {
          runner1st <- batter
        }
      }
      
      ####single result####
    } else if (result == "X1B") {
      
      prev1st <- runner1st
      prev2nd <- runner2nd
      prev3rd <- runner3rd 
      
      movements <- c()
      
      #run scores from 3B
      if (occupied3rd) {
        runs <- runs + 1
        movements <- c(movements, paste0(prev3rd, " scores from 3B."))
        runner3rd <- NA_character_
      }
      
      #runner on 2B
      if (occupied2nd) {
        score2B <- rbinom(1, 1, 0.7) == 1 #probability of run scoring from 2B
        if (score2B) {
          runs <- runs + 1
          movements <- c(movements, paste0(prev2nd, " scores from 2B."))
          runner2nd <- NA_character_
        } else {
          if (is.na(runner3rd)) {
            runner3rd <- runner2nd
            runner2nd <- NA_character_
            movements <- c(movements, paste0(prev2nd, " advances from 2B to 3B."))
          }
        }
      }
      
      #runner on 1B
      if (occupied1st) {
        first_to_third <- rbinom(1, 1, 0.5) == 1 #probability of moving 1B to 3B
        if (first_to_third) {
          if (is.na(runner3rd)) {
            runner3rd <- prev1st
            movements <- c(movements, paste0(prev1st, " advances from 1B to 3B."))
          } else if (is.na(runner2nd)) {
            runner2nd <- prev1st
            movements <- c(movements, paste0(prev1st, " advances from 1B to 3B."))
          }
        } else {
          #if 2B is empty, runner on 1B advances to 2B
          if (is.na(runner2nd)) {
            runner2nd <- prev1st
            movements <- c(movements, paste0(prev1st, " advances from 1B to 2B."))
          } else if (is.na(runner3rd)) {
            runner3rd <- prev1st
            movements <- c(movements, paste0(prev1st, " advances from 1B to 3B."))
          }
        }
        runner1st <- NA_character_
      }
      
      if(is.na(runner1st)) {
        runner1st <- batter
      }
      
      log_parts <- c()
      log_parts <- c(log_parts, paste0(batter, " singles."))
      
      if (length(movements) > 0) {
        log_parts <- paste(log_parts, paste(movements, collapse = " "))
      }
      if (runs > 0) {
        log_parts <- paste0(log_parts, " ", runs, " run(s) score.")
      }
      
      log <- paste(log_parts, collapse = " ")
      
      ####double result####
    } else if (result == "X2B") {
      
      prev1st <- runner1st
      prev2nd <- runner2nd
      prev3rd <- runner3rd
      
      movements <- c()
      
      if (occupied3rd) {
        runs <- runs + 1
        movements <- c(movements, paste0(prev3rd, " scores from 3B."))
        runner3rd <- NA_character_
      } 
      
      if (occupied2nd) {
        runs <- runs + 1
        movements <- c(movements, paste0(prev2nd, " scores from 2B."))
        runner2nd <- NA_character_
      } 
      
      if (occupied1st) {
        runner3rd <- prev1st
        movements <- c(movements, paste0(prev1st, " advances from 1B to 3B."))
        runner1st <- NA_character_
      }
      
      runner2nd <- batter
      
      log_parts <- c()
      log_parts <- c(log_parts, paste0(batter, " doubles."))
      
      if (length(movements) > 0) {
        log_parts <- paste(log_parts, paste(movements, collapse = " "))
      }
      if (runs > 0) {
        log_parts <- paste0(log_parts, " ", runs, " run(s) score.") 
      }
      
      log <- paste(log_parts, collapse = " ")
      
      ####triple result####
    } else if (result == "X3B") {
      if (occupied3rd) {
        runs <- runs + 1
      }
      
      if (occupied2nd) {
        runs <- runs + 1
      }
      
      if (occupied1st) {
        runs <- runs + 1
      }
      
      runner1st <- NA_character_
      runner2nd <- NA_character_
      runner3rd <- batter
      
      log <- paste0(batter, " triples.")
      if (runs > 0) {
        log <- paste0(log, " ", runs, " run(s) score.")
      }
      
      ####home run result#####
    } else if (result == "HR") {
      if (occupied3rd) {
        runs <- runs + 1
      }
      
      if (occupied2nd) {
        runs <- runs + 1
      }
      
      if (occupied1st) {
        runs <- runs + 1
      }
      
      runs <- runs + 1
      runner1st <- NA_character_
      runner2nd <- NA_character_
      runner3rd <- NA_character_
      
      log <- paste0(batter, " homers. ", runs, " run(s) score.")
      
      ####out result####
    } else if (result %in% c("K", "OUT")) {
      
      prev1st <- runner1st
      prev2nd <- runner2nd
      prev3rd <- runner3rd
      
      if (result == "K") {
        outs <- outs + 1
        log <- paste0(batter, " strikes out.")
      } else {
        ####double play####
        if (!is.na(runner1st) && outs <= 1 && rbinom(1, 1, dp_prob) == 1) {
          outs <- outs + 2
          runner1st <- NA_character_
          log <- paste0(batter, " hits into a double play.")
        } else {
          outs <- outs + 1
          log <- paste0(batter, " hits into an out.")
          
          choose_fc_target <- function(on1st, on2nd, on3rd) {
            targets <- c()
            
            if (!is.na(on1st)) {
              targets <- c(targets, "2B")
            }
            if (!is.na(on2nd)) {
              targets <- c(targets, "3B")
            }
            if (!is.na(on3rd)) {
              targets <- c(targets, "HOME")
            }
            probs <- c("2B" = 0.5, "3B" = 0.25, "HOME" = 0.25) [targets]
            probs <- probs / sum(probs)
            sample(targets, 1, prob = probs)
          }
          ####fielder's choice####
          if (!is.na(prev1st) && state$outs <= 1 && rbinom(1, 1, fc_prob) == 1) {
            fc_target <- choose_fc_target(prev1st, prev2nd, prev3rd)
            runner1st <- batter
            fc_log <- paste0(batter, " reaches on a fielder's choice. ")
            
            if (fc_target == "HOME") {
              runner3rd <- NA_character_
              fc_log <- paste0(fc_log, prev3rd, " out at home.")
              
              if (!is.na(prev2nd)) {
                runner3rd <- prev2nd
                runner2nd <- NA_character_
              }
            } else if (fc_target == "3B") {
              runs <- runs + 1
              runner2nd <- NA_character_
              
              if (!is.na(prev3rd)) {
                runs <- runs + 1
                runner3rd <- NA_character_
                fc_log <- paste0(fc_log, prev2nd, " out at 3B. ", prev3rd, "scores.")
              } else {
                fc_log <- paste0(fc_log, prev2nd, " out at 3B.")
              }
            } else if (fc_target == "2B") {
              runner2nd <- NA_character_
              
              if (!is.na(prev3rd)) {
                runs <- runs + 1
                runner3rd <- NA_character_
                fc_log <- paste0(fc_log, prev1st, " out at 2B. ", prev3rd, " scores.")
              } else {
                fc_log <- paste0(fc_log, prev1st, " out at 2B.")
              }
              
              if (!is.na(prev2nd)) {
                runner3rd <- prev2nd
              }
            }
          }
          ####runner advancing on an out####
          #rbi out#
          if (!grepl("fielder's choice", log, fixed = TRUE) && outs <= 2) {
            
            adv_out_mov <- c()
            if (!is.na(prev3rd) && state$outs <= 1) {
              if (rbinom(1, 1, rbi_out_prob) == 1) {
                runs <- runs + 1
                runner3rd <- NA_character_
                adv_out_mov <- paste0(prev3rd, " scores from 3B.")
              }
            }
            #2B to 3B#
            if (rbinom(1, 1, adv_out_prob) == 1) {
              if (!is.na(prev2nd) && is.na(runner3rd)) {
                runner3rd <- prev2nd
                runner2nd <- NA_character_
                adv_out_mov <- paste0(prev2nd, " advances from 2B to 3B.")
              }
              if (!is.na(prev1st) && is.na(runner2nd)) {
                runner2nd <- prev1st
                runner1st <- NA_character_
                adv_out_mov <- paste0(prev1st, " advances from 1B to 2B.")
              }
            }
            if (length(adv_out_mov) > 0) {
              log <- paste(log, paste(adv_out_mov, collapse = " "))
            }
          }
        }
      }
    }
    
    base1 <- ifelse(is.na(runner1st), "Empty", runner1st)
    base2 <- ifelse(is.na(runner2nd), "Empty", runner2nd)
    base3 <- ifelse(is.na(runner3rd), "Empty", runner3rd)
    
    if (logs) {
      cat(log, "\n")
    cat("Outs: ", outs, " | Runners on: ", paste0("[1B: ", base1, " 2B: ", base2,
                                                  " 3B: ", base3, "]"), "\n")
    }
    
    list(state = list(outs = outs, on1st = runner1st, on2nd = runner2nd, on3rd = runner3rd),
         runs = runs)
  
}

####simulate inning function####
sim_inning <- function (lineup_df, start_idx, inning, dp_prob = 0.1, logs = TRUE) {
  
  if (logs) {
    cat("\n --- Start of inning ", inning, " ---\n")
  }
  
  state <- list(outs = 0, on1st = NA_character_, on2nd = NA_character_, on3rd = NA_character_)
  runs <- 0
  idx <- start_idx
  n <- nrow(lineup_df)
  
  while (state$outs < 3) {
    batter <- lineup_df$Player[idx]
    result <- sample_pa(lineup_df[idx, ])
    res <- adv_game(state, result, batter, dp_prob, logs = logs)
    state <- res$state
    runs <- runs + res$runs
    state <- stolen_base(state, lineup_df, logs = logs)
    idx <- ifelse(idx == n, 1, idx + 1)
  }
  
  if (logs) {
    cat("--- Inning ", inning, " over. ", runs, " runs scored. --- \n")
  }
  
  return(list(runs = runs, next_idx = idx))
  
}

####simulate game function####
sim_game <- function(lineup_df, dp_prob = 0.1, logs = TRUE) {
  total_runs <- 0
  idx <- 1
  for (inning in 1:9) {
    inn <- sim_inning(lineup_df, idx, inning, dp_prob, logs = logs)
    total_runs <- total_runs + inn$runs
    idx <- inn$next_idx
  }
  if (logs) {
    cat("\n --- Final Score: ", total_runs, "runs scored. ---")
  }
  
  return(total_runs)
}

####stolen base function####
stolen_base <- function(state, lineup_df, logs = TRUE) {
  if (state$outs >= 3) {
    return(state)
  }
  if (!is.na(state$on1st) && is.na(state$on2nd)){
    runner_name <- state$on1st
    runner_row <- lineup_df[lineup_df$Player == runner_name, ]
  
    if(nrow(runner_row) == 1) {
      sb <- as.numeric(runner_row$SB)
      cs <- as.numeric(runner_row$CS)
      
      if(is.na(sb)) {
        sb <- 0
      }
      
      if(is.na(cs)) {
        cs <- 0
      }
    
      no_attempt <- max(0, 1 - (sb + cs))
      
      attempt <- sample(c("steal", "caught_stealing", "no_attempt"), size = 1,
                        prob = c(sb, cs, no_attempt))
      if (attempt == "no_attempt") {
        return(state)
      }
      if (attempt == "steal") {
        if (logs) {
          cat(runner_name, " steals 2B.\n")
        }
        state$on1st <- NA_character_
        state$on2nd <- runner_name
      } else if (attempt == "caught_stealing") {
        if (logs) {
          cat(runner_name, " caught stealing 2B.\n")
        }
        state$on1st <- NA_character_
        state$outs <- state$outs + 1
      }
      base1 <- ifelse(is.na(state$on1st), "Empty", state$on1st)
      base2 <- ifelse(is.na(state$on2nd), "Empty", state$on2nd)
      base3 <- ifelse(is.na(state$on3rd), "Empty", state$on3rd)
      
      if (logs) {
        cat("Outs: ", state$outs, " | Runners on: ", paste0("[1B: ", base1, " 2B: ", base2,
                                                    " 3B: ", base3, "]"), "\n")
      }
    }
  }
  return(state)
}

simulate_lineup <- function(lineup_df, n_games) {
  future_map_int (
    1:n_games, ~ sim_game(lineup_df, logs = FALSE), .options = furrr_options(seed = TRUE)
  )
}

plan(multisession)
set.seed(1)
n_games <- 100000

results <- map_dfr(
  names(lineups), function(lineup_name) {
    lineup_df <- create_lineup(stats, lineups[[lineup_name]])
    runs <- simulate_lineup(lineup_df, n_games)
    tibble(lineup = lineup_name, game = seq_len(n_games), runs = runs)
  }
)

# ##generate order of random lineup
# 
random_lineup <- sample(1:9)
print(random_lineup)

glimpse(results)
write_csv(results, "results.csv")

summary <- results %>%
  group_by (lineup) %>%
  summarise (
    mean_runs = mean(runs),
    sd_runs = sd(runs),
    n = n(),
    se = sd_runs / sqrt(n),
    ci_lower = mean_runs - 1.96 * se,
    ci_upper = mean_runs + 1.96 * se,
    .groups = "drop"
  )

library(ggplot2)
ggplot(summary, aes(x = lineup, y = mean_runs)) +
  geom_point() + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15) +
  labs(x = "Lineup", y = "Mean runs per game")

library(dplyr)
library(purrr)

####one-way ANOVA test####

res.aov <- aov(runs ~ lineup, data = results)
summary(res.aov)

####Tukey's HSD test####
tukey_test <- TukeyHSD(res.aov)
print(tukey_test)


print(summary, digits=5)
