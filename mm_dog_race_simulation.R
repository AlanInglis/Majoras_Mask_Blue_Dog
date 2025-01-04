###########################################
# Relevant C Source Excerpts (z_en_racedog.c)
###########################################
#
# 1) sBaseSpeeds array:
#    static f32 sBaseSpeeds[][2] = {
#        { 0.0f, 0.0f }, // 0 => DOG_COLOR_DEFAULT
#        { 5.0f, 5.5f }, // 1 => DOG_COLOR_WHITE
#        { 5.0f, 5.0f }, // 2 => DOG_COLOR_GRAY
#        { 5.5f, 5.0f }, // 3 => DOG_COLOR_BEIGE
#        { 4.5f, 5.5f }, // 4 => DOG_COLOR_BROWN
#        { 6.0f, 4.0f }, // 5 => DOG_COLOR_BLUE
#        { 4.0f, 6.0f }, // 6 => DOG_COLOR_GOLD
#    };
#
# 2) DogColor enum (the dog's fur colour):
#    typedef enum {
#        /* 0 */ DOG_COLOR_DEFAULT,
#        /* 1 */ DOG_COLOR_WHITE,
#        /* 2 */ DOG_COLOR_GRAY,
#        /* 3 */ DOG_COLOR_BEIGE,
#        /* 4 */ DOG_COLOR_BROWN,
#        /* 5 */ DOG_COLOR_BLUE,
#        /* 6 */ DOG_COLOR_GOLD
#    } DogColor;
#
# 3) Condition distribution:
#    - 4 dogs = good
#    - 5 dogs = neutral
#    - 5 dogs = bad
#
# 4) Speed logic:
#    - At the start, the blue dog (enum=5) uses its "first" base speed (6.0).
#      Others effectively start at 5.0.
#    - After 1/4, they may switch to second base speed if they've crossed
#      "pointToUseSecondBaseSpeed".
#    - In the last 1/4, they may sprint (if not in bad condition) with a
#      multiplier up to 2.0.
#
# 5) The array pointToUseSecondBaseSpeed from sDogInfo[]:
#    e.g. dog 0 => 9, dog 1 => 9, dog 2 => 10, dog 3 => 9, dog 4 => 8, etc.
#
# Below we replicate these mechanics in R with a time-step approach,
# plus a base R progress bar and user-friendly dog selection.
###########################################


###########################################
# 1. Data and Constants
###########################################

# sBaseSpeeds (in R, 1-based rows):
#   row1 => DOG_COLOR_DEFAULT(0) => {0.0, 0.0}
#   row2 => DOG_COLOR_WHITE  (1) => {5.0, 5.5}
#   row3 => DOG_COLOR_GRAY   (2) => {5.0, 5.0}
#   row4 => DOG_COLOR_BEIGE  (3) => {5.5, 5.0}
#   row5 => DOG_COLOR_BROWN  (4) => {4.5, 5.5}
#   row6 => DOG_COLOR_BLUE   (5) => {6.0, 4.0}
#   row7 => DOG_COLOR_GOLD   (6) => {4.0, 6.0}
sBaseSpeeds <- matrix(
  c(
    0.0, 0.0,   # row1 => Default
    5.0, 5.5,   # row2 => White
    5.0, 5.0,   # row3 => Gray
    5.5, 5.0,   # row4 => Beige
    4.5, 5.5,   # row5 => Brown
    6.0, 4.0,   # row6 => Blue
    4.0, 6.0    # row7 => Gold
  ),
  ncol = 2, byrow = TRUE
)

# The 14 dogs' colours, from sDogInfo in the game:
#  dog 0 => 3 (Beige), dog 1 => 1 (White), dog 2 => 5 (Blue), dog 3 => 2 (Gray),
#  dog 4 => 4 (Brown), dog 5 => 2 (Gray), dog 6 => 3 (Beige), dog 7 => 1 (White),
#  dog 8 => 1 (White), dog 9 => 6 (Gold), dog 10 => 2 (Gray), dog 11 => 3 (Beige),
#  dog 12 => 1 (White), dog 13 => 4 (Brown)
dog_colours <- c(3,1,5,2,4,2,3,1,1,6,2,3,1,4)
n_dogs      <- length(dog_colours)  # 14

# Condition distribution: 4 good, 5 neutral, 5 bad
dog_condition_types <- c(rep("good",4), rep("neutral",5), rep("bad",5))

# from sDogInfo: the distance point to use second base speed
#  in the game, these are path indices 8..10, but we map them
#  to (index/12)*race_length in the code
pt_to_use_2nd <- c(9,9,10,9,8,9,9,9,9,8,9,9,9,8)

# Full colour labels (0..6 => 7 total). We'll exclude "Default" from final outputs.
color_labels <- c("Default","White","Gray","Beige","Brown","Blue","Gold")


###########################################
# 1a. Build dog_labels for finishing matrix
###########################################
# Each dog gets a label like "beige_1", "white_1", "gray_1", etc.
build_dog_labels <- function(dog_colours, color_labels) {
  color_counts <- setNames(as.list(rep(0L, length(color_labels))), color_labels)
  labels_out   <- character(length(dog_colours))
  
  for (i in seq_along(dog_colours)) {
    c_enum  <- dog_colours[i]
    # note +1 to handle R's 1-based row indexing
    c_label <- color_labels[c_enum + 1]
    color_counts[[c_label]] <- color_counts[[c_label]] + 1
    # e.g. "beige_1", "white_2", "blue_1", etc.
    labels_out[i] <- paste0(tolower(c_label), "_", color_counts[[c_label]])
  }
  labels_out
}

dog_labels <- build_dog_labels(dog_colours, color_labels)

# Condition multiplier function
# Good => ×1.2, Bad => ÷1.2, Neutral => ×1.0
condition_multiplier <- function(cond) {
  if (cond == "good") return(1.2)
  if (cond == "bad")  return(1/1.2)
  1.0
}


###########################################
# 2. RNG (LCG) Implementation in R
###########################################
# We store the seed as a double in [0, 2^32)
# to avoid integer overflow in 32-bit multiplication.

RNGenv <- new.env()
RNGenv$seed <- 0.0  # user can set, e.g. RNGenv$seed <- 1234.0

rand_centered_float <- function(range = 1.0) {
  # The constants 1664525 and 1013904223 are from the original code
  RNGenv$seed <- (RNGenv$seed * 1664525 + 1013904223) %% 4294967296
  # convert [0,1) => [-0.5,0.5] => scale by 'range'
  ((RNGenv$seed / 4294967296) - 0.5) * range
}


###########################################
# 3. Main Simulation Function 
###########################################
simulate_dograce <- function(n_sims,
                             race_length = 1000,
                             selected_dog = 1,  # can be numeric or a string label
                             show_progress = TRUE) {
  # 3.1. Convert selected_dog to numeric if string
  if (is.character(selected_dog)) {
    dog_index <- match(selected_dog, dog_labels)
    if (is.na(dog_index)) {
      stop("Error: 'selected_dog' label not found in dog_labels. Provided: ", selected_dog)
    }
    selected_dog <- dog_index
  } else if (!is.numeric(selected_dog)) {
    stop("selected_dog must be numeric or character.")
  }
  
  # Safety: ensure the numeric index is in 1..n_dogs
  if (selected_dog < 1 || selected_dog > n_dogs) {
    stop("selected_dog numeric index out of range (1..", n_dogs, ").")
  }
  
  finish_matrix  <- matrix(0, nrow = n_sims, ncol = n_dogs)
  winners        <- integer(n_sims)
  winner_colours <- integer(n_sims)
  
  # Optionally create a progress bar
  if (show_progress) {
    pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  }
  
  ###########################################
  # Function to simulate one race
  ###########################################
  simulate_one_race <- function() {
    # randomly assign 4 good, 5 neutral, 5 bad
    assigned_conds <- sample(dog_condition_types, n_dogs, replace = FALSE)
    
    # Build dog_data for all 14 dogs
    dog_data <- lapply(seq_len(n_dogs), function(i) {
      list(
        colour       = dog_colours[i],
        condition    = assigned_conds[i],
        dist_for_2nd = (pt_to_use_2nd[i] / 12) * race_length,
        sprint_mult  = NA_real_,
        finished     = FALSE,
        finish_rank  = NA_integer_,
        position     = 0.0,
        speed        = 0.0
      )
    })
    
    # sSprintTimer increments once the first-place dog passes 3/4
    sSprintTimer      <- 0
    quarter_len       <- 0.25 * race_length
    three_quarter_len <- 0.75 * race_length
    
    # At race start, if dog is Blue(enum=5), use sBaseSpeeds[6,1]=6.0
    # Others => 5.0
    for (k in seq_len(n_dogs)) {
      col <- dog_data[[k]]$colour
      if (col == 5) {
        dog_data[[k]]$speed <- sBaseSpeeds[col + 1, 1]  # row6 => 6.0
      } else {
        dog_data[[k]]$speed <- 5.0
      }
    }
    
    get_first_place <- function(dd) which.max(vapply(dd, `[[`, numeric(1), "position"))
    
    ###########################################
    # Speed update logic each step
    ###########################################
    update_speeds <- function(dd, sprint_time) {
      idx_first <- get_first_place(dd)
      for (k in seq_along(dd)) {
        dpos <- dd[[k]]$position
        col  <- dd[[k]]$colour
        cond <- dd[[k]]$condition
        
        base_spd <- 0.0
        if (dpos < quarter_len) {
          # first quarter
          if (col == 5) {
            base_spd <- sBaseSpeeds[col + 1, 1] + rand_centered_float(1.0)
          } else {
            base_spd <- 5.0 + rand_centered_float(1.0)
          }
          
        } else if (dpos < three_quarter_len) {
          # from 1/4 to 3/4
          if (dpos >= dd[[k]]$dist_for_2nd) {
            base_spd <- sBaseSpeeds[col + 1, 2] + rand_centered_float(1.0)
          } else {
            base_spd <- 5.0 + rand_centered_float(1.0)
          }
          
        } else {
          # final 1/4
          # if sprint_mult not set, do it now (unless bad)
          if (is.na(dd[[k]]$sprint_mult)) {
            if (cond == "bad") {
              dd[[k]]$sprint_mult <- 1.0
            } else {
              # 2.0 max if sSprintTimer >=100
              if (sprint_time < 100) {
                dd[[k]]$sprint_mult <- 200.0 / (200.0 - sprint_time)
              } else {
                dd[[k]]$sprint_mult <- 2.0
              }
            }
          }
          base_spd <- sBaseSpeeds[col + 1, 2] + rand_centered_float(1.0)
          base_spd <- base_spd * dd[[k]]$sprint_mult
        }
        
        # Apply condition multiplier
        base_spd <- base_spd * condition_multiplier(cond)
        
        # The selected dog has a cap of 7.5, others 7.0
        max_spd <- if (k == selected_dog) 7.5 else 7.0
        base_spd <- min(base_spd, max_spd)
        
        dd[[k]]$speed <- base_spd
      }
      dd
    }
    
    # Step until all 14 dogs finish
    rank_counter <- 0
    while (rank_counter < n_dogs) {
      idx_first <- get_first_place(dog_data)
      
      # increment sSprintTimer once first-place dog crosses 3/4
      if (!dog_data[[idx_first]]$finished &&
          dog_data[[idx_first]]$position >= three_quarter_len) {
        sSprintTimer <- sSprintTimer + 1
      }
      
      # update speeds
      dog_data <- update_speeds(dog_data, sSprintTimer)
      
      # move dogs
      for (k in seq_along(dog_data)) {
        if (!dog_data[[k]]$finished) {
          dog_data[[k]]$position <- dog_data[[k]]$position + dog_data[[k]]$speed
          if (dog_data[[k]]$position >= race_length) {
            dog_data[[k]]$finished <- TRUE
            rank_counter <- rank_counter + 1
            dog_data[[k]]$finish_rank <- rank_counter
          }
        }
      }
    }
    
    dog_data
  }
  
  ###########################################
  # 3.2. Run all simulations
  ###########################################
  for (sim_id in seq_len(n_sims)) {
    race_result <- simulate_one_race()
    
    # record finishing ranks
    for (j in seq_len(n_dogs)) {
      finish_matrix[sim_id, j] <- race_result[[j]]$finish_rank
    }
    
    # find the winner (rank=1)
    dog_winner <- which.min(vapply(race_result, `[[`, numeric(1), "finish_rank"))
    winners[sim_id]        <- dog_winner
    winner_colours[sim_id] <- race_result[[dog_winner]]$colour
    
    # update progress bar if desired
    if (show_progress) {
      setTxtProgressBar(pb, sim_id)
    }
  }
  
  if (show_progress) {
    close(pb)
  }
  
  ###########################################
  # 4. Summarise results
  ###########################################
  dog_win_counts   <- tabulate(winners, nbins = n_dogs)
  dog_win_probs    <- dog_win_counts / n_sims
  
  # colour is 0..6 => shift by +1 for R indexing
  col_win_counts <- tabulate(winner_colours + 1, nbins = length(color_labels))
  col_win_probs  <- col_win_counts / n_sims
  
  mean_finish_positions <- colMeans(finish_matrix)
  
  # Remove "Default" from final colour results
  col_win_probs_no_def    <- col_win_probs[-1]
  color_labels_no_default <- color_labels[-1]
  
  # Races for 99.99% chance => (1 - p)^N <= 0.0001 => N >= log(0.0001)/log(1 - p)
  races_for_99_99 <- sapply(col_win_probs_no_def, function(p) {
    if (p <= 0) return(Inf)
    ceiling(log(0.0001) / log(1 - p))
  })
  
  # Name dog-based results with dog_labels
  dog_win_prob_named <- setNames(dog_win_probs, dog_labels)
  colnames(finish_matrix) <- dog_labels
  mean_finish_positions_named  <- setNames(mean_finish_positions, dog_labels)
  
  # Name colour-based results (excluding "Default")
  colour_win_probability_named <- setNames(col_win_probs_no_def, color_labels_no_default)
  races_for_99_99_named        <- setNames(races_for_99_99,      color_labels_no_default)
  
  ###########################################
  # Final output
  ###########################################
  list(
    dog_win_probability        = sort(dog_win_prob_named, decreasing = T),
    colour_win_probability     = sort(colour_win_probability_named, decreasing = T),
    mean_finish_position       = sort(mean_finish_positions_named, decreasing = F),
    races_for_99_99_per_colour = races_for_99_99_named,
    finish_positions_all       = finish_matrix
  )
}

###########################################
# 4. Example Usage
###########################################

set.seed(999)        # controls R's default RNG for sample()
RNGenv$seed <- 1234.0  # RNG function seed
results <- simulate_dograce(n_sims = 1000, race_length = 1000, selected_dog = 'blue_1', show_progress = TRUE)
results$dog_win_probability
results$colour_win_probability
results$mean_finish_position
results$races_for_99_99_per_colour
head(results$finish_positions_all)




