#########################################
### Majora's Mask Dog Race Simulation ###
#########################################


# Create Dog Data ---------------------------------------------------------

# This section defines the initial setup for the dog race simulation, 
# including the dogs' attributes and simulation logic.

# Define dog colours and speeds
# Each dog colour is associated with specific speeds for the race phases: 
# the first quarter and the remaining three-quarters.
dogs <- data.frame(
  color                     = c("White", "Grey", "Beige", "Brown", "Blue", "Gold"), # Dog colours in the race
  first_quarter_speed       = c(5.0,     5.0,    5.5,     4.5,     6.0,    4.0),    # Speed for the first 25% of the race
  last_three_quarters_speed = c(5.5,     5.0,    5.0,     5.5,     4.0,    6.0),    # Speed for the last 75% of the race
  count                     = c(4,       3,      3,       2,       1,      1)       # Number of dogs per colour
)

# Create individual rows for each dog by replicating based on the count of dogs for each colour
all_dogs <- dogs[rep(seq_len(nrow(dogs)), times = dogs$count), ]

# Generate unique names for each dog using its colour and an index number within its colour group
dog_index_within_colour <- ave(all_dogs$color, all_dogs$color, FUN = seq_along)
dog_names <- paste0(all_dogs$color, "_", dog_index_within_colour)


# END of create dog data --------------------------------------------------
# -------------------------------------------------------------------------
# Random Number Generator Function ----------------------------------------

# Here we create the RNG used to add speed fluctuations to the dog. It was made to mirror the 
# source code found at XXXXXX.

# Define constants for the random number generator (RNG)
# These constants match those used in XXXX's source code.
RAND_MULTIPLIER <- 1664525    # Multiplier constant for RNG
RAND_INCREMENT <- 1013904223  # Increment constant for RNG
MODULUS <- 2^32               # Modulus for the RNG, ensuring it loops within 32-bit integers

# RNG Function: Computes the next random state using a Linear Congruential Generator (LCG).
# LCG is a simple and widely used algorithm for generating pseudo-random numbers.
# It works by applying a linear formula to compute the next number in a sequence based on the previous one.
# In this case, the formula is: (current_state * multiplier + increment) % modulus and 
# it mirrors the pseudo-random number generator used in the original game (N64 code).
next_rng <- function(rng) {
  return((rng * RAND_MULTIPLIER + RAND_INCREMENT) %% MODULUS)
}

# Function to map an RNG state to a random fluctuation in the range [-0.5, 0.5]
# The fluctuation represents randomness in the dog's speed during the race. 
# This is applied every frame of the game. 
map_rng_to_speed <- function(rng) {
  normalised_rng <- rng / (MODULUS - 1)  # Normalize the RNG state to [0, 1]
  fluctuation <- (normalised_rng * 1.0) - 0.5  # Map to the range [-0.5, 0.5]
  return(fluctuation)
}


# END of RNG function -----------------------------------------------------
# -------------------------------------------------------------------------
# Simulate Dog Race Function ----------------------------------------------


# Main simulation function for the dog race
simulate_dograce <- function(n_sims = 1000, 
                             fps = 30, 
                             race_length = 1000, 
                             selected_dog = NULL) {
  # Set a fixed seed for reproducibility
  set.seed(1701)
  
  # Define distances for race phases
  first_quarter_distance <- race_length * 0.25        # First 25% of the race
  last_three_quarters_distance <- race_length * 0.75  # Remaining 75% of the race
  n_dogs <- nrow(all_dogs)                            # Total number of dogs in the race
  
  # Initialise win counters for dog colours and individual dogs
  unique_colours <- unique(all_dogs$color)
  colour_win_counts <- setNames(rep(0, length(unique_colours)), unique_colours)
  dog_win_counts <- setNames(rep(0, n_dogs), dog_names)
  
  # Matrix to store the finishing positions of all dogs across simulations
  finish_position <- matrix(0, nrow = n_sims, ncol = n_dogs)
  colnames(finish_position) <- dog_names
  
  # Progress bar to monitor simulation progress
  pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
  
  for (sim_i in seq_len(n_sims)) {
    # Randomly assign conditions to dogs: "good", "neutral", or "bad"
    # The proportions match the original game (4 good, 5 neutral, 5 bad).
    conditions <- sample(c(rep("good", 4), rep("neutral", 5), rep("bad", 5)))
    speed_multiplier <- ifelse(conditions == "good", 1.2, ifelse(conditions == "bad", 1 / 1.2, 1))
    
    # Initialise distance covered by each dog
    distances <- rep(0, n_dogs)
    
    # Initialise RNG states for each dog
    rng_states <- sample(0:(MODULUS - 1), n_dogs)
    
    # Initialise sprint-related variables
    sSprintTimer <- 0
    sprint_multipliers <- rep(1.0, n_dogs)
    
    # Simulate the race frame by frame
    while (any(distances < race_length)) {
      # Determine the rank (position) of each dog based on their current distance
      positions <- rank(-distances, ties.method = "first")
      
      for (dog_i in seq_len(n_dogs)) {
        if (distances[dog_i] < race_length) {
          # Update the RNG state for this dog and compute a fluctuation
          rng_state <- next_rng(rng_states[dog_i])
          rng_states[dog_i] <- rng_state
          fluctuation <- map_rng_to_speed(rng_state)
          
          # Determine the base speed based on the race phase
          base_speed <- if (distances[dog_i] < first_quarter_distance) {
            all_dogs$first_quarter_speed[dog_i]
          } else {
            # Apply sprint multiplier in the final stretch of the race
            if (is.na(sprint_multipliers[dog_i]) && distances[dog_i] >= last_three_quarters_distance) {
              sprint_multipliers[dog_i] <- if (sSprintTimer < 100) {
                200 / (200 - sSprintTimer)
              } else {
                2.0
              }
              sSprintTimer <- sSprintTimer + 1
            }
            sprint_multipliers[dog_i] * all_dogs$last_three_quarters_speed[dog_i]
          }
          
          # Apply a bonus for good condition if the dog is not in the lead
          if (conditions[dog_i] == "good" && positions[dog_i] > 1) {
            base_speed <- base_speed * 1.2
          }
          
          # Compute the dog's speed for this frame, including fluctuation
          speed_this_frame <- base_speed + fluctuation
          
          # Update the dog's distance
          distances[dog_i] <- distances[dog_i] + (speed_this_frame / fps)
        }
      }
    }
    
    # Determine final finishing positions for this simulation
    finish_positions <- rank(-distances, ties.method = "first")
    finish_position[sim_i, ] <- finish_positions
    
    # Update win counts for the winning dog's colour and individual identity
    winner_index <- which(finish_positions == 1)
    winner_colour <- all_dogs$color[winner_index]
    colour_win_counts[winner_colour] <- colour_win_counts[winner_colour] + 1
    dog_win_counts[dog_names[winner_index]] <- dog_win_counts[dog_names[winner_index]] + 1
    
    # Update progress bar
    setTxtProgressBar(pb, sim_i)
  }
  close(pb)
  
  # Compute probabilities and races needed for 99.99% win certainty
  prob_each_colour <- colour_win_counts / n_sims
  prob_each_dog <- dog_win_counts / n_sims
  races_needed <- ceiling(log(0.0001) / log(1 - prob_each_colour))
  
  # Return results as a list
  list(
    prob_each_colour       = sort(prob_each_colour, decreasing = TRUE),
    prob_each_dog          = sort(prob_each_dog, decreasing = TRUE),
    races_needed_99.99     = races_needed,
    finish_position = finish_position,
    dog_names              = dog_names
  )
}

# END of simulation function ----------------------------------------------
# -------------------------------------------------------------------------
# Example -----------------------------------------------------------------

# Simulate the race 1000 times with a race length of 100 and selected dog is the gold dog.
results <- simulate_dograce(n_sims = 100, race_length = 100, selected_dog = "Gold_1")

# View probabilities of each colour and individual dog's win chances
results$prob_each_colour   # probablility of each (overall) colour winning
results$prob_each_dog      # probablility of each individual dog winning 
results$races_needed_99.99 # Number of races needed to be run to have a 99.99% chance of winning 
results$finish_position    # Each dogs finish position over all simulations



# END of example ----------------------------------------------------------
# -------------------------------------------------------------------------
# Visualisations ----------------------------------------------------------











