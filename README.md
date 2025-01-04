# Majora's Mask Dog Race Minigame Simulation

<!-- badges: start -->
<img src="https://raw.githubusercontent.com/AlanInglis/Majoras_Mask_Blue_Dog/refs/heads/main/badges/mm_heart.jpeg" width="241" height="209" align="right" />
<!-- badges: end -->



This repository contains an R implementation of a simulation replicating the mechanics of a dog race, 
inspired by a classic game from the Nintendo 64 era and the Vidya James YouTube
video [Why Majora's Mask's Blue Dog Took 25 Years to Win the Race](https://www.youtube.com/watch?v=Y1l6Xj4PLEk).
The simulation accounts for individual dog characteristics, race conditions, 
and randomised speed variations, all contributing to a realistic and probabilistic outcome. 
The **original C source code** is hosted on the Marjoa's Mask [zeldaret](https://github.com/zeldaret/mm) GitHub repo.



## Features

- **Dog Attributes**: Each dog has specific base speeds for the first quarter and last three-quarters of the race.
- **Randomised Conditions**: Dogs can have one of three conditions (Good, Neutral, Bad), which affects their speed multiplier during the race.
- **Random Number Generation (RNG)**: Utilises a Linear Congruential Generator (LCG) for replicating frame-by-frame speed fluctuations.
- **Sprint Mechanic**: Implements a sprint speed multiplier for dogs during the final stretch of the race.
- **Customisable Race Length**: The total length of the race is adjustable.
- **Tracking and Statistics**: Outputs probabilities of winning for each dog and colour group, alongside the number of races needed for a 99.99% chance of winning.

---

## How It Works

### Simulation Mechanics

1. **Dog Setup**: 
   - Each dog is assigned a colour, a name (e.g., `White_1`, `Gold_1`), and base speeds for the first and last parts of the race.
   - Dogs are evenly distributed across colour groups.

2. **Randomised Conditions**:
   - Dogs are randomly assigned one of three conditions:
     - **Good**: 1.2x speed multiplier.
     - **Neutral**: 1.0x speed multiplier.
     - **Bad**: 1/1.2x speed multiplier.

3. **RNG and Fluctuations**:
   - Each dog’s speed fluctuates frame-by-frame based on a pseudo-random generator.
   - The fluctuation is mapped to a range of `[-0.5, 0.5]`.

4. **Race Progression**:
   - Dogs move frame-by-frame based on their base speed, condition multiplier, and RNG fluctuation.
   - A sprint multiplier is applied to dogs during the final stretch of the race, increasing their speed further.

5. **Winning Conditions**:
   - The simulation tracks the finish positions of all dogs for each race.
   - Probabilities of winning are calculated for individual dogs and colour groups.

6. **Output**:
   - Probabilities for each dog and colour.
   - Number of races required for a 99.99% chance of winning.
   - A matrix of finish positions for all simulations.

---

## Installation

1. Clone the repository:
   ```{bash}
   git clone https://github.com/yourusername/mm-dog-race-simulation.git
   cd mm-dog-race-simulation
   ```

2. Install R and the required libraries:
   - Ensure you have R installed on your system.
   - The script relies only on base R functions, so no additional packages are required.
   
Or, just simply copy and paste the main function!

---

## Usage

### Run the Simulation

To run the simulation, source the R script and call the `simulate_dograce` function:

```
source("mm_dog_race_simulation.R")

# Run a simulation with 1000 races and a race length of 1000 units
results <- simulate_dograce(n_sims = 1000, race_length = 1000, selected_dog = "Gold_1")

# View results
results$prob_each_colour  # Probabilities of each colour winning
results$prob_each_dog     # Probabilities of each individual dog winning
```

---

## Parameters

- `n_sims` (integer): Number of races to simulate. Default is `1000`.
- `fps` (integer): Frames per second, influencing how frequently speeds are recalculated. Default is `30`.
- `race_length` (numeric): Total length of the race. Default is `1000`.
- `selected_dog` (character): Name of a dog to track during the simulation (e.g., `"Gold_1"`). If `NULL`, no specific tracking is performed.

---

## Outputs

The function returns a list with the following elements:

- `prob_each_colour`: Probabilities for each colour to win the race.
- `prob_each_dog`: Probabilities for each individual dog to win the race.
- `races_needed_99.99`: Number of races required for a 99.99% chance of winning for each colour.
- `finish_time_matrix`: Matrix containing the finish times of all dogs across all simulations.
- `finish_position_matrix`: Matrix containing the finish positions of all dogs across all simulations.

---

## Example Analysis

### Viewing Probabilities

To see the probability of each colour winning:

```
results$prob_each_colour
```

To see the probability of each individual dog winning:

```
results$prob_each_dog
```

### Races Needed for High Certainty

Determine the number of races needed for each colour to have a 99.99% chance of winning:

```
results$races_needed_99.99
```

### Analysing Finish Positions

View the finish positions of all dogs in a specific simulation:

```
results$finish_position_matrix[1, ]
```

### Tracking a Selected Dog

If a `selected_dog` is specified, the simulation will also output its finish positions across all races. For example:

```
table(results$selected_dog_positions)
```

---

## Contributing

Contributions are welcome! If you find a bug or have a suggestion for improvement, please create an issue or submit a pull request.

---

## License

This project is licensed under the MIT License. See the LICENSE file for details.

---


## Acknowledgements

This project was inspired by the dog race mechanics from a classic Nintendo 64 game found at XXXXXXXXXX.
Special thanks to the community for making the game's source code available for study and analysis and to 
XXXXXXXX for inspiring this analysis.



<!-- badges: start -->
<img src="https://raw.githubusercontent.com/AlanInglis/Majoras_Mask_Blue_Dog/refs/heads/main/badges/MM_Dog_Blue_Model.webp" width="250" height="245" align="left" />
<!-- badges: end -->





