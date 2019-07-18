library(jsonlite)
library(ggplot2)
library(dplyr)
library(tidyselect)
library(readr)
theme_set(theme_classic())

# Read in all servers' JSONL (JSON Lines)
# Slow but it works. Could instead pre-process the JSON files.
paths <- dir("data", full.names = TRUE, pattern = "\\.json$")
servers <- unlist(lapply(paths, readLines), recursive = FALSE)
characters <- lapply(servers, fromJSON)

#' Determine the build, given a character and its attributes and skills.
#' @param char a nested list with character data
#' @return The build name as a character vector
determine_build <- function(char) {
  stopifnot(is.list(char$a))
  
  if (
    (
      char$a$strength$creation == 100 &&
      (
        char$sk$heavy_weapons$training == "Specialized" ||
        char$sk$light_weapons$training == "Specialized" ||
        char$sk$two_handed_combat$training == "Specialized"
      )
    ) ||
    (
      char$a$coordination$creation == 100 &&
      char$sk$finesse_weapons$training == "Specialized" &&
      char$sk$missile_weapons$training != "Specialized"
    ) 
  ) {
    return("MELEE")
  } else if (
    char$a$focus$creation == 100 &&
    char$a$self$creation == 100 &&
    (
      char$sk$war_magic$training == "Specialized" ||
      char$sk$void_magic$training == "Specialized"
    )
  ) {
    return("MAGIC")
  } else if (
    char$a$coordination$creation == 100 &&
    char$a$self$creation < 100 &&
    char$sk$missile_weapons$training == "Specialized"
  ) {
    return("MISSILE")
  } else {
    return(NA)
  }
}

# Convert each list element into a data.frame
char_rows <- lapply(characters, function(char) {
  # Skip chars with no attributes (stubs)
  if (!is.list(char$a)) {
    return(data.frame())
  }
  
  tryCatch({
    data.frame(name = char$n,
               server = char$s,
               level = char$l,
               ntitles = length(char$ti),
               build = determine_build(char),
               strength_creation = char$a$strength$creation,
               endurance_creation = char$a$endurance$creation,
               coordination_creation = char$a$coordination$creation,
               quickness_creation = char$a$quickness$creation,
               focus_creation = char$a$focus$creation,
               self_creation = char$a$self$creation,
               melee_defense_base = char$sk$melee_defense$base,
               melee_defense_training = char$sk$melee_defense$training,
               magic_defense_base = char$sk$magic_defense$base,
               magic_defense_training = char$sk$magic_defense$training,
               missile_defense_base = char$sk$missile_defense$base,
               missile_defense_training = char$sk$missile_defense$training,
               missile_weapons_base = char$sk$missile_weapons$base,
               missile_weapons_training = char$sk$missile_weapons$training,
               light_weapons_base = char$sk$light_weapons$base,
               light_weapons_training = char$sk$light_weapons$training,
               heavy_weapons_base = char$sk$heavy_weapons$base,
               heavy_weapons_training = char$sk$heavy_weapons$training,
               finesse_weapons_base = char$sk$finesse_weapons$base,
               finesse_weapons_training = char$sk$finesse_weapons$training,
               two_handed_combat_base = char$sk$two_handed_combat$base,
               two_handed_combat_training = char$sk$two_handed_combat$training,
               war_magic_base = char$sk$war_magic$base,
               war_magic_training = char$sk$war_magic$training,
               void_magic_base = char$sk$void_magic$base,
               void_magic_training = char$sk$void_magic$training,
               life_magic_base = char$sk$life_magic$base,
               life_magic_training = char$sk$life_magic$training,
               fletching_base = char$sk$fletching$base,
               fletching_training = char$sk$fletching$training,
               stringsAsFactors = FALSE)
  },
  error = function(e) {
    warning(char$s, "-", char$n, ": ", e)
    
    return(data.frame())
  })
})

# And rbind them
char_df <- do.call(rbind, char_rows)

# Pre-clean: Remove all low level characters
char_df <- char_df %>%
  filter(level> 10)

# Pre-clean: Remove all characters with only one title
char_df <- char_df %>% 
  filter(ntitles > 1)

# Calculate Zarto's numbers
stats_df <- char_df %>%
  group_by(build, level) %>% 
  summarise_at(
    vars(ends_with("base")), 
    list(
      mean = ~ round(mean(.)),
      min = ~ min(.),
      max = ~ max(.)))

# Add in sample sizes
stats_df <- char_df %>% 
  group_by(build, level) %>% 
  summarise(n = n()) %>% 
  left_join(stats_df)

# Plot for fun
char_df %>%
  group_by(level, build, melee_defense_training) %>%
  summarise(mean_melee_d = mean(melee_defense_base)) %>%
  ggplot(aes(level, mean_melee_d, color = melee_defense_training)) +
  geom_point(fill = NA, alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 275, 25)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  facet_wrap(~ build, ncol = 1)

# Write out
write_csv(stats_df, "results/skills_by_level_and_build.csv")
