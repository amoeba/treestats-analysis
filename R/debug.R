#' Debug helpers, various things I needed while debugging other code

#' Print one row into a tidy string
#' 
#' Run like:
#' print_one(char_df[sample(seq_len(nrow(char_df)), size = 1, replace = FALSE),])
#' 
#' @param row A list (from subsetting a data.frame)
print_one <- function(row) {
  char <- Filter(function(c) {c$s == row$server && c$n == row$name}, characters)[[1]]
  spec_skills <- specialized_skills(char)
  
  cat(row$server,
      "-",
      row$name,
      " (",
      row$build, 
      
      ") :", 
      paste(row$strength_creation, 
            row$endurance_creation,
            row$coordination_creation,
            row$quickness_creation,
            row$focus_creation,
            row$self_creation, sep = "/"),
      "[",
      spec_skills,
      "]"
  )
}

#' Create a string represening the specialized skills the character has
#' @param char A list of the character's info
specialized_skills <- function(char) {
  paste(vapply(Filter(function(skill) { skill$training == "Specialized"}, char$sk),
               function(skill) { skill$name },
               "",USE.NAMES = FALSE), collapse = ", ")
}

