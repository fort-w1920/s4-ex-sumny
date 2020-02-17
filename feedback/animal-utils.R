# is  x in [lo, hi]?
in_range <- function(x, lo = 0, hi = 1) {
  x >= lo & x <= hi
}

#-------------------------------------------------------------------------------
# special flag used in meet-methods:
same_species_different_sex <- function(animal1, animal2) {
  class(animal1) == class(animal2) && animal1@female != animal2@female
}


#-------------------------------------------------------------------------------

# general purpose validity function to declutter class definitions:
validate_animal <- function(object, weight_range = c(0, Inf),
                            hide_range = c(0, 1),
                            seek_range = c(0, 1)) {
  library(checkmate)
  invalids <- character()
  if (!test_string(object@name, min.chars = 1)) {
    invalids <- c(invalids, "animals need a 'name'.")
  }
  if (!test_number(object@weight,
                   lower = weight_range[1], upper = weight_range[2])) {
    invalids <- c(invalids, sprintf(
      "weight must be in [%g, %g]",
      weight_range[1], weight_range[2]))
  }
  if (!test_flag(object@female)) {
    invalids <- c(invalids, "'female' must be TRUE or FALSE.")
  }
  if (is(object, "prey")) {
    if (!test_number(object@hide,
                     lower = hide_range[1], upper = hide_range[2])) {
      invalids <- c(invalids, sprintf(
        "hide must be in [%g, %g]",
        hide_range[1], hide_range[2]))
    }
  }
  if (is(object, "predator")) {
    if (!test_number(object@seek,
                     lower = seek_range[1], upper = seek_range[2])) {
      invalids <- c(invalids, sprintf(
        "seek must be in [%g, %g]",
        seek_range[1], seek_range[2]))
    }
  }
  if (!length(invalids)) TRUE else invalids
}
