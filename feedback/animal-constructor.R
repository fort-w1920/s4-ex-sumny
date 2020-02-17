# idea: constructors for subclasses call constructors of superclasses to avoid
# repeating the code for assigning random values to missing slots as much as
# possible.
# drawback: output for validity testing refers to uppermost superclass
# for which inputs are invalid (since their validity is checked first when their
# constructor is called), not the class from which objects are being
# constructed...
# NB: since our classes implement validity-functions, we don't need input checks
#   here.
# NB: I didn't write much documentation here because purpose/inputs/outputs are
#   mostly self-explanantory. I should have, though...

animal <- function(name, weight, female) {
  if (missing(name)) {
    name <- make_name(length = sample(3:9, size = 1))
  }
  if (missing(weight)) {
    weight <- runif(1, 1, 30)
  }
  if (missing(female)) {
    female <- sample(c(TRUE, FALSE), 1)
  }
  .animal(name = name, weight = weight, female = female)
}

# prey -------------------------------------------------------------------------

prey <- function(name, weight, female, hide) {
  if (missing(hide)) {
    hide <- runif(1)
  }
  .prey(animal(name, weight, female), hide = hide)
}
mouse <- function(name, weight, female, hide) {
  if (missing(weight)) {
    weight <- runif(1, min = 0.5, max = 1)
  }
  if (missing(hide)) {
    hide <- runif(1, min = 0.6, max = 1)
  }
  .mouse(prey(name, weight, female, hide))
}
rabbit <- function(name, weight, female, hide) {
  if (missing(weight)) {
    weight <- runif(1, min = 1, max = 5)
  }
  if (missing(hide)) {
    hide <- runif(1, min = 0.3, max = 0.8)
  }
  .rabbit(prey(name, weight, female, hide))
}
deer <- function(name, weight, female, hide) {
  if (missing(weight)) {
    weight <- runif(1, min = 15, max = 30)
  }
  if (missing(hide)) {
    hide <- runif(1, min = 0.2, max = 0.7)
  }
  .deer(prey(name, weight, female, hide))
}

# predator----------------------------------------------------------------------

predator <- function(name, weight, female, seek) {
  if (missing(seek)) {
    seek <- runif(1)
  }
  .predator(animal(name, weight, female), seek = seek)
}
hawk <- function(name, weight, female, seek) {
  if (missing(weight)) {
    weight <- runif(1, min = 3, max = 8)
  }
  if (missing(seek)) {
    seek <- runif(1, min = 0.6, max = 1)
  }
  .hawk(predator(name, weight, female, seek))
}
lynx <- function(name, weight, female, seek) {
  if (missing(weight)) {
    weight <- runif(1, min = 20, max = 60)
  }
  if (missing(seek)) {
    seek <- runif(1, min = 0.5, max = 0.9)
  }
  .lynx(predator(name, weight, female, seek))
}
