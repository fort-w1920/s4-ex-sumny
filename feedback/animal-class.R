.animal <- setClass("animal",
  slots = c(name = "character", weight = "numeric", female = "logical"),
  prototype = prototype(name = "an_animal", weight = 1, female = TRUE),
  validity = function(object) {
    validate_animal(object)
  }
)
# NB: .animal is now a "class generator function" we can use to create new
#     objects from its class (see constructor functions for usage).
#     not really necessary, could also just use `new("animal", ....)` there.

# prey -------------------------------------------------------------------------

#' hide scalar numeric in (0,1); only for (subclasses of) `prey`

.prey <- setClass("prey",
  contains = "animal", slots = c(hide = "numeric"),
  prototype = prototype(name = "a_prey", hide = 0.5),
  validity = function(object) {
    validate_animal(object)
  }
)

.mouse <- setClass("mouse",
  contains = "prey",
  validity = function(object) {
    validate_animal(object, weight_range = c(.5, 1), hide_range = c(.6, 1))
  }
)

.rabbit <- setClass("rabbit",
  contains = "prey",
  validity = function(object) {
    validate_animal(object, weight_range = c(1, 5), hide_range = c(.3, .8))
  }
)

.deer <- setClass("deer",
  contains = "prey",
  validity = function(object) {
    validate_animal(object, weight_range = c(10, 40), hide_range = c(.2, .7))
  }
)

# predator ---------------------------------------------------------------------

#' seek  scalar numeric in (0,1); only for (subclasses of) `predator`

.predator <- setClass("predator",
  contains = "animal", slots = list(seek = "numeric"),
  prototype = prototype(name = "a_predator", seek = 0.5),
  validity = function(object) {
    validate_animal(object)
  }
)

.hawk <- setClass("hawk",
  contains = "predator",
  validity = function(object) {
    validate_animal(object, weight_range = c(3, 8), seek_range = c(.6, 1))
  }
)

.lynx <- setClass("lynx",
  contains = "predator",
  validity = function(object) {
    validate_animal(object, weight_range = c(20, 60), seek_range = c(.5, .9))
  }
)
