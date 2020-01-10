library(methods)

# random pronouncable strings with length <length>
make_name <- function(length = 7L) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1L] <- sample(toupper(consonants), 1L)
  name[seq(3L, length, by = 2L)] <-
    sample(consonants, size = ceiling(length / 2L) - 1L, replace = TRUE)
  name[seq(2L, length, by = 2L)] <-
    sample(vowels, size = floor(length / 2L), replace = TRUE
  )
  paste(name, collapse = "")
}

# random biological sex (logical w.r.t. female)
make_sex <- function() {
  sample(c(FALSE, TRUE), size = 1L, prob = c(0.5, 0.5))
}

# animal class, constructor and validator
setClass("animal",
  slots = c(
    name = "character",
    weight = "numeric",
    female = "logical"
  )
)
animal <- function(name, weight, female) {
  new("animal", name = name, weight = weight, female = female)
}
setValidity("animal", function(object) {
  if (length(object@name) != 1L || identical(object@name, "")) {
    return("@name must be a non-empty character of length 1.")
  }
  if (length(object@weight) != 1L || object@weight < 0 || is.na(object@weight)) {
    return("@weight must be a numeric of length 1 in [0, Inf].")
  }
  if (length(object@female) != 1L) {
    return("@female must be a logical of length 1.")
  }
  TRUE
})

# prey subclass
setClass("prey",
  contains = "animal",
  slots = c(
    hide = "numeric"
  )
)

# mouse subclass and constructor and validator
setClass("mouse",
  contains = "prey"
)
mouse <- function(name = make_name(), weight = runif(1L, min = 0.5, max = 1),
  female = make_sex(), hide = runif(1L, min = 0.6, max = 1)) {
  new("mouse", name = name, weight = weight, female = female, hide = hide)
}
setValidity("mouse", function(object) {
  if (object@weight < 0.5 || object@weight > 1) {
    return("@weight must be in [0.5, 1]")
  }
  if (object@hide < 0.6 || object@hide > 1) {
    return("@hide must be in [0.6, 1]")
  }
  TRUE
})

# rabbit subclass and constructor and validator
setClass("rabbit",
  contains = "prey"
)
rabbit <- function(name = make_name(), weight = runif(1L, min = 1, max = 5),
  female = make_sex(), hide = runif(1L, min = 0.3, max = 0.8)) {
  new("rabbit", name = name, weight = weight, female = female, hide = hide)
}
setValidity("rabbit", function(object) {
  if (object@weight < 1 || object@weight > 5) {
    return("@weight must be in [1, 5]")
  }
  if (object@hide < 0.3 || object@hide > 0.8) {
    return("@hide must be in [0.3, 0.8]")
  }
  TRUE
})

# deer subclass and constructor and validator
setClass("deer",
  contains = "prey"
)
deer <- function(name = make_name(), weight = runif(1L, min = 15, max = 30),
  female = make_sex(), hide = runif(1L, min = 0.2, max = 0.7)) {
  new("deer", name = name, weight = weight, female = female, hide = hide)
}
setValidity("deer", function(object) {
  if (object@weight < 15 || object@weight > 30) {
    return("@weight must be in [15, 30]")
  }
  if (object@hide < 0.2 || object@hide > 0.7) {
    return("@hide must be in [0.2, 0.7]")
  }
  TRUE
})

# predator subclass
setClass("predator",
  contains = "animal",
  slots = c(
    seek = "numeric"
  )
)

# hawk subclass and constructor and validator
setClass("hawk",
  contains = "predator"
)
hawk <- function(name = make_name(), weight = runif(1L, min = 3, max = 8),
  female = make_sex(), seek = runif(1L, min = 0.6, max = 1)) {
  new("hawk", name = name, weight = weight, female = female, seek = seek)
}
setValidity("hawk", function(object) {
  if (object@weight < 3 || object@weight > 8) {
    return("@weight must be in [3, 8]")
  }
  if (object@seek < 0.6 || object@seek > 1) {
    return("@seek must be in [0.6, 1]")
  }
  TRUE
})

# lynx subclass and constructor and validator
setClass("lynx",
  contains = "predator"
)
lynx <- function(name = make_name(), weight = runif(1L, min = 20, max = 60),
  female = make_sex(), seek =  runif(1L, min = 0.5, max = 0.9)) {
  new("lynx", name = name, weight = weight, female = female, seek = seek)
}
setValidity("lynx", function(object) {
  if (object@weight < 20 || object@weight > 60) {
    return("@weight must be in [20, 60]")
  }
  if (object@seek < 0.5 || object@seek > 0.9) {
    return("@seek must be in [0.5, 0.9]")
  }
  TRUE
})

# show methods
setMethod("show", signature("animal"), function(object) {
  cat("## ", class(object), " '", object@name, "' ", "(",
    if (object@female) "f" else "m", ") ", "\n", sep = "")
  cat("##   weight: ", object@weight, "\n", sep = "")
})
setMethod("show", signature("prey"), function(object) {
  callNextMethod()
  cat("##   hide: ", object@hide, "\n", sep = "")
})
setMethod("show", signature("predator"), function(object) {
  callNextMethod()
  cat("##   seek: ", object@seek, "\n", sep = "")
})


# meet generic
setGeneric("meet",
  function(animal_1, animal_2, prob = NULL) standardGeneric("meet"))
setMethod("meet", signature("animal", "animal"),
  function(animal_1, animal_2, prob) {
  if (identical(animal_1, animal_2)) {
    return(paste0(class(animal_1), " '", animal_1@name, "' ", "gazes at ",
                  ifelse(animal_1@female, "her", "his"),
                  " reflection in a puddle\n"))
  }
  cat(sample(c(
             paste0(class(animal_1), " '", animal_1@name, "' ", "& ",
                    class(animal_2), " '", animal_2@name, "' ",
                    "ignore each other\n"),
             paste0(class(animal_1), " '", animal_1@name, "' ", "& ",
                    class(animal_2), " '", animal_2@name, "' ",
                    "sniff each others' butt\n"),
             paste0(class(animal_1), " '", animal_1@name, "' ", "& ",
                    class(animal_2), " '", animal_2@name, "' ",
                    "make sweet, sweet love\n"),
             paste0(class(animal_1), " '", animal_1@name, "' ", "& ",
                    class(animal_2), " '", animal_2@name, "' ",
                    "fight for territory\n"),
             paste0(class(animal_2), " '", animal_2@name, "' ",
                    "kills and eats ", class(animal_1), " '", animal_1@name,
                    "\n"),
             paste0(class(animal_1), " '", animal_1@name, "' ",
                    "escapes from ", class(animal_2), " '", animal_2@name,
                    "\n")
             ), size = 1L, prob = prob)
  )
})
setMethod("meet", signature("prey", "prey"),
  function(animal_1, animal_2, prob) {
  prob <- if ((class(animal_1) == class(animal_2)) &&
    (animal_1@female != animal_2@female)) {
    c(1/4, 1/4, 1/2, 0, 0, 0)
  } else {
    c(1/2, 1/2, 0, 0, 0, 0)
  }
  callNextMethod(animal_1 = animal_1, animal_2 = animal_2, prob = prob)
})
setMethod("meet", signature("predator", "predator"),
  function(animal_1, animal_2, prob) {
  prob <- if ((class(animal_1) == class(animal_2)) &&
    (animal_1@female != animal_2@female)) {
    c(0, 0, 1/2, 1/2, 0, 0)
  } else {
    c(1/3, 1/3, 0, 1/3, 0, 0)
  }
  callNextMethod(animal_1 = animal_1, animal_2 = animal_2, prob = prob)
})
setMethod("meet", signature("prey", "predator"),
  function(animal_1, animal_2, prob) {
  prob <- if ((animal_1@weight >= 0.05 * animal_2@weight) &&
    (animal_1@weight <= 0.7 * animal_2@weight)) {
    prob_kill <- min(1, max(0, 0.6 + animal_2@seek - animal_1@hide))
    c(0, 0, 0, 0, prob_kill, 1 - prob_kill)
  } else {
    c(1/2, 1/2, 0, 0, 0, 0)
  }
  callNextMethod(animal_1 = animal_1, animal_2 = animal_2, prob = prob)
})
setMethod("meet", signature("predator", "prey"),
  function(animal_1, animal_2, prob) {
  tmp <- animal_2
  animal_2 <- animal_1
  animal_1 <- tmp
  callNextMethod(animal_1 = animal_1, animal_2 = animal_2, prob = prob)
})

