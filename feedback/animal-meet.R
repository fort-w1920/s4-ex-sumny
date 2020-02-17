# idea: 'meet'-methods of subclasses determine the type of encounter (i.e.,
# <outcome>) based on the rules given in the exercise, then call the
# 'meet'-method for signature ('animal', 'animal') with CallNextMethod() to
# generate the output.

setGeneric("meet", function(animal1, animal2, ...) {
  standardGeneric("meet")
})

setMethod("meet", c("animal", "animal"), function(animal1, animal2, outcome) {
  label1 <- paste(class(animal1), sQuote(animal1@name))
  if (identical(animal1, animal2)) {
    return(paste(
      label1, "gazes at", ifelse(animal1@female, yes = "her", no = "his"),
      "reflection in a puddle\n"
    ))
  }
  label2 <- paste(class(animal2), sQuote(animal2@name))
  pair <- paste(label1, "&", label2)
  if (missing(outcome)) {
    stop(pair, " meet, but nothing seems to happen...")
  }
  # 'escape' switches order because first animal is the predator:
  switch(outcome,
    ignore = paste(pair, "ignore each other\n"),
    sniff = paste(pair, "sniff each others' butts\n"),
    sexytime = paste(pair, "make sweet, sweet love\n"),
    kill = paste(label1, "kills and eats", label2, "\n"),
    escape = paste(label2, "escapes from", label1, "\n"),
    fight = paste(pair, "fight for territory\n")
  )
})

# prey-prey --------------------------------------------------------------------

setMethod("meet", c("prey", "prey"), function(animal1, animal2) {
  if (same_species_different_sex(animal1, animal2)) {
    outcome <- sample(c("ignore", "sniff", "sexytime"),
      size = 1, prob = c(1 / 4, 1 / 4, 1 / 2)
    )
  } else {
    outcome <- sample(c("ignore", "sniff"), size = 1)
  }
  callNextMethod(animal1 = animal1, animal2 = animal2, outcome = outcome)
})

# predator-prey ----------------------------------------------------------------

setMethod("meet", c("predator", "prey"), function(animal1, animal2) {
  predator <- animal1
  prey <- animal2
  if (in_range(prey@weight, 0.05 * predator@weight, 0.7 * predator@weight)) {
    killrisk <- pmin(1, pmax(0, 0.6 + (predator@seek - prey@hide)))
    outcome <- sample(c("kill", "escape"),
      size = 1,
      prob = c(killrisk, 1 - killrisk)
    )
  } else {
    outcome <- sample(c("ignore", "sniff"), size = 1)
  }
  callNextMethod(animal1 = predator, animal2 = prey, outcome = outcome)
})

setMethod("meet", c("prey", "predator"), function(animal1, animal2) {
  # call 'predator-prey' method defined above
  meet(animal2, animal1)
})

# predator-predator ------------------------------------------------------------

setMethod("meet", c("predator", "predator"), function(animal1, animal2) {
  if (same_species_different_sex(animal1, animal2)) {
    outcome <- sample(c("fight", "sexytime"), size = 1)
  } else {
    outcome <- sample(c("ignore", "sniff", "fight"), size = 1)
  }
  callNextMethod(animal1 = animal1, animal2 = animal2, outcome = outcome)
})
