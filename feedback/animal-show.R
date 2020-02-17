setMethod("show", c("animal"), function(object) {
  label <-
    paste(class(object), sQuote(object@name),
          ifelse(object@female, "(f)", "(m)"), "\n",
          " weight:", format(object@weight, digits = options()$digits), "\n")
  cat(label)
})

setMethod("show", c("predator"), function(object) {
  callNextMethod()
  cat("  seek: ",  format(object@seek, digits = options()$digits), "\n")
})

setMethod("show", c("prey"), function(object) {
  callNextMethod()
  cat("  hide: ",  format(object@hide, digits = options()$digits), "\n")
})
