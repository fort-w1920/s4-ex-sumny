# example code for animal class: (results may vary.)
set.seed(20191121)
animals <- list(
  mouse(female = TRUE),
  rabbit(),
  hawk(female = TRUE),
  deer(),
  lynx(female = TRUE),
  lynx(female = FALSE),
  deer(),
  mouse(female = FALSE),
  deer(female = TRUE)
)

for (animal1 in animals[1:5]) {
  for (animal2 in animals[9:5]) {
    cat(meet(animal1, animal2))
  }
}

