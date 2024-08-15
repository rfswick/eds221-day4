file_prefix <- c("temp", "pH", "salinity")
file_suffix <- c(1, 2, 3, 4)

for (i in 1:length(file_prefix)) {
  for (j in 1:length(file_suffix)){
    print(paste0(file_prefix[i], "_", file_suffix[j]))
  }
}

# Making functions

birddog_sum <- function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

x <- birddog_sum(bird = 2, dog = 5)

double_it <- function(x) {
  print(2*x)
}

double_it(4)
double_it(1:4)

exclaim_age <- function(age) {
  print(paste("I am", age, "years old!"))
}

exclaim_age(age = 28)

find_max <- function(val1, val2) {
  if(val1 > val2) {
    return(val1)
  } else if (val2 > val1) {
    return(val2)
  }
}

5 * find_max(7, 3)

quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)
starting_i <- c(1, 3, 5, 7)
for (i in starting_i) {
  half_splits <- quarter_splits[i] + quarter_splits[i + 1]
  print(half_splits)
}

# ! operator before variable checks to see if the user inputted variable is not in vector, if it is not in the vector, it returns TRUE and the error message will run
animal_age <- function(animal, age) {
  if (!animal %in% c("dog", "goat")) {
    stop("Oops! Animal must be a dog or goat!")
  }
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number greater than 0.")
  }
  if (age <= 0) {
    stop("The age must be a number greater than 0.")
  } 
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat"){
    print(age * 4.7)
  } 
}

animal_age( animal = "dog", age = -8)
animal_age(animal = "cow", age = 2)


dog_choice <- data.frame(dog_names = c("Khora", "Teddy", "Waffle", "Banjo"),
                         food = c("everything", "salmon", "pancakes", "chicken"))
library(tidyverse)

dog_menu <- function(name) {
  my_sub <- dog_choice |>
    dplyr::filter(dog_names == name)
  print(paste0("My name is ", my_sub$dog_names, ".", " And I love to eat ", my_sub$food, "."))
}

my_sub <- dog_choice |>
  dplyr::filter(dog_names == "Khora")
print(paste0("My name is ", my_sub$dog_names, ".", " And I love to eat ", my_sub$food, "."))

dog_menu("Khora")

calc_windpower <- function(rho, radius, windspeed) {
  if (windspeed > 130) {
    warning("Wow, that's fast! Are you sure?")
  }
  
  if (rho > 1.225) {
    warning("That air density is suspicious. Are you sure?")
  }
  
  if (radius < 0) {
    stop("Rotor radius must be a positive value (meters).")
  }
  
  print(0.3*rho*pi*(radius^2)*(windspeed^3))
}

calc_windpower(rho = 2, radius = 1, windspeed = 50)
















