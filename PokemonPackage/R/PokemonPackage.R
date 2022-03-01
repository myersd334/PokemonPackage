# for all functions, you can return values based on the first or random record if there are multiple matches,

# 1.  Write a function to print some information about a pokemon, Print its name, classification, and primary and secondary types
#' Function for getting data about a single pokemon
#' @param pokemon name of the pokemon we want information about
#' @examples pokeDex("Charmander")
#' @examples pokeDex("Litwick")
#' @return None
#' @export pokeDex
pokeDex<-function(pokemon){
  return(pokemon_data[pokemon_data$Pokemon.Name==pokemon,c(3,4, 10, 11)][1,])
}


# 2. Write a function that takes in 2 pokemon and determines if the attack stat of the first pokemon is higher than the defense stat of the second.
#' Function for battling pokemon
#' @param attacker name of the attacking pokemon
#' @param defender name of the defending pokemon
#' @examples atkCheck("Charmander","Urshifu" )
#' @return None
#' @export
atkCheck<-function(attacker, defender){
  if(pokemon_data[pokemon_data$Pokemon.Name==attacker, ]$Attack.Stat[[1]] > pokemon_data[pokemon_data$Pokemon.Name==defender, ]$Defense.Stat[[1]]){
    return(paste(attacker, "wins"))
  }else{
    return(paste(defender, "wins"))
  }#else
}

# 3. Write a function that returns the result of trying to catch the pokemon at a particular health
# using a modification of the formula found below catchProb=((3 * HPmax - 2 * HPcurrent) * ratemodified / (3 * HPmax))/100
#from here: https://bulbapedia.bulbagarden.net/wiki/Catch_rate#General_capture_method_.28Generation_II_onwards.29
#calculate the percent likelihood then sample c(TRUE,FALSE) with probabilities c(catchProb,1-catchProb) and return the result
#get the HPmax from the data
#' Function to see if you catch the pokemon or not
#' @param pokemon name of the pokemon we want to catch
#' @param HPcurrent current health of pokemon
#' @examples HPcurrent("Charmander", 20 )
#' @return None
#' @export
pokeBall<-function(pokemon, HPcurrent){
  HPmax <-pokemon_data[pokemon_data$Pokemon.Name==pokemon, ]$Health.Stat[[1]]
  ratemodified <-pokemon_data[pokemon_data$Pokemon.Name==pokemon, ]$Catch.Rate[[1]]
  catchProb <-((3 * HPmax - 2 * HPcurrent) * ratemodified / (3 * HPmax))/100
  catchStatus<-sample(c(T, F),size=1, prob=c(catchProb,1-catchProb),replace=T)
  return(catchStatus)
}


# Then
#1.  create a package with the data set and functions
#2.  roxygen notes, generate the documentation,
#3.  install the package
#4.  view your documentaion with ?pokeDex etc
#5.  use your functions
library(R6)
#' R6 Class representing a Pokemon
#'
#' A pokemon has a name, two types, and HP.
#' @importFrom R6 R6Class
#' @export
Pokemon = R6Class(
  "Pokemon",  #name of the class
  public = list( #list of publicly available fields (attributes) for the class
    #' @field name The name of the Pokemon.
    name = NULL,
    #' @field type1 The primary type of Pokemon.
    type1 = NULL,
    #' @field type2 The secondary type of Pokemon.
    type2 = NULL,
    #' @field starting_HP The starting number of HP points.
    starting_HP = NULL,
    #' @field updated_HP The updated number of HP points after the fight function is run.
    updated_HP = NULL,
    #' @field awake Whether or not the Pokemon is awake. The Pokemon is awake if the HP is greater than 0.
    awake = NULL,
    #' @description
    #' Create a Pokemon. Return the Pokemon name, primary type, secondary type, starting HP, and updated HP.
    #' @param name
    #' @return A new `Pokemon` object
    initialize = function(name = NA){
      self$name <- name
      self$type1 <- pokemon_data[pokemon_data$Pokemon.Name == self$name,c(10)]
      self$type2 <- pokemon_data[pokemon_data$Pokemon.Name == self$name,c(11)]
      self$starting_HP <- pokemon_data[pokemon_data$Pokemon.Name == self$name,c(25)]
      self$updated_HP = self$starting_HP
      self$awake <- TRUE},
    #' @description Give information about the Pokemon in list form.
    #' @examples
    #' p = Pokemon$new("Charmander")
    #' p$show()
    show = function(){
      cat("Name:", self$name, "\nType 1:",self$type1, "\nType 2:",self$type2, "\nStarting HP:", self$starting_HP, "\nUpdated HP:", self$updated_HP,"\nAwake:", self$awake[[self$awake]])
    },
    #' @description Change the Pokemon HP based on input from the user.
    #' @param c
    #' @examples
    #' p = new$Pokemon("Charmander")
    #' p$changeHP(3)
    changeHP = function(c){
      if(self$updated_HP <= 0){self$awake = FALSE}
      self$updated_HP = self$updated_HP + c},
    #' @description Take two pokemon and make them fight. Both Pokemon will lose 20 points per fight.
    #' @param `Pokemon`
    #' @examples p = Pokemon$new("Charmander")
    #' opponent = Pokemon$new("Squirtle")
    #' p$fight(opponent)
    #' @return
    fight = function(opponent){
      if (self$awake == TRUE && opponent$awake == TRUE){
        opponent$updated_HP = opponent$changeHP(-20)
        self$updated_HP = self$changeHP(-20)}
      else{print("Both pokemon must be awake to fight.")}
      }
  )
)

#' Pokemon Data
#' @format Each observation represents a different Pokemon. The observation includes the Pokemon name, type1, type2, starting HP, and whether or not they are awake.
#' @source: kaggle.com
'pokemon_data'

#Testing out adding coments to github
