library(PokemonPackage)
data("pokemon_data")

test_that("awake changes to false if HP is less than 0.", {
  p1 = Pokemon$new("Squirtle")
  p2 = Pokemon$new("Charmander")
  while(p1$updated_HP > 0 && p2$updated_HP > 0){
    p1$fight(p2)
  }
  if(p1$updated_HP < p2$updated_HP){
    expect_equal(p1$awake, FALSE)
  }else{
    expect_equal(p2$awake, FALSE)
  }
})

test_that("if Pokemon is not awake it cannot fight.", {
  p1 = Pokemon$new("Golurk")
  p2 = Pokemon$new("Dewpider")
  p1$awake = FALSE
  expect_equal(p1$fight(p2), "Both pokemon must be awake to fight.")
  expect_equal(p2$fight(p1), "Both pokemon must be awake to fight.")
  })





