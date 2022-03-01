library(PokemonPackage)
data("pokemon_data")
test_that("initialize function works for Poekmon that exists.", {
  p = Pokemon$new("Litwick")
  expect_equal(p$name, "Litwick")
  expect_equal(p$starting_HP, 50)
  expect_equal(p$type1, "Ghost")
  expect_equal(p$type2, "Fire")
  expect_equal(p$awake, TRUE)
})

#test_that("initialize function works for Pokemon that doesn't exist.",{
 # p = Pokemon$new("Darian")
  #expect_equal(p$name, NULL)
 # expect_equal(p$starting_HP, NULL)
 # expect_equal(p$type1, NULL)
  #expect_equal(p$type2, NULL)
  #expect_equal(p$awake, )
#})



