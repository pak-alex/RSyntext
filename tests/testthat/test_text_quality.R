context("Test text_quality")

#'

#' This script tests the text_quality function of the RSyntext package.
#'
#'
#' text_quality function of the RSyntext package, checks the
#' quality of the string in terms of spelling errors and toxicity content.
#' It takes in a string as input and returns a dataframe.
#' The function performs necessary cleaning on the input string.
#'



#' Test for output type

test_that("Test that output is of type dataframe", {

  txt <-  "This str has words spelllll wrong. This string has a slag word shitty."

  expect_true(class(text_quality(txt))=="data.frame")

})


#' Test for values returned in the dataframe
test_that("Test that spell error and toxic_words are lists", {

  txt <-  "This str has words spelllll wrong. This string has a slag word shitty."

  output <- text_quality(txt)

  expect_true(typeof(output$spell_error)=="list")
  expect_true(typeof(output$toxic_words)=="list")

})

#' Test for values returned in the dataframe
test_that("Test that count of spell error and toxic_words is integer", {

  txt <-  "This str has words spelllll wrong. This string has a slag word shitty."

  output <- text_quality(txt)

  expect_true(typeof(output$count_spell_error)=="integer")
  expect_true(typeof(output$count_toxic_words)=="integer")

})

#' Test for values returned in the dataframe
test_that("Test that proportion of spell error and toxic_words is double", {

  txt <-  "This str has words spelllll wrong. This string has a slag word shitty."

  output <- text_quality(txt)

  expect_true(typeof(output$proportion_spell_error)=="double")
  expect_true(typeof(output$proportion_toxic_words)=="double")

})


#' Test for values returned in the dataframe
test_that("Test that output contains non-negative values", {

  txt <-  "This str has words spelllll wrong. This string has a slag word shitty."

  output <- text_quality(txt)

  expect_true(typeof(output$count_spell_error)>=0)
  expect_true(typeof(output$count_spell_error)>=0)
  expect_true(typeof(output$proportion_spell_error)>=0)
  expect_true(typeof(output$proportion_toxic_words)>=0)

})



#' Test for valid output
test_that("Output should be empty", {


  output <- text_quality("")

  expect_true(output$spell_error=="")
  expect_true(output$toxic_words=="")
  expect_true(output$count_spell_error==0)
  expect_true(output$count_spell_error==0)
  expect_true(output$proportion_spell_error==0)
  expect_true(output$proportion_toxic_words==0)


})


#' Test for valid output
test_that("Output should be empty", {

  txt <- "!@#^&*(*&!&@_@_@)((@}}}}"

  output <- text_quality(txt)

  expect_true(output$spell_error=="")
  expect_true(output$toxic_words=="")
  expect_true(output$count_spell_error==0)
  expect_true(output$count_spell_error==0)
  expect_true(output$proportion_spell_error==0)
  expect_true(output$proportion_toxic_words==0)

})



#' Test for functionality
test_that("Test that text_quality gives the expected output", {

  txt <-  "This string is correct."

  output <- text_quality(txt)

  expect_true(output$spell_error=="")
  expect_true(output$toxic_words=="")
  expect_true(output$count_spell_error==0)
  expect_true(output$count_spell_error==0)
  expect_true(output$proportion_spell_error==0)
  expect_true(output$proportion_toxic_words==0)

})


#' Test for functionality
test_that("Test that text_quality gives the expected output", {

  txt <-  "This string is correct and has pronouns Harjyot, Alex, Yenan."

  output <- text_quality(txt)

  expect_true(output$spell_error=="")
  expect_true(output$toxic_words=="")
  expect_true(output$count_spell_error==0)
  expect_true(output$count_spell_error==0)
  expect_true(output$proportion_spell_error==0)
  expect_true(output$proportion_toxic_words==0)

})


#' Test for functionality
test_that("Test that text_quality gives the expected output", {

  txt <-  "This string has words spelllll wrong. This string has a slag word shitty."

  output <- text_quality(txt)

  expect_true(output$spell_error[[1]][1]=="spelllll")
  expect_true(output$toxic_words[[1]][1]=="shitty")
  expect_true(output$count_spell_error==1)
  expect_true(output$count_toxic_words==1)
  expect_true(output$proportion_spell_error>=0.07692308-0.02 &
                output$proportion_spell_error<=0.07692308+0.02)
  expect_true(output$proportion_toxic_words>=0.07692308-0.02 &
                output$proportion_toxic_words<=0.07692308+0.02)

})



