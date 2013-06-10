context('Testing fetch functions.')

test_that(".csv works (from online directory)",{
  tst <- fetch("BodyFat.csv")
}) 

test_that(".r works",{
  fetch("DiffEQ.R")
  expect_true(exists("make.predator.prey"))
}) 

test_that(".rdata & .rda works",{
  fetch("CFPB.rda")
  expect_that(nrow(CFPB),equals(91555))
}) 
remove(CFPB)

test_that(".rmd & .html works",{
  tst <- fetch("USCOTS/syllabus-example.Rmd")
  expect_true(grepl("Randomization-based Inference",test, fixed=TRUE))
}) 

test_that("fetch from packages works", {
  tst <- fetch("Marriage") ##normal input
  tst2 <- fetch("mosaic/Marriage") ## input with specified package
  tst3 <- fetch("Marriage.ext") ## input with undesired file extension
  expect_identical(tst, tst2)
  expect_identical(tst,tst3)
  expect_that(nrow(tst), equals(98))
})

test_that(".fetchRemote works", {
  
})