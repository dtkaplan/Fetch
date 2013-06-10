context('Testing fetch functions.')

test_that(".csv works (from online directory)",{
  tst <- fetch("BodyFat.csv")
  expect_that(nrow(tst),equals(252))
}) 

test_that(".r works",{
  fetch("DiffEQ.R")
  expect_true(exists("make.predator.prey"))
}) 
remove(reset.dyn.functions,fhn, predator.prey, competition, newton.cooling, SIR, RJ,
       make.predator.prey, make.competition, make.fhn, make.newtoncooling, make.SIR,
       make.RJ, show.traj, show.nullclines, jacobianAtPoint, traj.plot, soln.plot,
       flow.plot, solve.DE, rk)

test_that(".rdata & .rda works",{
  fetch("CFPB.rda")
  expect_that(nrow(CFPB),equals(91555))
}) 
remove(CFPB)

test_that(".rmd & .html works",{
  tst <- fetch("USCOTS/syllabus-example.Rmd")
  expect_true(grepl("Randomization-based Inference",tst, fixed=TRUE))
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
  fetch("DTK/whistle.Rdata")
  tst <- fetch("DTK/stan-data.csv")
  expect_that(length(whistle),equals(110250))
  expect_that(nrow(tst),equals(222))
})

test_that("fetch from cwd works", {
  tstM <- matrix(0,10,10)
  write.table(tstM,file="tst.csv",sep=",")
  tst <- fetch("tst.csv")
  unlink("tst.csv")
  expect_that(prod(dim(tst) == c(10,10)),equals(1))
  expect_that(sum(sum(tst != 0)), equals(0))
})