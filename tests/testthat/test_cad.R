library(otsad)
context("Contextual Anomaly Detector")

# Auxiliar function IntToBinarySens

test_that("IntToBinarySens", {
  expect_equal(IntToBinarySens(2,3), c(0,3,4))
  expect_equal(IntToBinarySens(0,4), 2*(0:(4-1))) 
})



# Auxiliar function for default testing

get.default.context.operator <- function(){
  context.operator <-  new.env()
  max.left.semicontexts <- 7
  assign("max.left.semicontexts", max.left.semicontexts, envir = context.operator)
  assign("fact.dict", list(left = new.env(), right = new.env()), 
         envir = context.operator)
  assign("sctxt.dict", list(left = new.env(), right = new.env()), 
         envir = context.operator)
  assign("sctxt.value.list", list(left = list(), right = list()), 
         envir = context.operator)
  assign("crossed.sctxt.list", list(left = list(), right = list()), 
         envir = context.operator)
  assign("ctxt.value.list", list, envir = context.operator)
  assign("new.ctxt.id", FALSE, envir = context.operator)
  
  return(context.operator)
}

get.default.local.environment <- function(){
  context.operator <- get.default.context.operator()
  local.environment <- new.env()
  rest.period <- 1
  max.active.neurons <- 15
  result.values.history <- 1.0
  base.threshold <- 0.75
  assign("rest.period", rest.period, envir = local.environment)
  assign("result.values.history", result.values.history, 
         envir = local.environment)
  assign("max.active.neurons.num", max.active.neurons, 
         envir = local.environment)
  assign("context.operator", context.operator, 
         envir = local.environment)
  assign("potential.new.ctxt", list(), 
         envir = local.environment)
  assign("left.facts", list(), 
         envir = local.environment)
  assign("base.threshold", base.threshold,
         envir = local.environment)
  
  return(local.environment)
}


test_that("UpdateContextsAndGetActive-First",{
  expect_equal(UpdateContextsAndGetActive.CAD(new.ctxt.flag = FALSE,
                                              get.default.context.operator()),
               list(list(), 0, list()))
})

test_that("ContextCrosser-First-LOR.1",{
  expect_equal(ContextCrosser.CAD(left.or.right = 1, 
                                  new.ctxt.flag = FALSE,
                                  fact.list = c(0,2,4),
                                  potential.new.contexts = list(),
                                  context.operator = get.default.context.operator()),
               list(list(), 0, list()))
})
test_that("ContextCrosser-First-LOR.0",{
  expect_equal(ContextCrosser.CAD(left.or.right = 0, 
                                  new.ctxt.flag = FALSE,
                                  fact.list = c(0,2,4),
                                  potential.new.contexts = list(),
                                  context.operator = get.default.context.operator()),
               0)
})

test_that("Step-First",{
  # Creation of Context operator
  local.environment <- get.default.local.environment()  
  expect_equal(Step.CAD(datum = c(0,2,4), local.environment = local.environment),
               c(0,0))
})

test_that("GetAnomalyScore-First", {
  expect_equal(GetAnomalyScore.CAD(datum = c(0,2,4), 
                                   local.environment = get.default.local.environment()), 
               0.0)
})

test_that("CAD-First", {
  expect_equal(ContextualAnomalyDetector(data = -1.749), 0.0)
})

test_that("GetContextByFacts", {
    context.list <- list(left = list(c(0,2,4)),
                        right = list(c(1,2,4)))
    context.operator <- get.default.context.operator()
    context.operator$context.list <- context.list
    expect_equal(GetContextByFacts.CAD(context.list,
                                       context.operator, 
                                       zero.level = 1),
                 TRUE)
})

test_that("CAD-FirstReal", {
  expect_equal(ContextualAnomalyDetector(data = c(-1.749, 0.3426804, 1.1530358),
                                         rest.period = 2), 
                                         rep(0.0, 3))
})
