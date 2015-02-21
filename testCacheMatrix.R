source('cachematrix.R')

library("testthat")

test_that("test calculating and caching the inverse", {
  M <- c(1, 2, 3, 4, 5, 6, 7, 8, 8, 10, 20, 30, 40, 50, 60, 71)
  dim(M) <- c(4, 4) 
  Mc <- makeCacheMatrix(M)
  
  expect_equal(Mc$getinv(), NULL)
  
  Mc_inv <- cacheSolve(Mc)
  expect_equal(solve(M), Mc$getinv())
  expect_equal(solve(M), Mc_inv)
  
  Mc_inv <- cacheSolve(Mc)
  expect_equal(solve(M), Mc$getinv())
  expect_equal(solve(M), Mc_inv)
})