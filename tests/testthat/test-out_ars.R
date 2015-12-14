context("test-out_ars: Check output of ars matches known distributions")

test_that("test-out_ars: Outputs are Validated", {
  
  # Test 1 - Check that output of ars is normal when input is normal pdf
  # set.seed(0)
  # n <- 1000
  # D <- c(-Inf,Inf)
  # out <- ars(n,dnorm,D)
  # p <- ks.test(out,pnorm)$p.value
  # expect_gt(p,0.01)
  # 
  # # Test 2 - Check that output of ars is exponential when input is exponential
  # set.seed(0)
  # # pdf
  # n <- 1000
  # D <- c(0,Inf)
  # out <- ars(n,dexp,D)
  # p <- ks.test(out,pexp)$p.value
  # expect_gt(p,0.01)
  # 
  # # Test 3 - Check that output of ars is uniform when input is uniform pdf
  # set.seed(0)
  # n <- 1000
  # D <- c(0,1)
  # out <- ars(n,dunif,D)
  # p <- ks.test(out,punif)$p.value
  # expect_gt(p,0.01)
  
})