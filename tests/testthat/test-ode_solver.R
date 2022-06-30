test_that("ode solver correct", {
  lambda = 0.3
  Cap = 10
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, 0, Cap, forc)
  Teq = 1/lambda/Cap * Feq

  expect_gte(obs[21], Teq * 0.95)
  expect_lte(obs[21], Teq * 1.05)

  lambda = 0.3
  Cap = 20
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, 0, Cap, forc)
  Teq = 1/lambda/Cap * Feq

  expect_gte(obs[21], Teq * 0.95)
  expect_lte(obs[21], Teq * 1.05)

  lambda = 0.2
  Cap = 10
  Feq = 3
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, 0, Cap, forc)
  Teq = 1/lambda/Cap * Feq

  expect_gte(obs[21], Teq * 0.95)
  expect_lte(obs[21], Teq * 1.05)

  lambda = 0.3
  Cap = 20
  Feq = 5
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, 0, 0, Cap, forc)
  Teq = 1/lambda/Cap * Feq

  expect_gte(obs[21], Teq * 0.95)
  expect_lte(obs[21], Teq * 1.05)

  # influence of T0
  lambda = 0.3
  Cap = 10
  Feq = 3
  T0 = 1
  forc <- c(0, numeric(20) + Feq)
  obs <- solve_ebm(lambda, 1, T0, 0, Cap, forc)
  Teq = 1/lambda/Cap * Feq + T0

  expect_gte(obs[21], Teq * 0.95)
  expect_lte(obs[21], Teq * 1.05)
})

test_that("ODE solver for multibox", {


})
