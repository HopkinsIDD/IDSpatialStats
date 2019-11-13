context("estimate transdist temporal")

test_that("Data checks performed", {
  
  set.seed(1)
  dist.func <- alist(n=1, 
                     a=100, 
                     b=100, 
                     rnorm(n, a, b)) 
  
  a <- sim.epidemic(R=3,
                    gen.t.mean=7,
                    gen.t.sd=2,
                    min.cases=5,
                    tot.generations=3,
                    trans.kern.func=dist.func)
  
  a$t <- as.character(a$t)
  
  expect_that(est.transdist.temporal(epi.data=a,
                                         gen.t.mean=7,
                                         gen.t.sd=2,
                                         t1=0,
                                         max.sep=1e10,
                                         max.dist=1e10,
                                         n.transtree.reps=5), 
              throws_error())
  
  expect_that(est.transdist.temporal.bootstrap.ci(epi.data=a,
                                                  gen.t.mean=7,
                                                  gen.t.sd=2,
                                                  t1=0,
                                                  max.sep=1e10,
                                                  max.dist=1e10,
                                                  n.transtree.reps=5,
                                                  boot.iter=5,
                                                  ci.low=0.025,
                                                  ci.high=0.975),
              throws_error())
  
})

test_that("Outputs numeric vector of appropriate length", {
  
  set.seed(1)
  dist.func <- alist(n=1, 
                     a=100, 
                     b=100, 
                     rnorm(n, a, b)) 
  
  a <- sim.epidemic(R=3,
                    gen.t.mean=7,
                    gen.t.sd=2,
                    min.cases=5,
                    tot.generations=5,
                    trans.kern.func=dist.func)
  
  b <- est.transdist.temporal(epi.data=a,
                              gen.t.mean=7,
                              gen.t.sd=2,
                              t1=0,
                              max.sep=1e10,
                              max.dist=1e10,
                              n.transtree.reps=3)
  
  expect_true(is.data.frame(b))
  expect_true(is.numeric(b$pt.est))
  expect_true(length(unique(a[,3])) == nrow(b))
  
})

test_that("Border condition: zero transmission distance", {
  
  tmax <- 10
  x <- rep(0, tmax)
  y <- rep(0, tmax)
  t <- rep(0, tmax)
  for(i in 2:tmax) t[i] <- t[i-1] + 1
  a <- cbind(x, y, t)
  
  set.seed(1)
  b <- est.transdist.temporal(epi.data=a,
                              gen.t.mean=1,
                              gen.t.sd=0.001,
                              t1=0,
                              max.sep=5,
                              max.dist=1e10,
                              n.transtree.reps=2)
  
  expect_true(length(unique(a[,3])) == nrow(b))
  expect_true(sum(b$pt.est, na.rm=T) == 0)
  
  b <- est.transdist.temporal.bootstrap.ci(epi.data=a,
                                           gen.t.mean=1,
                                           gen.t.sd=0.001,
                                           t1=0,
                                           max.sep=5,
                                           max.dist=1e10,
                                           n.transtree.reps=2,
                                           boot.iter=2)
  
  expect_true(length(unique(a[,3])) == nrow(b))
  expect_true(sum(b$ci.low, na.rm=T) == 0)
  expect_true(sum(b$ci.high, na.rm=T) == 0)
  
})

test_that("Estimates confidence intervals", {
  
  set.seed(1)
  dist.func <- alist(n=1, a=1/100, rexp(n, a)) 
  
  a <- sim.epidemic(R=3,
                    gen.t.mean=7,
                    gen.t.sd=2,
                    min.cases=5,
                    tot.generations=5,
                    trans.kern.func=dist.func)
  
  b <- est.transdist.temporal.bootstrap.ci(epi.data=a,
                                           gen.t.mean=7,
                                           gen.t.sd=2,
                                           t1=0,
                                           max.sep=1e10,
                                           max.dist=1e10,
                                           n.transtree.reps=5,
                                           boot.iter=10,
                                           ci.low=0.025,
                                           ci.high=0.975)
  
  expect_true(is.data.frame(b))
  expect_true(is.numeric(b$pt.est))
  expect_true(is.numeric(b$ci.low))
  expect_true(is.numeric(b$ci.high))
  expect_true(length(unique(a[,3])) == nrow(b))

})

