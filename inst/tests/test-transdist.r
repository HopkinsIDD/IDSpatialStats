context("estimate transdist")

test_that("Data checks performed", {
  
  msg <- 'Epidemic data must be an integer or numeric matrix'
  msg <- paste(strwrap(msg), collapse = "\n")
  
  set.seed(1)
  dist.func <- alist(n=1, 
                     a=100, 
                     b=100, 
                     rnorm(n, a, b)) 
  
  a <- sim.epidemic(R=1.5,
                    gen.t.mean=7,
                    gen.t.sd=2,
                    min.cases=10,
                    tot.generations=12,
                    trans.kern.func=dist.func)
  
  expect_that(est.transdist(epi.data=as.character(a),
                            gen.t.mean=7,
                            gen.t.sd=2,
                            t1=0,
                            max.sep=1e10,
                            max.dist=1e10,
                            n.transtree.reps=10,
                            silent=TRUE), 
              throws_error(msg))
  
  expect_that(est.transdist(epi.data=as.data.frame(a),
                            gen.t.mean=7,
                            gen.t.sd=2,
                            t1=0,
                            max.sep=1e10,
                            max.dist=1e10,
                            n.transtree.reps=10,
                            silent=TRUE), 
              throws_error(msg))
})

test_that("Outputs list and numerics", {
  
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
  
  b <- est.transdist(epi.data=a,
                     gen.t.mean=7,
                     gen.t.sd=2,
                     t1=0,
                     max.sep=1e10,
                     max.dist=1e10,
                     n.transtree.reps=5,
                     silent=TRUE)
  
  expect_true(is.list(b))
  expect_true(is.numeric(b$mu.est))
  expect_true(is.numeric(b$sigma.est))
  expect_true(is.numeric(b$bound.mu.est))
  expect_true(is.numeric(b$bound.sigma.est))
  
})

test_that("Border condition: zero transmission distance ", {
  
  tmax <- 10
  x <- rep(0, tmax)
  y <- rep(0, tmax)
  t <- rep(0, tmax)
  for(i in 2:tmax) t[i] <- t[i-1] + 1
  a <- cbind(x, y, t)
  
  set.seed(1)
  b <- est.transdist(epi.data=a,
                     gen.t.mean=1,
                     gen.t.sd=0.001,
                     t1=0,
                     max.sep=1e10,
                     max.dist=1e10,
                     n.transtree.reps=5,
                     silent=TRUE)
  
  expect_true(b$mu.est == 0)
  expect_true(b$sigma.est == 0)
  expect_true(b$bound.mu.est == 0)
  expect_true(b$bound.sigma.est == 0)
})

