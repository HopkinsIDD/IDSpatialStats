case.times <- c(1,2,2,3,3)
gen <- c(0, 2/3, 1/3, 0, 0)
t.density <- gen/sum(gen)

a <- est.wt.matrix(case.times=case.times, gen.t.dist=t.density)
