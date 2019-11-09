\donttest{
     
set.seed(1)

dist_func <- alist(n=1, a=1/100, rexp(n, a)) # Exponential transmission kernel with mean = sd = 100

# Simulate epidemic with constant R value
a <- sim_epi(R=1.5,
             gen_t_mean=7,
             gen_t_sd=2,
             tot_gen=15,
             min_cases=100,
             kern_func=dist_func)

plot_sim(a)

# Simulate an epidemic with variable R value
r1 <- 2
r2 <- 0.25
tg <- 25
R <- seq(r1, r2, (r2 -r1)/(tg - 1))

b <- sim_epi(R=R,
             gen_t_mean=7,
             gen_t_sd=2,
             tot_gen=tg,
             min_cases=100,
             kern_func=dist_func)

plot_sim(b)

}
