\donttest{
# Load data
r.max<-seq(20,1000,20)
r.min<-seq(0,980,20)
r.mid<-(r.max+r.min)/2
sero.type.func<-function(a,b,tlimit=20){
        if(a[5]==b[5]&(abs(a[3]-b[3])<=tlimit)){rc=1}
        else{rc=2}
        return(rc)
}

data(DengueSimRepresentative)
sero.type.rep.func<-function(a,b,tlimit=20){
     if(a[5]==1&b[5]==1&(abs(a[3]-b[3])<=tlimit)){rc=1}
     else{if(a[5]==1&b[5]==-999){rc=2}else{rc=3}}
     return(rc)
}

# get point estimate
Dengue.tau = get.tau(DengueSimRepresentative, sero.type.rep.func, r.max, r.min, "representative",
                     data.frame = TRUE)

# perform graphical hypothesis test using a global envelope test
Dengue.GET = get.tau.GET(DengueSimRepresentative, sero.type.rep.func, r.max,r.min, 
                         permutations = 50, "representative")

# plot point estimate with global envelope and simulation of the null distribution
plot.tau(x = Dengue.tau, r.mid = TRUE, GET.res = Dengue.GET)

# if the graphical hypothesis test and p-value interval suggests evidence against H_0, 
# and the graph suggests clustering, the range of this can be estimated
tausim = get.tau.bootstrap(DengueSimRepresentative, sero.type.rep.func, r.max, r.min, 100, 
                           "representative", data.frame = FALSE)
Dengue.dparam = get.tau.D.param.est(r = r.max, tausim = tausim, Dengue.GET)
median(Dengue.dparam$envelope) # median estimate for the clustering endpoint
Dengue.dparam$envelope # 95% BCa CI
plot.tau(Dengue.tau, tausim = tausim, d.param.est = Dengue.dparam)
}