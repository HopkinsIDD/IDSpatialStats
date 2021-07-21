data(DengueSimR01)

g <- get.cross.PCF(epi.data=DengueSimR01, type=5, hom=2, het=NULL, r=NULL, correction='none')

plot(g$pcf, type='l', xlab='r', ylab='cross PCF')
abline(h=1, col='red', lty=2)
