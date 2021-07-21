data(DengueSimR01)

k <- get.cross.K(epi.data=DengueSimR01, type=5, hom=2, het=NULL, r=NULL, correction='border')

plot(k[,2], type='l', col='red', lty=2, xlab='r', ylab='cross K function')
lines(k$border)
