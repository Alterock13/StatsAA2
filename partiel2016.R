expQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e],log(1-seq(1:e)/(e+1)),ylim=c(-2.5,0.1), main="Q-Q Plot for exp law")
  abline(v=0)
  abline(h=0)
}

normQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qnorm(seq(1:e)/(e+1)), main="Q-Q Plot for normal law")
  abline(h=0)
}

uniQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qunif(seq(1:e)/(e+1)), main="Q-Q Plot for uni law")
  abline(v=0)
  abline(h=0)
}

afficheSameLarg <- function(x){
  #nbClasses = 1 + log2(length(x));
  x <- sort(x)
  histx <- hist(x, prob=T, main="Meme largeur", col="red")
  midl <- histx$mid ;
  mlon <- length(midl);
  densites <- histx$density
  segments(midl[1:mlon-1],densites[1: mlon-1],
           midl[2:mlon],densites[2:mlon],
           lwd=3)
}

afficheSameEffect <- function(x){
  k <- 1 + log2(length(x));
  k <- round(k)
  x <- sort(x)
  bornes <- c(x[1]-0.025*(x[length(x)]-x[1]), quantile(x,seq(1,k-1)/k),x[length(x)]+0.025*(x[length(x)]-x[1]))
  print(bornes)
  histx <- hist(x, prob=T, breaks=bornes, main="Meme effectif", col="blue",)
  midl <- histx$mid ;
  mlon <- length(midl);
  densites <- histx$density
  segments(midl[1:mlon-1],densites[1: mlon-1],
           midl[2:mlon],densites[2:mlon],
           lwd=3)
}


ech = c(0.22,0.24,0.29,0.33,0.47,0.85,1.14,1.5,1.51,1.64,1.96,2.27,2.44,2.75,2.99,3.15,3.85,4.66,5.04,5.06,6.41,7.58,7.81,8.00,8.24,10.15,12.24,13.78,16.12)

expQQplot(ech)

#cat("lambda",lambda,"\n")
afficheSameLarg(ech)

## On essaye avec la méthode des moments
Moyenne = sum(ech)/length(ech)
print(Moyenne)
lambda = 1/Moyenne

estim = function(n, lambda, j){
  lexp = c(n);
  for (i in 1:n){
    val = rexp(j, lambda)
    #
    lexp[i] = (1/mean(val))
    #lexp[i] = (max(val))
  }
  return(lexp)
}


a = estim(100000,10,30)

print(paste('Biais', abs(10 - mean(a))))
print(paste('Conv', var(a)))
print(mean(lambdaempiri))