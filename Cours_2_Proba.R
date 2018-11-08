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

#ech = c(54.8,55.4,57.7,59.6,60.1,61.2,62.0,63.1,63.5,64.2,65.2,65.4,65.9,66.0,67.6,68.1,69.5,70.6,71.5,73.4);
#afficheSameEffect(ech)

## Exo 
ech <- runif(10000, 1)
#plot(ecdf(ech))
#par(mfrow = c(1, 2)) 
#afficheSameEffect(ech)
#afficheSameLarg(ech)

ech <- rexp(10000, 1)
expQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e],log(1-seq(1:e)/(e+1)),ylim=c(-2.5,0.1), main="Q-Q Plot for exp law")
  abline(v=0)
  abline(h=0)
}

ech <- rnorm(10000, 1)
normQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qnorm(seq(1:e)/(e+1)), main="Q-Q Plot for normal law")
  abline(h=0)
}

ech <- runif(10000, 1)
uniQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qunif(seq(1:e)/(e+1)), main="Q-Q Plot for uni law")
  abline(v=0)
  abline(h=0)
}

## Exo TP2
ech <- runif(1000)
normQQplot(ech)
uniQQplot(ech)
expQQplot(ech)