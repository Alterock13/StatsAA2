x <- c(0.22,0.24,0.29,0.33,0.47,0.85,1.14,1.50,1.51,1.64,1.96,2.27,2.44,2.75,2.99,3.13,3.85,4.66,5.04,5.06,6.41,7.58,7.81,8.00,8.24,10.15,12.24,13.78,16.12)

afficheSameEffect <- function(x){
  k <- 1 + log2(length(x));
  k <- round(k);
  x <- sort(x);
  bornes <- c(x[1]-0.025*(x[length(x)]-x[1]), quantile(x,seq(1,k-1)/k),x[length(x)]+0.025*(x[length(x)]-x[1]));
  plot(density(x), ylim=c(0,0.2), main="Histogramme même effectif et densité")
  histx <- hist(x, prob=T, breaks=bornes, main="Meme effectif", col="blue",add=TRUE)
  midl <- histx$mid ;
  mlon <- length(midl);
  densites <- histx$density
  segments(midl[1:mlon-1],densites[1: mlon-1],
           midl[2:mlon],densites[2:mlon],
           lwd=3)
}

afficheSameEffect(x)

expQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e],qexp(seq(1:e)/(e+1)), main="Q-Q Plot for exp law")
  abline(v=0)
  abline(h=0)
  abline(lm(qexp(seq(1:e)/(e+1)) ~ x[1:(e)]), col="red")
}

normQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qnorm(seq(1:e)/(e+1)), main="Q-Q Plot for normal law")
  abline(h=0)
  abline(lm(qnorm(seq(1:e)/(e+1)) ~ x[1:(e)]), col="red")
}

uniQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qunif(seq(1:e)/(e+1)), main="Q-Q Plot for uni law")
  abline(v=0)
  abline(h=0)
  abline(lm(qunif(seq(1:e)/(e+1)) ~ x[1:(e)]), col="red")
}



gammaQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qgamma(seq(1:e)/(e+1), shape=1), main="Q-Q Plot for Gamma law")
  abline(v=0)
  abline(h=0)
  abline(lm(qgamma(seq(1:e)/(e+1), shape=1) ~ x[1:(e)]), col="red")
}


fAllQQPlot <- function(x){
  par(mfrow=c(2,3))
  afficheSameEffect(x)
  plot(ecdf(x), main="Fontion de répartition empirique")
  expQQplot(x)
  normQQplot(x)
  uniQQplot(x)
  gammaQQplot(x)
}

fAllQQPlot(x)

#Moyenne empirique
moy <- mean(x)
#
lambda = 1/moy ## DEPEND DE LA LOI !!!

simuExp <- function(nIt){
  lambdaSim <- 0
  #lambda <- 10
  scoreDispersion <- 0
  for(i in 1:nIt){
    vect <- rexp(30, lambda)
    lambdaSim <- 1/mean(vect) + lambdaSim
    scoreDispersion <- scoreDispersion + abs(lambdaSim-lambda)
  }
  lambdaSim <- lambdaSim/nIt
  cat("\nlambda calcule = ",lambda)
  cat("\nlambda Simule = ",lambdaSim)
  cat("\nbiais = ", ((lambdaSim/lambda)*100)-100, "%")
  cat("\nScore dispersion = ", scoreDispersion)
}

simuExp(1)

#lambdaSim != lambda donc biais
#calculBiais = (LambdaSim/LambdaRExp)*100
#calculConvergence -> FAIRE UNE SEULE SIMU //MAIS AVEC UN GRAND N (ICI 30 INITIALEMENT)