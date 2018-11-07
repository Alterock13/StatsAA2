x <- c(0.22,1.64,5.06,0.24,1.96,6.41,0.29,2.27,7.58,0.29,2.44,7.81,0.33,2.75,8.00,0.47,2.99,8.24,0.85,3.15,10.15,1.14,3.85,12.24,1.50,4.66,13.78,1.51,5.04,16.12)

expQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e],log(1-seq(1:e)/(e+1)),ylim=c(-2.5,0.1), main="Q-Q Plot for exp law")
  #qqplot(qexp(ppoints(length(x))), x, main="Q-Q Plot for exponential law", xlab = "Theoritical quantities", ylab = "Sample quantities");
  #qqline(x, distribution = qexp, col="red");  abline(v=0)
  abline(h=0)
}

normQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qnorm(seq(1:e)/(e+1)), main="Q-Q Plot for normal law")
  #qqplot(qnorm(ppoints(length(x))), x, main="Q-Q Plot for normal law", xlab = "Theoritical quantities", ylab = "Sample quantities");
  #qqline(x, distribution = qnorm, col="red");
  abline(h=0)
}

uniQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qunif(seq(1:e)/(e+1)), main="Q-Q Plot for uni law")
  #qqplot(qunif(ppoints(length(x))), x, main="Q-Q Plot for uniform law", xlab = "Theoritical quantities", ylab = "Sample quantities")
  #qqline(x, distribution = qunif, col="red") 
  abline(v=0)
  abline(h=0)
}

fAllQQPlot <- function(x){
  expQQplot(x)
  normQQplot(x)
  uniQQplot(x)
}

afficheSameLarg <- function(x){
  #nbClasses = 1 + log2(length(x));
  x <- sort(x);
  plot(density(x));
  histx <- hist(x, prob=T, main="Meme largeur", col="red",add=TRUE);
  midl <- histx$mid ;
  mlon <- length(midl);
  densites <- histx$density;
  segments(midl[1:mlon-1],densites[1: mlon-1],
           midl[2:mlon],densites[2:mlon],
           lwd=3);
}

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

par(mfrow=c(2,3))
#Moyenne empirique
moy <- mean(x)
#
lambda = 1/moy;
cat("\nlambda = ",lambda)

simuExp <- function(nIt){
  lambdaSim <- 0
  lambda <-10
  scoreDispersion <- 0
  for(i in 1:nIt){
    #vect <- runif(30, 0, lambda)
    #lambdaSim <- max(vect) + lambdaSim
    vect <- rexp(30, lambda)
    lambdaSim <- 1/mean(vect) + lambdaSim
    scoreDispersion <- scoreDispersion + abs(lambdaSim-lambda);
  }
  lambdaSim <- lambdaSim/nIt;
  cat("\nLambdaSim",lambdaSim)
  cat("\nBiais",lambdaSim/lambda)
  cat("\nPourcentageBiais = ", ((lambdaSim/lambda)*100)-100, "%")
  cat("\nScore dispersion = ", scoreDispersion)
  #cat("\nBetter Lambda = ", lambda-((lambdaSim/lambda)*100)-100)
}


fAllQQPlot(x)
afficheSameEffect(x)
simuExp(10000)

#lambdaSim != lambda donc biais
#calculBiais = (LambdaSim/LambdaRExp)*100
#calculConvergence -> FAIRE UNE SEULE SIMU

#simuExp(1)
solution = log(0.9)/-(lambda*(30/31))
cat("\nSolution = ", solution)
