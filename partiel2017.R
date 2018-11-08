## Question 1
afficheSameEffect <- function(x,adding=FALSE){
  k <- 1 + log2(length(x));
  k <- round(k)
  x <- sort(x)
  bornes <- c(x[1]-0.025*(x[length(x)]-x[1]), quantile(x,seq(1,k-1)/k),x[length(x)]+0.025*(x[length(x)]-x[1]))
  bornes <- c(bornes, 0)
  print(bornes)
  histx <- hist(x, prob=T, breaks=bornes, main="Meme effectif", add=adding, col=rgb(0,0,1,0.5))
  midl <- histx$mid ;
  mlon <- length(midl);
  densites <- histx$density
  segments(midl[1:mlon-1],densites[1: mlon-1],
           midl[2:mlon],densites[2:mlon],
           lwd=3)
}


ech = c(-0.97,-0.96,-0.93,-0.91,-0.90,-0.87,-0.85,-0.80,-0.75,-0.73,-0.66,-0.65,-0.63,-0.60,-0.58,-0.51,-0.48,-0.45,-0.44,-0.40,-0.37,-0.33,-0.30,-0.25,-0.12,-0.08,0.25,0.41,0.51,0.68)
triangedensity <- function(x,t){
  res = c()
  for(v in x){
    res = c(res,(t-v)/(2*t^2))
  }
  return(res)
} 

#par(c(2,1))
plot(seq(-1,1,0.1),triangedensity(seq(-1,1,0.1),1))
afficheSameEffect(ech, TRUE)

## Question 2

triangFDR <- function(x,t){
    return(1-(t-x)^2/(4*t^2))
} 

edf <- function(n,x){
  return((1/n)*sum(x))
}

cumuedf <-function(edf){
  return(sqrt(1-edf))
}

#plot(seq(-1,1,0.1),triangFDR(seq(-1,1,0.1),1))
#resEDF = edf(seq(1,10,0.1),ech)
print(length(resEDF))
#plot(seq(0,1),cumuedf(resEDF), col="red")
#plot(seq,sqrt)
