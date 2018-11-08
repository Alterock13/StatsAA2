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


ech = c(1.0,2.1,2.8,3.7,5.3,6.0,6.5,7.2,8.2,9.5)
#cat("Marius :", 2*mean(ech))
#cat("\nJeannete :", max(ech))


valeur0 = runif(1000,0,11)

meanDiff = c()
VarianceTot = c()
MoyenneTot = c()

for(n in seq(10,100, by=10)){
  JeanetteMax = c()
  MaruisMax = c()
  JeanetteDiff = c()
  for(i in 0:1000){
    valeur = sample(valeur0,n)
    MaruisMax=c(MaruisMax,2*mean(valeur))
    JeanetteMax=c(JeanetteMax,max(valeur))
    JeanetteDiff=c(JeanetteDiff,11-max(valeur))
    #cat("Marius :", 2*mean(valeur))
    #cat("\nJeannete :", max(valeur))
  }
  meanDiff = c(meanDiff,mean(JeanetteDiff))
  VarianceTot = c(VarianceTot,var(JeanetteMax))
  MoyenneTot = c(MoyenneTot, mean(JeanetteMax))
}
print("Jeanette")
cat("Moyenne",mean(JeanetteMax))
cat("\nVariance",var(JeanetteMax),"\n")
print("Marius")
cat("Moyenne",mean(MaruisMax))
cat("\nVariance",var(MaruisMax),"\n")
print(length(meanDiff))
par(mfrow = c(1, 2)) 
plot(seq(10,100, by=10),meanDiff)
lines(seq(10,100, by=10),meanDiff, col="dark blue", lty=3)
#points(seq(10,100, by=10),VarianceTot)
#lines(seq(10,100, by=10),VarianceTot, col="dark red", lty=3)
plot(seq(10,100, by=10),MoyenneTot)
abline(h=11)
lines(seq(10,100, by=10),MoyenneTot, col="dark red", lty=3)