set.seed(1)
nrep <- 10000 # 10000000
scores <- numeric(length=0)
plot(x = 0, y = 0, ylim = c(11.5, 13.5), xlim=c(0,nrep), pch=20, 
     xlab="index", ylab="mean")
for(i in 1:nrep){
  rolls <- sample(1:6, size=4, replace=TRUE)
  scores[i] <- sum(rolls[-which.min(rolls)])
  #points(i, mean(scores))
  if(i%%10 == 0) print (i)
  points(i, mean(scores[scores > 0]), pch=20)
}
abline(h=12.2446, col='red')
mean(scores)
sd(scores)
scores <- sort(scores)
scores[c(0.025*nrep,0.975*nrep)]
hist(scores)
#barplot(scores)

ord.4 <- function(x){
  ((1/6)^4)*(4*x^3 - 6*x^2 + 4*x - 1)
}
prob.4 <- ord.4(c(1:6))
ev.4 <- sum(c(1:6)*prob.4)

ord.3 <- function(x){
  ord.4(x) + ((1/6)^4)*4*((x^3)*(6-x) - ((x-1)^3)*(7-x))
}
prob.3 <- ord.3(c(1:6))
ev.3 <- sum(c(1:6)*prob.3)

ord.2 <- function(x){
  ord.3(x) + ((1/6)^3)*((x^2)*((6-x)^2) - ((x-1)^2)*((7-x)^2))
}
prob.2 <- ord.2(c(1:6))
ev.2 <- sum(c(1:6)*prob.2)
