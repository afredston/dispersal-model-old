pred_prey <- function(t, x, pars){
  N <- x[1]
  P <- x[2]
  r = pars[1]
  C = pars[2]
  B = pars[3]
  d = pars[4]
  
  dN = r * N - C * P * N
  dP = B * P * N - d * P
  list(c(dN, dP))
}

library(deSolve)

  t = seq(from=2, to=10, by=0.1) #time interval
  init = c(0.4, 0.2) #inital values
  r = 1
  d = 1
  C = 1
  B = 6.5
  pars = c(r, C, B, d)
  predprey_output <- ode(init, t, pred_prey, pars)
  par(mfrow=c(1,2))
  plot(predprey_output[,'time'], predprey_output[,2], col="blue", lwd=2,xlab="Time", ylab="Prey", main="Prey", type="l")
  grid()
  plot(predprey_output[,'time'], predprey_output[,3], col="red", lwd=2,xlab="Time", ylab="Predator", main="Predator",type="l")
  grid()