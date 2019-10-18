kernel_data <- function(stock, smoother, from = -0.25, to = 0.25){
  options(warn=-1)
  data <- na.omit(getSymbols.yahoo(stock, auto.assign = F))
  returns <- na.omit(ClCl(data))
  ker_fit <-density(returns, bw = "nrd0", adjust = smoother, kernel = "gaussian", 
                    weights = NULL, window = kernel, n = 512, from = from, to = to)

  return(ker_fit)
  options(warn=1)
}

# plot(ker_fit, main = paste("Density of the Returns of ", stock))
# range_1 <- ker_fit$x < 0; range_2 <- ker_fit$x > 0
# polygon(ker_fit, border="black", lwd = 3)
# polygon(c(ker_fit$x[range_1],0), c(ker_fit$y[range_1],0), col="#ff8566", border="#ffcccc", lwd = 0.1)
# polygon(c(0,ker_fit$x[range_2]), c(0,ker_fit$y[range_2]), col="#8dd88d", border="#d9f2d9", lwd = 0.1)
# par(usr = c(0, 1, 0, 1))
# text(0.15,0.5,paste(length(returns[which(returns <= from)])," smaller\n observations\n ←"))
# text(0.85,0.5,paste(length(returns[which(returns >= to)])," larger\n observations\n →"))