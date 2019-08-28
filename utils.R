histLogROE <- function(data, filename = "hist.pdf"){
  # Draw histogram of Log-transformed Absolute ROE
  
  # Args:
  # data - ROE data
  # filename - filename of saved plot
  
  par(mfrow = c(1,1))           # Set drawing panel
  pdf(filename)                 # Open pdf file 
  hist(log(abs(data$ROE)), 
       breaks = 100,
       main = NULL,
       xlab = "Log-transformed Absolute ROE",
       ylab = "Frequency")
  dev.off()
}

plotRPMSE <- function(RPMSE, filename = "RPMSE.pdf"){
  # Draw RPMSE of onestep/oneshot with different taus
  
  # Args:
  # RPMSE - RPMSE of ROE data using onestep/oneshot
  # filename - filename of saved plot
  
  win.graph(width=2800,height=2000)
  par(mfrow = c(3,2))
  for(i in 1:P)
  {
    plot(RPMSE[,i]~Tau, pch = 4, type = "o", col = "lightblue", ylim = c(0,0.4), 
         xlab = expression(tau), ylab = "RPMSE", lwd=2, xaxt = "n")
    axis(1, Tau, labels = Tau)
    lines(RPMSE[,i+P]~Tau, pch = 1, lty = 5, type = "o",col = "red", lwd = 2)
    abline(h = 1)
  }
  savePlot("RPMSE", type = c("pdf"), device = dev.cur(), restoreConsole = T)
  dev.off()
}