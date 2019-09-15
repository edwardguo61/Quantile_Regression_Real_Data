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

plotDL <- function(LOSS, filename = "DL.pdf"){
  # Draw DL of onestep/oneshot with different taus
  
  # Args:
  # LOSS - LOSS of global/onestep/oneshot rqs for tau sequence
  # filename - filename of saved plot
  
  win.graph(width = 2000, height = 1400)
  par(mfrow = c(1,1))
  # plot the DL of oneshot estimation
  plot(LOSS$loss_oneshot - LOSS$loss_global ~ Tau,
       pch = 4, type = "o", col = "lightblue", lwd = 2,
       xlab = expression(tau), ylab = "DL", xaxt = "n")
  axis(1, Tau, labels = Tau)
  # plot the DL of onestep estimation
  lines(LOSS$loss_onestep - LOSS$loss_global ~ Tau,
        pch = 1, lty = 5, type = "o", col = "red", lwd = 2)
  savePlot(filename, type = c("pdf"), device = dev.cur(), restoreConsole = T)
  dev.off()
}