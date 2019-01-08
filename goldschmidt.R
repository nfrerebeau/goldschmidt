# Import data ==================================================================
goldschmidt <- read.table("goldschmidt.csv", sep = ",", dec = ".",
                          header = TRUE, row.names = NULL, encoding = "UTF-8")

# Plot diagram =================================================================
# Set graphical parameters -----------------------------------------------------
par(fig = c(0, 1, 0, 1), mar = c(4, 5, 1, 1) + 0.1,
    cex = 1, cex.axis = 1, cex.lab = 1, lwd = 1.2,
    xpd = FALSE, las = 1, pty = "s", new = FALSE)

# Plot diagram -----------------------------------------------------------------
plot(
  x = goldschmidt$z, y = goldschmidt$r,
  pch = c(17, 16, 0, 15)[interaction(goldschmidt$stokes, goldschmidt$ion, drop = TRUE)],
  col = c("#EE6677", "#228833", "#4477AA")[goldschmidt$ion],
  xlab = "Ionic charge", ylab = expression("Ionic radius ("*ring(A)*")"),
  xlim = c(0, 7), ylim = c(0, 2),
  bty = "l", xaxs = "i", yaxs = "i"
)

# Add labels -------------------------------------------------------------------
goldschmidt_lab <- as.character(goldschmidt$symbol)
ion <- which(goldschmidt_lab %in% c("Mg", "Cu", "Fe(II)", "Fe(III)",
                                    "Ce(III)", "Ce(IV)"))
goldschmidt_lab[ion] <- c("Cu\nMg","", # workaround
                          expression("Fe"^"2+"), 
                          expression("Fe"^"3+"), 
                          expression("Ce"^"3+"), 
                          expression("Ce"^"4+"))

text(
  x = goldschmidt$z, y = goldschmidt$r,
  labels = goldschmidt_lab,
  pos = c(1,2,2,2,2,2,2,2,3,2,2,2,1,2,2,1,2,3,2,1,2,4,2,2,4,2,2,2,2,2),
  cex = 0.75
)

# Add sectors ------------------------------------------------------------------
abline(a = 0, b = 1, lty = 3, lwd = 1.2, col = "darkgrey")
text(x = 1.6, y = 1.8, srt = 75, labels = "z/r = 1", cex = 0.75, col = "darkgrey")
abline(a = 0, b = 3^-1, lty = 1, lwd = 1.2)
text(x = 5, y = 1.75, srt = 50, labels = "z/r = 3", cex = 0.75)
abline(a = 0, b = 10^-1, lty = 1, lwd = 1.2)
text(x = 6, y = 0.66, srt = 20, labels = "z/r = 10", cex = 0.75)

plotrix::arctext("antistokes", center = c(0,0), radius = 6.5, middle = pi/2.17, 
                 cex = 0.66, col = "darkgrey")
plotrix::arctext("stokes", center = c(0,0), radius = 6.5, middle = pi/2.9, 
                 cex = 0.66, col = "darkgrey")
plotrix::arctext("soluble cations", center = c(0,0), radius = 7, middle = pi/2.9, 
                 cex = 0.75)
plotrix::arctext("hydrolysates", center = c(0,0), radius = 7, middle = pi/5,
                 cex = 0.75)
plotrix::arctext("oxyanions", center = c(0,0), radius = 7, middle = pi/19.5,
                 cex = 0.75)
