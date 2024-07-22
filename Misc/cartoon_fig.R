par(
  oma=c(5,1,0,1.5),# Room for the title and legend
  mfrow=c(1,2)
)
plot(c(0,1),
     c(0,1),
     type="n",
     cex.lab=1.5,
     xlab="Absolute wealth",
     ylab="Age at first reproduction",
     xaxt="n",
     yaxt="n",
     bty="l"
)
lines(c(0,0.9),
      c(0.1,1),
      lwd=3,
      col=1
)
lines(c(0.1,1),
      c(0,0.9),
      lwd=3,
      col=2
)
legend(x =0.5,y=-0.25,inset = 0,
       legend = c("Q-Q", "R-(e)S","EU","N-L"), 
       col=1:4, lwd=5, cex=1, horiz = TRUE,xpd=NA)
plot(c(0,1),
     c(0,1),
     type="n",
     cex.lab=1.5,
     xlab="Wealth variability",
     ylab="Age at first reproduction",
     xaxt="n",
     yaxt="n",
     bty="l"
)
lines(c(0,0.9),
      c(0.9,0),
      lwd=3,
      col=3
)
lines(c(0.1,1),
      c(1,0.1),
      lwd=3,
      col=4
)

