########################################
# Example plot
########################################

data.ex <- c(0.15, 0.10, 0.03, 0.015, 0.02, 0.02, 0.02, 0.02, 0.02, 0.025, 0.03, 0.04, 
             0.05, 0.06, 0.08, 0.09, 0.09, 0.08, 0.04, 0.02, 0.00, 0.00)

data.ex1 <- c(0.15, 0.10, 0.03, 0.015, 0.02, 0.02, 0.02, 0.02, 0.02, 0.025, 0.03, 0.04, 
              0.05, 0.06, 0.08, 0.12, 0.12, 0.05, 0.02, 0.01, 0.00, 0.00)
lci <- c(0, 1, seq(5,95,5))
uci <- c(1, seq(4,95,5), 100)
# Class mark
age <- c((lci+uci)/2, 100)

le <- sum(data.ex*age)
theil.ex <- sum(data.ex * log(le/age))

pol.d <- data.frame()
for(k in 1:length(data.ex)) {
  for(j in 1:length(data.ex)) {
    if(k!=j) {
      pol.d <-rbind(pol.d,abs(age[k]-age[j])*data.ex[k]^(1 + 0.5)* data.ex[j])
    }
  }
}
pol.ex <- sum(pol.d)/ (2 * le^(1 - 0.5))

png("example2.png")
plot(age, data.ex, type = "l", xlab = "Age", col = "darkcyan",lwd = 2,
     ylab = "Density", panel.first = grid(col="gray78"), cex.axis = 1.5, cex.lab = 1.5)
points(age, data.ex1, type = "l", col = "darkcyan", lwd = 2)
legend(3, 0.156, c("I = 0.85, P = 0.72", "I = 0.84, P = 0.73"), 
       cex = 1.5, lty = c(1, 1), col = c("darkblue", "darkcyan"))
dev.off()

abline(v=14)

le <- sum(data.ex1*age)
theil.ex <- sum(data.ex1 * log(le/age))


pol.d <- data.frame()
for(k in 1:length(data.ex1)) {
  for(j in 1:length(data.ex1)) {
    if(k!=j) {
      pol.d <-rbind(pol.d,abs(age[k]-age[j])*data.ex1[k]^(1 + 0.5)* data.ex1[j])
    }
  }
}
pol.ex <- sum(pol.d)/ (2 * le^(1 - 0.5))
