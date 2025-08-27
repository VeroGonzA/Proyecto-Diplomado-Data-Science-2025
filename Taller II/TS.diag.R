####################
## Box.Ljung.Test ##
####################

Box.Ljung.Test = function (z, lag = NULL, main = NULL, col = "black", border = "blue", xaxt = "s") 
{
    if (is.null(lag)) {
        lag = 10
    }
    k = lag
    n = length(z)
    aux = acf(z, plot = FALSE, lag.max = k, na.action = na.pass)
    p.value = vector("numeric")
    Q = vector("numeric")
    for (j in 1:k) {
        rho = aux$acf[2:(j + 1), , 1]
        Q[j] = sum(n * (n + 2) * rho^2/(n - 1:j))
        p.value[j] = 1 - pchisq(Q[j], df = j)
    }
    if (is.null(main)) {
        main = expression("p values for Ljung-Box statistic")
    }
    plot(p.value ~ c(1:k), ylim = c(0, 1), bty = "n", las = 1, 
        lwd = 2, xlim = c(0, k), main = main, xlab = "Lag", ylab = "p-value", 
        pch = 20, col = col, xaxt = xaxt)
    abline(h = 0.05, lty = 2, col = border)
}

#############
## TS.diag ##
#############

TS.diag = function(X, col = "gray", border = "darkblue", breaks = seq(-10,10,1), lag.max = 24, xaxt = "s"){
par(mfrow = c(2,3), bty = "n", las = 1)
Box.Ljung.Test(X, lag = lag.max, col = col, border = border, xaxt = xaxt)
axis(1, 0:lag.max)
aux = acf(X, plot = F, lag.max = lag.max, na.action = na.pass)$acf[,,1]
plot(aux ~ c(0:lag.max), col = col, lwd = 2, xaxt = "n", main = expression("Auto Correlation Function"), las = 1, ylim = c(-1,1), type = "h", xlab = "Lag")
abline(h = c(-1,+1)*qnorm(0.975)/sqrt(length(X)), lty = 2, col = border)
abline(h = 0)
axis(1, 0:lag.max)
plot(c(scale(X))~c(time(X)), ylim = c(-3,+3), type = "h", col = col, ylab = "", main = expression("Standardized Residuals"), xlab = "")
abline(h = c(-3,-2,0,+2,+3), lty = c(3,2,1,2,3), col = c(border,border,"black",border,border))
upper = max(abs(X))
hist(scale(X), freq = F, col = col, main = expression("Histogram"), xlim = c(min(breaks), max(breaks)), border = "white", ylim = c(0,max(c(density(scale(X),na.rm = T)$y),0.4)), xlab = "Standardized Residuals", breaks = breaks)
x = seq(-4, +4,0.001)
lines(dnorm(x, mean = 0, sd = 1)~x, lwd = 2, col = border)
qqnorm(c(X), main = expression("Normal QQ-plot"), pch = 20, lwd = 2, col = col, xlim = c(-4,4))
qqline(c(X), lwd = 2, col = border)
Fn = ecdf(scale(X))
plot(Fn(x)~x, type = "s", col = col, lwd = 2, xlab = "Standardized Residuals", main = expression("Empirical Cumulative Distribution Function"), ylab = "")
lines(pnorm(x, mean = 0, sd = 1)~x, lwd = 2, col = border)
}
