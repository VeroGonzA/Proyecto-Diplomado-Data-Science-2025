TS.summary = function(Y, fit, fixed){
print(summary(fit))
print(summary.arima(fit, fixed = fixed))

M = cbind(ks.test(scale(fit$res), "pnorm")$p.value,
lmtest::bptest(fit$res ~ time(fit$res))$p.value)
colnames(M) = c("Kolmogorov-Smirnov test", "Breusch-Pagan test")
rownames(M) = "p-value"
print(M)

r2 = cbind(1-var(fit$res, na.rm = T)/var(Y, na.rm = T))
colnames(r2) = "Adjusted R-squared"
rownames(r2) = ""
print(r2)
}


