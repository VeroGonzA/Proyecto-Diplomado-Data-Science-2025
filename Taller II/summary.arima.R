summary.arima <- function(fit, fixed = NULL){
fit <- mod
if(is.null(fixed)){
	fixed <- fit$coef
	fixed[fit$coef != 0] <- NA
}	
z.value = fit$coef[is.na(fixed)]/sqrt(diag(fit$var.coef))
p.value = 2*(1-pnorm(abs(z.value)))
Tabla <- round(cbind(fit$coef[is.na(fixed)],sqrt(diag(fit$var.coef)), z.value, p.value),4)
colnames(Tabla) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
Tabla
}