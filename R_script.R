library(quantmod)
library(TTR)
library(psych)
library(tseries)
library(knitr)
library(xtable)
library(lmtest)
library(forecast)
library(gridExtra)
library(ggplot2)
library(stats)
library(aTSA)


start_date <- "2021-01-01"
end_date <- "2022-12-31"
EEM <- getSymbols("EEM", 
                  from = start_date, 
                  to = end_date, 
                  auto.assign = FALSE)
data <- Cl(EEM)


plot(data, 
     main = "Cours quotidien du EEM index",
     type = "l", 
     col = "4")


dt_diff = diff(data)
dt_diff = na.omit(dt_diff)
adf1 = adf.test(dt_diff)
kpss1 = kpss.test(dt_diff)

plot(dt_diff)

# Tableau des résultats
stationarity1 <- data.frame(
  "Test" = c("ADF", "KPSS"),
  "p-value" = c(adf1$p.value, kpss1$p.value),
  "Stationnarité" = c("Stationnaire", "Stationnaire")
)

kable(stationarity1)


dt_diff <- as.data.frame(dt_diff)
# ACF
acf_data <- acf(dt_diff$EEM.Close, main = "ACF Correlogram", plot = FALSE)

# PACF
pacf_data <- pacf(dt_diff$EEM.Close, main = "PACF Correlogram", plot = FALSE)

# Création du graphique de l'ACF + bandes de significance
acf_plot <- ggplot(data.frame(lag = acf_data$lag, acf = acf_data$acf), aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  geom_hline(yintercept = 1.96/sqrt(length(dt_diff$EEM.Close)), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1.96/sqrt(length(dt_diff$EEM.Close)), linetype = "dashed", color = "red") +
  labs(title = "ACF Correlogram") +
  xlab("Lags") +
  ylab("ACF")

# Création du graphique de PACF + bandes de significance
pacf_plot <- ggplot(data.frame(lag = pacf_data$lag, pacf = pacf_data$acf), aes(x = lag, y = pacf)) +
  geom_bar(stat = "identity", fill = "green", width = 0.5) +
  geom_hline(yintercept = 1.96/sqrt(length(dt_diff$EEM.Close)), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1.96/sqrt(length(dt_diff$EEM.Close)), linetype = "dashed", color = "red") +
  labs(title = "PACF Correlogram") +
  xlab("Lags") +
  ylab("PACF")

# Combiner les graphiques en un seul
combined_plot <- grid.arrange(acf_plot, pacf_plot, nrow = 1)



model_2 <- auto.arima(data, 
                      seasonal = FALSE, 
                      approximation = FALSE, 
                      stepwise = TRUE, 
                      nmodels = 5, 
                      trace = TRUE, 
                      method = c("CSS-ML"))


model_1 <- arima(data, 
                 order = c(1,1,1),
                 method = c("CSS-ML")
)
plot(forecast(model_1))
plot(forecast(model_2))


norm_1 <- jarque.bera.test(resid(model_1))
norm_2 <- jarque.bera.test(resid(model_2))
norm_1
norm_2


boxplot(resid(model_1))
boxplot(resid(model_2))


residuals_1 = checkresiduals(model_1, plot = FALSE)
residuals_2 = checkresiduals(model_2, plot = FALSE)


layout(1:3)
autoplot(model_1)
autoplot(model_2)


resid_adf_1 = adf.test(resid(model_1))
resid_adf_2 = adf.test(resid(model_2))
resid_kpss_1 = kpss.test(resid(model_1))
resid_kpss_2 = kpss.test(resid(model_2))
# Tableau des résultats
resid_sta <- data.frame(
  "model" = c("ARIMA(1,1,1)", "ARIMA(2,1,2)"),
  "Test ADF" = c(resid_adf_1$p.value, resid_adf_2$p.value),
  "Test KPSS" = c(resid_kpss_1$p.value, resid_kpss_2$p.value),
  "Stationnarité" = c("Stationnaire", "Stationnaire")
)
kable(resid_sta)


kable(accuracy(forecast(model_1)))
kable(accuracy(forecast(model_2)))


model_2 <- arima(data, 
                 include.mean = TRUE,
                 order = c(2,1,2),
                 method = c("CSS-ML"))
hetero_1 = arch.test(model_2, output = FALSE)
hetero_2 = arch.test(model_1, output = FALSE)

hetero_1
hetero_2