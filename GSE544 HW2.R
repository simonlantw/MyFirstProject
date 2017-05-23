#2-a
data = read.csv("Final DataSet.csv")
training = data[1:125,]
test = data[126:167,]

#2-b
m1 = lm(Temperature ~ CO2, data = training)
summary(m1)

#2-c
alpha_hat = m1$coefficients[1] ; alpha_hat
beta_hat = m1$coefficients[2] ; beta_hat
s = 0.1295
n = 125
Var_x = var(training$CO2) ; Var_x
Sxx = Var_x*(n-1) ; Sxx
E_x = mean(training$CO2) ; E_x

# 95% Confidence Interval (taking into account parameter uncertainty)
n = 42
for(i in 1:nrow(test)){
  test$fittedvalue[i] = alpha_hat + beta_hat*test$CO2[i]
  test$S_yhat_x[i] = s*(1/n + (test$CO2[i] - E_x)^2/Sxx)^0.5 #S(yhat|x)
  test$Lower[i] = test$fittedvalue[i] - qt(0.975, df = n-2)*test$S_yhat_x[i] #lower confidence interval
  test$Upper[i] = test$fittedvalue[i] + qt(0.975, df = n-2)*test$S_yhat_x[i] #upper confidence interval
}

# assuming away parameter uncertainty and focusing solely on the standard error of the regression
# and assuming the error is normally distributed
for(i in 1:nrow(test)){
  test$Lower_N[i] = test$fittedvalue[i] - qnorm(0.975)*test$S_yhat_x[i]
  test$Upper_N[i] = test$fittedvalue[i] + qnorm(0.975)*test$S_yhat_x[i]
}

#2-d
s = 0.1295
n = 42
Var_x = var(test$CO2) ; Var_x
Sxx = Var_x*(n-1) ; Sxx
E_x = mean(test$CO2) ; E_x

for(i in 1:nrow(test)){
  test$S_y_x[i] = s*(1 + 1/n + (test$CO2[i] - E_x)^2/Sxx)^0.5 #S(y|x)
  test$Lower_P[i] = test$fittedvalue[i] - qt(0.975, df = n-2)*test$S_y_x[i] #lower prediction interval
  test$Upper_P[i] = test$fittedvalue[i] + qt(0.975, df = n-2)*test$S_y_x[i] #upper prediction interval
}

# assuming away parameter uncertainty and focusing solely on the standard error of the regression
# and assuming the error is normally distributed
for(i in 1:nrow(test)){
  test$Lower_P_N[i] = test$fittedvalue[i] - qnorm(0.975)*s
  test$Upper_P_N[i] = test$fittedvalue[i] + qnorm(0.975)*s
}
  
#How many times in the training set, which contains 42 observations
#did the prediction interval contain the realized value of the global average annual temperature?
counts_1 = 0
for(i in 1:nrow(test)){
  if(test$Temperature[i] > test$Lower_P[i] & test$Temperature[i] < test$Upper_P[i]){
    counts_1 = counts_1 + 1
  }
}
counts_1

counts_2 = 0
for(i in 1:nrow(test)){
  if(test$Temperature[i] > test$Lower_P_N[i] & test$Temperature[i] < test$Upper_P_N[i]){
    counts_2 = counts_2 + 1
  }
}
counts_2

#2-e
library(plotly)
p <- plot_ly() %>%
  add_lines(x = test$Year, y = test$fittedvalue,
            color = I("darkblue"), name = "fitted values") %>%
  add_ribbons(x = test$Year, ymin = test$Lower, ymax = test$Upper,
              color = I("darksalmon"), name = "95% confidence") %>%
  add_ribbons(x = test$Year, ymin = test$Lower_P, ymax = test$Upper_P,
              color = I("yellow"), name = "95% prediction") %>%
  add_lines(x = test$Year, y = test$Temperature,
            color = I("darkred"), name = "test set actuals") %>%
  add_lines(x = training$Year, y = training$Temperature,
            color = I("gray45"), name = "train set actuals") %>%
  layout(title = paste('Temp ~ CO2 with 95% intervals'))
p

write.csv(test, "GSE544 HW2.csv")

#2-f
pit = pnorm(test$Temperature, test$fittedvalue, sd = .1295)

hist(pit, freq = FALSE)
plot(ecdf(pit))
abline(a = 0, b = 1)
# The charts are saying that the Interval is too wide, also from part d, we know that our prediction percentage is 97%,
# where we set it 95%, so we say it is overpredicting.

