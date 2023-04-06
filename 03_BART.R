library(tidyverse)
library(tidymodels)
library(bartMachine) 
#library(dbarts) #alternative that doesn't depend on java 

## Get Data
# https://github.com/paulmj32/data-analytics-2023 
load("/Users/paul/Downloads/df_hurricanes.Rda")
head(df_hurricanes)
summary(df_hurricanes)

## Train/Test 
set.seed(23)
df_split = initial_split(df_hurricanes, prop = 0.80, strata = "ln_hrs")
df_train = training(df_split)
df_test = testing(df_split)

x.train = df_train %>% dplyr::select(-ln_hrs)
y.train = df_train$ln_hrs
x.test = df_test %>% dplyr::select(-ln_hrs)
y.test = df_test$ln_hrs

## BART
bart = bartMachine(X = x.train, y = y.train)
summary(bart)

# Check model assumptions
check_bart_error_assumptions(bart)
plot_convergence_diagnostics(bart)

# Plots
plot_y_vs_yhat(bart, credible_intervals = TRUE)
plot_y_vs_yhat(bart, prediction_intervals = TRUE)

# Test predictions
predictions = predict(bart, x.test)
CI = round(calc_credible_intervals(bart, x.test, ci_conf = 0.95), 2)
gg = data.frame(mean = predictions,
                lower = CI[,1],
                upper = CI[,2],
                actual = y.test
                ) 
#%>% arrange(actual)
gg$index = seq.int(nrow(gg))
ggplot(data = gg) +
  geom_point(aes(y = actual, x = index), col = "gray30") +
  geom_line(aes(y = mean, x = index), col = "red") +
  geom_ribbon(aes(x = index, ymin = lower, ymax = upper), fill = "red", alpha = 0.5) +
  ylab("Outage Duration (ln-hrs)") +
  xlab("Outage Event") +
  ggtitle("Predicting Hurricane Outages") +
  theme(plot.title = element_text(hjust = 0.5))

rsq_bart = 1 - sum((y.test - predictions)^2) / sum((y.test - mean(y.test))^2)

# Variable Importance
vimp = investigate_var_importance(bart)
