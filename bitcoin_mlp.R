###########################################################
#MLP com bitcoin
###########################################################
#1- set.seed

set.seed(0)
###########################################################
#2- data

#bases de dados completa
btc <- read_csv("/Users/ian.furlani/MBA/TCC/tcc_dsa/dados/BTC.csv")
bitcoin <- rev(btc$Price)

## Create a daily Date object - helps my work on dates
start_date <- ymd("2018-01-01")
end_date <- ymd("2022-03-31")
days <- seq(start_date, end_date, "days")
range <- data.frame(days)
summary(range)

#Unindo data + valores... nao sei se é necessario
tabela_btc <- data.frame(range, bitcoin)
head(tabela_btc)

# compreendendo os dados
summary(tabela_btc) #min: 2018-01-01, max: 2022-03-31
class(tabela_btc) #data.frame
class(tabela_btc$days) #date
class(tabela_btc$bitcoin) #numeric

# plot
ggplot(data = tabela_btc) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = bitcoin), color = 'black') 
########################################################### até aqui ok
#Normalization

## How much of the data to use for training vs validation:
split_point <- "2021-06-01"
table(ifelse(tabela_btc$days < split_point, "training set", "testing set")) #só pra contar os pontos

## Save for later use (when converting back to original scale):
normalization_constants <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = tabela_btc$bitcoin[tabela_btc$days < split_point]) #nao entedi
)
tabela_btc$normalized <- (tabela_btc$bitcoin - normalization_constants$mean)/normalization_constants$std.dev

head(tabela_btc)

# plot
ggplot(data = tabela_btc) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'blue') 

###########################################################
#4- Feature Space >> criando os lags
tabela_btc <- filter(tabela_btc, days < '2022-01-01')


mlts <- mlts_transform(tabela_btc, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 28 lags, 4 semanas
str(mlts)

# plot lags
ggplot(data = mlts) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'blue')

###########################################################
#5 - Training

# Split:
training_idx <- which(mlts$dt < split_point)
testing_idx <- which(mlts$dt >= split_point)
head(training_idx) #vetor de 1 a X
head(testing_idx) #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features <- grep("(mlts_lag_[0-9]+)", names(mlts), value = TRUE)
nn_features
nn_formula <- as.formula(paste("y ~", paste(nn_features, collapse = " + ")))
nn_formula

# Train:

nn_model <- neuralnet(
  nn_formula, mlts[training_idx, c("y", nn_features)],
  linear.output = TRUE, hidden = c(5), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment

# Predict:
nn_predictions <- as.numeric(neuralnet::compute(
  nn_model, mlts[testing_idx, nn_features])$net.result
)



# Re-scale:
predictions <- data.frame(
  date = mlts$dt[testing_idx],
  normalized = nn_predictions,
  denormalized = (nn_predictions * normalization_constants$std.dev) + normalization_constants$mean
)

head(predictions)

# Predict:
nn_predictions1 <- as.numeric(neuralnet::compute(
  nn_model, mlts[training_idx, nn_features])$net.result
)


# Re-scale:
predictions1 <- data.frame(
  date = mlts$dt[training_idx],
  normalized = nn_predictions1,
  denormalized = (nn_predictions1 * normalization_constants$std.dev) + normalization_constants$mean
)

# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01"),
      aes(x = days, y = bitcoin)) + geom_line() +
      geom_line(aes(x = date, y = denormalized), color = "red", data = predictions) +
      geom_line(aes(x = date, y = denormalized), color = "blue", data = filter(predictions1, date >= "2018-01-01"))

###########################################################
#Accuracy do modelo
 
#early stopping keras

# Forecasting
tail(tabela_btc)

new_data <- rbind(
  tail(tabela_btc, 29),
  data.frame(
    days = as.Date("2022-04-01"),
    bitcoin = NA,
    normalized = NA
  )
)

summary(new_data)
tail(new_data)

# new_data will have two rows, only the second of which we actually care about
new_mlts <- mlts_transform(new_data, days, normalized, p = 28, extras = TRUE, extrasAsFactors = TRUE, granularity = "day")
new_mlts <- cbind(
  new_mlts[-1, ], # don't need to forecast known outcome
  dummy(
    new_mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"),
             drop = FALSE],
    object = mlts_categories, int = TRUE
  )[-1, ]
)

str(new_mlts)

# Forecast on normalized scale:
nn_forecast <- as.numeric(neuralnet::compute(
  nn_model, new_mlts[, nn_features])$net.result
)

# Re-scale forecast to original scale:
(nn_forecast * normalization_constants$std.dev) + normalization_constants$mean

###########################################################
# Multiple forecast
base1 <- filter(tabela_btc, days <= "2021-12-31")
tail(base1)

new_data <- rbind(
  tail(base1, 29),
  data.frame(
    days = seq(
      as.Date("2022-01-01"),
      as.Date("2022-01-01") + 60,
      "day"
    ),
    bitcoin = NA,
    normalized = NA
  )
); rownames(new_data) <- NULL

new_data


for (d in 30:nrow(new_data)) {
  new_mlts <- mlts_transform(
    new_data[(d - 29):d, ],
    days, normalized, p = 28,
    extras = TRUE, extrasAsFactors = TRUE,
    granularity = "day")
  new_mlts <- cbind(
    new_mlts[-1, ], # don't need to forecast known outcome
    dummy(
      new_mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"),
               drop = FALSE],
      object = mlts_categories, int = TRUE
    )[-1, ]
  )
  new_data$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model, new_mlts[, nn_features])$net.result
  )
}

str(new_mlts)

new_data$forecast <- (new_data$normalized * normalization_constants$std.dev) + normalization_constants$mean

ggplot(dplyr::filter(tabela_btc, days >= "2018-01-01"),
      aes(x = days, y = bitcoin)) + geom_line() +
      geom_line(aes(y = forecast), color = "red", data = dplyr::filter(new_data, days >= "2022-01-01")) +
      geom_line(aes(y = bitcoin), color = "blue", data = dplyr::filter(tabela_btc, days <= "2022-01-01"))
new_data




new_data <- cbind(new_data, filter(tabela_btc, days >= "2020-12-03"))
tabela_btc2 <- filter(tabela_btc, days >= '2022-01-01')

resultado <- data.frame(new_data, tabela_btc2)

accuracy(resultado$forecast, resultado$bitcoin.1)
