#https://bearloga.github.io/maltese/neuralnet.html


###########################################################
#1 -  packages
suppressPackageStartupMessages(library(tidyverse))
library(maltese)
library(neuralnet)
remotes::install_github("bearloga/maltese")
###########################################################
# set.seed
set.seed(0)
###########################################################
#2- data
data("r_enwiki", package = "maltese")
head(r_enwiki)

# compreendendo os dados
summary(r_enwiki) #min: 2015-10-01, max: 2017-05-16
class(r_enwiki) #data.frame

class(r_enwiki$date) #"POSIXct" "POSIXt" ???
class(r_enwiki$pageviews) #numeric

# plot
ggplot(r_enwiki, aes(x = date, y = pageviews)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Date", y = "Pageviews",
       title = "<https://en.wikipedia.org/wiki/R_(programming_language)> pageviews")
###########################################################
#3- normalization

## How much of the data to use for training vs validation:
split_point <- "2017-02-01"
table(ifelse(r_enwiki$date < split_point, "training set", "testing set"))

## Save for later use (when converting back to original scale):
normalization_constants <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = r_enwiki$pageviews[r_enwiki$date < split_point])
)
r_enwiki$normalized <- (r_enwiki$pageviews - normalization_constants$mean)/normalization_constants$std.dev

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts <- mlts_transform(r_enwiki, date, normalized, p = 21, extras = TRUE, extrasAsFactors = TRUE) # 21 lags
str(mlts)

# Convert factors to dummy variables because neuralnet only supports numeric features:

mlts_categories <- categories(mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"), drop = FALSE])
mlts_dummied <- cbind(mlts, dummy(
  mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"),
       drop = FALSE],
  object = mlts_categories, int = TRUE
))
str(mlts_dummied, list.len = 30)


###########################################################
#5 - Training
# Split:
#training_idx <- which(mlts_dummied$dt < split_point)
#testing_idx <- which(mlts_dummied$dt >= split_point)

training_idx <- which(mlts$dt < split_point)
testing_idx <- which(mlts$dt >= split_point)

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features <- grep("(mlts_lag_[0-9]+)|(mlts_extras_((weekday)|(month)|(monthday)|(week))_.*)", names(mlts), value = TRUE)
nn_formula <- as.formula(paste("y ~", paste(nn_features, collapse = " + ")))

# Train:
set.seed(0)
nn_model <- neuralnet(
  nn_formula, mlts[training_idx, c("y", nn_features)],
  linear.output = TRUE, hidden = c(9, 7, 5), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

summary(nn_model)
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

ggplot(dplyr::filter(r_enwiki, date >= "2016-10-01"),
       aes(x = date, y = pageviews)) +
  geom_line() +
  geom_line(aes(y = denormalized), color = "red",
            data = predictions) +
  theme_minimal() +
  labs(x = "Date", y = "Pageviews",
       title = "Forecast of <https://en.wikipedia.org/wiki/R_(programming_language)> pageviews",
       subtitle = "Red is for predictions made with a neural network with 3 layers containing 9, 7, and 5 hidden neurons, respectively")



###########################################################
#7- Forecasting
new_data <- rbind(
  tail(r_enwiki, 22),
  data.frame(
    date = as.Date("2017-05-17"),
    pageviews = NA,
    normalized = NA
  )
)
# new_data will have two rows, only the second of which we actually care about
new_mlts <- mlts_transform(new_data, date, normalized, p = 21, extras = TRUE, extrasAsFactors = TRUE, granularity = "day")
new_mlts <- cbind(
  new_mlts[-1, ], # don't need to forecast known outcome
  dummy(
    new_mlts[, c("mlts_extras_weekday", "mlts_extras_month", "mlts_extras_monthday", "mlts_extras_week"),
             drop = FALSE],
    object = mlts_categories, int = TRUE
  )[-1, ]
)

# Forecast on normalized scale:
nn_forecast <- as.numeric(neuralnet::compute(
  nn_model, new_mlts[, nn_features])$net.result
)

# Re-scale forecast to original scale:
(nn_forecast * normalization_constants$std.dev) + normalization_constants$mean

################################################
# Forecasting

new_data <- rbind(
  tail(r_enwiki, 22),
  data.frame(
    date = seq(
      as.Date("2017-05-17"),
      as.Date("2017-05-17") + 90,
      "day"
    ),
    pageviews = NA,
    normalized = NA
  )
); rownames(new_data) <- NULL
new_data

for (d in 23:nrow(new_data)) {
  new_mlts <- mlts_transform(
    new_data[(d - 22):d, ],
    date, normalized, p = 21,
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

new_data

new_data$pageviews <- (new_data$normalized * normalization_constants$std.dev) + normalization_constants$mean


ggplot(dplyr::filter(r_enwiki, date >= "2016-10-01"),
       aes(x = date, y = pageviews)) +
  geom_line() +
  geom_line(aes(y = pageviews), color = "red",
            data = dplyr::filter(new_data, date >= "2017-05-16")) +
  theme_minimal() +
  labs(x = "Date", y = "Pageviews",
       title = "90 day forecast of <https://en.wikipedia.org/wiki/R_(programming_language)> pageviews",
       subtitle = "Red is for predictions made with a neural network with 3 layers containing 9, 7, and 5 hidden neurons, respectively")

