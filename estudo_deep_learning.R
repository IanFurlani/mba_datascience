#https://medium.datadriveninvestor.com/utilizing-long-short-term-memory-networks-to-forecast-the-price-of-bitcoin-in-r-8a77a6512ddc

set.seed(0)

# Read Data #
BTC <- (getSymbols("BTC-USD", env=NULL))
BTC_DF <- as.data.frame(BTC) #Convert data to dataframe
head(BTC_DF)
# Add the date to the data frame as a 7th column #
BTC_DF$Date <- time(BTC)
# Remove unnecessary columns and rename columns #
BTC_Data <- BTC_DF[, c(2,7)]
colnames(BTC_Data) <- c('High', 'Date')
# Remove Null rows #
Bit_Coin <- na.exclude(BTC_Data)
head(Bit_Coin)

qplot(Date, High, data = Bit_Coin)

##############################################################
# Convert data frame to a tibble #
Bit_Coin %>% tk_tbl(Bit_Coin)
# Visualize #
ggplot(data=Bit_Coin, aes(x=Date, y=High, group=1)) +
  theme_bw() +
  geom_line()+
  geom_point()

#LSTM models are known as autoregressive time series models, in which observations from previous dates (lags) are used as input in a regression model to predict/forecast future dates’ values. Obviously, to be able to use a LSTM model we need the data to have autocorrelation, and if it does, we must determine our optimal lag value. Let’s check!

# Craete ACF #
tidy_acf <- function(data, High, lags = 0:20) {
  
  value_expr <- enquo(High)
  
  acf_values <- data %>%
    pull(High) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  na.action = na.omit
  return(ret)
}
max_lag <- 12 * 40
# Find Bit Coin's ACF Values #
Bit_Coin %>%
  tidy_acf(High, lags = 0:max_lag)
# Plot the ACF data #
Bit_Coin %>%
  tidy_acf(High, lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 365, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "1 Year Mark", x = 250, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Bit Coin")

#######################3
# Focus in on Interesting Area #
Bit_Coin %>%
  tidy_acf(High, lags = 75:150) %>%
  ggplot(aes(lag, acf)) +
  geom_vline(xintercept = 112, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  theme_tq() +
  labs(title = "ACF: Bitcoin",
       subtitle = "Zoomed in on Lags 75 to 150")
##############################
# Train and Test Data #
periods_train <- 2436
periods_test  <- 112
rolling_origin_resamples <- rolling_origin(
  Bit_Coin,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = FALSE
)
rolling_origin_resamples
# Create a function that plots the Split #
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data #
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = Date) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes #
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize #
  g <- data_manipulated %>%
    ggplot(aes(x = Date, y = High, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Training vs Testing"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    Bit_Coin_time_summary <- Bit_Coin %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(Bit_Coin_time_summary$start, 
                              Bit_Coin_time_summary$end))
  }
  
  return(g)
}
# Plot #
rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")

# Combine Training and Testing data #
split    <- rolling_origin_resamples$splits[[1]]
split_id <- rolling_origin_resamples$id[[1]]
df_trn <- training(split)
df_tst <- testing(split)
df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_tst %>% add_column(key = "testing")
) %>% 
  as_tbl_time(index = Date)
df

# Preprocess using recipe #
rec_obj <- recipe(High ~ ., df) %>%
  step_sqrt(High) %>%
  step_center(High) %>%
  step_scale(High) %>%
  prep()
df_processed_tbl <- bake(rec_obj, df)
df_processed_tbl

# Capture the Center and Scale history #
center_history <- rec_obj$steps[[2]]$means["High"]
scale_history  <- rec_obj$steps[[3]]$sds["High"]
c("center" = center_history, "scale" = scale_history)


