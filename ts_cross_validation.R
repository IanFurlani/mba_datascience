###########################################################
# TS CROSS VALIDATION
###########################################################

# PRE PROCESSAMENTO
# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)

# VISUALIZACAO
head(google_2015)
autoplot(google_2015)

# PRE PROCESSAMENTO 2
# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  relocate(Date, Symbol, .id)

head(google_2015)
head(google_2015_tr)

# MODELO
mean1 <- meanf(google_2015_tr$Close, h=10)




# TSCV accuracy
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) #%>%
  #forecast::accuracy(google_2015)

# Training set accuracy
google_2015 %>%
  model(RW(Close ~ drift())) %>%
  accuracy()
