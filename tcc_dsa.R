###########################################
# TCC ARQUIVO FINAL
# Created by: Ian Furlani
###########################################
# FRAMEWORK based on https://otexts.com/fpp3/a-tidy-forecasting-workflow.html
# 1 Tidy
# 2 Visualise
# 3 Specify
# 4 Estimate
# 5 Evaluate
# 6 Forecast
###########################################
set.seed(0)
###########################################

# 1 Tidy - construir a tabela final, ainda sem divisao treino e teste

#bases de dados completa
btc <- read_csv("/Users/ian.furlani/MBA/TCC/tcc_dsa/dados/BTC.csv")
eth <- read_csv("/Users/ian.furlani/MBA/TCC/tcc_dsa/dados/ETH.csv")
bnc <- read_csv("/Users/ian.furlani/MBA/TCC/tcc_dsa/dados/BNB.csv")

## Create a daily Date object - helps my work on dates
start_date <- ymd("2018-01-01")
end_date <- ymd("2022-03-31")
days <- seq(start_date, end_date, "days")
range <- data.frame(days)
summary(range)

## Create a time series object
inds <- seq(as.Date("2018-01-01"), as.Date("2022-03-31"), by = "day")
bitcoin <- ts(rev(btc$Price),start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)
bitcoin2 <- rev(btc$Price)
etherium2 <- rev(eth$Price)
binance2 <- rev(bnc$Price)

etherium <- ts(rev(eth$Price),start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)
binance <- ts(rev(bnc$Price),start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)

#Check bitcoin
class(bitcoin)
summary(bitcoin)
autoplot(bitcoin)
ts.plot(bitcoin)


#Unindo data + valores... nao sei se é necessario
tabela_final <- data.frame(range, bitcoin, etherium, binance)
tabela_final2 <- ts(tabela_final)
tabela_final2
class(tabela_final)
head(tabela_final)
summary(tabela_final)

#Unindo data + valores... nao sei se é necessario
tabela_btc <- data.frame(range, bitcoin2)
tabela_eth <- data.frame(range, etherium2)
tabela_bnc <- data.frame(range, binance2)

head(tabela_btc)
class(tabela_btc$days)
class(tabela_btc$bitcoin)

tabela_btc %>% ggplot(aes(x = 1:nrow(tabela_btc), y = bitcoin)) +
  geom_line()

#serie normalizada
#serie log
#series dinferenciada
###########################################

# 2 Visualise - analise preliminar

#VAI PRO TCC
#plot dos dados

cols1 <- c("Bitcoin" = "blue", "Etherium" = "red", "Binance coin" = "orange")

  jpeg("fig2.jpeg", width = 8, height = 4.5, units = c("in"), res = 900)
  ggplot(data = tabela_final) + 
  theme_few() +
  labs(x = 'Dia', y = 'Preço de fechamento ($)') +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Bitcoin")) +
  geom_line(mapping = aes(x = days, y = etherium, colour = "Etherium")) +
  geom_line(mapping = aes(x = days, y = binance, colour = "Binance coin")) +
  scale_color_manual(name = "", values = cols1) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))
  dev.off()

  
  #VAI PRO TCC  
# Plot dados normalizados >> facilitar a correlacao
  jpeg("fig3.jpeg", width = 8, height = 4.5, units = c("in"), res = 900)
  ggplot(data = tabela_final) +
  theme_few() +
  labs(x = 'Dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = BETS::normalize(bitcoin, mode = "scale"), colour = 'Bitcoin')) +
  geom_line(mapping = aes(x = days, y = BETS::normalize(etherium, mode = "scale"), colour = 'Etherium')) +
  geom_line(mapping = aes(x = days, y = BETS::normalize(binance, mode = "scale"), colour = 'Binance coin')) +
  scale_color_manual(name = "", values = cols1) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))
dev.off()

# Plot log(series) >> facilitar a visualizacao
ggplot(data = tabela_final) +
  theme_light() +
  labs(title = 'Log do preço de fechamento($) por dia', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = log(bitcoin)), color = 'black') +
  geom_line(mapping = aes(x = days, y = log(etherium)), color = 'blue') +
  geom_line(mapping = aes(x = days, y = log(binance)), color = 'red')

###########################################

# Decomposicoes
#decomposicao aditiva
decomp_btc = decompose(bitcoin, type = "additive")
decomp_eth = decompose(etherium, type = "additive")
decomp_bnb = decompose(binance, type = "additive")
plot(decomp_btc)
plot(decomp_eth)
plot(decomp_bnb)

###########################################

# Avaliar estacionariedade (claramente nao estacionário)
adf.test(tabela_final$bitcoin) # p-value = 0.4054 > 0.05 (nao estacionario)
adf.test(tabela_final$etherium) # p-value = 0.4522 > 0.05 (nao estacionario)
adf.test(tabela_final$binance) # p-value = 0.3881 > 0.05 (nao estacionario)

# Avaliar estacionariedade log
log_bitcoin <- log(tabela_final$bitcoin)
log_etherium <- log(tabela_final$etherium)
log_binance <- log(tabela_final$binance)
adf.test(log_bitcoin) # p-value = 0.2621 > 0.05 (nao estacionario)
adf.test(log_etherium) # p-value = 0.356 > 0.05 (nao estacionario)
adf.test(log_binance) # p-value = 0.4636 > 0.05 (nao estacionario)

# retirar tendencia
diff_btc <- diff(tabela_final[,2])
diff_eth <- diff(tabela_final[,3])
diff_bnb <- diff(tabela_final[,4])
autoplot(diff_btc)
autoplot(diff_eth)
autoplot(diff_bnb)

# retirar tendencia - log
diff_log_btc <- diff(log(tabela_final[,2]))
diff_log_eth <- diff(log(tabela_final[,3]))
diff_log_bnb <- diff(log(tabela_final[,4]))
autoplot(diff_log_btc)
autoplot(diff_log_eth)
autoplot(diff_log_bnb)

# agora todos sao estacionários
adf.test(diff_btc) # p-value < 0.01
adf.test(diff_eth) # p-value < 0.01
adf.test(diff_bnb) # p-value < 0.01
adf.test(diff_log_btc) # p-value < 0.01
adf.test(diff_log_eth) # p-value < 0.01
adf.test(diff_log_bnb) # p-value < 0.01

# Normalizar???
norm_bitcoin <- BETS::normalize(bitcoin, mode = "scale") 
norm_etherium <- BETS::normalize(etherium, mode = "scale") 
norm_binance <- BETS::normalize(binance, mode = "scale") 

maxmin_btc <- BETS::normalize(bitcoin, mode = "maxmin") 
summary(norm_btc)
summary(maxmin_btc)

plot(norm_btc)
plot(maxmin_btc)

###########################################

#ACF e PACF autocorrelacao
pacf(tabela_final[,2], lag.max = 360)
acf(tabela_final[,2])

acf_bit <- acf(coredata(tabela_final[,2]), lag.max = 365) #bit
acf_eth <- acf(coredata(tabela_final[,3]), lag.max = 365) #bit
acf_bnc <- acf(coredata(tabela_final[,4]), lag.max = 365) #bit

acf_total <- data.frame(acf_bit$lag, acf_bit$acf, acf_eth$acf, acf_bnc$acf)
acf_total

#VAI PRO TCC
jpeg("fig4.jpeg", width = 8, height = 4.5, units = c("in"), res = 900)
ggplot(data = acf_total) +
  theme_few() +
  labs(x = 'Lag', y = 'Autocorrelacao') +
  geom_line(mapping = aes(x = acf_bit.lag, y = acf_bit.acf, colour = 'Bitcoin')) +
  geom_line(mapping = aes(x = acf_bit.lag, y = acf_eth.acf, colour = 'Etherium')) +
  geom_line(mapping = aes(x = acf_bit.lag, y = acf_bnc.acf, colour = 'Binance coin')) +
  scale_color_manual(name = "", values = cols1) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))
dev.off()




## Examples from Venables & Ripley
autoplot(lh)
acf(lh)
pacf(lh)

###########################################
# Visualizacoes complementares
#performance analytics
chart.Correlation(tabela_final[,2:4], histogram = TRUE)

#sazonalidade
ggseasonplot(bitcoin)
ggseasonplot(log(bitcoin))
ggseasonplot(diff_btc)


###########################################

# 3 Specify

###########################################
###########################################

class(tabela_final$days)
#BENCHMARK bitcoin
#mean method

head(bitcoin)

#VAI PRO TCC
plot(window(bitcoin, start=c(2018), end=c(2023))) #serie total só pra visualizar
lines(window(bitcoin, start=c(2019,1), end=c(2019,30)), col='blue') #janela serie teste
lines(window(bitcoin, start=c(2019,180), end=c(2019,210)), col='blue') #janela serie teste
lines(window(bitcoin, start=c(2020,1), end=c(2020,30)), col='blue') #janela serie teste
lines(window(bitcoin, start=c(2020,180), end=c(2020,210)), col='blue') #janela serie teste
lines(window(bitcoin, start=c(2021,1), end=c(2021,30)), col='blue') #janela serie teste
lines(window(bitcoin, start=c(2021,180), end=c(2021,210)), col='blue') #janela serie teste
lines(window(bitcoin, start=c(2022,1), end=c(2022,30)), col='blue') #janela serie teste

####
cols2 <- c("Treino" = "black", "Teste" = "red")
jpeg("fig5.jpeg", width = 8, height = 4.5, units = c("in"), res = 900)
ggplot(data = tabela_final) + 
  theme_few() +
  labs(x = 'Dia', y = 'Preço de fechamento ($)') +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Treino")) +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Teste"), data = filter(tabela_final, days >= c("2019-01-01") & days <= c("2019-01-30"))) +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Teste"), data = filter(tabela_final, days >= c("2019-07-01") & days <= c("2019-07-30"))) +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Teste"), data = filter(tabela_final, days >= c("2020-01-01") & days <= c("2020-01-30"))) +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Teste"), data = filter(tabela_final, days >= c("2020-07-01") & days <= c("2020-07-30"))) +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Teste"), data = filter(tabela_final, days >= c("2021-01-01") & days <= c("2021-01-30"))) +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Teste"), data = filter(tabela_final, days >= c("2021-07-01") & days <= c("2021-07-30"))) +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Teste"), data = filter(tabela_final, days >= c("2022-01-01") & days <= c("2022-01-30"))) +
  scale_color_manual(name = "", values = cols2) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))
dev.off()
####

bit1_tr <- window(bitcoin, start=c(2018), end=c(2019)) #treino
bit1_ts <- window(bitcoin, start=c(2019,1), end=c(2019,30)) #teste = treino + 30

bit2_tr <- window(bitcoin, start=c(2018), end=c(2019,179)) #treino
bit2_ts <- window(bitcoin, start=c(2019,180), end=c(2019,210)) #teste = treino + 30

bit3_tr <- window(bitcoin, start=c(2018), end=c(2020)) #treino
bit3_ts <- window(bitcoin, start=c(2020,1), end=c(2020,30)) #teste = treino + 30

bit4_tr <- window(bitcoin, start=c(2018), end=c(2020, 179)) #treino
bit4_ts <- window(bitcoin, start=c(2020,180), end=c(2020, 210)) #teste = treino + 3

bit5_tr <- window(bitcoin, start=c(2018), end=c(2021)) #treino
bit5_ts <- window(bitcoin, start=c(2021,1), end=c(2021,30)) #teste = treino + 3

bit6_tr <- window(bitcoin, start=c(2018), end=c(2021, 179)) #treino
bit6_ts <- window(bitcoin, start=c(2021,180), end=c(2021,210)) #teste = treino + 3

bit7_tr <- window(bitcoin, start=c(2018), end=c(2022)) #treino
bit7_ts <- window(bitcoin, start=c(2022,1), end=c(2022,30)) #teste = treino + 3

#MODELOS
mean1 <- meanf(bit1_tr, h=30) #modelo + forecast 30
lines(mean1$fitted, col = 'orange')
lines(mean1$mean, col = 'red')

mean2 <- meanf(bit2_tr, h=30) #modelo + forecast 30
#lines(mean2$fitted, col = 'orange')
lines(mean2$mean, col = 'red')

mean3 <- meanf(bit3_tr, h=30) #modelo + forecast 30
#lines(mean3$fitted, col = 'orange')
lines(mean3$mean, col = 'red')

mean4 <- meanf(bit4_tr, h=30) #modelo + forecast 30
#lines(mean4$fitted, col = 'orange')
lines(mean4$mean, col = 'red')

mean5 <- meanf(bit5_tr, h=30) #modelo + forecast 30
#lines(mean5$fitted, col = 'orange')
lines(mean5$mean, col = 'red')

mean6 <- meanf(bit6_tr, h=30) #modelo + forecast 30
#lines(mean6$fitted, col = 'orange')
lines(mean6$mean, col = 'red')

mean7 <- meanf(bit7_tr, h=30) #modelo + forecast 30
#lines(mean7$fitted, col = 'orange')
lines(mean7$mean, col = 'red')

#VAI PRO TCC Treino e TESTE BENCHMARK
plot(mean1$x) 
lines(mean1$fitted, col = 'blue')
lines(mean1$mean, col = 'red')



ggplot(data = tabela_final) + 
  theme_few() +
  labs(x = 'Dia', y = 'Preço de fechamento ($)') +
  geom_line(mapping = aes(x = days, y = bitcoin, colour = "Treino"), data = filter(tabela_final, days >= c("2018-01-01") & days <= c("2019-01-30"))) +
  #geom_line(mapping = aes(x = days, y = fitted, colour = "Teste"), data = mean1) +
  #geom_line(mapping = aes(x = days, y = mean, colour = "Teste"), data = filter(mean1, days >= c("2018-01-01") & days <= c("2019-01-30"))) +
  scale_color_manual(name = "", values = cols2) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))


  
  
  
  
3#accuracy
ac1 <- forecast::accuracy(mean1, bit1_ts)
ac2 <- forecast::accuracy(mean2, bit2_ts)
ac3 <- forecast::accuracy(mean3, bit3_ts)
ac4 <- forecast::accuracy(mean4, bit4_ts)
ac5 <- forecast::accuracy(mean5, bit5_ts)
ac6 <- forecast::accuracy(mean6, bit6_ts)
ac7 <- forecast::accuracy(mean7, bit7_ts)

accuracy_mean <- bind_rows(ac1[2,],ac2[2,],ac3[2,],ac4[2,],ac5[2,],ac6[2,],ac7[2,])
accuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), accuracy_mean)
accuracy_mean
  
ggplot(data = accuracy_mean) + 
  theme_light() +
  ylim(0,150) +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'orange') 

###################### etherium benchmark

head(etherium)
plot(window(etherium, start=c(2018), end=c(2023))) #serie total só pra visualizar
lines(window(etherium, start=c(2019,1), end=c(2019,30)), col='blue') #janela serie teste
lines(window(etherium, start=c(2019,180), end=c(2019,210)), col='blue') #janela serie teste
lines(window(etherium, start=c(2020,1), end=c(2020,30)), col='blue') #janela serie teste
lines(window(etherium, start=c(2020,180), end=c(2020,210)), col='blue') #janela serie teste
lines(window(etherium, start=c(2021,1), end=c(2021,30)), col='blue') #janela serie teste
lines(window(etherium, start=c(2021,180), end=c(2021,210)), col='blue') #janela serie teste
lines(window(etherium, start=c(2022,1), end=c(2022,30)), col='blue') #janela serie teste

eth1_tr <- window(etherium, start=c(2018), end=c(2019)) #treino
eth1_ts <- window(etherium, start=c(2019,1), end=c(2019,30)) #teste = treino + 30

eth2_tr <- window(etherium, start=c(2018), end=c(2019,179)) #treino
eth2_ts <- window(etherium, start=c(2019,180), end=c(2019,210)) #teste = treino + 30

eth3_tr <- window(etherium, start=c(2018), end=c(2020)) #treino
eth3_ts <- window(etherium, start=c(2020,1), end=c(2020,30)) #teste = treino + 30

eth4_tr <- window(etherium, start=c(2018), end=c(2020, 179)) #treino
eth4_ts <- window(etherium, start=c(2020,180), end=c(2020, 210)) #teste = treino + 3

eth5_tr <- window(etherium, start=c(2018), end=c(2021)) #treino
eth5_ts <- window(etherium, start=c(2021,1), end=c(2021,30)) #teste = treino + 3

eth6_tr <- window(etherium, start=c(2018), end=c(2021, 179)) #treino
eth6_ts <- window(etherium, start=c(2021,180), end=c(2021,210)) #teste = treino + 3

eth7_tr <- window(etherium, start=c(2018), end=c(2022)) #treino
eth7_ts <- window(etherium, start=c(2022,1), end=c(2022,30)) #teste = treino + 3

#MODELOS
ethmean1 <- meanf(eth1_tr, h=30) #modelo + forecast 30
#lines(mean1$fitted, col = 'orange')
lines(ethmean1$mean, col = 'red')

ethmean2 <- meanf(eth2_tr, h=30) #modelo + forecast 30
#lines(mean2$fitted, col = 'orange')
lines(ethmean2$mean, col = 'red')

ethmean3 <- meanf(eth3_tr, h=30) #modelo + forecast 30
#lines(mean3$fitted, col = 'orange')
lines(ethmean3$mean, col = 'red')

ethmean4 <- meanf(eth4_tr, h=30) #modelo + forecast 30
#lines(mean4$fitted, col = 'orange')
lines(ethmean4$mean, col = 'red')

ethmean5 <- meanf(eth5_tr, h=30) #modelo + forecast 30
#lines(mean5$fitted, col = 'orange')
lines(ethmean5$mean, col = 'red')

ethmean6 <- meanf(eth6_tr, h=30) #modelo + forecast 30
#lines(mean6$fitted, col = 'orange')
lines(ethmean6$mean, col = 'red')

ethmean7 <- meanf(eth7_tr, h=30) #modelo + forecast 30
#lines(mean7$fitted, col = 'orange')
lines(ethmean7$mean, col = 'red')


#accuracy
ethac1 <- forecast::accuracy(ethmean1, eth1_ts)
ethac2 <- forecast::accuracy(ethmean2, eth2_ts)
ethac3 <- forecast::accuracy(ethmean3, eth3_ts)
ethac4 <- forecast::accuracy(ethmean4, eth4_ts)
ethac5 <- forecast::accuracy(ethmean5, eth5_ts)
ethac6 <- forecast::accuracy(ethmean6, eth6_ts)
ethac7 <- forecast::accuracy(ethmean7, eth7_ts)

ethaccuracy_mean <- bind_rows(ethac1[2,],ethac2[2,],ethac3[2,],ethac4[2,],ethac5[2,],ethac6[2,],ethac7[2,])
ethaccuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), ethaccuracy_mean)
ethaccuracy_mean

ggplot(data = ethaccuracy_mean) + 
  theme_light() +
  #ylim(0,200) +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'orange') +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'red', data = accuracy_mean)

########################################### binance

head(binance)
plot(window(binance, start=c(2018), end=c(2023))) #serie total só pra visualizar
lines(window(binance, start=c(2019,1), end=c(2019,30)), col='blue') #janela serie teste
lines(window(binance, start=c(2019,180), end=c(2019,210)), col='blue') #janela serie teste
lines(window(binance, start=c(2020,1), end=c(2020,30)), col='blue') #janela serie teste
lines(window(binance, start=c(2020,180), end=c(2020,210)), col='blue') #janela serie teste
lines(window(binance, start=c(2021,1), end=c(2021,30)), col='blue') #janela serie teste
lines(window(binance, start=c(2021,180), end=c(2021,210)), col='blue') #janela serie teste
lines(window(binance, start=c(2022,1), end=c(2022,30)), col='blue') #janela serie teste

bnc1_tr <- window(binance, start=c(2018), end=c(2019)) #treino
bnc1_ts <- window(binance, start=c(2019,1), end=c(2019,30)) #teste = treino + 30

bnc2_tr <- window(binance, start=c(2018), end=c(2019,179)) #treino
bnc2_ts <- window(binance, start=c(2019,180), end=c(2019,210)) #teste = treino + 30

bnc3_tr <- window(binance, start=c(2018), end=c(2020)) #treino
bnc3_ts <- window(binance, start=c(2020,1), end=c(2020,30)) #teste = treino + 30

bnc4_tr <- window(binance, start=c(2018), end=c(2020, 179)) #treino
bnc4_ts <- window(binance, start=c(2020,180), end=c(2020, 210)) #teste = treino + 3

bnc5_tr <- window(binance, start=c(2018), end=c(2021)) #treino
bnc5_ts <- window(binance, start=c(2021,1), end=c(2021,30)) #teste = treino + 3

bnc6_tr <- window(binance, start=c(2018), end=c(2021, 179)) #treino
bnc6_ts <- window(binance, start=c(2021,180), end=c(2021,210)) #teste = treino + 3

bnc7_tr <- window(binance, start=c(2018), end=c(2022)) #treino
bnc7_ts <- window(binance, start=c(2022,1), end=c(2022,30)) #teste = treino + 3

#MODELOS
bncmean1 <- meanf(bnc1_tr, h=30) #modelo + forecast 30
#lines(mean1$fitted, col = 'orange')
lines(bncmean1$mean, col = 'red')

bncmean2 <- meanf(bnc2_tr, h=30) #modelo + forecast 30
#lines(mean2$fitted, col = 'orange')
lines(bncmean2$mean, col = 'red')

bncmean3 <- meanf(bnc3_tr, h=30) #modelo + forecast 30
#lines(mean3$fitted, col = 'orange')
lines(bncmean3$mean, col = 'red')

bncmean4 <- meanf(bnc4_tr, h=30) #modelo + forecast 30
#lines(mean4$fitted, col = 'orange')
lines(bncmean4$mean, col = 'red')

bncmean5 <- meanf(bnc5_tr, h=30) #modelo + forecast 30
#lines(mean5$fitted, col = 'orange')
lines(bncmean5$mean, col = 'red')

bncmean6 <- meanf(bnc6_tr, h=30) #modelo + forecast 30
#lines(mean6$fitted, col = 'orange')
lines(bncmean6$mean, col = 'red')

bncmean7 <- meanf(bnc7_tr, h=30) #modelo + forecast 30
#lines(mean7$fitted, col = 'orange')
lines(bncmean7$mean, col = 'red')


#accuracy
bncac1 <- forecast::accuracy(bncmean1, bnc1_ts)
bncac2 <- forecast::accuracy(bncmean2, bnc2_ts)
bncac3 <- forecast::accuracy(bncmean3, bnc3_ts)
bncac4 <- forecast::accuracy(bncmean4, bnc4_ts)
bncac5 <- forecast::accuracy(bncmean5, bnc5_ts)
bncac6 <- forecast::accuracy(bncmean6, bnc6_ts)
bncac7 <- forecast::accuracy(bncmean7, bnc7_ts)

bncaccuracy_mean <- bind_rows(bncac1[2,],bncac2[2,],bncac3[2,],bncac4[2,],bncac5[2,],bncac6[2,],bncac7[2,])
bncaccuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), bncaccuracy_mean)
bncaccuracy_mean

#ESSE GRAFICO VAI PRO TCC

#cols1 <- c("Bitcoin" = "blue", "Etherium" = "red", "Binance coin" = "orange")
jpeg("fig5.jpeg", width = 8, height = 4.5, units = c("in"), res = 900)
ggplot(data = accuracy_mean) + 
  theme_few() +
  labs(x = 'Amostra', y = 'MAPE') +
  geom_line(mapping = aes(x = index, y = MAPE, colour = "Bitcoin")) +
  geom_line(mapping = aes(x = index, y = MAPE, colour = "Etherium"), data = ethaccuracy_mean ) +
  geom_line(mapping = aes(x = index, y = MAPE, colour = "Binance coin"), data = bncaccuracy_mean ) +
  scale_color_manual(name = "", values = cols1) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))
dev.off()


accuracy_mean
ethaccuracy_mean
bncaccuracy_mean
#mlp >> neuralnet https://www.youtube.com/watch?v=lTMqXSSjCvk&t=201s
#https://bearloga.github.io/maltese/neuralnet.html

###########################################################
###########################################################


# MLP Final
# https://bearloga.github.io/maltese/neuralnet.html

# MPL1 BINANCE
# dados
bit1_mlp <- tabela_bnc %>% filter(days < "2019-01-01" )
summary(bit1_mlp)

#split point
split_point1 <- "2019-01-01"
max_point1 <- "2019-01-30"


## Save for later use (when converting back to original scale):
normalization_constants <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit1_mlp$binance2[bit1_mlp$days < split_point1]) #nao entedi
)
bit1_mlp$normalized <- (bit1_mlp$binance2 - normalization_constants$mean)/normalization_constants$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$binance2)

head(bit1_mlp)

# plot
ggplot(data = bit1_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts1 <- mlts_transform(bit1_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts1)

# plot lags
ggplot(data = mlts1) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx <- which(mlts1$dt < split_point1)
testing_idx <- which(mlts1$dt >= split_point1 & mlts1$dt <= max_point1)
head(training_idx) #vetor de 1 a X
testing_idx #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features <- grep("(mlts_lag_[0-9]+)", names(mlts1), value = TRUE)
nn_features
nn_formula <- as.formula(paste("y ~", paste(nn_features, collapse = " + ")))
nn_formula

# Train:
set.seed(0)
nn_model <- neuralnet(
  nn_formula, mlts1[training_idx, c("y", nn_features)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr <- as.numeric(neuralnet::compute(
  nn_model, mlts1[training_idx, nn_features])$net.result
)

# Re-scale:
predictions_tr <- data.frame(
  date = mlts1$dt[training_idx],
  normalized = nn_predictions_tr,
  denormalized = (nn_predictions_tr * normalization_constants$std.dev) + normalization_constants$mean
)

#Tipo esse vai pro tcc
# plot prediction vs real
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2019-01-31"),
      aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr) 

###########################################################
#7- Forecasting 30 days
tail(bit1_mlp)

bit1_mlp_ts <- rbind(
  tail(bit1_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2019-01-01"),
      as.Date("2019-01-01") + 30,
      "day"
    ),
    binance2 = NA,
    normalized = NA
  )
); rownames(bit1_mlp_ts) <- NULL

bit1_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit1_mlp_ts)) {
  new_mlts <- mlts_transform(
    bit1_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts <- new_mlts[-1, ] # don't need to forecast known outcome
    
  bit1_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model, new_mlts[, nn_features])$net.result
  )
}

new_mlts #1 linha com os 28 lags
bit1_mlp_ts


bit1_mlp_ts$forecast <- (bit1_mlp_ts$normalized * normalization_constants$std.dev) + normalization_constants$mean



# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2019-01-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit1_mlp_ts, days >= '2019-01-01'))



base <- data.frame(filter(bit1_mlp_ts, days >= "2019-01-01" & days <= "2019-01-31"), filter(tabela_bnc, days >= "2019-01-01" & days <= "2019-01-31"))

forecast::accuracy(base$forecast, base$binance2.1) 



###########################################################
###########################################################
# MPL2
# dados
bit2_mlp <- tabela_bnc %>% filter(days < "2019-07-01" )
summary(bit2_mlp)

#split point
split_point2 <- "2019-07-01"


## Save for later use (when converting back to original scale):
normalization_constants2 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit2_mlp$binance2[bit2_mlp$days < split_point2]) #nao entedi
)
bit2_mlp$normalized <- (bit2_mlp$binance2 - normalization_constants2$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit2_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts2 <- mlts_transform(bit2_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts2)

# plot lags
ggplot(data = mlts2) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx2 <- which(mlts2$dt < split_point2)
testing_idx2 <- which(mlts2$dt >= split_point2)
head(training_idx2) #vetor de 1 a X
testing_idx2 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features2 <- grep("(mlts_lag_[0-9]+)", names(mlts2), value = TRUE)
nn_features2
nn_formula2 <- as.formula(paste("y ~", paste(nn_features2, collapse = " + ")))
nn_formula2

# Train:
set.seed(0)
nn_model2 <- neuralnet(
  nn_formula2, mlts2[training_idx2, c("y", nn_features2)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr2 <- as.numeric(neuralnet::compute(
  nn_model2, mlts2[training_idx2, nn_features2])$net.result
)

# Re-scale:
predictions_tr2 <- data.frame(
  date = mlts2$dt[training_idx2],
  normalized = nn_predictions_tr2,
  denormalized = (nn_predictions_tr2 * normalization_constants2$std.dev) + normalization_constants2$mean
)

predictions_tr2

# plot prediction vs real
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2019-08-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr2) 

###########################################################
#7- Forecasting 30 days
tail(bit2_mlp)

bit2_mlp_ts <- rbind(
  tail(bit2_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2019-07-01"),
      as.Date("2019-07-01") + 30,
      "day"
    ),
    binance2 = NA,
    normalized = NA
  )
); rownames(bit1_mlp_ts) <- NULL

bit2_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit2_mlp_ts)) {
  new_mlts2 <- mlts_transform(
    bit2_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts2 <- new_mlts2[-1, ] # don't need to forecast known outcome
  
  bit2_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model2, new_mlts2[, nn_features2])$net.result
  )
}

new_mlts2

bit2_mlp_ts$forecast <- (bit2_mlp_ts$normalized * normalization_constants2$std.dev) + normalization_constants2$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2019-08-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr2)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit2_mlp_ts, days >= '2019-07-01'))

#acuracy mlp
bit2_mlp_ts

base2 <- data.frame(filter(bit2_mlp_ts, days >= "2019-07-01" & days <= "2019-07-30"), filter(tabela_bnc, days >= "2019-07-01" & days <= "2019-07-30"))
forecast::accuracy(base2$forecast, base2$binance2.1) 

#######################################
# MPL3 BINANCE
# dados
bit3_mlp <- tabela_bnc %>% filter(days < "2020-01-01" )
summary(bit3_mlp)

#split point
split_point3 <- "2020-01-01"


## Save for later use (when converting back to original scale):
normalization_constants3 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit3_mlp$binance2[bit3_mlp$days < split_point3]) #nao entedi
)
bit3_mlp$normalized <- (bit3_mlp$binance2 - normalization_constants3$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit3_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts3 <- mlts_transform(bit3_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts3)

# plot lags
ggplot(data = mlts3) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx3 <- which(mlts3$dt < split_point3)
testing_idx3 <- which(mlts3$dt >= split_point3)
head(training_idx3) #vetor de 1 a X
testing_idx3 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features3 <- grep("(mlts_lag_[0-9]+)", names(mlts3), value = TRUE)
nn_features3
nn_formula3 <- as.formula(paste("y ~", paste(nn_features3, collapse = " + ")))
nn_formula3

# Train:
set.seed(0)
nn_model3 <- neuralnet(
  nn_formula3, mlts3[training_idx3, c("y", nn_features3)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr3 <- as.numeric(neuralnet::compute(
  nn_model3, mlts3[training_idx3, nn_features3])$net.result
)

# Re-scale:
predictions_tr3 <- data.frame(
  date = mlts3$dt[training_idx3],
  normalized = nn_predictions_tr3,
  denormalized = (nn_predictions_tr3 * normalization_constants3$std.dev) + normalization_constants3$mean
)

predictions_tr3

# plot prediction vs real
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2020-01-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr3) 

###########################################################
#7- Forecasting 30 days
tail(bit3_mlp)

bit3_mlp_ts <- rbind(
  tail(bit3_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2020-01-01"),
      as.Date("2020-01-01") + 30,
      "day"
    ),
    binance2 = NA,
    normalized = NA
  )
); rownames(bit3_mlp_ts) <- NULL

bit3_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit3_mlp_ts)) {
  new_mlts3 <- mlts_transform(
    bit3_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts3 <- new_mlts3[-1, ] # don't need to forecast known outcome
  
  bit3_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model3, new_mlts3[, nn_features3])$net.result
  )
}

new_mlts3

bit3_mlp_ts$forecast <- (bit3_mlp_ts$normalized * normalization_constants3$std.dev) + normalization_constants3$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2020-01-30"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr3)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit3_mlp_ts, days >= '2020-01-01'))

#acuracy mlp
bit3_mlp_ts

base3 <- data.frame(filter(bit3_mlp_ts, days >= "2020-01-01" & days <= "2020-01-30"), filter(tabela_bnc, days >= "2020-01-01" & days <= "2020-01-30"))
forecast::accuracy(base3$forecast, base3$binance2.1) 

#####################################
#####################################
# MPL4 BINANCE
# dados
bit4_mlp <- tabela_bnc %>% filter(days < "2020-07-01" )
summary(bit4_mlp)

#split point
split_point4 <- "2020-07-01"


## Save for later use (when converting back to original scale):
normalization_constants4 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit4_mlp$binance2[bit4_mlp$days < split_point4]) #nao entedi
)
bit4_mlp$normalized <- (bit4_mlp$binance2 - normalization_constants4$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit4_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts4 <- mlts_transform(bit4_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts4)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx4 <- which(mlts4$dt < split_point4)
testing_idx4 <- which(mlts4$dt >= split_point4)
head(training_idx4) #vetor de 1 a X
testing_idx4 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features4 <- grep("(mlts_lag_[0-9]+)", names(mlts4), value = TRUE)
nn_features4
nn_formula4 <- as.formula(paste("y ~", paste(nn_features4, collapse = " + ")))
nn_formula4

# Train:
set.seed(0)
nn_model4 <- neuralnet(
  nn_formula4, mlts4[training_idx4, c("y", nn_features4)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr4 <- as.numeric(neuralnet::compute(
  nn_model4, mlts4[training_idx4, nn_features4])$net.result
)

# Re-scale:
predictions_tr4 <- data.frame(
  date = mlts4$dt[training_idx4],
  normalized = nn_predictions_tr4,
  denormalized = (nn_predictions_tr4 * normalization_constants4$std.dev) + normalization_constants4$mean
)

predictions_tr4

# plot prediction vs real
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2020-08-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr4) 

###########################################################
#7- Forecasting 30 days
tail(bit4_mlp)

bit4_mlp_ts <- rbind(
  tail(bit4_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2020-07-01"),
      as.Date("2020-07-01") + 30,
      "day"
    ),
    binance2 = NA,
    normalized = NA
  )
); rownames(bit4_mlp_ts) <- NULL

bit4_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit4_mlp_ts)) {
  new_mlts4 <- mlts_transform(
    bit4_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts4 <- new_mlts4[-1, ] # don't need to forecast known outcome
  
  bit4_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model4, new_mlts4[, nn_features4])$net.result
  )
}

new_mlts4

bit4_mlp_ts$forecast <- (bit4_mlp_ts$normalized * normalization_constants4$std.dev) + normalization_constants4$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2020-08-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr4)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit4_mlp_ts, days >= '2020-07-01'))

#acuracy mlp
bit4_mlp_ts

base4 <- data.frame(filter(bit4_mlp_ts, days >= "2020-07-01" & days <= "2020-07-30"), filter(tabela_bnc, days >= "2020-07-01" & days <= "2020-07-30"))
forecast::accuracy(base4$forecast, base4$binance2.1) 

#######################################
#######################################
# MPL5 BINANCE
# dados
bit5_mlp <- tabela_bnc %>% filter(days < "2021-01-01" )
summary(bit5_mlp)

#split point
split_point5 <- "2021-01-01"


## Save for later use (when converting back to original scale):
normalization_constants5 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit5_mlp$binance2[bit5_mlp$days < split_point5]) #nao entedi
)
bit5_mlp$normalized <- (bit5_mlp$binance2 - normalization_constants5$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit5_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts5 <- mlts_transform(bit5_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts5)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx5 <- which(mlts5$dt < split_point5)
testing_idx5 <- which(mlts5$dt >= split_point5)
head(training_idx5) #vetor de 1 a X
testing_idx5 #vetor de 1555 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features5 <- grep("(mlts_lag_[0-9]+)", names(mlts5), value = TRUE)
nn_features5
nn_formula5 <- as.formula(paste("y ~", paste(nn_features5, collapse = " + ")))
nn_formula5

# Train:
set.seed(0)
nn_model5 <- neuralnet(
  nn_formula5, mlts5[training_idx5, c("y", nn_features5)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr5 <- as.numeric(neuralnet::compute(
  nn_model5, mlts5[training_idx5, nn_features5])$net.result
)

# Re-scale:
predictions_tr5 <- data.frame(
  date = mlts5$dt[training_idx5],
  normalized = nn_predictions_tr5,
  denormalized = (nn_predictions_tr5 * normalization_constants5$std.dev) + normalization_constants5$mean
)

predictions_tr5

# plot prediction vs real
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2021-02-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr5) 

###########################################################
#7- Forecasting 30 days
tail(bit5_mlp)

bit5_mlp_ts <- rbind(
  tail(bit5_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2021-01-01"),
      as.Date("2021-01-01") + 30,
      "day"
    ),
    binance2 = NA,
    normalized = NA
  )
); rownames(bit5_mlp_ts) <- NULL

bit5_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit5_mlp_ts)) {
  new_mlts5 <- mlts_transform(
    bit5_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts5 <- new_mlts5[-1, ] # don't need to forecast known outcome
  
  bit5_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model5, new_mlts5[, nn_features5])$net.result
  )
}

new_mlts5

bit5_mlp_ts$forecast <- (bit5_mlp_ts$normalized * normalization_constants5$std.dev) + normalization_constants5$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2021-02-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr5)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit5_mlp_ts, days >= '2021-01-01'))

#acuracy mlp
bit5_mlp_ts

base5 <- data.frame(filter(bit5_mlp_ts, days >= "2021-01-01" & days <= "2021-01-30"), filter(tabela_bnc, days >= "2021-01-01" & days <= "2021-01-30"))
forecast::accuracy(base5$forecast, base5$binance2.1) 

#######################################
#######################################
# MPL6 BINANCE
# dados
bit6_mlp <- tabela_bnc %>% filter(days < "2021-07-01" )
summary(bit6_mlp)

#split point
split_point6 <- "2021-07-01"


## Save for later use (when converting back to original scale):
normalization_constants6 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit6_mlp$binance2[bit6_mlp$days < split_point6]) #nao entedi
)
bit6_mlp$normalized <- (bit6_mlp$binance2 - normalization_constants6$mean)/normalization_constants6$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit6_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts6 <- mlts_transform(bit6_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts6)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx6 <- which(mlts6$dt < split_point6)
testing_idx6 <- which(mlts6$dt >= split_point6)
head(training_idx6) #vetor de 1 a X
testing_idx6 #vetor de 1655 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features6 <- grep("(mlts_lag_[0-9]+)", names(mlts6), value = TRUE)
nn_features6
nn_formula6 <- as.formula(paste("y ~", paste(nn_features6, collapse = " + ")))
nn_formula6

# Train:
set.seed(0)
nn_model6 <- neuralnet(
  nn_formula6, mlts6[training_idx6, c("y", nn_features6)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr6 <- as.numeric(neuralnet::compute(
  nn_model6, mlts6[training_idx6, nn_features6])$net.result
)

# Re-scale:
predictions_tr6 <- data.frame(
  date = mlts6$dt[training_idx6],
  normalized = nn_predictions_tr6,
  denormalized = (nn_predictions_tr6 * normalization_constants6$std.dev) + normalization_constants6$mean
)

predictions_tr6

# plot prediction vs real
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2021-08-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr6) 

###########################################################
#7- Forecasting 30 days
tail(bit6_mlp)

bit6_mlp_ts <- rbind(
  tail(bit6_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    binance2 = NA,
    normalized = NA
  )
); rownames(bit6_mlp_ts) <- NULL

bit6_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit6_mlp_ts)) {
  new_mlts6 <- mlts_transform(
    bit6_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts6 <- new_mlts6[-1, ] # don't need to forecast known outcome
  
  bit6_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model6, new_mlts6[, nn_features6])$net.result
  )
}

new_mlts6

bit6_mlp_ts$forecast <- (bit6_mlp_ts$normalized * normalization_constants6$std.dev) + normalization_constants6$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2021-08-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr6)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit6_mlp_ts, days >= '2021-07-01'))

#acuracy mlp
bit6_mlp_ts

base6 <- data.frame(filter(bit6_mlp_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_bnc, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base6$forecast, base6$binance2.1) 

#######################################
#######################################
# MPL7 BINANCE
# dados
bit7_mlp <- tabela_bnc %>% filter(days < "2022-01-01" )
summary(bit7_mlp)

#split point
split_point7 <- "2022-01-01"


## Save for later use (when converting back to original scale):
normalization_constants7 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit7_mlp$binance2[bit7_mlp$days < split_point7]) #nao entedi
)
bit7_mlp$normalized <- (bit7_mlp$binance2 - normalization_constants7$mean)/normalization_constants7$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit7_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts7 <- mlts_transform(bit7_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts7)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx7 <- which(mlts7$dt < split_point7)
testing_idx7 <- which(mlts7$dt >= split_point7)
head(training_idx7) #vetor de 1 a X
testing_idx7 #vetor de 1755 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features7 <- grep("(mlts_lag_[0-9]+)", names(mlts7), value = TRUE)
nn_features7
nn_formula7 <- as.formula(paste("y ~", paste(nn_features7, collapse = " + ")))
nn_formula7

# Train:
set.seed(0)
nn_model7 <- neuralnet(
  nn_formula7, mlts7[training_idx7, c("y", nn_features7)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

###########################################################
#6- Assessment
#treino
nn_predictions_tr7 <- as.numeric(neuralnet::compute(
  nn_model7, mlts7[training_idx7, nn_features7])$net.result
)

# Re-scale:
predictions_tr7 <- data.frame(
  date = mlts7$dt[training_idx7],
  normalized = nn_predictions_tr7,
  denormalized = (nn_predictions_tr7 * normalization_constants7$std.dev) + normalization_constants7$mean
)

predictions_tr7

# plot prediction vs real
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2022-02-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr7) 

###########################################################
#7- Forecasting 30 days
tail(bit7_mlp)

bit7_mlp_ts <- rbind(
  tail(bit7_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2022-01-01"),
      as.Date("2022-01-01") + 30,
      "day"
    ),
    binance2 = NA,
    normalized = NA
  )
); rownames(bit7_mlp_ts) <- NULL

bit7_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit7_mlp_ts)) {
  new_mlts7 <- mlts_transform(
    bit7_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts7 <- new_mlts7[-1, ] # don't need to forecast known outcome
  
  bit7_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model7, new_mlts7[, nn_features7])$net.result
  )
}

new_mlts7

bit7_mlp_ts$forecast <- (bit7_mlp_ts$normalized * normalization_constants7$std.dev) + normalization_constants7$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days < "2022-02-01"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr7)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit7_mlp_ts, days >= '2022-01-01'))

#acuracy mlp
bit7_mlp_ts

base7 <- data.frame(filter(bit7_mlp_ts, days >= "2022-01-01" & days <= "2022-01-30"), filter(tabela_bnc, days >= "2022-01-01" & days <= "2022-01-30"))
forecast::accuracy(base7$forecast, base7$binance2.1) 

#######
#juntando todos os mapes
#accuracy
bnc_mlp_ac1 <- forecast::accuracy(base$forecast, base$binance2.1) 
bnc_mlp_ac2 <- forecast::accuracy(base2$forecast, base2$binance2.1) 
bnc_mlp_ac3 <- forecast::accuracy(base3$forecast, base3$binance2.1) 
bnc_mlp_ac4 <- forecast::accuracy(base4$forecast, base4$binance2.1) 
bnc_mlp_ac5 <- forecast::accuracy(base5$forecast, base5$binance2.1) 
bnc_mlp_ac6 <- forecast::accuracy(base6$forecast, base6$binance2.1) 
bnc_mlp_ac7 <- forecast::accuracy(base7$forecast, base7$binance2.1) 

bnc_mlp_accuracy_mean <- bind_rows(bnc_mlp_ac1[1,],bnc_mlp_ac2[1,],bnc_mlp_ac3[1,],bnc_mlp_ac4[1,],bnc_mlp_ac5[1,],bnc_mlp_ac6[1,],bnc_mlp_ac7[1,])
bnc_mlp_accuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), bnc_mlp_accuracy_mean)

btc_mlp_accuracy_mean 
eth_mlp_accuracy_mean
bnc_mlp_accuracy_mean


###########################################################
#MLP BITCOIN
###########################################################












# MPL1 BITCOIN
# dados
bit1_mlp <- tabela_btc %>% filter(days < "2019-01-01" )
summary(bit1_mlp)

#split point
split_point1 <- "2019-01-01"
max_point1 <- "2019-01-30"


## Save for later use (when converting back to original scale):
normalization_constants <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit1_mlp$bitcoin2[bit1_mlp$days < split_point1]) #nao entedi
)
bit1_mlp$normalized <- (bit1_mlp$bitcoin2 - normalization_constants$mean)/normalization_constants$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

head(bit1_mlp)

# plot
ggplot(data = bit1_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts1 <- mlts_transform(bit1_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts1)

# plot lags
ggplot(data = mlts1) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx <- which(mlts1$dt < split_point1)
testing_idx <- which(mlts1$dt >= split_point1 & mlts1$dt <= max_point1)
head(training_idx) #vetor de 1 a X
testing_idx #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features <- grep("(mlts_lag_[0-9]+)", names(mlts1), value = TRUE)
nn_features
nn_formula <- as.formula(paste("y ~", paste(nn_features, collapse = " + ")))
nn_formula

# Train:
set.seed(0)
nn_model <- neuralnet(
  nn_formula, mlts1[training_idx, c("y", nn_features)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr <- as.numeric(neuralnet::compute(
  nn_model, mlts1[training_idx, nn_features])$net.result
)

# Re-scale:
predictions_tr <- data.frame(
  date = mlts1$dt[training_idx],
  normalized = nn_predictions_tr,
  denormalized = (nn_predictions_tr * normalization_constants$std.dev) + normalization_constants$mean
)

#Tipo esse vai pro tcc
# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2019-01-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr) 

###########################################################
#7- Forecasting 30 days
tail(bit1_mlp)

bit1_mlp_ts <- rbind(
  tail(bit1_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2019-01-01"),
      as.Date("2019-01-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    normalized = NA
  )
); rownames(bit1_mlp_ts) <- NULL

bit1_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit1_mlp_ts)) {
  new_mlts <- mlts_transform(
    bit1_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts <- new_mlts[-1, ] # don't need to forecast known outcome
  
  bit1_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model, new_mlts[, nn_features])$net.result
  )
}

new_mlts #1 linha com os 28 lags
bit1_mlp_ts


bit1_mlp_ts$forecast <- (bit1_mlp_ts$normalized * normalization_constants$std.dev) + normalization_constants$mean



# plot prediction vs real + forecasting
##################3
#VAI PRO TCC
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2019-01-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit1_mlp_ts, days >= '2019-01-01'))

cols3 <- c("Bitcoin" = "black", "Modelo" = "blue", "Forecast" = "red")
jpeg("fig6.jpeg", width = 8, height = 4.5, units = c("in"), res = 900)
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2019-01-31")) + 
  theme_few() +
  labs(x = 'Dia', y = 'Preço de fechamento ($)') +
  geom_line(aes(x = days, y = bitcoin2, color = "Bitcoin"))  +
  geom_line(aes(x = date, y = denormalized, color = "Modelo"), data = predictions_tr)  +
  geom_line(aes(x = days, y = forecast, color = "Forecast"), data = filter(bit1_mlp_ts, days >= '2019-01-01')) +
  scale_color_manual(name = "", values = cols3) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))
dev.off()




base <- data.frame(filter(bit1_mlp_ts, days >= "2019-01-01" & days <= "2019-01-31"), filter(tabela_btc, days >= "2019-01-01" & days <= "2019-01-31"))

forecast::accuracy(base$forecast, base$bitcoin2.1) 



###########################################################
###########################################################
# MPL2
# dados
bit2_mlp <- tabela_btc %>% filter(days < "2019-07-01" )
summary(bit2_mlp)

#split point
split_point2 <- "2019-07-01"


## Save for later use (when converting back to original scale):
normalization_constants2 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit2_mlp$bitcoin2[bit2_mlp$days < split_point2]) #nao entedi
)
bit2_mlp$normalized <- (bit2_mlp$bitcoin2 - normalization_constants2$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit2_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts2 <- mlts_transform(bit2_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts2)

# plot lags
ggplot(data = mlts2) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx2 <- which(mlts2$dt < split_point2)
testing_idx2 <- which(mlts2$dt >= split_point2)
head(training_idx2) #vetor de 1 a X
testing_idx2 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features2 <- grep("(mlts_lag_[0-9]+)", names(mlts2), value = TRUE)
nn_features2
nn_formula2 <- as.formula(paste("y ~", paste(nn_features2, collapse = " + ")))
nn_formula2

# Train:
set.seed(0)
nn_model2 <- neuralnet(
  nn_formula2, mlts2[training_idx2, c("y", nn_features2)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr2 <- as.numeric(neuralnet::compute(
  nn_model2, mlts2[training_idx2, nn_features2])$net.result
)

# Re-scale:
predictions_tr2 <- data.frame(
  date = mlts2$dt[training_idx2],
  normalized = nn_predictions_tr2,
  denormalized = (nn_predictions_tr2 * normalization_constants2$std.dev) + normalization_constants2$mean
)

predictions_tr2

# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2019-08-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr2) 

###########################################################
#7- Forecasting 30 days
tail(bit2_mlp)

bit2_mlp_ts <- rbind(
  tail(bit2_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2019-07-01"),
      as.Date("2019-07-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    normalized = NA
  )
); rownames(bit1_mlp_ts) <- NULL

bit2_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit2_mlp_ts)) {
  new_mlts2 <- mlts_transform(
    bit2_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts2 <- new_mlts2[-1, ] # don't need to forecast known outcome
  
  bit2_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model2, new_mlts2[, nn_features2])$net.result
  )
}

new_mlts2

bit2_mlp_ts$forecast <- (bit2_mlp_ts$normalized * normalization_constants2$std.dev) + normalization_constants2$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2019-08-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr2)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit2_mlp_ts, days >= '2019-07-01'))

#acuracy mlp
bit2_mlp_ts

base2 <- data.frame(filter(bit2_mlp_ts, days >= "2019-07-01" & days <= "2019-07-30"), filter(tabela_btc, days >= "2019-07-01" & days <= "2019-07-30"))
forecast::accuracy(base2$forecast, base2$bitcoin2.1) 

#######################################
# MPL3 BINANCE
# dados
bit3_mlp <- tabela_btc %>% filter(days < "2020-01-01" )
summary(bit3_mlp)

#split point
split_point3 <- "2020-01-01"


## Save for later use (when converting back to original scale):
normalization_constants3 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit3_mlp$bitcoin2[bit3_mlp$days < split_point3]) #nao entedi
)
bit3_mlp$normalized <- (bit3_mlp$bitcoin2 - normalization_constants3$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit3_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts3 <- mlts_transform(bit3_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts3)

# plot lags
ggplot(data = mlts3) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx3 <- which(mlts3$dt < split_point3)
testing_idx3 <- which(mlts3$dt >= split_point3)
head(training_idx3) #vetor de 1 a X
testing_idx3 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features3 <- grep("(mlts_lag_[0-9]+)", names(mlts3), value = TRUE)
nn_features3
nn_formula3 <- as.formula(paste("y ~", paste(nn_features3, collapse = " + ")))
nn_formula3

# Train:
set.seed(0)
nn_model3 <- neuralnet(
  nn_formula3, mlts3[training_idx3, c("y", nn_features3)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr3 <- as.numeric(neuralnet::compute(
  nn_model3, mlts3[training_idx3, nn_features3])$net.result
)

# Re-scale:
predictions_tr3 <- data.frame(
  date = mlts3$dt[training_idx3],
  normalized = nn_predictions_tr3,
  denormalized = (nn_predictions_tr3 * normalization_constants3$std.dev) + normalization_constants3$mean
)

predictions_tr3

# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2020-01-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr3) 

###########################################################
#7- Forecasting 30 days
tail(bit3_mlp)

bit3_mlp_ts <- rbind(
  tail(bit3_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2020-01-01"),
      as.Date("2020-01-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    normalized = NA
  )
); rownames(bit3_mlp_ts) <- NULL

bit3_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit3_mlp_ts)) {
  new_mlts3 <- mlts_transform(
    bit3_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts3 <- new_mlts3[-1, ] # don't need to forecast known outcome
  
  bit3_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model3, new_mlts3[, nn_features3])$net.result
  )
}

new_mlts3

bit3_mlp_ts$forecast <- (bit3_mlp_ts$normalized * normalization_constants3$std.dev) + normalization_constants3$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2020-01-30"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr3)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit3_mlp_ts, days >= '2020-01-01'))

#acuracy mlp
bit3_mlp_ts

base3 <- data.frame(filter(bit3_mlp_ts, days >= "2020-01-01" & days <= "2020-01-30"), filter(tabela_btc, days >= "2020-01-01" & days <= "2020-01-30"))
forecast::accuracy(base3$forecast, base3$bitcoin2.1) 

#####################################
#####################################
# MPL4 BINANCE
# dados
bit4_mlp <- tabela_btc %>% filter(days < "2020-07-01" )
summary(bit4_mlp)

#split point
split_point4 <- "2020-07-01"


## Save for later use (when converting back to original scale):
normalization_constants4 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit4_mlp$bitcoin2[bit4_mlp$days < split_point4]) #nao entedi
)
bit4_mlp$normalized <- (bit4_mlp$bitcoin2 - normalization_constants4$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit4_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts4 <- mlts_transform(bit4_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts4)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx4 <- which(mlts4$dt < split_point4)
testing_idx4 <- which(mlts4$dt >= split_point4)
head(training_idx4) #vetor de 1 a X
testing_idx4 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features4 <- grep("(mlts_lag_[0-9]+)", names(mlts4), value = TRUE)
nn_features4
nn_formula4 <- as.formula(paste("y ~", paste(nn_features4, collapse = " + ")))
nn_formula4

# Train:
set.seed(0)
nn_model4 <- neuralnet(
  nn_formula4, mlts4[training_idx4, c("y", nn_features4)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr4 <- as.numeric(neuralnet::compute(
  nn_model4, mlts4[training_idx4, nn_features4])$net.result
)

# Re-scale:
predictions_tr4 <- data.frame(
  date = mlts4$dt[training_idx4],
  normalized = nn_predictions_tr4,
  denormalized = (nn_predictions_tr4 * normalization_constants4$std.dev) + normalization_constants4$mean
)

predictions_tr4

# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2020-08-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr4) 

###########################################################
#7- Forecasting 30 days
tail(bit4_mlp)

bit4_mlp_ts <- rbind(
  tail(bit4_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2020-07-01"),
      as.Date("2020-07-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    normalized = NA
  )
); rownames(bit4_mlp_ts) <- NULL

bit4_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit4_mlp_ts)) {
  new_mlts4 <- mlts_transform(
    bit4_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts4 <- new_mlts4[-1, ] # don't need to forecast known outcome
  
  bit4_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model4, new_mlts4[, nn_features4])$net.result
  )
}

new_mlts4

bit4_mlp_ts$forecast <- (bit4_mlp_ts$normalized * normalization_constants4$std.dev) + normalization_constants4$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2020-08-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr4)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit4_mlp_ts, days >= '2020-07-01'))

#acuracy mlp
bit4_mlp_ts

base4 <- data.frame(filter(bit4_mlp_ts, days >= "2020-07-01" & days <= "2020-07-30"), filter(tabela_btc, days >= "2020-07-01" & days <= "2020-07-30"))
forecast::accuracy(base4$forecast, base4$bitcoin2.1) 

#######################################
#######################################
# MPL5 BINANCE
# dados
bit5_mlp <- tabela_btc %>% filter(days < "2021-01-01" )
summary(bit5_mlp)

#split point
split_point5 <- "2021-01-01"


## Save for later use (when converting back to original scale):
normalization_constants5 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit5_mlp$bitcoin2[bit5_mlp$days < split_point5]) #nao entedi
)
bit5_mlp$normalized <- (bit5_mlp$bitcoin2 - normalization_constants5$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit5_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts5 <- mlts_transform(bit5_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts5)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx5 <- which(mlts5$dt < split_point5)
testing_idx5 <- which(mlts5$dt >= split_point5)
head(training_idx5) #vetor de 1 a X
testing_idx5 #vetor de 1555 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features5 <- grep("(mlts_lag_[0-9]+)", names(mlts5), value = TRUE)
nn_features5
nn_formula5 <- as.formula(paste("y ~", paste(nn_features5, collapse = " + ")))
nn_formula5

# Train:
set.seed(0)
nn_model5 <- neuralnet(
  nn_formula5, mlts5[training_idx5, c("y", nn_features5)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr5 <- as.numeric(neuralnet::compute(
  nn_model5, mlts5[training_idx5, nn_features5])$net.result
)

# Re-scale:
predictions_tr5 <- data.frame(
  date = mlts5$dt[training_idx5],
  normalized = nn_predictions_tr5,
  denormalized = (nn_predictions_tr5 * normalization_constants5$std.dev) + normalization_constants5$mean
)

predictions_tr5

# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2021-02-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr5) 

###########################################################
#7- Forecasting 30 days
tail(bit5_mlp)

bit5_mlp_ts <- rbind(
  tail(bit5_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2021-01-01"),
      as.Date("2021-01-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    normalized = NA
  )
); rownames(bit5_mlp_ts) <- NULL

bit5_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit5_mlp_ts)) {
  new_mlts5 <- mlts_transform(
    bit5_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts5 <- new_mlts5[-1, ] # don't need to forecast known outcome
  
  bit5_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model5, new_mlts5[, nn_features5])$net.result
  )
}

new_mlts5

bit5_mlp_ts$forecast <- (bit5_mlp_ts$normalized * normalization_constants5$std.dev) + normalization_constants5$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2021-02-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr5)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit5_mlp_ts, days >= '2021-01-01'))

#acuracy mlp
bit5_mlp_ts

base5 <- data.frame(filter(bit5_mlp_ts, days >= "2021-01-01" & days <= "2021-01-30"), filter(tabela_btc, days >= "2021-01-01" & days <= "2021-01-30"))
forecast::accuracy(base5$forecast, base5$bitcoin2.1) 

#######################################
#######################################
# MPL6 BINANCE
# dados
bit6_mlp <- tabela_btc %>% filter(days < "2021-07-01" )
summary(bit6_mlp)

#split point
split_point6 <- "2021-07-01"


## Save for later use (when converting back to original scale):
normalization_constants6 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit6_mlp$bitcoin2[bit6_mlp$days < split_point6]) #nao entedi
)
bit6_mlp$normalized <- (bit6_mlp$bitcoin2 - normalization_constants6$mean)/normalization_constants6$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit6_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts6 <- mlts_transform(bit6_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts6)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx6 <- which(mlts6$dt < split_point6)
testing_idx6 <- which(mlts6$dt >= split_point6)
head(training_idx6) #vetor de 1 a X
testing_idx6 #vetor de 1655 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features6 <- grep("(mlts_lag_[0-9]+)", names(mlts6), value = TRUE)
nn_features6
nn_formula6 <- as.formula(paste("y ~", paste(nn_features6, collapse = " + ")))
nn_formula6

# Train:
set.seed(0)
nn_model6 <- neuralnet(
  nn_formula6, mlts6[training_idx6, c("y", nn_features6)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr6 <- as.numeric(neuralnet::compute(
  nn_model6, mlts6[training_idx6, nn_features6])$net.result
)

# Re-scale:
predictions_tr6 <- data.frame(
  date = mlts6$dt[training_idx6],
  normalized = nn_predictions_tr6,
  denormalized = (nn_predictions_tr6 * normalization_constants6$std.dev) + normalization_constants6$mean
)

predictions_tr6

# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2021-08-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr6) 

###########################################################
#7- Forecasting 30 days
tail(bit6_mlp)

bit6_mlp_ts <- rbind(
  tail(bit6_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    normalized = NA
  )
); rownames(bit6_mlp_ts) <- NULL

bit6_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit6_mlp_ts)) {
  new_mlts6 <- mlts_transform(
    bit6_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts6 <- new_mlts6[-1, ] # don't need to forecast known outcome
  
  bit6_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model6, new_mlts6[, nn_features6])$net.result
  )
}

new_mlts6

bit6_mlp_ts$forecast <- (bit6_mlp_ts$normalized * normalization_constants6$std.dev) + normalization_constants6$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2021-08-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr6)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit6_mlp_ts, days >= '2021-07-01'))

#acuracy mlp
bit6_mlp_ts

base6 <- data.frame(filter(bit6_mlp_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_btc, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base6$forecast, base6$bitcoin2.1) 

#######################################
#######################################
# MPL7 BINANCE
# dados
bit7_mlp <- tabela_btc %>% filter(days < "2022-01-01" )
summary(bit7_mlp)

#split point
split_point7 <- "2022-01-01"


## Save for later use (when converting back to original scale):
normalization_constants7 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit7_mlp$bitcoin2[bit7_mlp$days < split_point7]) #nao entedi
)
bit7_mlp$normalized <- (bit7_mlp$bitcoin2 - normalization_constants7$mean)/normalization_constants7$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$bitcoin2)

# plot
ggplot(data = bit7_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts7 <- mlts_transform(bit7_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts7)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx7 <- which(mlts7$dt < split_point7)
testing_idx7 <- which(mlts7$dt >= split_point7)
head(training_idx7) #vetor de 1 a X
testing_idx7 #vetor de 1755 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features7 <- grep("(mlts_lag_[0-9]+)", names(mlts7), value = TRUE)
nn_features7
nn_formula7 <- as.formula(paste("y ~", paste(nn_features7, collapse = " + ")))
nn_formula7

# Train:
set.seed(0)
nn_model7 <- neuralnet(
  nn_formula7, mlts7[training_idx7, c("y", nn_features7)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

###########################################################
#6- Assessment
#treino
nn_predictions_tr7 <- as.numeric(neuralnet::compute(
  nn_model7, mlts7[training_idx7, nn_features7])$net.result
)

# Re-scale:
predictions_tr7 <- data.frame(
  date = mlts7$dt[training_idx7],
  normalized = nn_predictions_tr7,
  denormalized = (nn_predictions_tr7 * normalization_constants7$std.dev) + normalization_constants7$mean
)

predictions_tr7

# plot prediction vs real
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2022-02-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr7) 

###########################################################
#7- Forecasting 30 days
tail(bit7_mlp)

bit7_mlp_ts <- rbind(
  tail(bit7_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2022-01-01"),
      as.Date("2022-01-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    normalized = NA
  )
); rownames(bit7_mlp_ts) <- NULL

bit7_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit7_mlp_ts)) {
  new_mlts7 <- mlts_transform(
    bit7_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts7 <- new_mlts7[-1, ] # don't need to forecast known outcome
  
  bit7_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model7, new_mlts7[, nn_features7])$net.result
  )
}

new_mlts7

bit7_mlp_ts$forecast <- (bit7_mlp_ts$normalized * normalization_constants7$std.dev) + normalization_constants7$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days < "2022-02-01"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr7)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit7_mlp_ts, days >= '2022-01-01'))

#acuracy mlp
bit7_mlp_ts

base7 <- data.frame(filter(bit7_mlp_ts, days >= "2022-01-01" & days <= "2022-01-30"), filter(tabela_btc, days >= "2022-01-01" & days <= "2022-01-30"))
forecast::accuracy(base7$forecast, base7$bitcoin2.1) 

#######
#juntando todos os mapes
#accuracy
btc_mlp_ac1 <- forecast::accuracy(base$forecast, base$bitcoin2.1) 
btc_mlp_ac2 <- forecast::accuracy(base2$forecast, base2$bitcoin2.1) 
btc_mlp_ac3 <- forecast::accuracy(base3$forecast, base3$bitcoin2.1) 
btc_mlp_ac4 <- forecast::accuracy(base4$forecast, base4$bitcoin2.1) 
btc_mlp_ac5 <- forecast::accuracy(base5$forecast, base5$bitcoin2.1) 
btc_mlp_ac6 <- forecast::accuracy(base6$forecast, base6$bitcoin2.1) 
btc_mlp_ac7 <- forecast::accuracy(base7$forecast, base7$bitcoin2.1) 

btc_mlp_accuracy_mean <- bind_rows(btc_mlp_ac1[1,],btc_mlp_ac2[1,],btc_mlp_ac3[1,],btc_mlp_ac4[1,],btc_mlp_ac5[1,],btc_mlp_ac6[1,],btc_mlp_ac7[1,])
btc_mlp_accuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), btc_mlp_accuracy_mean)

btc_mlp_accuracy_mean 
eth_mlp_accuracy_mean
bnc_mlp_accuracy_mean


###########################################################
#MLP ETHERIUM
###########################################################


# MPL1 BITCOIN
# dados
bit1_mlp <- tabela_eth %>% filter(days < "2019-01-01" )
summary(bit1_mlp)

#split point
split_point1 <- "2019-01-01"
max_point1 <- "2019-01-30"


## Save for later use (when converting back to original scale):
normalization_constants <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit1_mlp$etherium2[bit1_mlp$days < split_point1]) #nao entedi
)
bit1_mlp$normalized <- (bit1_mlp$etherium2 - normalization_constants$mean)/normalization_constants$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$etherium2)

head(bit1_mlp)

# plot
ggplot(data = bit1_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts1 <- mlts_transform(bit1_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts1)

# plot lags
ggplot(data = mlts1) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx <- which(mlts1$dt < split_point1)
testing_idx <- which(mlts1$dt >= split_point1 & mlts1$dt <= max_point1)
head(training_idx) #vetor de 1 a X
testing_idx #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features <- grep("(mlts_lag_[0-9]+)", names(mlts1), value = TRUE)
nn_features
nn_formula <- as.formula(paste("y ~", paste(nn_features, collapse = " + ")))
nn_formula

# Train:
set.seed(0)
nn_model <- neuralnet(
  nn_formula, mlts1[training_idx, c("y", nn_features)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr <- as.numeric(neuralnet::compute(
  nn_model, mlts1[training_idx, nn_features])$net.result
)

# Re-scale:
predictions_tr <- data.frame(
  date = mlts1$dt[training_idx],
  normalized = nn_predictions_tr,
  denormalized = (nn_predictions_tr * normalization_constants$std.dev) + normalization_constants$mean
)

#Tipo esse vai pro tcc
# plot prediction vs real
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2019-01-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr) 

###########################################################
#7- Forecasting 30 days
tail(bit1_mlp)

bit1_mlp_ts <- rbind(
  tail(bit1_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2019-01-01"),
      as.Date("2019-01-01") + 30,
      "day"
    ),
    etherium2 = NA,
    normalized = NA
  )
); rownames(bit1_mlp_ts) <- NULL

bit1_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit1_mlp_ts)) {
  new_mlts <- mlts_transform(
    bit1_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts <- new_mlts[-1, ] # don't need to forecast known outcome
  
  bit1_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model, new_mlts[, nn_features])$net.result
  )
}

new_mlts #1 linha com os 28 lags
bit1_mlp_ts


bit1_mlp_ts$forecast <- (bit1_mlp_ts$normalized * normalization_constants$std.dev) + normalization_constants$mean



# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2019-01-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit1_mlp_ts, days >= '2019-01-01'))



base <- data.frame(filter(bit1_mlp_ts, days >= "2019-01-01" & days <= "2019-01-31"), filter(tabela_eth, days >= "2019-01-01" & days <= "2019-01-31"))

forecast::accuracy(base$forecast, base$etherium2.1) 



###########################################################
###########################################################
# MPL2
# dados
bit2_mlp <- tabela_eth %>% filter(days < "2019-07-01" )
summary(bit2_mlp)

#split point
split_point2 <- "2019-07-01"


## Save for later use (when converting back to original scale):
normalization_constants2 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit2_mlp$etherium2[bit2_mlp$days < split_point2]) #nao entedi
)
bit2_mlp$normalized <- (bit2_mlp$etherium2 - normalization_constants2$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$etherium2)

# plot
ggplot(data = bit2_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts2 <- mlts_transform(bit2_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts2)

# plot lags
ggplot(data = mlts2) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx2 <- which(mlts2$dt < split_point2)
testing_idx2 <- which(mlts2$dt >= split_point2)
head(training_idx2) #vetor de 1 a X
testing_idx2 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features2 <- grep("(mlts_lag_[0-9]+)", names(mlts2), value = TRUE)
nn_features2
nn_formula2 <- as.formula(paste("y ~", paste(nn_features2, collapse = " + ")))
nn_formula2

# Train:
set.seed(0)
nn_model2 <- neuralnet(
  nn_formula2, mlts2[training_idx2, c("y", nn_features2)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr2 <- as.numeric(neuralnet::compute(
  nn_model2, mlts2[training_idx2, nn_features2])$net.result
)

# Re-scale:
predictions_tr2 <- data.frame(
  date = mlts2$dt[training_idx2],
  normalized = nn_predictions_tr2,
  denormalized = (nn_predictions_tr2 * normalization_constants2$std.dev) + normalization_constants2$mean
)

predictions_tr2

# plot prediction vs real
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2019-08-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr2) 

###########################################################
#7- Forecasting 30 days
tail(bit2_mlp)

bit2_mlp_ts <- rbind(
  tail(bit2_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2019-07-01"),
      as.Date("2019-07-01") + 30,
      "day"
    ),
    etherium2 = NA,
    normalized = NA
  )
); rownames(bit1_mlp_ts) <- NULL

bit2_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit2_mlp_ts)) {
  new_mlts2 <- mlts_transform(
    bit2_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts2 <- new_mlts2[-1, ] # don't need to forecast known outcome
  
  bit2_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model2, new_mlts2[, nn_features2])$net.result
  )
}

new_mlts2

bit2_mlp_ts$forecast <- (bit2_mlp_ts$normalized * normalization_constants2$std.dev) + normalization_constants2$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2019-08-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr2)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit2_mlp_ts, days >= '2019-07-01'))

#acuracy mlp
bit2_mlp_ts

base2 <- data.frame(filter(bit2_mlp_ts, days >= "2019-07-01" & days <= "2019-07-30"), filter(tabela_eth, days >= "2019-07-01" & days <= "2019-07-30"))
forecast::accuracy(base2$forecast, base2$etherium2.1) 

#######################################
# MPL3 BINANCE
# dados
bit3_mlp <- tabela_eth %>% filter(days < "2020-01-01" )
summary(bit3_mlp)

#split point
split_point3 <- "2020-01-01"


## Save for later use (when converting back to original scale):
normalization_constants3 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit3_mlp$etherium2[bit3_mlp$days < split_point3]) #nao entedi
)
bit3_mlp$normalized <- (bit3_mlp$etherium2 - normalization_constants3$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$etherium2)

# plot
ggplot(data = bit3_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts3 <- mlts_transform(bit3_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts3)

# plot lags
ggplot(data = mlts3) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = dt, y = y)) + 
  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx3 <- which(mlts3$dt < split_point3)
testing_idx3 <- which(mlts3$dt >= split_point3)
head(training_idx3) #vetor de 1 a X
testing_idx3 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features3 <- grep("(mlts_lag_[0-9]+)", names(mlts3), value = TRUE)
nn_features3
nn_formula3 <- as.formula(paste("y ~", paste(nn_features3, collapse = " + ")))
nn_formula3

# Train:
set.seed(0)
nn_model3 <- neuralnet(
  nn_formula3, mlts3[training_idx3, c("y", nn_features3)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr3 <- as.numeric(neuralnet::compute(
  nn_model3, mlts3[training_idx3, nn_features3])$net.result
)

# Re-scale:
predictions_tr3 <- data.frame(
  date = mlts3$dt[training_idx3],
  normalized = nn_predictions_tr3,
  denormalized = (nn_predictions_tr3 * normalization_constants3$std.dev) + normalization_constants3$mean
)

predictions_tr3

# plot prediction vs real
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2020-01-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr3) 

###########################################################
#7- Forecasting 30 days
tail(bit3_mlp)

bit3_mlp_ts <- rbind(
  tail(bit3_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2020-01-01"),
      as.Date("2020-01-01") + 30,
      "day"
    ),
    etherium2 = NA,
    normalized = NA
  )
); rownames(bit3_mlp_ts) <- NULL

bit3_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit3_mlp_ts)) {
  new_mlts3 <- mlts_transform(
    bit3_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts3 <- new_mlts3[-1, ] # don't need to forecast known outcome
  
  bit3_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model3, new_mlts3[, nn_features3])$net.result
  )
}

new_mlts3

bit3_mlp_ts$forecast <- (bit3_mlp_ts$normalized * normalization_constants3$std.dev) + normalization_constants3$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2020-01-30"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr3)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit3_mlp_ts, days >= '2020-01-01'))

#acuracy mlp
bit3_mlp_ts

base3 <- data.frame(filter(bit3_mlp_ts, days >= "2020-01-01" & days <= "2020-01-30"), filter(tabela_eth, days >= "2020-01-01" & days <= "2020-01-30"))
forecast::accuracy(base3$forecast, base3$etherium2.1) 

#####################################
#####################################
# MPL4 BINANCE
# dados
bit4_mlp <- tabela_eth %>% filter(days < "2020-07-01" )
summary(bit4_mlp)

#split point
split_point4 <- "2020-07-01"


## Save for later use (when converting back to original scale):
normalization_constants4 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit4_mlp$etherium2[bit4_mlp$days < split_point4]) #nao entedi
)
bit4_mlp$normalized <- (bit4_mlp$etherium2 - normalization_constants4$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$etherium2)

# plot
ggplot(data = bit4_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts4 <- mlts_transform(bit4_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts4)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx4 <- which(mlts4$dt < split_point4)
testing_idx4 <- which(mlts4$dt >= split_point4)
head(training_idx4) #vetor de 1 a X
testing_idx4 #vetor de 1455 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features4 <- grep("(mlts_lag_[0-9]+)", names(mlts4), value = TRUE)
nn_features4
nn_formula4 <- as.formula(paste("y ~", paste(nn_features4, collapse = " + ")))
nn_formula4

# Train:
set.seed(0)
nn_model4 <- neuralnet(
  nn_formula4, mlts4[training_idx4, c("y", nn_features4)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr4 <- as.numeric(neuralnet::compute(
  nn_model4, mlts4[training_idx4, nn_features4])$net.result
)

# Re-scale:
predictions_tr4 <- data.frame(
  date = mlts4$dt[training_idx4],
  normalized = nn_predictions_tr4,
  denormalized = (nn_predictions_tr4 * normalization_constants4$std.dev) + normalization_constants4$mean
)

predictions_tr4

# plot prediction vs real
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2020-08-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr4) 

###########################################################
#7- Forecasting 30 days
tail(bit4_mlp)

bit4_mlp_ts <- rbind(
  tail(bit4_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2020-07-01"),
      as.Date("2020-07-01") + 30,
      "day"
    ),
    etherium2 = NA,
    normalized = NA
  )
); rownames(bit4_mlp_ts) <- NULL

bit4_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit4_mlp_ts)) {
  new_mlts4 <- mlts_transform(
    bit4_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts4 <- new_mlts4[-1, ] # don't need to forecast known outcome
  
  bit4_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model4, new_mlts4[, nn_features4])$net.result
  )
}

new_mlts4

bit4_mlp_ts$forecast <- (bit4_mlp_ts$normalized * normalization_constants4$std.dev) + normalization_constants4$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2020-08-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr4)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit4_mlp_ts, days >= '2020-07-01'))

#acuracy mlp
bit4_mlp_ts

base4 <- data.frame(filter(bit4_mlp_ts, days >= "2020-07-01" & days <= "2020-07-30"), filter(tabela_eth, days >= "2020-07-01" & days <= "2020-07-30"))
forecast::accuracy(base4$forecast, base4$etherium2.1) 

#######################################
#######################################
# MPL5 BINANCE
# dados
bit5_mlp <- tabela_eth %>% filter(days < "2021-01-01" )
summary(bit5_mlp)

#split point
split_point5 <- "2021-01-01"


## Save for later use (when converting back to original scale):
normalization_constants5 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit5_mlp$etherium2[bit5_mlp$days < split_point5]) #nao entedi
)
bit5_mlp$normalized <- (bit5_mlp$etherium2 - normalization_constants5$mean)/normalization_constants2$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$etherium2)

# plot
ggplot(data = bit5_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts5 <- mlts_transform(bit5_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts5)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx5 <- which(mlts5$dt < split_point5)
testing_idx5 <- which(mlts5$dt >= split_point5)
head(training_idx5) #vetor de 1 a X
testing_idx5 #vetor de 1555 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features5 <- grep("(mlts_lag_[0-9]+)", names(mlts5), value = TRUE)
nn_features5
nn_formula5 <- as.formula(paste("y ~", paste(nn_features5, collapse = " + ")))
nn_formula5

# Train:
set.seed(0)
nn_model5 <- neuralnet(
  nn_formula5, mlts5[training_idx5, c("y", nn_features5)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr5 <- as.numeric(neuralnet::compute(
  nn_model5, mlts5[training_idx5, nn_features5])$net.result
)

# Re-scale:
predictions_tr5 <- data.frame(
  date = mlts5$dt[training_idx5],
  normalized = nn_predictions_tr5,
  denormalized = (nn_predictions_tr5 * normalization_constants5$std.dev) + normalization_constants5$mean
)

predictions_tr5

# plot prediction vs real
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2021-02-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr5) 

###########################################################
#7- Forecasting 30 days
tail(bit5_mlp)

bit5_mlp_ts <- rbind(
  tail(bit5_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2021-01-01"),
      as.Date("2021-01-01") + 30,
      "day"
    ),
    etherium2 = NA,
    normalized = NA
  )
); rownames(bit5_mlp_ts) <- NULL

bit5_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit5_mlp_ts)) {
  new_mlts5 <- mlts_transform(
    bit5_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts5 <- new_mlts5[-1, ] # don't need to forecast known outcome
  
  bit5_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model5, new_mlts5[, nn_features5])$net.result
  )
}

new_mlts5

bit5_mlp_ts$forecast <- (bit5_mlp_ts$normalized * normalization_constants5$std.dev) + normalization_constants5$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2021-02-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr5)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit5_mlp_ts, days >= '2021-01-01'))

#acuracy mlp
bit5_mlp_ts

base5 <- data.frame(filter(bit5_mlp_ts, days >= "2021-01-01" & days <= "2021-01-30"), filter(tabela_eth, days >= "2021-01-01" & days <= "2021-01-30"))
forecast::accuracy(base5$forecast, base5$etherium2.1) 

#######################################
#######################################
# MPL6 BINANCE
# dados
bit6_mlp <- tabela_eth %>% filter(days < "2021-07-01" )
summary(bit6_mlp)

#split point
split_point6 <- "2021-07-01"


## Save for later use (when converting back to original scale):
normalization_constants6 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit6_mlp$etherium2[bit6_mlp$days < split_point6]) #nao entedi
)
bit6_mlp$normalized <- (bit6_mlp$etherium2 - normalization_constants6$mean)/normalization_constants6$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$etherium2)

# plot
ggplot(data = bit6_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts6 <- mlts_transform(bit6_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts6)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx6 <- which(mlts6$dt < split_point6)
testing_idx6 <- which(mlts6$dt >= split_point6)
head(training_idx6) #vetor de 1 a X
testing_idx6 #vetor de 1655 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features6 <- grep("(mlts_lag_[0-9]+)", names(mlts6), value = TRUE)
nn_features6
nn_formula6 <- as.formula(paste("y ~", paste(nn_features6, collapse = " + ")))
nn_formula6

# Train:
set.seed(0)
nn_model6 <- neuralnet(
  nn_formula6, mlts6[training_idx6, c("y", nn_features6)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

#plot(nn_model)

###########################################################
#6- Assessment
#treino
nn_predictions_tr6 <- as.numeric(neuralnet::compute(
  nn_model6, mlts6[training_idx6, nn_features6])$net.result
)

# Re-scale:
predictions_tr6 <- data.frame(
  date = mlts6$dt[training_idx6],
  normalized = nn_predictions_tr6,
  denormalized = (nn_predictions_tr6 * normalization_constants6$std.dev) + normalization_constants6$mean
)

predictions_tr6

# plot prediction vs real
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2021-08-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr6) 

###########################################################
#7- Forecasting 30 days
tail(bit6_mlp)

bit6_mlp_ts <- rbind(
  tail(bit6_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    etherium2 = NA,
    normalized = NA
  )
); rownames(bit6_mlp_ts) <- NULL

bit6_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit6_mlp_ts)) {
  new_mlts6 <- mlts_transform(
    bit6_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts6 <- new_mlts6[-1, ] # don't need to forecast known outcome
  
  bit6_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model6, new_mlts6[, nn_features6])$net.result
  )
}

new_mlts6

bit6_mlp_ts$forecast <- (bit6_mlp_ts$normalized * normalization_constants6$std.dev) + normalization_constants6$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2021-08-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr6)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit6_mlp_ts, days >= '2021-07-01'))

#acuracy mlp
bit6_mlp_ts

base6 <- data.frame(filter(bit6_mlp_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_eth, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base6$forecast, base6$etherium2.1) 

#######################################
#######################################
# MPL7 BINANCE
# dados
bit7_mlp <- tabela_eth %>% filter(days < "2022-01-01" )
summary(bit7_mlp)

#split point
split_point7 <- "2022-01-01"


## Save for later use (when converting back to original scale):
normalization_constants7 <- lapply(
  list(median = median, mad = mad, mean = mean, std.dev = sd),
  do.call, args = list(x = bit7_mlp$etherium2[bit7_mlp$days < split_point7]) #nao entedi
)
bit7_mlp$normalized <- (bit7_mlp$etherium2 - normalization_constants7$mean)/normalization_constants7$std.dev
#bit1_mlp$normalized2 <- scale(bit1_mlp$etherium2)

# plot
ggplot(data = bit7_mlp) + 
  theme_light() +
  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
  geom_line(mapping = aes(x = days, y = normalized), color = 'black')

###########################################################
#4- Feature Space >> criando variaveis extra alem dos lags
mlts7 <- mlts_transform(bit7_mlp, days, normalized, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts7)

# plot lags
#ggplot(data = mlts4) + 
#  theme_light() +
#  labs(title = 'Preço de fechamento($) por dia normalizado', x = 'dia', y = 'Preço ($)') +
#  geom_line(mapping = aes(x = dt, y = y)) + 
#  geom_line(mapping = aes(x = dt, y = mlts_lag_28), color = 'green')
###########################################################
#5 - Training

# Split:
training_idx7 <- which(mlts7$dt < split_point7)
testing_idx7 <- which(mlts7$dt >= split_point7)
head(training_idx7) #vetor de 1 a X
testing_idx7 #vetor de 1755 a Y

# neuralnet does not support the "y ~ ." formula syntax, so we cheat:
nn_features7 <- grep("(mlts_lag_[0-9]+)", names(mlts7), value = TRUE)
nn_features7
nn_formula7 <- as.formula(paste("y ~", paste(nn_features7, collapse = " + ")))
nn_formula7

# Train:
set.seed(0)
nn_model7 <- neuralnet(
  nn_formula7, mlts7[training_idx7, c("y", nn_features7)],
  linear.output = TRUE, hidden = c(3), threshold=0.04, act.fct="tanh", stepmax=1e7
)

###########################################################
#6- Assessment
#treino
nn_predictions_tr7 <- as.numeric(neuralnet::compute(
  nn_model7, mlts7[training_idx7, nn_features7])$net.result
)

# Re-scale:
predictions_tr7 <- data.frame(
  date = mlts7$dt[training_idx7],
  normalized = nn_predictions_tr7,
  denormalized = (nn_predictions_tr7 * normalization_constants7$std.dev) + normalization_constants7$mean
)

predictions_tr7

# plot prediction vs real
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2022-02-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr7) 

###########################################################
#7- Forecasting 30 days
tail(bit7_mlp)

bit7_mlp_ts <- rbind(
  tail(bit7_mlp, 29),
  data.frame(
    days = seq(
      as.Date("2022-01-01"),
      as.Date("2022-01-01") + 30,
      "day"
    ),
    etherium2 = NA,
    normalized = NA
  )
); rownames(bit7_mlp_ts) <- NULL

bit7_mlp_ts #base pronta pro forecast


for (d in 30:nrow(bit7_mlp_ts)) {
  new_mlts7 <- mlts_transform(
    bit7_mlp_ts[(d - 29):d, ],
    days, normalized, p = 28,
    extras = FALSE, extrasAsFactors = FALSE,
    granularity = "day")
  
  new_mlts7 <- new_mlts7[-1, ] # don't need to forecast known outcome
  
  bit7_mlp_ts$normalized[d] <- as.numeric(neuralnet::compute(
    nn_model7, new_mlts7[, nn_features7])$net.result
  )
}

new_mlts7

bit7_mlp_ts$forecast <- (bit7_mlp_ts$normalized * normalization_constants7$std.dev) + normalization_constants7$mean

# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days < "2022-02-01"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr7)  +
  geom_line(aes(x = days, y = forecast), color = "red", data = filter(bit7_mlp_ts, days >= '2022-01-01'))

#acuracy mlp
bit7_mlp_ts

base7 <- data.frame(filter(bit7_mlp_ts, days >= "2022-01-01" & days <= "2022-01-30"), filter(tabela_eth, days >= "2022-01-01" & days <= "2022-01-30"))
forecast::accuracy(base7$forecast, base7$etherium2.1) 

#######
#juntando todos os mapes
#accuracy
eth_mlp_ac1 <- forecast::accuracy(base$forecast, base$etherium2.1) 
eth_mlp_ac2 <- forecast::accuracy(base2$forecast, base2$etherium2.1) 
eth_mlp_ac3 <- forecast::accuracy(base3$forecast, base3$etherium2.1) 
eth_mlp_ac4 <- forecast::accuracy(base4$forecast, base4$etherium2.1) 
eth_mlp_ac5 <- forecast::accuracy(base5$forecast, base5$etherium2.1) 
eth_mlp_ac6 <- forecast::accuracy(base6$forecast, base6$etherium2.1) 
eth_mlp_ac7 <- forecast::accuracy(base7$forecast, base7$etherium2.1) 

eth_mlp_accuracy_mean <- bind_rows(eth_mlp_ac1[1,],eth_mlp_ac2[1,],eth_mlp_ac3[1,],eth_mlp_ac4[1,],eth_mlp_ac5[1,],eth_mlp_ac6[1,],eth_mlp_ac7[1,])
eth_mlp_accuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), eth_mlp_accuracy_mean)

btc_mlp_accuracy_mean 
eth_mlp_accuracy_mean
bnc_mlp_accuracy_mean





###########################################################
###########################################################
# MODELO RNN
###########################################################

# RNN1 
bit1_rnn <- tabela_btc %>% filter(days < "2019-01-01" )
tail(bit1_rnn)

minmax <- linscale(bit1_rnn$bitcoin2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bit1_rnn <- data.frame(bit1_rnn, minmax_df)

mlts1_rnn <- mlts_transform(bit1_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts1_rnn)
head(mlts1_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts1_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts1_rnn$y, dim = c(nrow(mlts1_rnn),1))
head(Y)

X <- array(c(mlts1_rnn$mlts_lag_1,
             mlts1_rnn$mlts_lag_2,
             mlts1_rnn$mlts_lag_3,
             mlts1_rnn$mlts_lag_4,
             mlts1_rnn$mlts_lag_5,
             mlts1_rnn$mlts_lag_6,
             mlts1_rnn$mlts_lag_7,
             mlts1_rnn$mlts_lag_8,
             mlts1_rnn$mlts_lag_9,
             mlts1_rnn$mlts_lag_10,
             mlts1_rnn$mlts_lag_11,
             mlts1_rnn$mlts_lag_12,
             mlts1_rnn$mlts_lag_13,
             mlts1_rnn$mlts_lag_14,
             mlts1_rnn$mlts_lag_15,
             mlts1_rnn$mlts_lag_16,
             mlts1_rnn$mlts_lag_17,
             mlts1_rnn$mlts_lag_18,
             mlts1_rnn$mlts_lag_19,
             mlts1_rnn$mlts_lag_20,
             mlts1_rnn$mlts_lag_21,
             mlts1_rnn$mlts_lag_22,
             mlts1_rnn$mlts_lag_23,
             mlts1_rnn$mlts_lag_24,
             mlts1_rnn$mlts_lag_25,
             mlts1_rnn$mlts_lag_26,
             mlts1_rnn$mlts_lag_27,
             mlts1_rnn$mlts_lag_28), 
             dim = c(nrow(mlts1_rnn),1, periodos_anteriores))
        
head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
                  )
  
 #plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_btc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                         minmax = minmax$minmax,
                         rev = TRUE)

#Plot resultados
finalresult <- data.frame(index = c(1:337),
                          real = bit1_rnn$bitcoin2[28:364],
                          pred = forecast1_treino_desnorm$x)

# plot prediction vs real treino
ggplot(data = finalresult,
       aes(x = index, y = real)) + geom_line() +
       geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)


###############################################################
#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bit1_rnn)

bit1_rnn_ts <- rbind(
  tail(bit1_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2019-01-01"),
      as.Date("2019-01-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    minmax.x = NA
  )
); rownames(bit1_rnn_ts) <- NULL

bit1_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bit1_rnn_ts)) {
  mlts1_rnn_teste <- mlts_transform(bit1_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts1_rnn_teste <- mlts1_rnn_teste[-1, ] # don't need to forecast known outcome

  X_teste <- array(c(mlts1_rnn_teste$mlts_lag_1,
                     mlts1_rnn_teste$mlts_lag_2,
                     mlts1_rnn_teste$mlts_lag_3,
                     mlts1_rnn_teste$mlts_lag_4,
                     mlts1_rnn_teste$mlts_lag_5,
                     mlts1_rnn_teste$mlts_lag_6,
                     mlts1_rnn_teste$mlts_lag_7,
                     mlts1_rnn_teste$mlts_lag_8,
                     mlts1_rnn_teste$mlts_lag_9,
                     mlts1_rnn_teste$mlts_lag_10,
                     mlts1_rnn_teste$mlts_lag_11,
                     mlts1_rnn_teste$mlts_lag_12,
                     mlts1_rnn_teste$mlts_lag_13,
                     mlts1_rnn_teste$mlts_lag_14,
                     mlts1_rnn_teste$mlts_lag_15,
                     mlts1_rnn_teste$mlts_lag_16,
                     mlts1_rnn_teste$mlts_lag_17,
                     mlts1_rnn_teste$mlts_lag_18,
                     mlts1_rnn_teste$mlts_lag_19,
                     mlts1_rnn_teste$mlts_lag_20,
                     mlts1_rnn_teste$mlts_lag_21,
                     mlts1_rnn_teste$mlts_lag_22,
                     mlts1_rnn_teste$mlts_lag_23,
                     mlts1_rnn_teste$mlts_lag_24,
                     mlts1_rnn_teste$mlts_lag_25,
                     mlts1_rnn_teste$mlts_lag_26,
                     mlts1_rnn_teste$mlts_lag_27,
                     mlts1_rnn_teste$mlts_lag_28), 
                     dim = c(nrow(mlts1_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast

  #pred1[d]<- as.data.frame(predictr(rnn_btc, X_teste))
  
  bit1_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_btc, X_teste))
  
}

bit1_rnn_ts

prev_desnorm <- linscale(bit1_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bit1_rnn_ts <- data.frame(bit1_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
#VAI PRO TCC


ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2019-01-31"),
  aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bit1_rnn_ts, days >= '2019-01-01'))


cols4 <- c("Bitcoin" = "black", "Modelo RNN" = "blue", "Forecast RNN" = "red")
finalresult_aux <- data.frame(days = seq(as.Date("2018-01-28"),as.Date("2018-01-28") + 336,"day"), finalresult)
head(finalresult_aux)

jpeg("fig8.jpeg", width = 8, height = 4.5, units = c("in"), res = 900)
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2019-01-31")) + 
  theme_few() +
  labs(x = 'Dia', y = 'Preço de fechamento ($)') +
  geom_line(aes(x = days, y = bitcoin2, color = "Bitcoin"))  +
  geom_line(aes(x = days, y = pred, color = "Modelo RNN"), data = finalresult_aux)  +
  geom_line(aes(x = days, y = prev_desnorm.x, color = "Forecast RNN"), data = filter(bit1_rnn_ts, days >= '2019-01-01')) +
  scale_color_manual(name = "", values = cols4) +
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),legend.position="top", legend.justification = 'left', legend.direction = 'horizontal',  text=element_text(size=11,family="Arial"))
dev.off()

finalresult_aux <- data.frame(days = seq(as.Date("2018-01-28"),as.Date("2018-01-28") + 336,"day"), finalresult)

head(finalresult_aux)

#MAPE
base_rnn1 <- data.frame(filter(bit1_rnn_ts, days >= "2019-01-01" & days <= "2019-01-30"), filter(tabela_btc, days >= "2019-01-01" & days <= "2019-01-30"))
forecast::accuracy(base_rnn1$prev_desnorm.x, base_rnn1$bitcoin2.1) 


###########################################################
###########################################################
#RNN 2
###########################################################
###########################################################
bit2_rnn <- tabela_btc %>% filter(days < "2019-07-01" )
tail(bit2_rnn)

minmax <- linscale(bit2_rnn$bitcoin2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bit2_rnn <- data.frame(bit2_rnn, minmax_df)

mlts2_rnn <- mlts_transform(bit2_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts2_rnn)
head(mlts2_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts2_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts2_rnn$y, dim = c(nrow(mlts2_rnn),1))
head(Y)

X <- array(c(mlts2_rnn$mlts_lag_1,
             mlts2_rnn$mlts_lag_2,
             mlts2_rnn$mlts_lag_3,
             mlts2_rnn$mlts_lag_4,
             mlts2_rnn$mlts_lag_5,
             mlts2_rnn$mlts_lag_6,
             mlts2_rnn$mlts_lag_7,
             mlts2_rnn$mlts_lag_8,
             mlts2_rnn$mlts_lag_9,
             mlts2_rnn$mlts_lag_10,
             mlts2_rnn$mlts_lag_11,
             mlts2_rnn$mlts_lag_12,
             mlts2_rnn$mlts_lag_13,
             mlts2_rnn$mlts_lag_14,
             mlts2_rnn$mlts_lag_15,
             mlts2_rnn$mlts_lag_16,
             mlts2_rnn$mlts_lag_17,
             mlts2_rnn$mlts_lag_18,
             mlts2_rnn$mlts_lag_19,
             mlts2_rnn$mlts_lag_20,
             mlts2_rnn$mlts_lag_21,
             mlts2_rnn$mlts_lag_22,
             mlts2_rnn$mlts_lag_23,
             mlts2_rnn$mlts_lag_24,
             mlts2_rnn$mlts_lag_25,
             mlts2_rnn$mlts_lag_26,
             mlts2_rnn$mlts_lag_27,
             mlts2_rnn$mlts_lag_28), 
           dim = c(nrow(mlts2_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_btc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados
finalresult <- data.frame(index = c(1:337),
                          real = bit2_rnn$bitcoin2[28:364],
                          pred = forecast1_treino_desnorm$x)

# plot prediction vs real treino
ggplot(data = finalresult,
       aes(x = index, y = real)) + geom_line() +
  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bit2_rnn)

bit2_rnn_ts <- rbind(
  tail(bit2_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2019-07-01"),
      as.Date("2019-07-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    minmax.x = NA
  )
); rownames(bit2_rnn_ts) <- NULL

bit2_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bit2_rnn_ts)) {
  mlts2_rnn_teste <- mlts_transform(bit2_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts2_rnn_teste <- mlts2_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts2_rnn_teste$mlts_lag_1,
                     mlts2_rnn_teste$mlts_lag_2,
                     mlts2_rnn_teste$mlts_lag_3,
                     mlts2_rnn_teste$mlts_lag_4,
                     mlts2_rnn_teste$mlts_lag_5,
                     mlts2_rnn_teste$mlts_lag_6,
                     mlts2_rnn_teste$mlts_lag_7,
                     mlts2_rnn_teste$mlts_lag_8,
                     mlts2_rnn_teste$mlts_lag_9,
                     mlts2_rnn_teste$mlts_lag_10,
                     mlts2_rnn_teste$mlts_lag_11,
                     mlts2_rnn_teste$mlts_lag_12,
                     mlts2_rnn_teste$mlts_lag_13,
                     mlts2_rnn_teste$mlts_lag_14,
                     mlts2_rnn_teste$mlts_lag_15,
                     mlts2_rnn_teste$mlts_lag_16,
                     mlts2_rnn_teste$mlts_lag_17,
                     mlts2_rnn_teste$mlts_lag_18,
                     mlts2_rnn_teste$mlts_lag_19,
                     mlts2_rnn_teste$mlts_lag_20,
                     mlts2_rnn_teste$mlts_lag_21,
                     mlts2_rnn_teste$mlts_lag_22,
                     mlts2_rnn_teste$mlts_lag_23,
                     mlts2_rnn_teste$mlts_lag_24,
                     mlts2_rnn_teste$mlts_lag_25,
                     mlts2_rnn_teste$mlts_lag_26,
                     mlts2_rnn_teste$mlts_lag_27,
                     mlts2_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts2_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_btc, X_teste))
  
  bit2_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_btc, X_teste))
  
}

bit2_rnn_ts

prev_desnorm <- linscale(bit2_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bit2_rnn_ts <- data.frame(bit2_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2019-07-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bit2_rnn_ts, days >= '2019-07-01'))

#MAPE

base_rnn2 <- data.frame(filter(bit2_rnn_ts, days >= "2019-07-01" & days <= "2019-07-30"), filter(tabela_btc, days >= "2019-07-01" & days <= "2019-07-30"))
forecast::accuracy(base_rnn2$prev_desnorm.x, base_rnn2$bitcoin2.1) 

###########################################################
###########################################################
#RNN 3
###########################################################
###########################################################
bit3_rnn <- tabela_btc %>% filter(days < "2020-01-01" )
tail(bit3_rnn)

minmax <- linscale(bit3_rnn$bitcoin2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bit3_rnn <- data.frame(bit3_rnn, minmax_df)

mlts3_rnn <- mlts_transform(bit3_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts3_rnn)
head(mlts3_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts3_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts3_rnn$y, dim = c(nrow(mlts3_rnn),1))
head(Y)

X <- array(c(mlts3_rnn$mlts_lag_1,
             mlts3_rnn$mlts_lag_2,
             mlts3_rnn$mlts_lag_3,
             mlts3_rnn$mlts_lag_4,
             mlts3_rnn$mlts_lag_5,
             mlts3_rnn$mlts_lag_6,
             mlts3_rnn$mlts_lag_7,
             mlts3_rnn$mlts_lag_8,
             mlts3_rnn$mlts_lag_9,
             mlts3_rnn$mlts_lag_10,
             mlts3_rnn$mlts_lag_11,
             mlts3_rnn$mlts_lag_12,
             mlts3_rnn$mlts_lag_13,
             mlts3_rnn$mlts_lag_14,
             mlts3_rnn$mlts_lag_15,
             mlts3_rnn$mlts_lag_16,
             mlts3_rnn$mlts_lag_17,
             mlts3_rnn$mlts_lag_18,
             mlts3_rnn$mlts_lag_19,
             mlts3_rnn$mlts_lag_20,
             mlts3_rnn$mlts_lag_21,
             mlts3_rnn$mlts_lag_22,
             mlts3_rnn$mlts_lag_23,
             mlts3_rnn$mlts_lag_24,
             mlts3_rnn$mlts_lag_25,
             mlts3_rnn$mlts_lag_26,
             mlts3_rnn$mlts_lag_27,
             mlts3_rnn$mlts_lag_28), 
           dim = c(nrow(mlts3_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_btc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados
finalresult <- data.frame(index = c(1:337),
                          real = bit3_rnn$bitcoin2[28:364],
                          pred = forecast1_treino_desnorm$x)

# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bit3_rnn)

bit3_rnn_ts <- rbind(
  tail(bit3_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2020-01-01"),
      as.Date("2020-01-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    minmax.x = NA
  )
); rownames(bit3_rnn_ts) <- NULL

bit3_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bit3_rnn_ts)) {
  mlts3_rnn_teste <- mlts_transform(bit3_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts3_rnn_teste <- mlts3_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts3_rnn_teste$mlts_lag_1,
                     mlts3_rnn_teste$mlts_lag_2,
                     mlts3_rnn_teste$mlts_lag_3,
                     mlts3_rnn_teste$mlts_lag_4,
                     mlts3_rnn_teste$mlts_lag_5,
                     mlts3_rnn_teste$mlts_lag_6,
                     mlts3_rnn_teste$mlts_lag_7,
                     mlts3_rnn_teste$mlts_lag_8,
                     mlts3_rnn_teste$mlts_lag_9,
                     mlts3_rnn_teste$mlts_lag_10,
                     mlts3_rnn_teste$mlts_lag_11,
                     mlts3_rnn_teste$mlts_lag_12,
                     mlts3_rnn_teste$mlts_lag_13,
                     mlts3_rnn_teste$mlts_lag_14,
                     mlts3_rnn_teste$mlts_lag_15,
                     mlts3_rnn_teste$mlts_lag_16,
                     mlts3_rnn_teste$mlts_lag_17,
                     mlts3_rnn_teste$mlts_lag_18,
                     mlts3_rnn_teste$mlts_lag_19,
                     mlts3_rnn_teste$mlts_lag_20,
                     mlts3_rnn_teste$mlts_lag_21,
                     mlts3_rnn_teste$mlts_lag_22,
                     mlts3_rnn_teste$mlts_lag_23,
                     mlts3_rnn_teste$mlts_lag_24,
                     mlts3_rnn_teste$mlts_lag_25,
                     mlts3_rnn_teste$mlts_lag_26,
                     mlts3_rnn_teste$mlts_lag_27,
                     mlts3_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts3_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_btc, X_teste))
  
  bit3_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_btc, X_teste))
  
}

bit3_rnn_ts

prev_desnorm <- linscale(bit3_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bit3_rnn_ts <- data.frame(bit3_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2020-01-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bit3_rnn_ts, days >= '2020-01-01'))

#MAPE

base_rnn3 <- data.frame(filter(bit3_rnn_ts, days >= "2020-01-01" & days <= "2020-01-30"), filter(tabela_btc, days >= "2020-01-01" & days <= "2020-01-30"))
forecast::accuracy(base_rnn3$prev_desnorm.x, base_rnn3$bitcoin2.1) 

###########################################################
###########################################################
#RNN 4
###########################################################
###########################################################
bit4_rnn <- tabela_btc %>% filter(days < "2020-07-01" )
tail(bit4_rnn)

minmax <- linscale(bit4_rnn$bitcoin2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bit4_rnn <- data.frame(bit4_rnn, minmax_df)

mlts4_rnn <- mlts_transform(bit4_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts4_rnn)
head(mlts4_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts4_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts4_rnn$y, dim = c(nrow(mlts4_rnn),1))
head(Y)

X <- array(c(mlts4_rnn$mlts_lag_1,
             mlts4_rnn$mlts_lag_2,
             mlts4_rnn$mlts_lag_3,
             mlts4_rnn$mlts_lag_4,
             mlts4_rnn$mlts_lag_5,
             mlts4_rnn$mlts_lag_6,
             mlts4_rnn$mlts_lag_7,
             mlts4_rnn$mlts_lag_8,
             mlts4_rnn$mlts_lag_9,
             mlts4_rnn$mlts_lag_10,
             mlts4_rnn$mlts_lag_11,
             mlts4_rnn$mlts_lag_12,
             mlts4_rnn$mlts_lag_13,
             mlts4_rnn$mlts_lag_14,
             mlts4_rnn$mlts_lag_15,
             mlts4_rnn$mlts_lag_16,
             mlts4_rnn$mlts_lag_17,
             mlts4_rnn$mlts_lag_18,
             mlts4_rnn$mlts_lag_19,
             mlts4_rnn$mlts_lag_20,
             mlts4_rnn$mlts_lag_21,
             mlts4_rnn$mlts_lag_22,
             mlts4_rnn$mlts_lag_23,
             mlts4_rnn$mlts_lag_24,
             mlts4_rnn$mlts_lag_25,
             mlts4_rnn$mlts_lag_26,
             mlts4_rnn$mlts_lag_27,
             mlts4_rnn$mlts_lag_28), 
           dim = c(nrow(mlts4_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_btc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bit4_rnn)

bit4_rnn_ts <- rbind(
  tail(bit4_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2020-07-01"),
      as.Date("2020-07-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    minmax.x = NA
  )
); rownames(bit4_rnn_ts) <- NULL

bit4_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bit4_rnn_ts)) {
  mlts4_rnn_teste <- mlts_transform(bit4_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts4_rnn_teste <- mlts4_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts4_rnn_teste$mlts_lag_1,
                     mlts4_rnn_teste$mlts_lag_2,
                     mlts4_rnn_teste$mlts_lag_3,
                     mlts4_rnn_teste$mlts_lag_4,
                     mlts4_rnn_teste$mlts_lag_5,
                     mlts4_rnn_teste$mlts_lag_6,
                     mlts4_rnn_teste$mlts_lag_7,
                     mlts4_rnn_teste$mlts_lag_8,
                     mlts4_rnn_teste$mlts_lag_9,
                     mlts4_rnn_teste$mlts_lag_10,
                     mlts4_rnn_teste$mlts_lag_11,
                     mlts4_rnn_teste$mlts_lag_12,
                     mlts4_rnn_teste$mlts_lag_13,
                     mlts4_rnn_teste$mlts_lag_14,
                     mlts4_rnn_teste$mlts_lag_15,
                     mlts4_rnn_teste$mlts_lag_16,
                     mlts4_rnn_teste$mlts_lag_17,
                     mlts4_rnn_teste$mlts_lag_18,
                     mlts4_rnn_teste$mlts_lag_19,
                     mlts4_rnn_teste$mlts_lag_20,
                     mlts4_rnn_teste$mlts_lag_21,
                     mlts4_rnn_teste$mlts_lag_22,
                     mlts4_rnn_teste$mlts_lag_23,
                     mlts4_rnn_teste$mlts_lag_24,
                     mlts4_rnn_teste$mlts_lag_25,
                     mlts4_rnn_teste$mlts_lag_26,
                     mlts4_rnn_teste$mlts_lag_27,
                     mlts4_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts4_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_btc, X_teste))
  
  bit4_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_btc, X_teste))
  
}

bit4_rnn_ts

prev_desnorm <- linscale(bit4_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bit4_rnn_ts <- data.frame(bit4_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2020-07-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bit4_rnn_ts, days >= '2020-07-01'))

#MAPE

base_rnn4 <- data.frame(filter(bit4_rnn_ts, days >= "2020-07-01" & days <= "2020-07-30"), filter(tabela_btc, days >= "2020-07-01" & days <= "2020-07-30"))
forecast::accuracy(base_rnn4$prev_desnorm.x, base_rnn4$bitcoin2.1) 

###########################################################
###########################################################
#RNN 5
###########################################################
###########################################################
bit5_rnn <- tabela_btc %>% filter(days < "2021-01-01" )
tail(bit5_rnn)

minmax <- linscale(bit5_rnn$bitcoin2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bit5_rnn <- data.frame(bit5_rnn, minmax_df)

mlts5_rnn <- mlts_transform(bit5_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts5_rnn)
head(mlts5_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts5_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts5_rnn$y, dim = c(nrow(mlts5_rnn),1))
head(Y)

X <- array(c(mlts5_rnn$mlts_lag_1,
             mlts5_rnn$mlts_lag_2,
             mlts5_rnn$mlts_lag_3,
             mlts5_rnn$mlts_lag_4,
             mlts5_rnn$mlts_lag_5,
             mlts5_rnn$mlts_lag_6,
             mlts5_rnn$mlts_lag_7,
             mlts5_rnn$mlts_lag_8,
             mlts5_rnn$mlts_lag_9,
             mlts5_rnn$mlts_lag_10,
             mlts5_rnn$mlts_lag_11,
             mlts5_rnn$mlts_lag_12,
             mlts5_rnn$mlts_lag_13,
             mlts5_rnn$mlts_lag_14,
             mlts5_rnn$mlts_lag_15,
             mlts5_rnn$mlts_lag_16,
             mlts5_rnn$mlts_lag_17,
             mlts5_rnn$mlts_lag_18,
             mlts5_rnn$mlts_lag_19,
             mlts5_rnn$mlts_lag_20,
             mlts5_rnn$mlts_lag_21,
             mlts5_rnn$mlts_lag_22,
             mlts5_rnn$mlts_lag_23,
             mlts5_rnn$mlts_lag_24,
             mlts5_rnn$mlts_lag_25,
             mlts5_rnn$mlts_lag_26,
             mlts5_rnn$mlts_lag_27,
             mlts5_rnn$mlts_lag_28), 
           dim = c(nrow(mlts5_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_btc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bit5_rnn)

bit5_rnn_ts <- rbind(
  tail(bit5_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-01-01"),
      as.Date("2021-01-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    minmax.x = NA
  )
); rownames(bit5_rnn_ts) <- NULL

bit5_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bit5_rnn_ts)) {
  mlts5_rnn_teste <- mlts_transform(bit5_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts5_rnn_teste <- mlts5_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts5_rnn_teste$mlts_lag_1,
                     mlts5_rnn_teste$mlts_lag_2,
                     mlts5_rnn_teste$mlts_lag_3,
                     mlts5_rnn_teste$mlts_lag_4,
                     mlts5_rnn_teste$mlts_lag_5,
                     mlts5_rnn_teste$mlts_lag_6,
                     mlts5_rnn_teste$mlts_lag_7,
                     mlts5_rnn_teste$mlts_lag_8,
                     mlts5_rnn_teste$mlts_lag_9,
                     mlts5_rnn_teste$mlts_lag_10,
                     mlts5_rnn_teste$mlts_lag_11,
                     mlts5_rnn_teste$mlts_lag_12,
                     mlts5_rnn_teste$mlts_lag_13,
                     mlts5_rnn_teste$mlts_lag_14,
                     mlts5_rnn_teste$mlts_lag_15,
                     mlts5_rnn_teste$mlts_lag_16,
                     mlts5_rnn_teste$mlts_lag_17,
                     mlts5_rnn_teste$mlts_lag_18,
                     mlts5_rnn_teste$mlts_lag_19,
                     mlts5_rnn_teste$mlts_lag_20,
                     mlts5_rnn_teste$mlts_lag_21,
                     mlts5_rnn_teste$mlts_lag_22,
                     mlts5_rnn_teste$mlts_lag_23,
                     mlts5_rnn_teste$mlts_lag_24,
                     mlts5_rnn_teste$mlts_lag_25,
                     mlts5_rnn_teste$mlts_lag_26,
                     mlts5_rnn_teste$mlts_lag_27,
                     mlts5_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts5_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_btc, X_teste))
  
  bit5_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_btc, X_teste))
  
}

bit5_rnn_ts

prev_desnorm <- linscale(bit5_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bit5_rnn_ts <- data.frame(bit5_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2021-01-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bit5_rnn_ts, days >= '2021-01-01'))

#MAPE

base_rnn5 <- data.frame(filter(bit5_rnn_ts, days >= "2021-01-01" & days <= "2021-01-30"), filter(tabela_btc, days >= "2021-01-01" & days <= "2021-01-30"))
forecast::accuracy(base_rnn5$prev_desnorm.x, base_rnn5$bitcoin2.1) 

###########################################################
###########################################################
#RNN 6
###########################################################
###########################################################
bit6_rnn <- tabela_btc %>% filter(days < "2021-07-01" )
tail(bit6_rnn)

minmax <- linscale(bit6_rnn$bitcoin2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bit6_rnn <- data.frame(bit6_rnn, minmax_df)

mlts6_rnn <- mlts_transform(bit6_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts6_rnn)
head(mlts6_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts6_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts6_rnn$y, dim = c(nrow(mlts6_rnn),1))
head(Y)

X <- array(c(mlts6_rnn$mlts_lag_1,
             mlts6_rnn$mlts_lag_2,
             mlts6_rnn$mlts_lag_3,
             mlts6_rnn$mlts_lag_4,
             mlts6_rnn$mlts_lag_5,
             mlts6_rnn$mlts_lag_6,
             mlts6_rnn$mlts_lag_7,
             mlts6_rnn$mlts_lag_8,
             mlts6_rnn$mlts_lag_9,
             mlts6_rnn$mlts_lag_10,
             mlts6_rnn$mlts_lag_11,
             mlts6_rnn$mlts_lag_12,
             mlts6_rnn$mlts_lag_13,
             mlts6_rnn$mlts_lag_14,
             mlts6_rnn$mlts_lag_15,
             mlts6_rnn$mlts_lag_16,
             mlts6_rnn$mlts_lag_17,
             mlts6_rnn$mlts_lag_18,
             mlts6_rnn$mlts_lag_19,
             mlts6_rnn$mlts_lag_20,
             mlts6_rnn$mlts_lag_21,
             mlts6_rnn$mlts_lag_22,
             mlts6_rnn$mlts_lag_23,
             mlts6_rnn$mlts_lag_24,
             mlts6_rnn$mlts_lag_25,
             mlts6_rnn$mlts_lag_26,
             mlts6_rnn$mlts_lag_27,
             mlts6_rnn$mlts_lag_28), 
           dim = c(nrow(mlts6_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_btc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bit6_rnn)

bit6_rnn_ts <- rbind(
  tail(bit6_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    minmax.x = NA
  )
); rownames(bit6_rnn_ts) <- NULL

bit6_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bit6_rnn_ts)) {
  mlts6_rnn_teste <- mlts_transform(bit6_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts6_rnn_teste <- mlts6_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts6_rnn_teste$mlts_lag_1,
                     mlts6_rnn_teste$mlts_lag_2,
                     mlts6_rnn_teste$mlts_lag_3,
                     mlts6_rnn_teste$mlts_lag_4,
                     mlts6_rnn_teste$mlts_lag_5,
                     mlts6_rnn_teste$mlts_lag_6,
                     mlts6_rnn_teste$mlts_lag_7,
                     mlts6_rnn_teste$mlts_lag_8,
                     mlts6_rnn_teste$mlts_lag_9,
                     mlts6_rnn_teste$mlts_lag_10,
                     mlts6_rnn_teste$mlts_lag_11,
                     mlts6_rnn_teste$mlts_lag_12,
                     mlts6_rnn_teste$mlts_lag_13,
                     mlts6_rnn_teste$mlts_lag_14,
                     mlts6_rnn_teste$mlts_lag_15,
                     mlts6_rnn_teste$mlts_lag_16,
                     mlts6_rnn_teste$mlts_lag_17,
                     mlts6_rnn_teste$mlts_lag_18,
                     mlts6_rnn_teste$mlts_lag_19,
                     mlts6_rnn_teste$mlts_lag_20,
                     mlts6_rnn_teste$mlts_lag_21,
                     mlts6_rnn_teste$mlts_lag_22,
                     mlts6_rnn_teste$mlts_lag_23,
                     mlts6_rnn_teste$mlts_lag_24,
                     mlts6_rnn_teste$mlts_lag_25,
                     mlts6_rnn_teste$mlts_lag_26,
                     mlts6_rnn_teste$mlts_lag_27,
                     mlts6_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts6_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_btc, X_teste))
  
  bit6_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_btc, X_teste))
  
}

bit6_rnn_ts

prev_desnorm <- linscale(bit6_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bit6_rnn_ts <- data.frame(bit6_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2021-07-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bit6_rnn_ts, days >= '2021-07-01'))

#MAPE

base_rnn6 <- data.frame(filter(bit6_rnn_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_btc, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base_rnn6$prev_desnorm.x, base_rnn6$bitcoin2.1) 

###########################################################
###########################################################
#RNN 7
###########################################################
###########################################################
bit7_rnn <- tabela_btc %>% filter(days < "2021-07-01" )
tail(bit7_rnn)

minmax <- linscale(bit7_rnn$bitcoin2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bit7_rnn <- data.frame(bit7_rnn, minmax_df)

mlts7_rnn <- mlts_transform(bit7_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts7_rnn)
head(mlts7_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts7_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts7_rnn$y, dim = c(nrow(mlts7_rnn),1))
head(Y)

X <- array(c(mlts7_rnn$mlts_lag_1,
             mlts7_rnn$mlts_lag_2,
             mlts7_rnn$mlts_lag_3,
             mlts7_rnn$mlts_lag_4,
             mlts7_rnn$mlts_lag_5,
             mlts7_rnn$mlts_lag_6,
             mlts7_rnn$mlts_lag_7,
             mlts7_rnn$mlts_lag_8,
             mlts7_rnn$mlts_lag_9,
             mlts7_rnn$mlts_lag_10,
             mlts7_rnn$mlts_lag_11,
             mlts7_rnn$mlts_lag_12,
             mlts7_rnn$mlts_lag_13,
             mlts7_rnn$mlts_lag_14,
             mlts7_rnn$mlts_lag_15,
             mlts7_rnn$mlts_lag_16,
             mlts7_rnn$mlts_lag_17,
             mlts7_rnn$mlts_lag_18,
             mlts7_rnn$mlts_lag_19,
             mlts7_rnn$mlts_lag_20,
             mlts7_rnn$mlts_lag_21,
             mlts7_rnn$mlts_lag_22,
             mlts7_rnn$mlts_lag_23,
             mlts7_rnn$mlts_lag_24,
             mlts7_rnn$mlts_lag_25,
             mlts7_rnn$mlts_lag_26,
             mlts7_rnn$mlts_lag_27,
             mlts7_rnn$mlts_lag_28), 
           dim = c(nrow(mlts7_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_btc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bit7_rnn)

bit7_rnn_ts <- rbind(
  tail(bit7_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    bitcoin2 = NA,
    minmax.x = NA
  )
); rownames(bit7_rnn_ts) <- NULL

bit7_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bit7_rnn_ts)) {
  mlts7_rnn_teste <- mlts_transform(bit7_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts7_rnn_teste <- mlts7_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts7_rnn_teste$mlts_lag_1,
                     mlts7_rnn_teste$mlts_lag_2,
                     mlts7_rnn_teste$mlts_lag_3,
                     mlts7_rnn_teste$mlts_lag_4,
                     mlts7_rnn_teste$mlts_lag_5,
                     mlts7_rnn_teste$mlts_lag_6,
                     mlts7_rnn_teste$mlts_lag_7,
                     mlts7_rnn_teste$mlts_lag_8,
                     mlts7_rnn_teste$mlts_lag_9,
                     mlts7_rnn_teste$mlts_lag_10,
                     mlts7_rnn_teste$mlts_lag_11,
                     mlts7_rnn_teste$mlts_lag_12,
                     mlts7_rnn_teste$mlts_lag_13,
                     mlts7_rnn_teste$mlts_lag_14,
                     mlts7_rnn_teste$mlts_lag_15,
                     mlts7_rnn_teste$mlts_lag_16,
                     mlts7_rnn_teste$mlts_lag_17,
                     mlts7_rnn_teste$mlts_lag_18,
                     mlts7_rnn_teste$mlts_lag_19,
                     mlts7_rnn_teste$mlts_lag_20,
                     mlts7_rnn_teste$mlts_lag_21,
                     mlts7_rnn_teste$mlts_lag_22,
                     mlts7_rnn_teste$mlts_lag_23,
                     mlts7_rnn_teste$mlts_lag_24,
                     mlts7_rnn_teste$mlts_lag_25,
                     mlts7_rnn_teste$mlts_lag_26,
                     mlts7_rnn_teste$mlts_lag_27,
                     mlts7_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts7_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_btc, X_teste))
  
  bit7_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_btc, X_teste))
  
}

bit7_rnn_ts

prev_desnorm <- linscale(bit7_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bit7_rnn_ts <- data.frame(bit7_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_btc, days >= "2018-01-01" & days <= "2021-07-31"),
       aes(x = days, y = bitcoin2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bit7_rnn_ts, days >= '2021-07-01'))

#MAPE

base_rnn7 <- data.frame(filter(bit7_rnn_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_btc, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base_rnn7$prev_desnorm.x, base_rnn7$bitcoin2.1) 




#juntando todos os mapes
#accuracy
btc_rnn_ac1 <- forecast::accuracy(base_rnn1$prev_desnorm.x, base_rnn1$bitcoin2.1)
btc_rnn_ac2 <- forecast::accuracy(base_rnn2$prev_desnorm.x, base_rnn2$bitcoin2.1)
btc_rnn_ac3 <- forecast::accuracy(base_rnn3$prev_desnorm.x, base_rnn3$bitcoin2.1)
btc_rnn_ac4 <- forecast::accuracy(base_rnn4$prev_desnorm.x, base_rnn4$bitcoin2.1)
btc_rnn_ac5 <- forecast::accuracy(base_rnn5$prev_desnorm.x, base_rnn5$bitcoin2.1) 
btc_rnn_ac6 <- forecast::accuracy(base_rnn6$prev_desnorm.x, base_rnn6$bitcoin2.1)
btc_rnn_ac7 <- forecast::accuracy(base_rnn7$prev_desnorm.x, base_rnn7$bitcoin2.1)

btc_rnn_accuracy_mean <- bind_rows(btc_rnn_ac1[1,],btc_rnn_ac2[1,],btc_rnn_ac3[1,],btc_rnn_ac4[1,],btc_rnn_ac5[1,],btc_rnn_ac6[1,],btc_rnn_ac7[1,])
btc_rnn_accuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), btc_rnn_accuracy_mean)

btc_rnn_accuracy_mean 
eth_rnn_accuracy_mean
bnc_rnn_accuracy_mean

#VAI PRO GRAFICO
ggplot(data = btc_rnn_accuracy_mean) + 
  theme_light() +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'black') 
  #geom_line(mapping = aes(x = index, y = MAPE), color = 'blue', data = eth_rnn_accuracy_mean) +
  #geom_line(mapping = aes(x = index, y = MAPE), color = 'red', data = bnc_rnn_accuracy_mean)

###########################################################















#bit
#espaco





















###########################################################

###########################################################
###########################################################
# MODELO RNN ETHERIUM
###########################################################

# RNN1 
eth1_rnn <- tabela_eth %>% filter(days < "2019-01-01" )
tail(eth1_rnn)

minmax <- linscale(eth1_rnn$etherium2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

eth1_rnn <- data.frame(eth1_rnn, minmax_df)

mlts1_rnn <- mlts_transform(eth1_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts1_rnn)
head(mlts1_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts1_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts1_rnn$y, dim = c(nrow(mlts1_rnn),1))
head(Y)

X <- array(c(mlts1_rnn$mlts_lag_1,
             mlts1_rnn$mlts_lag_2,
             mlts1_rnn$mlts_lag_3,
             mlts1_rnn$mlts_lag_4,
             mlts1_rnn$mlts_lag_5,
             mlts1_rnn$mlts_lag_6,
             mlts1_rnn$mlts_lag_7,
             mlts1_rnn$mlts_lag_8,
             mlts1_rnn$mlts_lag_9,
             mlts1_rnn$mlts_lag_10,
             mlts1_rnn$mlts_lag_11,
             mlts1_rnn$mlts_lag_12,
             mlts1_rnn$mlts_lag_13,
             mlts1_rnn$mlts_lag_14,
             mlts1_rnn$mlts_lag_15,
             mlts1_rnn$mlts_lag_16,
             mlts1_rnn$mlts_lag_17,
             mlts1_rnn$mlts_lag_18,
             mlts1_rnn$mlts_lag_19,
             mlts1_rnn$mlts_lag_20,
             mlts1_rnn$mlts_lag_21,
             mlts1_rnn$mlts_lag_22,
             mlts1_rnn$mlts_lag_23,
             mlts1_rnn$mlts_lag_24,
             mlts1_rnn$mlts_lag_25,
             mlts1_rnn$mlts_lag_26,
             mlts1_rnn$mlts_lag_27,
             mlts1_rnn$mlts_lag_28), 
           dim = c(nrow(mlts1_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_eth <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_eth, "error")),
           Error = attr(rnn_eth, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_eth, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)


# plot prediction vs real treino
ggplot(data = finalresult,
       aes(x = index, y = real)) + geom_line() +
  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)


###############################################################
#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(eth1_rnn)

eth1_rnn_ts <- rbind(
  tail(eth1_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2019-01-01"),
      as.Date("2019-01-01") + 30,
      "day"
    ),
    etherium2 = NA,
    minmax.x = NA
  )
); rownames(eth1_rnn_ts) <- NULL

eth1_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(eth1_rnn_ts)) {
  mlts1_rnn_teste <- mlts_transform(eth1_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts1_rnn_teste <- mlts1_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts1_rnn_teste$mlts_lag_1,
                     mlts1_rnn_teste$mlts_lag_2,
                     mlts1_rnn_teste$mlts_lag_3,
                     mlts1_rnn_teste$mlts_lag_4,
                     mlts1_rnn_teste$mlts_lag_5,
                     mlts1_rnn_teste$mlts_lag_6,
                     mlts1_rnn_teste$mlts_lag_7,
                     mlts1_rnn_teste$mlts_lag_8,
                     mlts1_rnn_teste$mlts_lag_9,
                     mlts1_rnn_teste$mlts_lag_10,
                     mlts1_rnn_teste$mlts_lag_11,
                     mlts1_rnn_teste$mlts_lag_12,
                     mlts1_rnn_teste$mlts_lag_13,
                     mlts1_rnn_teste$mlts_lag_14,
                     mlts1_rnn_teste$mlts_lag_15,
                     mlts1_rnn_teste$mlts_lag_16,
                     mlts1_rnn_teste$mlts_lag_17,
                     mlts1_rnn_teste$mlts_lag_18,
                     mlts1_rnn_teste$mlts_lag_19,
                     mlts1_rnn_teste$mlts_lag_20,
                     mlts1_rnn_teste$mlts_lag_21,
                     mlts1_rnn_teste$mlts_lag_22,
                     mlts1_rnn_teste$mlts_lag_23,
                     mlts1_rnn_teste$mlts_lag_24,
                     mlts1_rnn_teste$mlts_lag_25,
                     mlts1_rnn_teste$mlts_lag_26,
                     mlts1_rnn_teste$mlts_lag_27,
                     mlts1_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts1_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_eth, X_teste))
  
  eth1_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_eth, X_teste))
  
}

eth1_rnn_ts

prev_desnorm <- linscale(eth1_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

eth1_rnn_ts <- data.frame(eth1_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2019-01-31"),
  aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(eth1_rnn_ts, days >= '2019-01-01'))

#MAPE

base_rnn_eth1 <- data.frame(filter(eth1_rnn_ts, days >= "2019-01-01" & days <= "2019-01-30"), filter(tabela_eth, days >= "2019-01-01" & days <= "2019-01-30"))
forecast::accuracy(base_rnn_eth1$prev_desnorm.x, base_rnn_eth1$etherium2.1) 


###########################################################
###########################################################
#RNN 2
###########################################################
###########################################################
eth2_rnn <- tabela_eth %>% filter(days < "2019-07-01" )
tail(eth2_rnn)

minmax <- linscale(eth2_rnn$etherium2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

eth2_rnn <- data.frame(eth2_rnn, minmax_df)

mlts2_rnn <- mlts_transform(eth2_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts2_rnn)
head(mlts2_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts2_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts2_rnn$y, dim = c(nrow(mlts2_rnn),1))
head(Y)

X <- array(c(mlts2_rnn$mlts_lag_1,
             mlts2_rnn$mlts_lag_2,
             mlts2_rnn$mlts_lag_3,
             mlts2_rnn$mlts_lag_4,
             mlts2_rnn$mlts_lag_5,
             mlts2_rnn$mlts_lag_6,
             mlts2_rnn$mlts_lag_7,
             mlts2_rnn$mlts_lag_8,
             mlts2_rnn$mlts_lag_9,
             mlts2_rnn$mlts_lag_10,
             mlts2_rnn$mlts_lag_11,
             mlts2_rnn$mlts_lag_12,
             mlts2_rnn$mlts_lag_13,
             mlts2_rnn$mlts_lag_14,
             mlts2_rnn$mlts_lag_15,
             mlts2_rnn$mlts_lag_16,
             mlts2_rnn$mlts_lag_17,
             mlts2_rnn$mlts_lag_18,
             mlts2_rnn$mlts_lag_19,
             mlts2_rnn$mlts_lag_20,
             mlts2_rnn$mlts_lag_21,
             mlts2_rnn$mlts_lag_22,
             mlts2_rnn$mlts_lag_23,
             mlts2_rnn$mlts_lag_24,
             mlts2_rnn$mlts_lag_25,
             mlts2_rnn$mlts_lag_26,
             mlts2_rnn$mlts_lag_27,
             mlts2_rnn$mlts_lag_28), 
           dim = c(nrow(mlts2_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_eth <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_eth, "error")),
           Error = attr(rnn_eth, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_eth, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados
finalresult <- data.frame(index = c(1:337),
                          real = eth2_rnn$etherium2[28:364],
                          pred = forecast1_treino_desnorm$x)

# plot prediction vs real treino
ggplot(data = finalresult,
       aes(x = index, y = real)) + geom_line() +
  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(eth2_rnn)

eth2_rnn_ts <- rbind(
  tail(eth2_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2019-07-01"),
      as.Date("2019-07-01") + 30,
      "day"
    ),
    etherium2 = NA,
    minmax.x = NA
  )
); rownames(eth2_rnn_ts) <- NULL

eth2_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(eth2_rnn_ts)) {
  mlts2_rnn_teste <- mlts_transform(eth2_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts2_rnn_teste <- mlts2_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts2_rnn_teste$mlts_lag_1,
                     mlts2_rnn_teste$mlts_lag_2,
                     mlts2_rnn_teste$mlts_lag_3,
                     mlts2_rnn_teste$mlts_lag_4,
                     mlts2_rnn_teste$mlts_lag_5,
                     mlts2_rnn_teste$mlts_lag_6,
                     mlts2_rnn_teste$mlts_lag_7,
                     mlts2_rnn_teste$mlts_lag_8,
                     mlts2_rnn_teste$mlts_lag_9,
                     mlts2_rnn_teste$mlts_lag_10,
                     mlts2_rnn_teste$mlts_lag_11,
                     mlts2_rnn_teste$mlts_lag_12,
                     mlts2_rnn_teste$mlts_lag_13,
                     mlts2_rnn_teste$mlts_lag_14,
                     mlts2_rnn_teste$mlts_lag_15,
                     mlts2_rnn_teste$mlts_lag_16,
                     mlts2_rnn_teste$mlts_lag_17,
                     mlts2_rnn_teste$mlts_lag_18,
                     mlts2_rnn_teste$mlts_lag_19,
                     mlts2_rnn_teste$mlts_lag_20,
                     mlts2_rnn_teste$mlts_lag_21,
                     mlts2_rnn_teste$mlts_lag_22,
                     mlts2_rnn_teste$mlts_lag_23,
                     mlts2_rnn_teste$mlts_lag_24,
                     mlts2_rnn_teste$mlts_lag_25,
                     mlts2_rnn_teste$mlts_lag_26,
                     mlts2_rnn_teste$mlts_lag_27,
                     mlts2_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts2_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_eth, X_teste))
  
  eth2_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_eth, X_teste))
  
}

eth2_rnn_ts

prev_desnorm <- linscale(eth2_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

eth2_rnn_ts <- data.frame(eth2_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2019-07-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(eth2_rnn_ts, days >= '2019-07-01'))

#MAPE

base_rnn_eth2 <- data.frame(filter(eth2_rnn_ts, days >= "2019-07-01" & days <= "2019-07-30"), filter(tabela_eth, days >= "2019-07-01" & days <= "2019-07-30"))
forecast::accuracy(base_rnn_eth2$prev_desnorm.x, base_rnn_eth2$etherium2.1) 

###########################################################
###########################################################
#RNN 3
###########################################################
###########################################################
eth3_rnn <- tabela_eth %>% filter(days < "2020-01-01" )
tail(eth3_rnn)

minmax <- linscale(eth3_rnn$etherium2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

eth3_rnn <- data.frame(eth3_rnn, minmax_df)

mlts3_rnn <- mlts_transform(eth3_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts3_rnn)
head(mlts3_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts3_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts3_rnn$y, dim = c(nrow(mlts3_rnn),1))
head(Y)

X <- array(c(mlts3_rnn$mlts_lag_1,
             mlts3_rnn$mlts_lag_2,
             mlts3_rnn$mlts_lag_3,
             mlts3_rnn$mlts_lag_4,
             mlts3_rnn$mlts_lag_5,
             mlts3_rnn$mlts_lag_6,
             mlts3_rnn$mlts_lag_7,
             mlts3_rnn$mlts_lag_8,
             mlts3_rnn$mlts_lag_9,
             mlts3_rnn$mlts_lag_10,
             mlts3_rnn$mlts_lag_11,
             mlts3_rnn$mlts_lag_12,
             mlts3_rnn$mlts_lag_13,
             mlts3_rnn$mlts_lag_14,
             mlts3_rnn$mlts_lag_15,
             mlts3_rnn$mlts_lag_16,
             mlts3_rnn$mlts_lag_17,
             mlts3_rnn$mlts_lag_18,
             mlts3_rnn$mlts_lag_19,
             mlts3_rnn$mlts_lag_20,
             mlts3_rnn$mlts_lag_21,
             mlts3_rnn$mlts_lag_22,
             mlts3_rnn$mlts_lag_23,
             mlts3_rnn$mlts_lag_24,
             mlts3_rnn$mlts_lag_25,
             mlts3_rnn$mlts_lag_26,
             mlts3_rnn$mlts_lag_27,
             mlts3_rnn$mlts_lag_28), 
           dim = c(nrow(mlts3_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_eth <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_eth, "error")),
           Error = attr(rnn_eth, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_eth, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados
finalresult <- data.frame(index = c(1:337),
                          real = eth3_rnn$etherium2[28:364],
                          pred = forecast1_treino_desnorm$x)

# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(eth3_rnn)

eth3_rnn_ts <- rbind(
  tail(eth3_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2020-01-01"),
      as.Date("2020-01-01") + 30,
      "day"
    ),
    etherium2 = NA,
    minmax.x = NA
  )
); rownames(eth3_rnn_ts) <- NULL

eth3_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(eth3_rnn_ts)) {
  mlts3_rnn_teste <- mlts_transform(eth3_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts3_rnn_teste <- mlts3_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts3_rnn_teste$mlts_lag_1,
                     mlts3_rnn_teste$mlts_lag_2,
                     mlts3_rnn_teste$mlts_lag_3,
                     mlts3_rnn_teste$mlts_lag_4,
                     mlts3_rnn_teste$mlts_lag_5,
                     mlts3_rnn_teste$mlts_lag_6,
                     mlts3_rnn_teste$mlts_lag_7,
                     mlts3_rnn_teste$mlts_lag_8,
                     mlts3_rnn_teste$mlts_lag_9,
                     mlts3_rnn_teste$mlts_lag_10,
                     mlts3_rnn_teste$mlts_lag_11,
                     mlts3_rnn_teste$mlts_lag_12,
                     mlts3_rnn_teste$mlts_lag_13,
                     mlts3_rnn_teste$mlts_lag_14,
                     mlts3_rnn_teste$mlts_lag_15,
                     mlts3_rnn_teste$mlts_lag_16,
                     mlts3_rnn_teste$mlts_lag_17,
                     mlts3_rnn_teste$mlts_lag_18,
                     mlts3_rnn_teste$mlts_lag_19,
                     mlts3_rnn_teste$mlts_lag_20,
                     mlts3_rnn_teste$mlts_lag_21,
                     mlts3_rnn_teste$mlts_lag_22,
                     mlts3_rnn_teste$mlts_lag_23,
                     mlts3_rnn_teste$mlts_lag_24,
                     mlts3_rnn_teste$mlts_lag_25,
                     mlts3_rnn_teste$mlts_lag_26,
                     mlts3_rnn_teste$mlts_lag_27,
                     mlts3_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts3_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_eth, X_teste))
  
  eth3_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_eth, X_teste))
  
}

eth3_rnn_ts

prev_desnorm <- linscale(eth3_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

eth3_rnn_ts <- data.frame(eth3_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2020-01-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(eth3_rnn_ts, days >= '2020-01-01'))

#MAPE

base_rnn_eth3 <- data.frame(filter(eth3_rnn_ts, days >= "2020-01-01" & days <= "2020-01-30"), filter(tabela_eth, days >= "2020-01-01" & days <= "2020-01-30"))
forecast::accuracy(base_rnn_eth3$prev_desnorm.x, base_rnn_eth3$etherium2.1) 

###########################################################
###########################################################
#RNN 4
###########################################################
###########################################################
eth4_rnn <- tabela_eth %>% filter(days < "2020-07-01" )
tail(eth4_rnn)

minmax <- linscale(eth4_rnn$etherium2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

eth4_rnn <- data.frame(eth4_rnn, minmax_df)

mlts4_rnn <- mlts_transform(eth4_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts4_rnn)
head(mlts4_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts4_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts4_rnn$y, dim = c(nrow(mlts4_rnn),1))
head(Y)

X <- array(c(mlts4_rnn$mlts_lag_1,
             mlts4_rnn$mlts_lag_2,
             mlts4_rnn$mlts_lag_3,
             mlts4_rnn$mlts_lag_4,
             mlts4_rnn$mlts_lag_5,
             mlts4_rnn$mlts_lag_6,
             mlts4_rnn$mlts_lag_7,
             mlts4_rnn$mlts_lag_8,
             mlts4_rnn$mlts_lag_9,
             mlts4_rnn$mlts_lag_10,
             mlts4_rnn$mlts_lag_11,
             mlts4_rnn$mlts_lag_12,
             mlts4_rnn$mlts_lag_13,
             mlts4_rnn$mlts_lag_14,
             mlts4_rnn$mlts_lag_15,
             mlts4_rnn$mlts_lag_16,
             mlts4_rnn$mlts_lag_17,
             mlts4_rnn$mlts_lag_18,
             mlts4_rnn$mlts_lag_19,
             mlts4_rnn$mlts_lag_20,
             mlts4_rnn$mlts_lag_21,
             mlts4_rnn$mlts_lag_22,
             mlts4_rnn$mlts_lag_23,
             mlts4_rnn$mlts_lag_24,
             mlts4_rnn$mlts_lag_25,
             mlts4_rnn$mlts_lag_26,
             mlts4_rnn$mlts_lag_27,
             mlts4_rnn$mlts_lag_28), 
           dim = c(nrow(mlts4_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_eth <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_eth, "error")),
           Error = attr(rnn_eth, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_eth, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(eth4_rnn)

eth4_rnn_ts <- rbind(
  tail(eth4_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2020-07-01"),
      as.Date("2020-07-01") + 30,
      "day"
    ),
    etherium2 = NA,
    minmax.x = NA
  )
); rownames(eth4_rnn_ts) <- NULL

eth4_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(eth4_rnn_ts)) {
  mlts4_rnn_teste <- mlts_transform(eth4_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts4_rnn_teste <- mlts4_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts4_rnn_teste$mlts_lag_1,
                     mlts4_rnn_teste$mlts_lag_2,
                     mlts4_rnn_teste$mlts_lag_3,
                     mlts4_rnn_teste$mlts_lag_4,
                     mlts4_rnn_teste$mlts_lag_5,
                     mlts4_rnn_teste$mlts_lag_6,
                     mlts4_rnn_teste$mlts_lag_7,
                     mlts4_rnn_teste$mlts_lag_8,
                     mlts4_rnn_teste$mlts_lag_9,
                     mlts4_rnn_teste$mlts_lag_10,
                     mlts4_rnn_teste$mlts_lag_11,
                     mlts4_rnn_teste$mlts_lag_12,
                     mlts4_rnn_teste$mlts_lag_13,
                     mlts4_rnn_teste$mlts_lag_14,
                     mlts4_rnn_teste$mlts_lag_15,
                     mlts4_rnn_teste$mlts_lag_16,
                     mlts4_rnn_teste$mlts_lag_17,
                     mlts4_rnn_teste$mlts_lag_18,
                     mlts4_rnn_teste$mlts_lag_19,
                     mlts4_rnn_teste$mlts_lag_20,
                     mlts4_rnn_teste$mlts_lag_21,
                     mlts4_rnn_teste$mlts_lag_22,
                     mlts4_rnn_teste$mlts_lag_23,
                     mlts4_rnn_teste$mlts_lag_24,
                     mlts4_rnn_teste$mlts_lag_25,
                     mlts4_rnn_teste$mlts_lag_26,
                     mlts4_rnn_teste$mlts_lag_27,
                     mlts4_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts4_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_eth, X_teste))
  
  eth4_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_eth, X_teste))
  
}

eth4_rnn_ts

prev_desnorm <- linscale(eth4_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

eth4_rnn_ts <- data.frame(eth4_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2020-07-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(eth4_rnn_ts, days >= '2020-07-01'))

#MAPE

base_rnn_eth4 <- data.frame(filter(eth4_rnn_ts, days >= "2020-07-01" & days <= "2020-07-30"), filter(tabela_eth, days >= "2020-07-01" & days <= "2020-07-30"))
forecast::accuracy(base_rnn_eth4$prev_desnorm.x, base_rnn_eth4$etherium2.1) 

###########################################################
###########################################################
#RNN 5
###########################################################
###########################################################
eth5_rnn <- tabela_eth %>% filter(days < "2021-01-01" )
tail(eth5_rnn)

minmax <- linscale(eth5_rnn$etherium2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

eth5_rnn <- data.frame(eth5_rnn, minmax_df)

mlts5_rnn <- mlts_transform(eth5_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts5_rnn)
head(mlts5_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts5_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts5_rnn$y, dim = c(nrow(mlts5_rnn),1))
head(Y)

X <- array(c(mlts5_rnn$mlts_lag_1,
             mlts5_rnn$mlts_lag_2,
             mlts5_rnn$mlts_lag_3,
             mlts5_rnn$mlts_lag_4,
             mlts5_rnn$mlts_lag_5,
             mlts5_rnn$mlts_lag_6,
             mlts5_rnn$mlts_lag_7,
             mlts5_rnn$mlts_lag_8,
             mlts5_rnn$mlts_lag_9,
             mlts5_rnn$mlts_lag_10,
             mlts5_rnn$mlts_lag_11,
             mlts5_rnn$mlts_lag_12,
             mlts5_rnn$mlts_lag_13,
             mlts5_rnn$mlts_lag_14,
             mlts5_rnn$mlts_lag_15,
             mlts5_rnn$mlts_lag_16,
             mlts5_rnn$mlts_lag_17,
             mlts5_rnn$mlts_lag_18,
             mlts5_rnn$mlts_lag_19,
             mlts5_rnn$mlts_lag_20,
             mlts5_rnn$mlts_lag_21,
             mlts5_rnn$mlts_lag_22,
             mlts5_rnn$mlts_lag_23,
             mlts5_rnn$mlts_lag_24,
             mlts5_rnn$mlts_lag_25,
             mlts5_rnn$mlts_lag_26,
             mlts5_rnn$mlts_lag_27,
             mlts5_rnn$mlts_lag_28), 
           dim = c(nrow(mlts5_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_eth <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_eth, "error")),
           Error = attr(rnn_eth, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_eth, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(eth5_rnn)

eth5_rnn_ts <- rbind(
  tail(eth5_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-01-01"),
      as.Date("2021-01-01") + 30,
      "day"
    ),
    etherium2 = NA,
    minmax.x = NA
  )
); rownames(eth5_rnn_ts) <- NULL

eth5_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(eth5_rnn_ts)) {
  mlts5_rnn_teste <- mlts_transform(eth5_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts5_rnn_teste <- mlts5_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts5_rnn_teste$mlts_lag_1,
                     mlts5_rnn_teste$mlts_lag_2,
                     mlts5_rnn_teste$mlts_lag_3,
                     mlts5_rnn_teste$mlts_lag_4,
                     mlts5_rnn_teste$mlts_lag_5,
                     mlts5_rnn_teste$mlts_lag_6,
                     mlts5_rnn_teste$mlts_lag_7,
                     mlts5_rnn_teste$mlts_lag_8,
                     mlts5_rnn_teste$mlts_lag_9,
                     mlts5_rnn_teste$mlts_lag_10,
                     mlts5_rnn_teste$mlts_lag_11,
                     mlts5_rnn_teste$mlts_lag_12,
                     mlts5_rnn_teste$mlts_lag_13,
                     mlts5_rnn_teste$mlts_lag_14,
                     mlts5_rnn_teste$mlts_lag_15,
                     mlts5_rnn_teste$mlts_lag_16,
                     mlts5_rnn_teste$mlts_lag_17,
                     mlts5_rnn_teste$mlts_lag_18,
                     mlts5_rnn_teste$mlts_lag_19,
                     mlts5_rnn_teste$mlts_lag_20,
                     mlts5_rnn_teste$mlts_lag_21,
                     mlts5_rnn_teste$mlts_lag_22,
                     mlts5_rnn_teste$mlts_lag_23,
                     mlts5_rnn_teste$mlts_lag_24,
                     mlts5_rnn_teste$mlts_lag_25,
                     mlts5_rnn_teste$mlts_lag_26,
                     mlts5_rnn_teste$mlts_lag_27,
                     mlts5_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts5_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_eth, X_teste))
  
  eth5_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_eth, X_teste))
  
}

eth5_rnn_ts

prev_desnorm <- linscale(eth5_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

eth5_rnn_ts <- data.frame(eth5_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2021-01-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(eth5_rnn_ts, days >= '2021-01-01'))

#MAPE

base_rnn_eth5 <- data.frame(filter(eth5_rnn_ts, days >= "2021-01-01" & days <= "2021-01-30"), filter(tabela_eth, days >= "2021-01-01" & days <= "2021-01-30"))
forecast::accuracy(base_rnn_eth5$prev_desnorm.x, base_rnn_eth5$etherium2.1) 

###########################################################
###########################################################
#RNN 6
###########################################################
###########################################################
eth6_rnn <- tabela_eth %>% filter(days < "2021-07-01" )
tail(eth6_rnn)

minmax <- linscale(eth6_rnn$etherium2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

eth6_rnn <- data.frame(eth6_rnn, minmax_df)

mlts6_rnn <- mlts_transform(eth6_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts6_rnn)
head(mlts6_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts6_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts6_rnn$y, dim = c(nrow(mlts6_rnn),1))
head(Y)

X <- array(c(mlts6_rnn$mlts_lag_1,
             mlts6_rnn$mlts_lag_2,
             mlts6_rnn$mlts_lag_3,
             mlts6_rnn$mlts_lag_4,
             mlts6_rnn$mlts_lag_5,
             mlts6_rnn$mlts_lag_6,
             mlts6_rnn$mlts_lag_7,
             mlts6_rnn$mlts_lag_8,
             mlts6_rnn$mlts_lag_9,
             mlts6_rnn$mlts_lag_10,
             mlts6_rnn$mlts_lag_11,
             mlts6_rnn$mlts_lag_12,
             mlts6_rnn$mlts_lag_13,
             mlts6_rnn$mlts_lag_14,
             mlts6_rnn$mlts_lag_15,
             mlts6_rnn$mlts_lag_16,
             mlts6_rnn$mlts_lag_17,
             mlts6_rnn$mlts_lag_18,
             mlts6_rnn$mlts_lag_19,
             mlts6_rnn$mlts_lag_20,
             mlts6_rnn$mlts_lag_21,
             mlts6_rnn$mlts_lag_22,
             mlts6_rnn$mlts_lag_23,
             mlts6_rnn$mlts_lag_24,
             mlts6_rnn$mlts_lag_25,
             mlts6_rnn$mlts_lag_26,
             mlts6_rnn$mlts_lag_27,
             mlts6_rnn$mlts_lag_28), 
           dim = c(nrow(mlts6_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_eth <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_eth, "error")),
           Error = attr(rnn_eth, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_eth, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(eth6_rnn)

eth6_rnn_ts <- rbind(
  tail(eth6_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    etherium2 = NA,
    minmax.x = NA
  )
); rownames(eth6_rnn_ts) <- NULL

eth6_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(eth6_rnn_ts)) {
  mlts6_rnn_teste <- mlts_transform(eth6_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts6_rnn_teste <- mlts6_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts6_rnn_teste$mlts_lag_1,
                     mlts6_rnn_teste$mlts_lag_2,
                     mlts6_rnn_teste$mlts_lag_3,
                     mlts6_rnn_teste$mlts_lag_4,
                     mlts6_rnn_teste$mlts_lag_5,
                     mlts6_rnn_teste$mlts_lag_6,
                     mlts6_rnn_teste$mlts_lag_7,
                     mlts6_rnn_teste$mlts_lag_8,
                     mlts6_rnn_teste$mlts_lag_9,
                     mlts6_rnn_teste$mlts_lag_10,
                     mlts6_rnn_teste$mlts_lag_11,
                     mlts6_rnn_teste$mlts_lag_12,
                     mlts6_rnn_teste$mlts_lag_13,
                     mlts6_rnn_teste$mlts_lag_14,
                     mlts6_rnn_teste$mlts_lag_15,
                     mlts6_rnn_teste$mlts_lag_16,
                     mlts6_rnn_teste$mlts_lag_17,
                     mlts6_rnn_teste$mlts_lag_18,
                     mlts6_rnn_teste$mlts_lag_19,
                     mlts6_rnn_teste$mlts_lag_20,
                     mlts6_rnn_teste$mlts_lag_21,
                     mlts6_rnn_teste$mlts_lag_22,
                     mlts6_rnn_teste$mlts_lag_23,
                     mlts6_rnn_teste$mlts_lag_24,
                     mlts6_rnn_teste$mlts_lag_25,
                     mlts6_rnn_teste$mlts_lag_26,
                     mlts6_rnn_teste$mlts_lag_27,
                     mlts6_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts6_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_eth, X_teste))
  
  eth6_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_eth, X_teste))
  
}

eth6_rnn_ts

prev_desnorm <- linscale(eth6_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

eth6_rnn_ts <- data.frame(eth6_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2021-07-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(eth6_rnn_ts, days >= '2021-07-01'))

#MAPE

base_rnn_eth6 <- data.frame(filter(eth6_rnn_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_eth, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base_rnn_eth6$prev_desnorm.x, base_rnn_eth6$etherium2.1) 

###########################################################
###########################################################
#RNN 7
###########################################################
###########################################################
eth7_rnn <- tabela_eth %>% filter(days < "2021-07-01" )
tail(eth7_rnn)

minmax <- linscale(eth7_rnn$etherium2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

eth7_rnn <- data.frame(eth7_rnn, minmax_df)

mlts7_rnn <- mlts_transform(eth7_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts7_rnn)
head(mlts7_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts7_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts7_rnn$y, dim = c(nrow(mlts7_rnn),1))
head(Y)

X <- array(c(mlts7_rnn$mlts_lag_1,
             mlts7_rnn$mlts_lag_2,
             mlts7_rnn$mlts_lag_3,
             mlts7_rnn$mlts_lag_4,
             mlts7_rnn$mlts_lag_5,
             mlts7_rnn$mlts_lag_6,
             mlts7_rnn$mlts_lag_7,
             mlts7_rnn$mlts_lag_8,
             mlts7_rnn$mlts_lag_9,
             mlts7_rnn$mlts_lag_10,
             mlts7_rnn$mlts_lag_11,
             mlts7_rnn$mlts_lag_12,
             mlts7_rnn$mlts_lag_13,
             mlts7_rnn$mlts_lag_14,
             mlts7_rnn$mlts_lag_15,
             mlts7_rnn$mlts_lag_16,
             mlts7_rnn$mlts_lag_17,
             mlts7_rnn$mlts_lag_18,
             mlts7_rnn$mlts_lag_19,
             mlts7_rnn$mlts_lag_20,
             mlts7_rnn$mlts_lag_21,
             mlts7_rnn$mlts_lag_22,
             mlts7_rnn$mlts_lag_23,
             mlts7_rnn$mlts_lag_24,
             mlts7_rnn$mlts_lag_25,
             mlts7_rnn$mlts_lag_26,
             mlts7_rnn$mlts_lag_27,
             mlts7_rnn$mlts_lag_28), 
           dim = c(nrow(mlts7_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_eth <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_eth, "error")),
           Error = attr(rnn_eth, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_eth, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(eth7_rnn)

eth7_rnn_ts <- rbind(
  tail(eth7_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    etherium2 = NA,
    minmax.x = NA
  )
); rownames(eth7_rnn_ts) <- NULL

eth7_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(eth7_rnn_ts)) {
  mlts7_rnn_teste <- mlts_transform(eth7_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts7_rnn_teste <- mlts7_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts7_rnn_teste$mlts_lag_1,
                     mlts7_rnn_teste$mlts_lag_2,
                     mlts7_rnn_teste$mlts_lag_3,
                     mlts7_rnn_teste$mlts_lag_4,
                     mlts7_rnn_teste$mlts_lag_5,
                     mlts7_rnn_teste$mlts_lag_6,
                     mlts7_rnn_teste$mlts_lag_7,
                     mlts7_rnn_teste$mlts_lag_8,
                     mlts7_rnn_teste$mlts_lag_9,
                     mlts7_rnn_teste$mlts_lag_10,
                     mlts7_rnn_teste$mlts_lag_11,
                     mlts7_rnn_teste$mlts_lag_12,
                     mlts7_rnn_teste$mlts_lag_13,
                     mlts7_rnn_teste$mlts_lag_14,
                     mlts7_rnn_teste$mlts_lag_15,
                     mlts7_rnn_teste$mlts_lag_16,
                     mlts7_rnn_teste$mlts_lag_17,
                     mlts7_rnn_teste$mlts_lag_18,
                     mlts7_rnn_teste$mlts_lag_19,
                     mlts7_rnn_teste$mlts_lag_20,
                     mlts7_rnn_teste$mlts_lag_21,
                     mlts7_rnn_teste$mlts_lag_22,
                     mlts7_rnn_teste$mlts_lag_23,
                     mlts7_rnn_teste$mlts_lag_24,
                     mlts7_rnn_teste$mlts_lag_25,
                     mlts7_rnn_teste$mlts_lag_26,
                     mlts7_rnn_teste$mlts_lag_27,
                     mlts7_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts7_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_eth, X_teste))
  
  eth7_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_eth, X_teste))
  
}

eth7_rnn_ts

prev_desnorm <- linscale(eth7_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

eth7_rnn_ts <- data.frame(eth7_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_eth, days >= "2018-01-01" & days <= "2021-07-31"),
       aes(x = days, y = etherium2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(eth7_rnn_ts, days >= '2021-07-01'))

#MAPE

base_rnn_eth7 <- data.frame(filter(eth7_rnn_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_eth, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base_rnn_eth7$prev_desnorm.x, base_rnn7$etherium2.1) 




#juntando todos os mapes
#accuracy
eth_rnn_ac1 <- forecast::accuracy(base_rnn_eth1$prev_desnorm.x, base_rnn_eth1$etherium2.1)
eth_rnn_ac2 <- forecast::accuracy(base_rnn_eth2$prev_desnorm.x, base_rnn_eth2$etherium2.1)
eth_rnn_ac3 <- forecast::accuracy(base_rnn_eth3$prev_desnorm.x, base_rnn_eth3$etherium2.1)
eth_rnn_ac4 <- forecast::accuracy(base_rnn_eth4$prev_desnorm.x, base_rnn_eth4$etherium2.1)
eth_rnn_ac5 <- forecast::accuracy(base_rnn_eth5$prev_desnorm.x, base_rnn_eth5$etherium2.1) 
eth_rnn_ac6 <- forecast::accuracy(base_rnn_eth6$prev_desnorm.x, base_rnn_eth6$etherium2.1)
eth_rnn_ac7 <- forecast::accuracy(base_rnn_eth7$prev_desnorm.x, base_rnn_eth7$etherium2.1)

eth_rnn_accuracy_mean <- bind_rows(eth_rnn_ac1[1,],eth_rnn_ac2[1,],eth_rnn_ac3[1,],eth_rnn_ac4[1,],eth_rnn_ac5[1,],eth_rnn_ac6[1,],eth_rnn_ac7[1,])
eth_rnn_accuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), eth_rnn_accuracy_mean)

btc_rnn_accuracy_mean 
eth_rnn_accuracy_mean
bnc_rnn_accuracy_mean

#VAI PRO GRAFICO
ggplot(data = btc_rnn_accuracy_mean) + 
  theme_light() +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'black') +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'blue', data = eth_rnn_accuracy_mean) 
#geom_line(mapping = aes(x = index, y = MAPE), color = 'red', data = bnc_rnn_accuracy_mean)
















#etherium
#















###########################################################

###########################################################
###########################################################
# MODELO RNN BINANCE
###########################################################

# RNN1 
bnc1_rnn <- tabela_bnc %>% filter(days < "2019-01-01" )
tail(bnc1_rnn)

minmax <- linscale(bnc1_rnn$binance2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bnc1_rnn <- data.frame(bnc1_rnn, minmax_df)

mlts1_rnn <- mlts_transform(bnc1_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts1_rnn)
head(mlts1_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts1_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts1_rnn$y, dim = c(nrow(mlts1_rnn),1))
head(Y)

X <- array(c(mlts1_rnn$mlts_lag_1,
             mlts1_rnn$mlts_lag_2,
             mlts1_rnn$mlts_lag_3,
             mlts1_rnn$mlts_lag_4,
             mlts1_rnn$mlts_lag_5,
             mlts1_rnn$mlts_lag_6,
             mlts1_rnn$mlts_lag_7,
             mlts1_rnn$mlts_lag_8,
             mlts1_rnn$mlts_lag_9,
             mlts1_rnn$mlts_lag_10,
             mlts1_rnn$mlts_lag_11,
             mlts1_rnn$mlts_lag_12,
             mlts1_rnn$mlts_lag_13,
             mlts1_rnn$mlts_lag_14,
             mlts1_rnn$mlts_lag_15,
             mlts1_rnn$mlts_lag_16,
             mlts1_rnn$mlts_lag_17,
             mlts1_rnn$mlts_lag_18,
             mlts1_rnn$mlts_lag_19,
             mlts1_rnn$mlts_lag_20,
             mlts1_rnn$mlts_lag_21,
             mlts1_rnn$mlts_lag_22,
             mlts1_rnn$mlts_lag_23,
             mlts1_rnn$mlts_lag_24,
             mlts1_rnn$mlts_lag_25,
             mlts1_rnn$mlts_lag_26,
             mlts1_rnn$mlts_lag_27,
             mlts1_rnn$mlts_lag_28), 
           dim = c(nrow(mlts1_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_bnc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_bnc, "error")),
           Error = attr(rnn_bnc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_bnc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)


# plot prediction vs real treino
ggplot(data = finalresult,
       aes(x = index, y = real)) + geom_line() +
  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)


###############################################################
#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bnc1_rnn)

bnc1_rnn_ts <- rbind(
  tail(bnc1_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2019-01-01"),
      as.Date("2019-01-01") + 30,
      "day"
    ),
    binance2 = NA,
    minmax.x = NA
  )
); rownames(bnc1_rnn_ts) <- NULL

bnc1_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bnc1_rnn_ts)) {
  mlts1_rnn_teste <- mlts_transform(bnc1_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts1_rnn_teste <- mlts1_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts1_rnn_teste$mlts_lag_1,
                     mlts1_rnn_teste$mlts_lag_2,
                     mlts1_rnn_teste$mlts_lag_3,
                     mlts1_rnn_teste$mlts_lag_4,
                     mlts1_rnn_teste$mlts_lag_5,
                     mlts1_rnn_teste$mlts_lag_6,
                     mlts1_rnn_teste$mlts_lag_7,
                     mlts1_rnn_teste$mlts_lag_8,
                     mlts1_rnn_teste$mlts_lag_9,
                     mlts1_rnn_teste$mlts_lag_10,
                     mlts1_rnn_teste$mlts_lag_11,
                     mlts1_rnn_teste$mlts_lag_12,
                     mlts1_rnn_teste$mlts_lag_13,
                     mlts1_rnn_teste$mlts_lag_14,
                     mlts1_rnn_teste$mlts_lag_15,
                     mlts1_rnn_teste$mlts_lag_16,
                     mlts1_rnn_teste$mlts_lag_17,
                     mlts1_rnn_teste$mlts_lag_18,
                     mlts1_rnn_teste$mlts_lag_19,
                     mlts1_rnn_teste$mlts_lag_20,
                     mlts1_rnn_teste$mlts_lag_21,
                     mlts1_rnn_teste$mlts_lag_22,
                     mlts1_rnn_teste$mlts_lag_23,
                     mlts1_rnn_teste$mlts_lag_24,
                     mlts1_rnn_teste$mlts_lag_25,
                     mlts1_rnn_teste$mlts_lag_26,
                     mlts1_rnn_teste$mlts_lag_27,
                     mlts1_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts1_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_bnc, X_teste))
  
  bnc1_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_bnc, X_teste))
  
}

bnc1_rnn_ts

prev_desnorm <- linscale(bnc1_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bnc1_rnn_ts <- data.frame(bnc1_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2019-01-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = date, y = denormalized), color = "blue", data = predictions_tr)  +
  geom_line(aes(x = days, y = minmax.x), color = "red", data = filter(bnc1_rnn_ts, days >= '2019-01-01')) +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bnc1_rnn_ts, days >= '2019-01-01'))

#MAPE

base_rnn_bnc1 <- data.frame(filter(bnc1_rnn_ts, days >= "2019-01-01" & days <= "2019-01-30"), filter(tabela_bnc, days >= "2019-01-01" & days <= "2019-01-30"))
forecast::accuracy(base_rnn_bnc1$prev_desnorm.x, base_rnn_bnc1$binance2.1) 


###########################################################
###########################################################
#RNN 2
###########################################################
###########################################################
bnc2_rnn <- tabela_bnc %>% filter(days < "2019-07-01" )
tail(bnc2_rnn)

minmax <- linscale(bnc2_rnn$binance2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bnc2_rnn <- data.frame(bnc2_rnn, minmax_df)

mlts2_rnn <- mlts_transform(bnc2_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts2_rnn)
head(mlts2_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts2_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts2_rnn$y, dim = c(nrow(mlts2_rnn),1))
head(Y)

X <- array(c(mlts2_rnn$mlts_lag_1,
             mlts2_rnn$mlts_lag_2,
             mlts2_rnn$mlts_lag_3,
             mlts2_rnn$mlts_lag_4,
             mlts2_rnn$mlts_lag_5,
             mlts2_rnn$mlts_lag_6,
             mlts2_rnn$mlts_lag_7,
             mlts2_rnn$mlts_lag_8,
             mlts2_rnn$mlts_lag_9,
             mlts2_rnn$mlts_lag_10,
             mlts2_rnn$mlts_lag_11,
             mlts2_rnn$mlts_lag_12,
             mlts2_rnn$mlts_lag_13,
             mlts2_rnn$mlts_lag_14,
             mlts2_rnn$mlts_lag_15,
             mlts2_rnn$mlts_lag_16,
             mlts2_rnn$mlts_lag_17,
             mlts2_rnn$mlts_lag_18,
             mlts2_rnn$mlts_lag_19,
             mlts2_rnn$mlts_lag_20,
             mlts2_rnn$mlts_lag_21,
             mlts2_rnn$mlts_lag_22,
             mlts2_rnn$mlts_lag_23,
             mlts2_rnn$mlts_lag_24,
             mlts2_rnn$mlts_lag_25,
             mlts2_rnn$mlts_lag_26,
             mlts2_rnn$mlts_lag_27,
             mlts2_rnn$mlts_lag_28), 
           dim = c(nrow(mlts2_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_bnc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_bnc, "error")),
           Error = attr(rnn_bnc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_bnc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados
finalresult <- data.frame(index = c(1:337),
                          real = bnc2_rnn$binance2[28:364],
                          pred = forecast1_treino_desnorm$x)

# plot prediction vs real treino
ggplot(data = finalresult,
       aes(x = index, y = real)) + geom_line() +
  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bnc2_rnn)

bnc2_rnn_ts <- rbind(
  tail(bnc2_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2019-07-01"),
      as.Date("2019-07-01") + 30,
      "day"
    ),
    binance2 = NA,
    minmax.x = NA
  )
); rownames(bnc2_rnn_ts) <- NULL

bnc2_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bnc2_rnn_ts)) {
  mlts2_rnn_teste <- mlts_transform(bnc2_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts2_rnn_teste <- mlts2_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts2_rnn_teste$mlts_lag_1,
                     mlts2_rnn_teste$mlts_lag_2,
                     mlts2_rnn_teste$mlts_lag_3,
                     mlts2_rnn_teste$mlts_lag_4,
                     mlts2_rnn_teste$mlts_lag_5,
                     mlts2_rnn_teste$mlts_lag_6,
                     mlts2_rnn_teste$mlts_lag_7,
                     mlts2_rnn_teste$mlts_lag_8,
                     mlts2_rnn_teste$mlts_lag_9,
                     mlts2_rnn_teste$mlts_lag_10,
                     mlts2_rnn_teste$mlts_lag_11,
                     mlts2_rnn_teste$mlts_lag_12,
                     mlts2_rnn_teste$mlts_lag_13,
                     mlts2_rnn_teste$mlts_lag_14,
                     mlts2_rnn_teste$mlts_lag_15,
                     mlts2_rnn_teste$mlts_lag_16,
                     mlts2_rnn_teste$mlts_lag_17,
                     mlts2_rnn_teste$mlts_lag_18,
                     mlts2_rnn_teste$mlts_lag_19,
                     mlts2_rnn_teste$mlts_lag_20,
                     mlts2_rnn_teste$mlts_lag_21,
                     mlts2_rnn_teste$mlts_lag_22,
                     mlts2_rnn_teste$mlts_lag_23,
                     mlts2_rnn_teste$mlts_lag_24,
                     mlts2_rnn_teste$mlts_lag_25,
                     mlts2_rnn_teste$mlts_lag_26,
                     mlts2_rnn_teste$mlts_lag_27,
                     mlts2_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts2_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_bnc, X_teste))
  
  bnc2_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_bnc, X_teste))
  
}

bnc2_rnn_ts

prev_desnorm <- linscale(bnc2_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bnc2_rnn_ts <- data.frame(bnc2_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2019-07-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bnc2_rnn_ts, days >= '2019-07-01'))

#MAPE

base_rnn_bnc2 <- data.frame(filter(bnc2_rnn_ts, days >= "2019-07-01" & days <= "2019-07-30"), filter(tabela_bnc, days >= "2019-07-01" & days <= "2019-07-30"))
forecast::accuracy(base_rnn_bnc2$prev_desnorm.x, base_rnn_bnc2$binance2.1) 

###########################################################
###########################################################
#RNN 3
###########################################################
###########################################################
bnc3_rnn <- tabela_bnc %>% filter(days < "2020-01-01" )
tail(bnc3_rnn)

minmax <- linscale(bnc3_rnn$binance2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bnc3_rnn <- data.frame(bnc3_rnn, minmax_df)

mlts3_rnn <- mlts_transform(bnc3_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts3_rnn)
head(mlts3_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts3_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts3_rnn$y, dim = c(nrow(mlts3_rnn),1))
head(Y)

X <- array(c(mlts3_rnn$mlts_lag_1,
             mlts3_rnn$mlts_lag_2,
             mlts3_rnn$mlts_lag_3,
             mlts3_rnn$mlts_lag_4,
             mlts3_rnn$mlts_lag_5,
             mlts3_rnn$mlts_lag_6,
             mlts3_rnn$mlts_lag_7,
             mlts3_rnn$mlts_lag_8,
             mlts3_rnn$mlts_lag_9,
             mlts3_rnn$mlts_lag_10,
             mlts3_rnn$mlts_lag_11,
             mlts3_rnn$mlts_lag_12,
             mlts3_rnn$mlts_lag_13,
             mlts3_rnn$mlts_lag_14,
             mlts3_rnn$mlts_lag_15,
             mlts3_rnn$mlts_lag_16,
             mlts3_rnn$mlts_lag_17,
             mlts3_rnn$mlts_lag_18,
             mlts3_rnn$mlts_lag_19,
             mlts3_rnn$mlts_lag_20,
             mlts3_rnn$mlts_lag_21,
             mlts3_rnn$mlts_lag_22,
             mlts3_rnn$mlts_lag_23,
             mlts3_rnn$mlts_lag_24,
             mlts3_rnn$mlts_lag_25,
             mlts3_rnn$mlts_lag_26,
             mlts3_rnn$mlts_lag_27,
             mlts3_rnn$mlts_lag_28), 
           dim = c(nrow(mlts3_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_bnc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_bnc, "error")),
           Error = attr(rnn_bnc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_bnc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados
finalresult <- data.frame(index = c(1:337),
                          real = bnc3_rnn$binance2[28:364],
                          pred = forecast1_treino_desnorm$x)

# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bnc3_rnn)

bnc3_rnn_ts <- rbind(
  tail(bnc3_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2020-01-01"),
      as.Date("2020-01-01") + 30,
      "day"
    ),
    binance2 = NA,
    minmax.x = NA
  )
); rownames(bnc3_rnn_ts) <- NULL

bnc3_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bnc3_rnn_ts)) {
  mlts3_rnn_teste <- mlts_transform(bnc3_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts3_rnn_teste <- mlts3_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts3_rnn_teste$mlts_lag_1,
                     mlts3_rnn_teste$mlts_lag_2,
                     mlts3_rnn_teste$mlts_lag_3,
                     mlts3_rnn_teste$mlts_lag_4,
                     mlts3_rnn_teste$mlts_lag_5,
                     mlts3_rnn_teste$mlts_lag_6,
                     mlts3_rnn_teste$mlts_lag_7,
                     mlts3_rnn_teste$mlts_lag_8,
                     mlts3_rnn_teste$mlts_lag_9,
                     mlts3_rnn_teste$mlts_lag_10,
                     mlts3_rnn_teste$mlts_lag_11,
                     mlts3_rnn_teste$mlts_lag_12,
                     mlts3_rnn_teste$mlts_lag_13,
                     mlts3_rnn_teste$mlts_lag_14,
                     mlts3_rnn_teste$mlts_lag_15,
                     mlts3_rnn_teste$mlts_lag_16,
                     mlts3_rnn_teste$mlts_lag_17,
                     mlts3_rnn_teste$mlts_lag_18,
                     mlts3_rnn_teste$mlts_lag_19,
                     mlts3_rnn_teste$mlts_lag_20,
                     mlts3_rnn_teste$mlts_lag_21,
                     mlts3_rnn_teste$mlts_lag_22,
                     mlts3_rnn_teste$mlts_lag_23,
                     mlts3_rnn_teste$mlts_lag_24,
                     mlts3_rnn_teste$mlts_lag_25,
                     mlts3_rnn_teste$mlts_lag_26,
                     mlts3_rnn_teste$mlts_lag_27,
                     mlts3_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts3_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_bnc, X_teste))
  
  bnc3_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_bnc, X_teste))
  
}

bnc3_rnn_ts

prev_desnorm <- linscale(bnc3_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bnc3_rnn_ts <- data.frame(bnc3_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2020-01-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bnc3_rnn_ts, days >= '2020-01-01'))

#MAPE

base_rnn_bnc3 <- data.frame(filter(bnc3_rnn_ts, days >= "2020-01-01" & days <= "2020-01-30"), filter(tabela_bnc, days >= "2020-01-01" & days <= "2020-01-30"))
forecast::accuracy(base_rnn_bnc3$prev_desnorm.x, base_rnn_bnc3$binance2.1) 

###########################################################
###########################################################
#RNN 4
###########################################################
###########################################################
bnc4_rnn <- tabela_bnc %>% filter(days < "2020-07-01" )
tail(bnc4_rnn)

minmax <- linscale(bnc4_rnn$binance2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bnc4_rnn <- data.frame(bnc4_rnn, minmax_df)

mlts4_rnn <- mlts_transform(bnc4_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts4_rnn)
head(mlts4_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts4_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts4_rnn$y, dim = c(nrow(mlts4_rnn),1))
head(Y)

X <- array(c(mlts4_rnn$mlts_lag_1,
             mlts4_rnn$mlts_lag_2,
             mlts4_rnn$mlts_lag_3,
             mlts4_rnn$mlts_lag_4,
             mlts4_rnn$mlts_lag_5,
             mlts4_rnn$mlts_lag_6,
             mlts4_rnn$mlts_lag_7,
             mlts4_rnn$mlts_lag_8,
             mlts4_rnn$mlts_lag_9,
             mlts4_rnn$mlts_lag_10,
             mlts4_rnn$mlts_lag_11,
             mlts4_rnn$mlts_lag_12,
             mlts4_rnn$mlts_lag_13,
             mlts4_rnn$mlts_lag_14,
             mlts4_rnn$mlts_lag_15,
             mlts4_rnn$mlts_lag_16,
             mlts4_rnn$mlts_lag_17,
             mlts4_rnn$mlts_lag_18,
             mlts4_rnn$mlts_lag_19,
             mlts4_rnn$mlts_lag_20,
             mlts4_rnn$mlts_lag_21,
             mlts4_rnn$mlts_lag_22,
             mlts4_rnn$mlts_lag_23,
             mlts4_rnn$mlts_lag_24,
             mlts4_rnn$mlts_lag_25,
             mlts4_rnn$mlts_lag_26,
             mlts4_rnn$mlts_lag_27,
             mlts4_rnn$mlts_lag_28), 
           dim = c(nrow(mlts4_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_bnc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_bnc, "error")),
           Error = attr(rnn_bnc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_bnc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bnc4_rnn)

bnc4_rnn_ts <- rbind(
  tail(bnc4_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2020-07-01"),
      as.Date("2020-07-01") + 30,
      "day"
    ),
    binance2 = NA,
    minmax.x = NA
  )
); rownames(bnc4_rnn_ts) <- NULL

bnc4_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bnc4_rnn_ts)) {
  mlts4_rnn_teste <- mlts_transform(bnc4_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts4_rnn_teste <- mlts4_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts4_rnn_teste$mlts_lag_1,
                     mlts4_rnn_teste$mlts_lag_2,
                     mlts4_rnn_teste$mlts_lag_3,
                     mlts4_rnn_teste$mlts_lag_4,
                     mlts4_rnn_teste$mlts_lag_5,
                     mlts4_rnn_teste$mlts_lag_6,
                     mlts4_rnn_teste$mlts_lag_7,
                     mlts4_rnn_teste$mlts_lag_8,
                     mlts4_rnn_teste$mlts_lag_9,
                     mlts4_rnn_teste$mlts_lag_10,
                     mlts4_rnn_teste$mlts_lag_11,
                     mlts4_rnn_teste$mlts_lag_12,
                     mlts4_rnn_teste$mlts_lag_13,
                     mlts4_rnn_teste$mlts_lag_14,
                     mlts4_rnn_teste$mlts_lag_15,
                     mlts4_rnn_teste$mlts_lag_16,
                     mlts4_rnn_teste$mlts_lag_17,
                     mlts4_rnn_teste$mlts_lag_18,
                     mlts4_rnn_teste$mlts_lag_19,
                     mlts4_rnn_teste$mlts_lag_20,
                     mlts4_rnn_teste$mlts_lag_21,
                     mlts4_rnn_teste$mlts_lag_22,
                     mlts4_rnn_teste$mlts_lag_23,
                     mlts4_rnn_teste$mlts_lag_24,
                     mlts4_rnn_teste$mlts_lag_25,
                     mlts4_rnn_teste$mlts_lag_26,
                     mlts4_rnn_teste$mlts_lag_27,
                     mlts4_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts4_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_bnc, X_teste))
  
  bnc4_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_bnc, X_teste))
  
}

bnc4_rnn_ts

prev_desnorm <- linscale(bnc4_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bnc4_rnn_ts <- data.frame(bnc4_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2020-07-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bnc4_rnn_ts, days >= '2020-07-01'))

#MAPE

base_rnn_bnc4 <- data.frame(filter(bnc4_rnn_ts, days >= "2020-07-01" & days <= "2020-07-30"), filter(tabela_bnc, days >= "2020-07-01" & days <= "2020-07-30"))
forecast::accuracy(base_rnn_bnc4$prev_desnorm.x, base_rnn_bnc4$binance2.1) 

###########################################################
###########################################################
#RNN 5
###########################################################
###########################################################
bnc5_rnn <- tabela_bnc %>% filter(days < "2021-01-01" )
tail(bnc5_rnn)

minmax <- linscale(bnc5_rnn$binance2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bnc5_rnn <- data.frame(bnc5_rnn, minmax_df)

mlts5_rnn <- mlts_transform(bnc5_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts5_rnn)
head(mlts5_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts5_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts5_rnn$y, dim = c(nrow(mlts5_rnn),1))
head(Y)

X <- array(c(mlts5_rnn$mlts_lag_1,
             mlts5_rnn$mlts_lag_2,
             mlts5_rnn$mlts_lag_3,
             mlts5_rnn$mlts_lag_4,
             mlts5_rnn$mlts_lag_5,
             mlts5_rnn$mlts_lag_6,
             mlts5_rnn$mlts_lag_7,
             mlts5_rnn$mlts_lag_8,
             mlts5_rnn$mlts_lag_9,
             mlts5_rnn$mlts_lag_10,
             mlts5_rnn$mlts_lag_11,
             mlts5_rnn$mlts_lag_12,
             mlts5_rnn$mlts_lag_13,
             mlts5_rnn$mlts_lag_14,
             mlts5_rnn$mlts_lag_15,
             mlts5_rnn$mlts_lag_16,
             mlts5_rnn$mlts_lag_17,
             mlts5_rnn$mlts_lag_18,
             mlts5_rnn$mlts_lag_19,
             mlts5_rnn$mlts_lag_20,
             mlts5_rnn$mlts_lag_21,
             mlts5_rnn$mlts_lag_22,
             mlts5_rnn$mlts_lag_23,
             mlts5_rnn$mlts_lag_24,
             mlts5_rnn$mlts_lag_25,
             mlts5_rnn$mlts_lag_26,
             mlts5_rnn$mlts_lag_27,
             mlts5_rnn$mlts_lag_28), 
           dim = c(nrow(mlts5_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_bnc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_bnc, "error")),
           Error = attr(rnn_bnc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_bnc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bnc5_rnn)

bnc5_rnn_ts <- rbind(
  tail(bnc5_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-01-01"),
      as.Date("2021-01-01") + 30,
      "day"
    ),
    binance2 = NA,
    minmax.x = NA
  )
); rownames(bnc5_rnn_ts) <- NULL

bnc5_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bnc5_rnn_ts)) {
  mlts5_rnn_teste <- mlts_transform(bnc5_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts5_rnn_teste <- mlts5_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts5_rnn_teste$mlts_lag_1,
                     mlts5_rnn_teste$mlts_lag_2,
                     mlts5_rnn_teste$mlts_lag_3,
                     mlts5_rnn_teste$mlts_lag_4,
                     mlts5_rnn_teste$mlts_lag_5,
                     mlts5_rnn_teste$mlts_lag_6,
                     mlts5_rnn_teste$mlts_lag_7,
                     mlts5_rnn_teste$mlts_lag_8,
                     mlts5_rnn_teste$mlts_lag_9,
                     mlts5_rnn_teste$mlts_lag_10,
                     mlts5_rnn_teste$mlts_lag_11,
                     mlts5_rnn_teste$mlts_lag_12,
                     mlts5_rnn_teste$mlts_lag_13,
                     mlts5_rnn_teste$mlts_lag_14,
                     mlts5_rnn_teste$mlts_lag_15,
                     mlts5_rnn_teste$mlts_lag_16,
                     mlts5_rnn_teste$mlts_lag_17,
                     mlts5_rnn_teste$mlts_lag_18,
                     mlts5_rnn_teste$mlts_lag_19,
                     mlts5_rnn_teste$mlts_lag_20,
                     mlts5_rnn_teste$mlts_lag_21,
                     mlts5_rnn_teste$mlts_lag_22,
                     mlts5_rnn_teste$mlts_lag_23,
                     mlts5_rnn_teste$mlts_lag_24,
                     mlts5_rnn_teste$mlts_lag_25,
                     mlts5_rnn_teste$mlts_lag_26,
                     mlts5_rnn_teste$mlts_lag_27,
                     mlts5_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts5_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_bnc, X_teste))
  
  bnc5_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_bnc, X_teste))
  
}

bnc5_rnn_ts

prev_desnorm <- linscale(bnc5_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bnc5_rnn_ts <- data.frame(bnc5_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2021-01-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bnc5_rnn_ts, days >= '2021-01-01'))

#MAPE

base_rnn_bnc5 <- data.frame(filter(bnc5_rnn_ts, days >= "2021-01-01" & days <= "2021-01-30"), filter(tabela_bnc, days >= "2021-01-01" & days <= "2021-01-30"))
forecast::accuracy(base_rnn_bnc5$prev_desnorm.x, base_rnn_bnc5$binance2.1) 

###########################################################
###########################################################
#RNN 6
###########################################################
###########################################################
bnc6_rnn <- tabela_bnc %>% filter(days < "2021-07-01" )
tail(bnc6_rnn)

minmax <- linscale(bnc6_rnn$binance2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bnc6_rnn <- data.frame(bnc6_rnn, minmax_df)

mlts6_rnn <- mlts_transform(bnc6_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts6_rnn)
head(mlts6_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts6_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts6_rnn$y, dim = c(nrow(mlts6_rnn),1))
head(Y)

X <- array(c(mlts6_rnn$mlts_lag_1,
             mlts6_rnn$mlts_lag_2,
             mlts6_rnn$mlts_lag_3,
             mlts6_rnn$mlts_lag_4,
             mlts6_rnn$mlts_lag_5,
             mlts6_rnn$mlts_lag_6,
             mlts6_rnn$mlts_lag_7,
             mlts6_rnn$mlts_lag_8,
             mlts6_rnn$mlts_lag_9,
             mlts6_rnn$mlts_lag_10,
             mlts6_rnn$mlts_lag_11,
             mlts6_rnn$mlts_lag_12,
             mlts6_rnn$mlts_lag_13,
             mlts6_rnn$mlts_lag_14,
             mlts6_rnn$mlts_lag_15,
             mlts6_rnn$mlts_lag_16,
             mlts6_rnn$mlts_lag_17,
             mlts6_rnn$mlts_lag_18,
             mlts6_rnn$mlts_lag_19,
             mlts6_rnn$mlts_lag_20,
             mlts6_rnn$mlts_lag_21,
             mlts6_rnn$mlts_lag_22,
             mlts6_rnn$mlts_lag_23,
             mlts6_rnn$mlts_lag_24,
             mlts6_rnn$mlts_lag_25,
             mlts6_rnn$mlts_lag_26,
             mlts6_rnn$mlts_lag_27,
             mlts6_rnn$mlts_lag_28), 
           dim = c(nrow(mlts6_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_bnc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_bnc, "error")),
           Error = attr(rnn_bnc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_bnc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bnc6_rnn)

bnc6_rnn_ts <- rbind(
  tail(bnc6_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    binance2 = NA,
    minmax.x = NA
  )
); rownames(bnc6_rnn_ts) <- NULL

bnc6_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bnc6_rnn_ts)) {
  mlts6_rnn_teste <- mlts_transform(bnc6_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts6_rnn_teste <- mlts6_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts6_rnn_teste$mlts_lag_1,
                     mlts6_rnn_teste$mlts_lag_2,
                     mlts6_rnn_teste$mlts_lag_3,
                     mlts6_rnn_teste$mlts_lag_4,
                     mlts6_rnn_teste$mlts_lag_5,
                     mlts6_rnn_teste$mlts_lag_6,
                     mlts6_rnn_teste$mlts_lag_7,
                     mlts6_rnn_teste$mlts_lag_8,
                     mlts6_rnn_teste$mlts_lag_9,
                     mlts6_rnn_teste$mlts_lag_10,
                     mlts6_rnn_teste$mlts_lag_11,
                     mlts6_rnn_teste$mlts_lag_12,
                     mlts6_rnn_teste$mlts_lag_13,
                     mlts6_rnn_teste$mlts_lag_14,
                     mlts6_rnn_teste$mlts_lag_15,
                     mlts6_rnn_teste$mlts_lag_16,
                     mlts6_rnn_teste$mlts_lag_17,
                     mlts6_rnn_teste$mlts_lag_18,
                     mlts6_rnn_teste$mlts_lag_19,
                     mlts6_rnn_teste$mlts_lag_20,
                     mlts6_rnn_teste$mlts_lag_21,
                     mlts6_rnn_teste$mlts_lag_22,
                     mlts6_rnn_teste$mlts_lag_23,
                     mlts6_rnn_teste$mlts_lag_24,
                     mlts6_rnn_teste$mlts_lag_25,
                     mlts6_rnn_teste$mlts_lag_26,
                     mlts6_rnn_teste$mlts_lag_27,
                     mlts6_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts6_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_bnc, X_teste))
  
  bnc6_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_bnc, X_teste))
  
}

bnc6_rnn_ts

prev_desnorm <- linscale(bnc6_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bnc6_rnn_ts <- data.frame(bnc6_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2021-07-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bnc6_rnn_ts, days >= '2021-07-01'))

#MAPE

base_rnn_bnc6 <- data.frame(filter(bnc6_rnn_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_bnc, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base_rnn_bnc6$prev_desnorm.x, base_rnn_bnc6$binance2.1) 

###########################################################
###########################################################
#RNN 7
###########################################################
###########################################################
bnc7_rnn <- tabela_bnc %>% filter(days < "2021-07-01" )
tail(bnc7_rnn)

minmax <- linscale(bnc7_rnn$binance2, minmax = list(mn = 0, mx = 1))
minmax_df <- data.frame(minmax$x)

bnc7_rnn <- data.frame(bnc7_rnn, minmax_df)

mlts7_rnn <- mlts_transform(bnc7_rnn, days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE) # 21 lags
str(mlts7_rnn)
head(mlts7_rnn)

periodos_anteriores = 28 #editar futuramente

#plot(mlts7_rnn$y)
#criando o df de treino e teste, pacote pede esse tipo de dado
Y <- array(mlts7_rnn$y, dim = c(nrow(mlts7_rnn),1))
head(Y)

X <- array(c(mlts7_rnn$mlts_lag_1,
             mlts7_rnn$mlts_lag_2,
             mlts7_rnn$mlts_lag_3,
             mlts7_rnn$mlts_lag_4,
             mlts7_rnn$mlts_lag_5,
             mlts7_rnn$mlts_lag_6,
             mlts7_rnn$mlts_lag_7,
             mlts7_rnn$mlts_lag_8,
             mlts7_rnn$mlts_lag_9,
             mlts7_rnn$mlts_lag_10,
             mlts7_rnn$mlts_lag_11,
             mlts7_rnn$mlts_lag_12,
             mlts7_rnn$mlts_lag_13,
             mlts7_rnn$mlts_lag_14,
             mlts7_rnn$mlts_lag_15,
             mlts7_rnn$mlts_lag_16,
             mlts7_rnn$mlts_lag_17,
             mlts7_rnn$mlts_lag_18,
             mlts7_rnn$mlts_lag_19,
             mlts7_rnn$mlts_lag_20,
             mlts7_rnn$mlts_lag_21,
             mlts7_rnn$mlts_lag_22,
             mlts7_rnn$mlts_lag_23,
             mlts7_rnn$mlts_lag_24,
             mlts7_rnn$mlts_lag_25,
             mlts7_rnn$mlts_lag_26,
             mlts7_rnn$mlts_lag_27,
             mlts7_rnn$mlts_lag_28), 
           dim = c(nrow(mlts7_rnn),1, periodos_anteriores))

head(X)

# treinando  RNN

rnn_bnc <- trainr(Y, X,
                  network_type = 'rnn',
                  sigmoid = c("Gompertz"),
                  hidden_dim = c(3),
                  learningrate = 0.1,
                  numepochs = 300
)

#plotando erro , nao é necessario
data.frame(Epoch = 1:length(attr(rnn_bnc, "error")),
           Error = attr(rnn_bnc, "error")[1:300]) %>%
  ggplot(aes(x = Epoch, y = Error)) +
  geom_line(col = "red")


#previsoes base treino

forecast1_treino <- as.data.frame(predictr(rnn_bnc, X))

forecast1_treino_desnorm <- linscale(forecast1_treino$V1,
                                     minmax = minmax$minmax,
                                     rev = TRUE)

#Plot resultados


# plot prediction vs real treino
#ggplot(data = finalresult,
#       aes(x = index, y = real)) + geom_line() +
#  geom_line(aes(x = index, y = pred), color = "blue", data = finalresult)

#1 ahed forecasting
###############################################################
#base teste , mesma utilizada na mlp

tail(bnc7_rnn)

bnc7_rnn_ts <- rbind(
  tail(bnc7_rnn, 29),
  data.frame(
    days = seq(
      as.Date("2021-07-01"),
      as.Date("2021-07-01") + 30,
      "day"
    ),
    binance2 = NA,
    minmax.x = NA
  )
); rownames(bnc7_rnn_ts) <- NULL

bnc7_rnn_ts #base pronta pro rnn 

###############################################################
#loop RNN

for (d in 30:nrow(bnc7_rnn_ts)) {
  mlts7_rnn_teste <- mlts_transform(bnc7_rnn_ts[(d - 29):d, ],days, minmax.x, p = 28, extras = FALSE, extrasAsFactors = FALSE, granularity = "day")
  mlts7_rnn_teste <- mlts7_rnn_teste[-1, ] # don't need to forecast known outcome
  
  X_teste <- array(c(mlts7_rnn_teste$mlts_lag_1,
                     mlts7_rnn_teste$mlts_lag_2,
                     mlts7_rnn_teste$mlts_lag_3,
                     mlts7_rnn_teste$mlts_lag_4,
                     mlts7_rnn_teste$mlts_lag_5,
                     mlts7_rnn_teste$mlts_lag_6,
                     mlts7_rnn_teste$mlts_lag_7,
                     mlts7_rnn_teste$mlts_lag_8,
                     mlts7_rnn_teste$mlts_lag_9,
                     mlts7_rnn_teste$mlts_lag_10,
                     mlts7_rnn_teste$mlts_lag_11,
                     mlts7_rnn_teste$mlts_lag_12,
                     mlts7_rnn_teste$mlts_lag_13,
                     mlts7_rnn_teste$mlts_lag_14,
                     mlts7_rnn_teste$mlts_lag_15,
                     mlts7_rnn_teste$mlts_lag_16,
                     mlts7_rnn_teste$mlts_lag_17,
                     mlts7_rnn_teste$mlts_lag_18,
                     mlts7_rnn_teste$mlts_lag_19,
                     mlts7_rnn_teste$mlts_lag_20,
                     mlts7_rnn_teste$mlts_lag_21,
                     mlts7_rnn_teste$mlts_lag_22,
                     mlts7_rnn_teste$mlts_lag_23,
                     mlts7_rnn_teste$mlts_lag_24,
                     mlts7_rnn_teste$mlts_lag_25,
                     mlts7_rnn_teste$mlts_lag_26,
                     mlts7_rnn_teste$mlts_lag_27,
                     mlts7_rnn_teste$mlts_lag_28), 
                   dim = c(nrow(mlts7_rnn_teste),1, periodos_anteriores)) 
  
  
  ##forecast
  
  #pred1[d]<- as.data.frame(predictr(rnn_bnc, X_teste))
  
  bnc7_rnn_ts$minmax.x[d] <- as.numeric(predictr(rnn_bnc, X_teste))
  
}

bnc7_rnn_ts

prev_desnorm <- linscale(bnc7_rnn_ts$minmax.x,
                         minmax = minmax$minmax,
                         rev = TRUE)

bnc7_rnn_ts <- data.frame(bnc7_rnn_ts, prev_desnorm$x)


# plot prediction vs real + forecasting
ggplot(filter(tabela_bnc, days >= "2018-01-01" & days <= "2021-07-31"),
       aes(x = days, y = binance2)) + geom_line() +
  geom_line(aes(x = days, y = prev_desnorm.x), color = "orange", data = filter(bnc7_rnn_ts, days >= '2021-07-01'))

#MAPE

base_rnn_bnc7 <- data.frame(filter(bnc7_rnn_ts, days >= "2021-07-01" & days <= "2021-07-30"), filter(tabela_bnc, days >= "2021-07-01" & days <= "2021-07-30"))
forecast::accuracy(base_rnn_bnc7$prev_desnorm.x, base_rnn_bnc7$binance2.1) 




#juntando todos os mapes
#accuracy
bnc_rnn_ac1 <- forecast::accuracy(base_rnn_bnc1$prev_desnorm.x, base_rnn_bnc1$binance2.1)
bnc_rnn_ac2 <- forecast::accuracy(base_rnn_bnc2$prev_desnorm.x, base_rnn_bnc2$binance2.1)
bnc_rnn_ac3 <- forecast::accuracy(base_rnn_bnc3$prev_desnorm.x, base_rnn_bnc3$binance2.1)
bnc_rnn_ac4 <- forecast::accuracy(base_rnn_bnc4$prev_desnorm.x, base_rnn_bnc4$binance2.1)
bnc_rnn_ac5 <- forecast::accuracy(base_rnn_bnc5$prev_desnorm.x, base_rnn_bnc5$binance2.1) 
bnc_rnn_ac6 <- forecast::accuracy(base_rnn_bnc6$prev_desnorm.x, base_rnn_bnc6$binance2.1)
bnc_rnn_ac7 <- forecast::accuracy(base_rnn_bnc7$prev_desnorm.x, base_rnn_bnc7$binance2.1)

bnc_rnn_accuracy_mean <- bind_rows(bnc_rnn_ac1[1,],bnc_rnn_ac2[1,],bnc_rnn_ac3[1,],bnc_rnn_ac4[1,],bnc_rnn_ac5[1,],bnc_rnn_ac6[1,],bnc_rnn_ac7[1,])
bnc_rnn_accuracy_mean <- data.frame(index = c(1, 2, 3, 4, 5, 6, 7), bnc_rnn_accuracy_mean)

btc_rnn_accuracy_mean 
eth_rnn_accuracy_mean
bnc_rnn_accuracy_mean

#VAI PRO GRAFICO
ggplot(data = btc_rnn_accuracy_mean) + 
  theme_light() +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'black') +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'blue', data = eth_rnn_accuracy_mean) +
  geom_line(mapping = aes(x = index, y = MAPE), color = 'red', data = bnc_rnn_accuracy_mean)

