# Rede neural recorrente
# https://www.youtube.com/watch?v=szM6dt1gU-c&t=9s

###########################################
set.seed(0)
###########################################

# 1 Tidy - construir a tabela final, ainda sem divisao treino e teste

#bases de dados completa
btc <- read_csv("/Users/ian.furlani/MBA/TCC/tcc_dsa/dados/BTC.csv")
eth <- read_csv("/Users/ian.furlani/MBA/TCC/tcc_dsa/dados/ETH.csv")
bnb <- read_csv("/Users/ian.furlani/MBA/TCC/tcc_dsa/dados/BNB.csv")

## Create a daily Date object - helps my work on dates
start_date <- ymd("2018-01-01")
end_date <- ymd("2022-03-31")
days <- seq(start_date, end_date, "days")
range <- data.frame(days)
summary(range)

## Create a time series object
inds <- seq(as.Date("2018-01-01"), as.Date("2022-03-31"), by = "day")
bitcoin <- ts(rev(btc$Price),start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)
etherium <- ts(rev(eth$Price),start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)
binance <- ts(rev(bnb$Price),start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)

#Check bitcoin
class(bitcoin)
summary(bitcoin)
autoplot(bitcoin)
ts.plot(bitcoin)

#Unindo data + valores... nao sei se é necessario
tabela_btc <- data.frame(range, bitcoin)
head(tabela_btc)

tabela_btc %>% ggplot(aes(x = 1:nrow(tabela_btc), y = bitcoin)) +
  geom_line()

#####################################################
# Preparacao dos dados

#tudo comeca com essa tabela aqui
head(tabela_btc)

periodos_anteriores = 3 #editar futuramente
n_col = ncol(tabela_btc)

for (i in 1:periodos_anteriores) {
  for (j in 1:nrow(tabela_btc)) {
    if (j - periodos_anteriores <= 0) {
    } else {
      tabela_btc[j, n_col + i] = tabela_btc[j - i, 2]
    }
  }
}

#criou as colunas de lags
head(tabela_btc)

#apagar linhas com valores nulor
tabela_btc_rnn <- tabela_btc[(periodos_anteriores + 1):(nrow(tabela_btc)), -1]
names(tabela_btc_rnn) <- c("y", "x_1", "x_2", "x_3")

#retirou as linhas de valores nulos, retirou a coluna de data, renomeou as colunas
head(tabela_btc_rnn)
 
# normalizacao dos dados (melhor para o modelo)
minmax_price <- linscale(tabela_btc_rnn$y, minmax = list(mn = 0, mx = 1))
minmax_x1 <- linscale(tabela_btc_rnn$x_1, minmax = list(mn = 0, mx = 1))
minmax_x2 <- linscale(tabela_btc_rnn$x_2, minmax = list(mn = 0, mx = 1))
minmax_x3 <- linscale(tabela_btc_rnn$x_3, minmax = list(mn = 0, mx = 1))

#tipo lista
class(minmax_price)



#criando o df de treino e teste
Y <- array(minmax_price$x, dim = c(nrow(tabela_btc_rnn) - 100, 1))
X <- array(c(minmax_x1$x[1:(nrow(tabela_btc_rnn)-100)],
             minmax_x2$x[1:(nrow(tabela_btc_rnn)-100)],
             minmax_x3$x[1:(nrow(tabela_btc_rnn)-100)]),
            dim = c(nrow(tabela_btc_rnn)-100,1, periodos_anteriores))

head(Y)
head(X)

# treinando  RNN

rnn_btc <- trainr(Y, X,
                  hidden_dim = c(3),
                  learningrate = 0.5,
                  numepochs = 20)

#plotando erro , nao é necessario
data.frame(Epoch = 5:length(attr(rnn_btc, "error")),
           Error = attr(rnn_btc, "error")[5:20]) %>%
      ggplot(aes(x = Epoch, y = Error)) +
      geom_line(col = "red")
           
#previsoes
entrada_h <- array(c(minmax_x1$x[(nrow(tabela_btc_rnn)-100):nrow(tabela_btc)],
                     minmax_x2$x[(nrow(tabela_btc_rnn)-100):nrow(tabela_btc)],
                     minmax_x3$x[(nrow(tabela_btc_rnn)-100):nrow(tabela_btc)]),
                   dim = c(nrow(tabela_btc_rnn)-(nrow(tabela_btc_rnn)-100),1, periodos_anteriores))

pred_new <- as.data.frame(predictr(rnn_btc, entrada_h))

pred_old <- as.data.frame(predictr(rnn_btc, X))

prev_desnorm <- linscale(pred_old$V1,
                         minmax = minmax_price$minmax,
                         rev = TRUE)

prev_desnorm_new <- linscale(pred_new$V1,
                         minmax = minmax_price$minmax,
                         rev = TRUE)


#Plot resultados
finalresult <- data.frame(real = tabela_btc_rnn$y[1:1448],
                           pred = prev_desnorm$x)

finalresult$id = 1:nrow(final_result)
finalresult %>% gather(var, value, -id) -> finalresult
finalresult = rbind(finalresult, data.frame(id = 1448:(1448 + length(prev_desnorm_new$x)-1),
                                             var = rep("new", length(prev_desnorm_new$x)),
                                             value = prev_desnorm_new$x))

finalresult %>% ggplot(aes(x = id,
                           y = value, colour = var)) +
  geom_line(na.rm = T)
