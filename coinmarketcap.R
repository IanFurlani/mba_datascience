library(pacman)
pacman::p_load(data.table, fixest, coinmarketcapr)

key <-'8d5c561e-e4b3-4bf3-9464-647f09c0bee6'
coinmarketcapr::setup(key)

cryptos <- get_crypto_listings(limit = 10)

plot_top_currencies()