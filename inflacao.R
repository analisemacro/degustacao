



library(quantmod)
library(ecoseries)

getSymbols('CPIAUCSL', src = "FRED")

cpi.eua = window(ts(CPIAUCSL, start=c(1947,01), freq=12),
                 start=c(1994,07))

ipca = window(ts(series_ipeadata('36482', 
                                 periodicity = 'M')$serie_36482$valor,
                 start=c(1979,12), freq=12), start=c(1994,07))

par(mfrow=c(1,2))
plot(cpi.eua, main='CPI EUA')
plot(ipca, main='IPCA Br')

(tail(cpi.eua,1)/head(cpi.eua,1)-1)*100
(tail(ipca,1)/head(ipca,1)-1)*100
