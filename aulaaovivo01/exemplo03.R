###################################################
###### Dessazonalizar séries econômicas ###########


library(ecoseries)
library(seasonal)
library(zoo)

## Baixar dados do CAGED
saldo_caged = series_ipeadata("272844966", periodicity = 'M')$serie_272844966

ggplot(saldo_caged, aes(x=data, y=valor))+
  geom_line()

saldo_caged =
  saldo_caged %>%
  mutate(valor_sa = final(seas(ts(valor, start=c(1999,05), 
                                  freq=12))))

ggplot(saldo_caged, aes(x=data, y=valor_sa/1000))+
  geom_line()+
  geom_hline(yintercept=0, colour='red', linetype='dashed')

saldo_caged = 
  saldo_caged %>%
  mutate(ma_12 = rollapply(valor_sa, 12, mean, align='right',
                           fill=NA))

ggplot(saldo_caged, aes(x=data, y=ma_12/1000))+
  geom_line()
