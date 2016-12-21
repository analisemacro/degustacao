###############################
###### Refutando Bresser ######


library(ggplot2)

## Formação Bruta de Capital vs. Saldo em Transações Correntes

data <- read.csv2('data.csv', sep=';', dec=',')

tail(data)

ggplot(data, aes(fbc, stc))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title='Formação Bruta de Capital vs. Saldo em Transações Correntes (% PIB)')

## Formação Bruta de Capital Fixo vs. Exportações Líquidas

data2 <- read.csv2('data2.csv', sep=';', dec=',')

tail(data2)

ggplot(data2, aes(fbcf, el))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(title='Formação Bruta de Capital Fixo vs. Exportações Líquidas (% PIB)')
