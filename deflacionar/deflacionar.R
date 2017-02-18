

library(ggplot2)
library(ggthemr)
library(forecast)

ggthemr('dust')

data <- read.csv2('data.csv', header=T, sep=';', dec=',')
data$date <- as.Date(data$date, format='%d/%m/%Y')

tail(data)

ggplot(data, aes(date, previdencia/1000))+geom_line(size=.8)+
  xlab('')+ylab('R$ bilhões')+
  labs(title='Despesa com Benefícios Previdenciários')

data$prevreal <- data[,3]*(tail(data[,2],1)/data[,2]) 

ggplot(data, aes(date, previdencia/1000))+
  geom_line(size=.8, aes(color='Série Nominal'))+
  geom_line(size=.8, aes(date, prevreal/1000, color='Série Real'))+
  xlab('')+ylab('R$ bilhões')+
  labs(title='Despesa com Benefícios Previdenciários',
       colour='',
       caption='analisemacro.com.br')+
  theme(legend.position="top")

data.ts <- ts(data[,-1], start=c(1997,01), freq=12)

cmav(data.ts[,2], ma=12)
