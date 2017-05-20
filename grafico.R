########## Crédito Público vs. Crédito Privado #######################


library(rbcb)
library(xts)
library(reshape2)
library(scales)
library(ggplot2)

privado <- get_series(2043, start_date = '2000-01-01')
publico <- get_series(2007, start_date = '2000-01-01')

dates <- seq(as.Date('2000-01-01'), as.Date('2017-03-01'), by='1 month')

data <- data.frame(privado=privado$`2043`/(publico$`2007`+privado$`2043`)*100,
                   publico=publico$`2007`/(publico$`2007`+privado$`2043`)*100)

data <- xts(data, order.by=dates)

data <- data.frame(time = index(data), melt(as.data.frame(data)))

ggplot(data, aes(x = time, y = value)) + 
  geom_area(aes(colour = variable, fill = variable))+
  xlab('')+ylab('Participação Percentual')+
  labs(title='Crédito Público vs. Crédito Privado',
       caption='Fonte: analisemacro.com.br com dados do Banco Central.')+
  theme(legend.position = 'bottom',
        legend.title=element_blank())+
  scale_x_date(breaks = date_breaks("2 years"),
               labels = date_format("%Y"))
