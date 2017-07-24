



library(BETS)
library(ggplot2)
library(forecast)
library(gridExtra)

nfsp <- window(BETS.get(5793),start=c(2002,11))
dbgg <- window(BETS.get(13762), start=c(2006,12))



g1 <- autoplot(nfsp)+
  geom_line(size=.8)+
  scale_x_discrete(limits=c(2003:2017))+
  labs(title='',
       subtitle='Necessidade (Primária) de Financiamento do Setor Público (% PIB)')+
  ylab('% PIB')+xlab('')
  
g2 <- autoplot(dbgg)+
  geom_line(size=.8)+
  scale_x_discrete(limits=c(2007:2017))+
  labs(title='',
       subtitle='Dívida Bruta do Governo Geral (% PIB)',
       caption='Fonte: analisemacro.com.br com dados do BCB.')+
  ylab('% PIB')+xlab('')


grid.arrange(g1, g2,
             top = "Por que aumentar imposto?",
             layout_matrix = matrix(c(1,2), 
                                    ncol=1, byrow=TRUE))
