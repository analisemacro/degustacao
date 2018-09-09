###########################
### Desemprego vs. Uber ###
###########################

### Carregar pacotes que serão utilizados

library(gtrendsR)
library(ggplot2)
library(xts)
library(sidrar)
library(forecast)
library(vars)
library(aod)

############## Coleta e Tratamento de Dados ########################

### Coleta de Dados

## Desemprego - SIDRA/IBGE
desemprego = get_sidra(api='/t/6381/n1/all/v/4099/p/all/d/v4099%201')

# Tratamento                
View(desemprego)
desemprego = ts(desemprego$Valor, start=c(2012,03), freq=12)
autoplot(desemprego)

## 'Procura pela Uber" - Usamos como proxy as pesquisas do GTrends
trends <- gtrends('uber', geo='BR')

# Tratamento
View(trends)
View(trends$related_queries)

gtrends <- data.frame(time=trends$interest_over_time$date,
                      empregos=trends$interest_over_time$hits)
gtrends$time = as.Date(gtrends$time, format='%d/%m/%Y')
gtrends = xts(gtrends$empregos, order.by=gtrends$time)
gtrends = ts(apply.monthly(gtrends, FUN=mean), start=c(2013,09), freq=12)

autoplot(gtrends)

## Juntar as duas séries
data <- ts.intersect(desemprego, gtrends)
colnames(data) <- c('desemprego', 'uber')



######## Ver correlação
cor(data)
cor = data.frame(time=as.Date(time(data)), desemprego=data[,1],
                 uber=data[,2])

ggplot(cor, aes(x=desemprego, y=uber)) +
  geom_point(shape=1, size=4, colour='darkblue') +   
  geom_smooth(method=lm) +
  xlab('Desemprego PNAD Contínua') +
  ylab(' Uber (Pesquisa Google Trends)')+
  labs(title='Desemprego vs. Uber',
       subtitle='Taxa de Desemprego PNAD Contínua vs. Pesquisas pela palavra Uber no Google Trends',
       caption='Fonte: analisemacro.com.br')+
  theme_minimal()


################## Criar Modelo VAR #####################

def <- VARselect(data,lag.max=12,type="both")
def$selection 

### Vetor Autorregressivo
var <- VAR(data, p=5, type='both')
serial.test(var)
plot(stability(var))

### Função Impulso-Resposta
irf = irf(var, impulse='desemprego', response='uber', 
          n.ahead = 12, boot=T, ortho=T, cumulative=F)

lags = 1:13
df.irf <- data.frame(irf=irf$irf, lower=irf$Lower, upper=irf$Upper,
                     lags=lags)
colnames(df.irf) <- c('irf', 'lower', 'upper', 'lags')
number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot(data = df.irf,aes(x=lags,y=irf)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = irf), size=.8)+
  geom_ribbon(aes(x=lags, ymax=upper, 
                  ymin=lower), 
              fill="blue", alpha=.1) +
  xlab("") + ylab("Interesse pela Uber") + 
  ggtitle("Resposta ao Impulso no Desemprego") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  geom_line(colour = 'black')+
  scale_x_continuous(breaks=number_ticks(13))+
  theme_bw()


### Teste de Wald

var6 <- VAR(data, p=6, type='both')

### Wald Test 01: Uber não granger causa Desemprego

wald.test(b=coef(var6$varresult[[1]]), 
          Sigma=vcov(var6$varresult[[1]]), 
          Terms=c(2,4,6,8,10))

### Wald Test 02: Desemprego não granger causa Uber

wald.test(b=coef(var6$varresult[[2]]), 
          Sigma=vcov(var6$varresult[[2]]), 
          Terms= c(1,3,5,7,9))

