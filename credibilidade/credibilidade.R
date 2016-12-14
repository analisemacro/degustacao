#######################################################
####### Credibilidadde da Política Monetária ##########
#######################################################

### Último dado

end <- "2016-10-07"


### Pacotes

library(xts)
library(forecast)
library(ggplot2)
library(ggthemes)
library(easyGgplot2)
library(mFilter)
library(grid)
library(png)

### Importar dados

dados <- read.table(file='credibilidade.csv', sep=';', dec=',', header=T)

tail(dados)

dados <- dados[complete.cases(dados),]

### Primeira coluna como datas

dados$date = as.Date(dados$date, format="%d/%m/%Y")

### Ordenar séries de acordo com datas

cred <- xts(x = dados$credibilidade, 
             order.by = dados$date)

### Mensalizar

credibilidade <- ts(apply.monthly(cred, FUN=mean), start=c(2000,01), freq=12)

### Filtro HP

cred.hp <- hpfilter(credibilidade, type='lambda', freq=14400)

data <- ts.intersect(credibilidade, cred.hp$trend)
colnames(data) <- c('Índice de Credibilidade', 'Tendência (Filtro HP)')

### Gráfico

img <- readPNG('logo.png')
g <- rasterGrob(img, interpolate=TRUE)

autoplot(data[,c(1,2)]) +
  geom_line(size=0.8) +
  ylab('Índice Normalizado') +
  xlab('') +
  scale_x_discrete(limits=2000:2016) +
  ggtitle('Índice de Credibilidade da Política Monetária brasileira') +
  theme_economist(base_size = 11) +
  scale_color_economist(stata=FALSE) +
  labs(colour = "")+
  annotate("rect", fill = "gray", alpha = 0.5, 
           xmin = 2016.2, 
           xmax = 2016.9,
           ymin = -Inf, ymax = Inf)+
  annotation_custom(g, xmin=2013.0, xmax=2015.9, 
                    ymin=0.71, ymax=1.1)+
  annotate('text', x=2009, y=0.25, 
           label='Fonte: elaboração própria 
           com base em Mendonça e Souza (2007).',
           colour='red', size=5)
