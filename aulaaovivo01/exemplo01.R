###########################################################
############ Análise da Inflação no Brasil  ###############


library(sidrar)
library(tidyverse)
library(tstools)
library(BETS)

### Inflação cheia
ipca = 
  '/t/1737/n1/all/v/2266/p/all/d/v2266%2013' %>%
  get_sidra(api=.) %>%
  mutate(date = parse_date(`Mês (Código)`, format="%Y%m")) %>%
  rename(indice = Valor) %>%
  mutate(inflacao_mensal = (indice/lag(indice,1)-1)*100,
         inflacao_anual = (indice/lag(indice,12)-1)*100) %>%
  select(date, indice, inflacao_mensal, inflacao_anual) %>%
  as_tibble()


filter(ipca, date > '2016-01-01') %>%
  ggplot(aes(x=date, y=inflacao_anual))+
  geom_line(size=.8)

### Núcleos de Inflação
codes = c(4466,11426,11427,16121,16122, 27838, 27839)
nucleos = BETSget(codes, from='2012-01-01', data.frame=T)
data_nucleos = matrix(NA, nrow=nrow(nucleos[[1]]),
                      ncol=length(codes))
for(i in 1:length(codes)){
  data_nucleos[,i] = t(nucleos[[i]]$value)
}

colnames(data_nucleos) = c('ipca_ms', 'ipca_ma', 'ipca_ex0',
                           'ipca_ex1', 'ipca_dp', 'ipca_ex2',
                           'ipca_ex3')

nucleos_vm =
  data_nucleos %>%
  as_tibble() %>%
  mutate(date = nucleos[[1]]$date) %>%
  select(date, everything())

nucleos_12m = 
  data_nucleos %>%
  ts(start=c(2006,07), freq=12) %>%
  acum_p(12) %>%
  as_tibble() %>%
  mutate(date = nucleos[[1]]$date) %>%
  select(date, everything()) %>%
  drop_na()

### Metas de Inflação

meta = c(rep(4.5,12*7-11), rep(4.25, 12), rep(4, 8))
meta_max = c(rep(4.5+2,12*5-11), rep(4.5+1.5,12*2),
             rep(4.25+1.5,12), rep(4+1.5, 8))
meta_min = c(rep(4.5-2,12*5-11), rep(4.5-1.5,12*2),
             rep(4.25-1.5,12), rep(4-1.5, 8))
data_meta = 
  tibble(date = seq(as.Date('2012-12-01'), as.Date('2020-08-01'),
                    by='1 month'),
         meta_min = meta_min,
         meta = meta,
         meta_max = meta_max)

### Inflação no domicílio

alim_dom_01 = 
  get_sidra(api='/t/1419/n1/all/v/63/p/all/c315/7171/d/v63%202')

alim_dom_02 = 
  get_sidra(api='/t/7060/n1/all/v/63/p/all/c315/7171/d/v63%202') 

alim_dom = full_join(alim_dom_01, alim_dom_02) %>%
  mutate(date = parse_date(`Mês (Código)`, format="%Y%m")) %>%
  mutate(inflacao_12m = acum_p(Valor,12)) %>%
  select(date, Valor, inflacao_12m)


ggplot(alim_dom, aes(x=date, y=inflacao_12m))+
  geom_line()

### Visualização dos dados

filter(ipca, date > '2012-12-01') %>%
  ggplot(aes(x=date))+
  geom_line(aes(y=inflacao_anual, colour='Inflação Cheia'),
            size=.8)+
  geom_line(data=na.omit(nucleos_12m), 
            aes(y=rowMeans(nucleos_12m[,-1]),
                                  colour='Média dos Núcleos'),
            size=.8)+
  geom_line(data=na.omit(alim_dom), 
            aes(y=inflacao_12m,
                               colour='Alimentação no Domicílio'),
            size=.8)+
  geom_ribbon(data=data_meta,
              aes(ymin=meta_min, ymax=meta_max), 
              colour='grey70', alpha=0.3)+
  geom_line(data=data_meta,
            aes(y=meta, colour='Meta'), size=.8)+
  scale_colour_manual('',
                      values=c('Inflação Cheia'='blue',
                               'Média dos Núcleos'='red',
                               'Alimentação no Domicílio'='orange',
                               'Meta'='black'))+
  theme(legend.position = 'top')+
  labs(x='', y='%', 
       title='Inflação Cheia, Núcleos e Inflação de Alimentos',
       caption='Fonte: analisemacro.com.br com dados do IBGE')
