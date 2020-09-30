#################################################
### Tratando dados da Produção Industrial #######

library(sidrar)
library(tidyverse)
library(tstools)
library(BETS)


pim_sa = 
  '/t/3653/n1/all/v/3134/p/all/c544/129314/d/v3134%201' %>%
  get_sidra(api=.) %>%
  mutate(date = parse_date(`Mês (Código)`, format='%Y%m')) %>%
  select(date, Valor) %>%
  mutate(var_margem = (Valor/lag(Valor,1)-1)*100) %>%
  as_tibble()

ggplot(pim_sa, aes(x=date, y=Valor))+
  geom_line()


pim = get_sidra(api='/t/3653/n1/all/v/3135/p/all/c544/129314/d/v3135%201')
pim = 
  pim %>%
  mutate(date = parse_date(`Mês (Código)`, format='%Y%m')) %>%
  select(date, Valor) %>%
  mutate(var_interanual = (Valor/lag(Valor,12)-1)*100)

ggplot(pim, aes(x=date, y=var_interanual))+
  geom_line()


pim = 
  pim %>%
  mutate(var_anual = acum_i(Valor,12))

data_pim = inner_join(pim_sa, pim, by='date') %>%
  rename(pim_sa = Valor.x,
         pim = Valor.y)

data_pim

data_pim %>%
  gather(variavel, valor, -date) %>%
  filter(date > '2014-01-01') %>%
  ggplot(aes(x=date, y=valor, colour=variavel))+
  geom_line(size=.8)+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none')

