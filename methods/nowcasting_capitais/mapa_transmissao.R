library(tidyverse)
library(lubridate)
library(geobr)

url <- "macsaud_serie_estimativas_tendencia_sem_filtro_febre.csv"

srag_macro <- read_csv2(file = url)

srag_macro %>% 
  filter(escala == "casos") %>% 
  group_by(`Ano epidemiológico`) %>% 
  summarise(Total = sum(`casos estimados`))


srag_macro <- srag_macro %>% filter(escala == "incidência")



epiweek.aux.tbl <- srag_macro %>% 
  group_by(`Ano epidemiológico`, `Semana epidemiológica`) %>% 
  tally() %>% 
  rowid_to_column("Time") 

# Criando uma coluna com os sabados
epiweek.aux.tbl$sabado <- seq.Date(ymd("2020-01-04"), by = 7, length.out = nrow(epiweek.aux.tbl))



srag_macro <- srag_macro %>% 
  left_join(epiweek.aux.tbl, 
            by = c("Semana epidemiológica", "Ano epidemiológico")) 


trans.lab <- c("Extremamente alta", 
               "Muito alta", 
               "Alta", 
               "Epidêmica", 
               "Pré epidêmica") 


srag_macro <- srag_macro %>% 
  mutate(
    Incidencia = `casos estimados`,
    Transmissao = case_when(
      Incidencia < .5 ~ trans.lab[5],
      Incidencia >= 0.5 & Incidencia < 1 ~ trans.lab[4],
      Incidencia >= 1 & Incidencia < 5 ~ trans.lab[3],
      Incidencia > 5 & Incidencia < 10 ~ trans.lab[2],
      Incidencia > 10  ~ trans.lab[1]),
    code_health_marcroregion = CO_MACSAUD * 10 + 8,
    Transmissao = factor(Transmissao, levels = trans.lab)
  ) 


# Mapa

# Baixando os dados para mapear as macrorregioes 
macro_map <- read_health_region(year = 2013, macro = T)
# state_map <- read_state()

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())




macro_map.aux <- macro_map %>% left_join( srag_macro %>% filter(Time > max(srag_macro$Time)-30), 
                                          by = "code_health_marcroregion")


ggplot(data = macro_map.aux) +
  geom_sf(mapping = aes(fill=Transmissao), size=.15) +
  scale_fill_manual(name="Transmissão \n comunitária", values = c('#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8')) + 
  theme_minimal(base_size = 14) +
  no_axis +
  theme(legend.position = "bottom") +
  facet_wrap(~ sabado, nrow=5) + 
  ggtitle("Transmissão comunitária da COVID-19 segundo SRAG, \n por macrorregiões de saúde")


# Mapa da ultima semana separado

ggplot(data = macro_map.aux %>% filter(Time == max(epiweek.aux.tbl$Time))) +
  geom_sf(mapping = aes(fill=Transmissao), size=.15) +
  scale_fill_manual(name="Transmissão \n comunitária", values = c('#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8')) + 
  theme_minimal(base_size = 18) +
  no_axis +
  theme(legend.position = c(.2, .35)) +
  labs(
    title = "Transmissão comunitária da COVID-19 segundo SRAG, \n por macrorregiões de saúde",
    subtitle = "Semana epidemiológica 22/2021"
    )

# Estatisticas da ultima semana
srag_macro %>% filter(Time == max(epiweek.aux.tbl$Time)) %>% group_by(Transmissao) %>% tally()
