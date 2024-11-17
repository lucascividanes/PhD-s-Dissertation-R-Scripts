library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)
library(sjPlot)

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"),
                             COD_IBGE = c("3550308", "3304557", "2304400", "2927408",
                                          "3106200", "1302603", "4106902", "2611606",
                                          "5208707", "4314902", "1501402", "3518800", "3509502", "2111300"))

municipalities <- data.frame(UF = c("SP"),
                             MUN = c("SÃO PAULO"),
                             COD_IBGE = c("3550308"))

df_npo <- data.frame()

################### ibas

iba_sp <- read_delim("02_out/iba_sp.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                         grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(year == 2019)

################### npos

npo <- read_excel("01_in/ipea/area_subarea.xlsx") %>% 
  clean_names() %>%
  subset(edmu_cd_municipio %in% municipalities$COD_IBGE)

npo$tx_nome_classe_atividade_economica <- npo$tx_nome_classe_atividade_economica %>% 
  str_replace_all("Ã§Ã£", "çã") %>% 
  str_replace_all("Ã§Ãµ", "çõ") %>% 
  str_replace_all("Ã£", "ã") %>% 
  str_replace_all("Ã", "à") %>% 
  str_replace_all("Ãª", "ê") %>% 
  str_replace_all("à§", "ç") %>%
  str_trim()

npo$edmu_nm_municipio <- npo$edmu_nm_municipio %>% 
  str_replace_all("Ã£", "ã")

npo %>%
  group_by(year = year(dt_fundacao_osc)) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = year, y = N)) + 
  geom_line() + 
  theme_minimal()

npo %>%
  group_by(year = year(dt_fundacao_osc)) %>% 
  summarise(N = n()) %>% 
  mutate(cum_N = cumsum(N)) %>% 
  ggplot(aes(x = year, y = cum_N)) + 
  geom_line() + 
  theme_minimal()

npo %>%
  group_by(tx_nome_classe_atividade_economica) %>% 
  summarise(N = n()) %>% 
  mutate(PCT = N/sum(N)) %>% 
  arrange(-N) %>% 
  mutate(CUM_PCT = cumsum(PCT)) %>% 
  subset(PCT > 0.01)

area_subarea <- npo %>% 
  select(cd_identificador_osc:outras_atividades_associativas) %>% 
  gather(key="area_subarea", value="sim", 12:22) %>%
  subset(!is.na(sim)) %>% 
  group_by(area_subarea) %>% 
  summarise(N = n_distinct(cd_identificador_osc)) %>% 
  mutate(PCT = N/sum(N)) %>% 
  arrange(-N) %>% 
  mutate(CUM_PCT = cumsum(PCT))

nppp_npos <- read_delim("02_out/nppp_npos.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = "."), trim_ws = TRUE) %>% 
  gather(key="area_subarea", value="sim", 4:14) %>%
  subset(!is.na(sim)) %>% 
  group_by(area_subarea) %>% 
  summarise(N_PPP = n_distinct(cnpj_da_entidade)) %>% 
  mutate(PCT_PPP = N_PPP/sum(N_PPP)) %>% 
  arrange(-N_PPP) %>% 
  mutate(CUM_PCT_PPP = cumsum(PCT_PPP)) %>% 
  left_join(area_subarea, by = c("area_subarea"))

####################### 

nppp_edges <- read_delim("02_out/nppp_edges.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE)

nppp_edges %>% distinct(to) %>% count()
nppp_edges %>% distinct(from) %>% count()

