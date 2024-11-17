library(tidyverse)
library(sjPlot)
library(sf)
library(classInt)
library(nnet)
library(janitor)
library(knitr)
library(stargazer)

set_theme(theme_minimal())

indicators <- read_delim("02_out/indicators.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO == "SÃO PAULO")

profile <- read_delim("02_out/profile.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO == "SÃO PAULO") %>% 
  select(ANO_ELEICAO, NR_VOTAVEL, NM_URNA_CANDIDATO, DS_SIT_TOT_TURNO)

candidatos <- read_delim("02_out/margins.csv", 
                         delim = ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO == "SÃO PAULO") %>% 
  select(ANO_ELEICAO, NR_CANDIDATO, NR_CPF_CANDIDATO)

indicators <- indicators %>%
  left_join(profile, by = c("ANO_ELEICAO", "NR_VOTAVEL")) %>%
  left_join(candidatos, by = c("ANO_ELEICAO", "NR_VOTAVEL" = "NR_CANDIDATO")) %>%
  select(ANO_ELEICAO, NM_URNA_CANDIDATO, D, G, QT_VOTOS_VER_TOTAL, PCT_VOTOS_VER_TOTAL, DS_SIT_TOT_TURNO, NR_CPF_CANDIDATO)
indicators$NM_URNA_CANDIDATO <- iconv(indicators$NM_URNA_CANDIDATO, from = 'UTF-8', to = 'ASCII//TRANSLIT')

tipo <- indicators %>% subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"))

tipo$tipo <- NA

tipo$tipo[tipo$ANO_ELEICAO == 2016 &
            tipo$D > mean(tipo$D[tipo$ANO_ELEICAO == 2016], na.rm = T) &
            tipo$G > mean(tipo$G[tipo$ANO_ELEICAO == 2016], na.rm = T)] <- "DOMINANT-CONCENTRATED"
tipo$tipo[tipo$ANO_ELEICAO == 2016 &
            tipo$D <= mean(tipo$D[tipo$ANO_ELEICAO == 2016], na.rm = T) &
            tipo$G > mean(tipo$G[tipo$ANO_ELEICAO == 2016], na.rm = T)] <- "SHARED-CONCENTRATED"
tipo$tipo[tipo$ANO_ELEICAO == 2016 &
            tipo$D > mean(tipo$D[tipo$ANO_ELEICAO == 2016], na.rm = T) &
            tipo$G <= mean(tipo$G[tipo$ANO_ELEICAO == 2016], na.rm = T)] <- "DOMINANT-SCATTERED"
tipo$tipo[tipo$ANO_ELEICAO == 2016 &
            tipo$D <= mean(tipo$D[tipo$ANO_ELEICAO == 2016], na.rm = T) &
            tipo$G <= mean(tipo$G[tipo$ANO_ELEICAO == 2016], na.rm = T)] <- "SHARED-SCATTERED"

tipo$tipo[tipo$ANO_ELEICAO == 2020 &
            tipo$D > mean(tipo$D[tipo$ANO_ELEICAO == 2020], na.rm = T) &
            tipo$G > mean(tipo$G[tipo$ANO_ELEICAO == 2020], na.rm = T)] <- "DOMINANT-CONCENTRATED"
tipo$tipo[tipo$ANO_ELEICAO == 2020 &
            tipo$D <= mean(tipo$D[tipo$ANO_ELEICAO == 2020], na.rm = T) &
            tipo$G > mean(tipo$G[tipo$ANO_ELEICAO == 2020], na.rm = T)] <- "SHARED-CONCENTRATED"
tipo$tipo[tipo$ANO_ELEICAO == 2020 &
            tipo$D > mean(tipo$D[tipo$ANO_ELEICAO == 2020], na.rm = T) &
            tipo$G <= mean(tipo$G[tipo$ANO_ELEICAO == 2020], na.rm = T)] <- "DOMINANT-SCATTERED"
tipo$tipo[tipo$ANO_ELEICAO == 2020 &
            tipo$D <= mean(tipo$D[tipo$ANO_ELEICAO == 2020], na.rm = T) &
            tipo$G <= mean(tipo$G[tipo$ANO_ELEICAO == 2020], na.rm = T)] <- "SHARED-SCATTERED"

tipo <- tipo %>% select(ANO_ELEICAO, NM_URNA_CANDIDATO, tipo)

plot_grpfrq(tipo$tipo, tipo$ANO_ELEICAO,
         coord.flip = T, show.n = F, geom.colors = "gs")

iba_sp <- read_delim("02_out/iba_sp.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                         grouping_mark = "."), trim_ws = TRUE) %>% 
  left_join(indicators, by = c("election_year" = "ANO_ELEICAO", "councilor" = "NM_URNA_CANDIDATO"))

iba_sp <- iba_sp %>% left_join(tipo, by = c("election_year" = "ANO_ELEICAO", "councilor" = "NM_URNA_CANDIDATO"))

iba_sp %>% group_by(executor) %>% summarise(value = sum(value)) %>%
  mutate(pct = value/sum(value)) %>% arrange(-value)

iba_sp %>% group_by(sub) %>% summarise(value = sum(value)) %>%
  mutate(pct = value/sum(value))

plot_frq(iba_sp$executor, weight.by = iba_sp$value,
         coord.flip = T, show.n = F, sort.frq = c("asc"))

plot_frq(iba_sp$sub, weight.by = iba_sp$value,
         coord.flip = T, show.n = F, sort.frq = c("asc"))

plot_frq(iba_sp$policy, weight.by = iba_sp$value,
         coord.flip = T, show.n = F, sort.frq = c("asc"))

plot_frq(iba_sp$region_5, weight.by = iba_sp$value,
         coord.flip = T, show.n = F, sort.frq = c("asc"))

plot_frq(iba_sp$region_8, weight.by = iba_sp$value,
         coord.flip = T, show.n = F, sort.frq = c("asc"))

plot_frq(iba_sp$ideology, weight.by = iba_sp$value,
         coord.flip = T, show.n = F, sort.frq = c("asc"))

aux_sp <- iba_sp %>% subset(sub == 1) %>% group_by(executor, election_year) %>% summarise(value = sum(value))

shape <- read_sf(str_c("01_in/shapefiles/SIRGAS_SHP_subprefeitura_polygon.shp")) %>% 
  st_transform(31983) %>% 
  st_make_valid() %>% 
  mutate(sp_nome = str_c("SUBPREFEITURA ", sp_nome)) %>% 
  left_join(aux_sp, by = c("sp_nome" = "executor"))

ggplot(shape) + 
  geom_sf(aes(fill = value), size = 0) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "", 
       subtitle = "",
       caption = "",
       fill = "IBAs resources",
       x = "", y = "") +
  #facet_grid(~TYPO) +
  facet_grid(~election_year) +
  theme_void()

votes_ter <- read_delim("02_out/ql_councilor_sp.csv", 
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                  grouping_mark = "."), trim_ws = TRUE) %>%
  distinct(ANO_ELEICAO, sp_nome, QT_VOTOS_TER)

shape <- read_sf(str_c("01_in/shapefiles/SIRGAS_SHP_subprefeitura_polygon.shp")) %>% 
  st_transform(31983) %>% 
  st_make_valid() %>% 
  mutate(sp_nome = str_c("SUBPREFEITURA ", sp_nome)) %>% 
  left_join(votes_ter, by = c("sp_nome"))

ggplot(shape) + 
  geom_sf(aes(fill = QT_VOTOS_TER), size = 0) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "", 
       subtitle = "",
       caption = "",
       fill = "Valid Votes",
       x = "", y = "") +
  #facet_grid(~TYPO) +
  facet_grid(~ANO_ELEICAO) +
  theme_void()

plot_xtab(iba_sp$tipo, iba_sp$policy, weight.by = iba_sp$value,
          margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = TRUE,
          show.prc = T, show.n = F)

aux_iba_sp <- iba_sp %>% subset(sub == 1)
  
plot_xtab(iba_sp$tipo, iba_sp$region_5, weight.by = iba_sp$value,
          margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = TRUE,
          show.prc = T, show.n = F)

############################

ql_typo_sp <- read_delim("02_out/ql_typo_sp.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE) %>% 
  mutate(CD_APONDE = as.character(CD_APONDE))

ano <- "2016"
mun <- "SÃO PAULO"

shape <- read_sf(str_c("01_in/shapefiles/", iconv(mun, from = 'UTF-8', to = 'ASCII//TRANSLIT'), "_area_de_ponderacao.shp")) %>%
  st_transform(31983) %>% 
  rename(any_of(c("CD_APONDE" = "CD_APonde"))) %>% 
  st_make_valid() %>% 
  #left_join(select(subset(ql_typo_sp, ANO_ELEICAO == ano), TYPO, CD_APONDE, QL), by = "CD_APONDE") %>% 
  left_join(select(ql_typo_sp, ANO_ELEICAO, TYPO, CD_APONDE, QL), by = "CD_APONDE") %>% 
  subset(!is.na(TYPO))

ggplot(shape) + 
  geom_sf(aes(fill = QL), size = 0) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "", 
       subtitle = "",
       caption = "",
       fill = "QL",
       x = "", y = "") +
  #facet_grid(~TYPO) +
  facet_grid(TYPO ~ ANO_ELEICAO, switch = "y") +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 90))

######################

ql_councilor_sp <- read_delim("02_out/ql_councilor_sp.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE) #%>% 
  #mutate(NM_URNA_CANDIDATO = iconv(NM_URNA_CANDIDATO, from = 'UTF-8', to = 'ASCII//TRANSLIT'))


iba_sp_sub <- read_delim("02_out/iba_sp.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                         grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(sub == 1)

iba_sp_sub <- iba_sp_sub %>% 
  left_join(select(ql_councilor_sp, ANO_ELEICAO, NM_URNA_CANDIDATO, sp_nome, QL), by = c("election_year" = "ANO_ELEICAO", "councilor" = "NM_URNA_CANDIDATO", "executor" = "sp_nome")) %>% 
  left_join(distinct(indicators, ANO_ELEICAO, NM_URNA_CANDIDATO, NR_CPF_CANDIDATO), by = c("election_year" = "ANO_ELEICAO", "councilor" = "NM_URNA_CANDIDATO"))

iba_sp_sub %>% 
  group_by(councilor, QL) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ggplot(aes(x = QL, y = value)) +
  geom_point() +
  geom_smooth(method = "lm")

# teste <- iba_sp_sub
# intervals <- classIntervals(teste$QL, 15, style = "equal")
# teste$classe <- cut(teste$QL, breaks = intervals$brks, include.lowest = T)
# teste %>%
#   group_by(classe) %>%
#   summarise(value = mean(value, na.rm = T), N = n()) %>% 
#   ggplot(aes(x = classe, y = value)) +
#   geom_point(aes(size = N)) +
#   geom_smooth()

lisa_councilor_sp <- read_delim("02_out/lisa_councilor_sp.csv", 
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                  grouping_mark = "."), trim_ws = TRUE) %>% 
  left_join(distinct(indicators, ANO_ELEICAO, NM_URNA_CANDIDATO, NR_CPF_CANDIDATO), by = c("ANO_ELEICAO", "NM_URNA_CANDIDATO"))

iba_sp_sub <- iba_sp_sub %>%
  subset(election_year == 2016) %>% 
  mutate(election_year_t_4 = election_year + 4) %>%
  left_join(select(lisa_councilor_sp, NR_CPF_CANDIDATO, sp_nome, ANO_ELEICAO, SD, LISA), by = c("NR_CPF_CANDIDATO", "executor" = "sp_nome", "election_year" = "ANO_ELEICAO")) %>% 
  left_join(select(lisa_councilor_sp, NR_CPF_CANDIDATO, sp_nome, ANO_ELEICAO, SD, LISA), by = c("NR_CPF_CANDIDATO", "executor" = "sp_nome", "election_year_t_4" = "ANO_ELEICAO"))

teste <- nrow(iba_sp_sub)
iba_sp_sub <- na.omit(iba_sp_sub)
print(nrow(iba_sp_sub) / teste)

iba_sp_sub$iba_use <- "Nonterritorial"
iba_sp_sub$iba_use[iba_sp_sub$LISA.x %in% c("Alto-Alto", "Alto-Baixo") | iba_sp_sub$SD.x >= 5] <- "Contingent"
iba_sp_sub$iba_use[iba_sp_sub$iba_use == "Nonterritorial" & (iba_sp_sub$LISA.y %in% c("Alto-Alto", "Alto-Baixo") | iba_sp_sub$SD.y >= 5)] <- "Noncontingent"

plot_frq(iba_sp_sub$iba_use)

iba_sp_sub$iba_use <- factor(iba_sp_sub$iba_use, levels = c("Nonterritorial", "Noncontingent", "Contingent"))

nec_sp <- read_delim("02_out/nec_sp.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                                                                                  grouping_mark = "."), trim_ws = TRUE)
idhm <- read_csv("01_in/geosampa/DEINFO_IDH_UDH_2000_2010_Dados.csv", 
                 locale = locale(encoding = "latin1")) %>% 
  clean_names() %>% 
  group_by(prefreg) %>% 
  summarise(median_idhm = median(idhm, na.rm  = T)) %>% 
  mutate(prefreg = str_replace_all(str_to_upper(str_c("SUBPREFEITURA ", prefreg)), "/", "-"))
idhm$prefreg <- iconv(idhm$prefreg, from = 'UTF-8', to = 'ASCII//TRANSLIT')

ter <- lisa_councilor_sp %>% 
  group_by(ANO_ELEICAO, NM_URNA_CANDIDATO) %>% 
  summarise(N_TER = sum(LISA %in% c("Alto-Alto", "Alto-Baixo") | SD >= 5, na.rm = T))

rank <- read_delim("02_out/indicators.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO == "SÃO PAULO")

candidatos <- read_delim("02_out/margins.csv", 
                         delim = ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO == "SÃO PAULO") %>% 
  select(ANO_ELEICAO, NR_CANDIDATO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO)

profile <- read_delim("02_out/profile.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO == "SÃO PAULO") %>% 
  select(ANO_ELEICAO, NR_VOTAVEL, NM_URNA_CANDIDATO)

rank <- rank %>%
  left_join(candidatos, by = c("ANO_ELEICAO", "NR_VOTAVEL" = "NR_CANDIDATO")) %>% 
  left_join(profile, by = c("ANO_ELEICAO", "NR_VOTAVEL")) %>%
  arrange(ANO_ELEICAO, DS_COMPOSICAO_COLIGACAO, -PCT_VOTOS_VER_TOTAL) %>% 
  group_by(ANO_ELEICAO, DS_COMPOSICAO_COLIGACAO) %>% 
  mutate(RANK = rank(-PCT_VOTOS_VER_TOTAL)) %>% 
  ungroup() %>% 
  group_by(ANO_ELEICAO, DS_COMPOSICAO_COLIGACAO) %>% 
  mutate(LIST = max(RANK[DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")]),
         LIST = case_when(is.infinite(LIST) ~ NA,
                          TRUE ~ LIST)) %>%
  ungroup() %>% 
  mutate(RANK = ((RANK + 1) / (LIST + 2))) %>% 
  select(ANO_ELEICAO, NM_URNA_CANDIDATO, RANK)
  
iba_sp_sub <- iba_sp_sub %>%
  left_join(nec_sp, by = c("election_year" = "ANO_ELEICAO", "executor" = "sp_nome")) %>%
  left_join(ter, by = c("election_year" = "ANO_ELEICAO", "councilor" = "NM_URNA_CANDIDATO")) %>% 
  left_join(idhm, by = c("executor" = "prefreg")) %>%
  left_join(rank, by = c("election_year" = "ANO_ELEICAO", "councilor" = "NM_URNA_CANDIDATO")) %>% 
  group_by(councilor) %>% mutate(N_IBA = n()) %>% ungroup()

model <- multinom(iba_use ~ QL + NEC + RANK + median_idhm + N_IBA + N_TER, data = iba_sp_sub)

stargazer(model)

tab_model(model)
plot_model(model, type = "pred", terms = "QL [all]")

median(iba_sp_sub$QL, na.rm = T)
sd(iba_sp_sub$QL, na.rm = T)

new <- data.frame(QL = c(1:20), NEC = median(iba_sp_sub$NEC, na.rm = T), RANK = median(iba_sp_sub$RANK, na.rm = T),
                  median_idhm = median(iba_sp_sub$median_idhm, na.rm = T), N_IBA = median(iba_sp_sub$N_IBA, na.rm = T),
                  N_TER = median(iba_sp_sub$N_TER, na.rm = T))

predict(model, newdata = new, type = "probs")
