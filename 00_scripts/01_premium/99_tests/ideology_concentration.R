library(tidyverse)
library(sjPlot)

indicators <- read_delim("02_out/indicators.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE)

profile <- read_delim("02_out/profile.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE)


profile <- profile %>% left_join(select(indicators, ANO_ELEICAO, NM_MUNICIPIO, NR_VOTAVEL, D, G, PCT_VOTOS_VER_TOTAL), by = c("ANO_ELEICAO", "NM_MUNICIPIO", "NR_VOTAVEL"))

#ideology

left <- c("PC do B", "PCB", "PCO", "PDT", "PSB", "PSOL", "PSTU", "PT", "UP", "PPL")
center <- c("CIDADANIA", "MDB", "PATRIOTA", "PMB", "PMN", "PROS", "PSD", "PSDB", "PV", "REDE", "SOLIDARIEDADE", "PMDB", "PPS", "PRP", "SD", "PEN")
right <- c("AVANTE", "DC", "NOVO", "DEM", "PL", "PODE", "PP", "PRTB", "PSC", "PSL", "PTB", "PTC", "REPUBLICANOS", "PRB", "PTN", "PR", "PSDC", "PHS", "PT do B")

profile$IDEOLOGIA <- NA
profile$IDEOLOGIA[profile$SG_PARTIDO %in% left] <- "LEFT"
profile$IDEOLOGIA[profile$SG_PARTIDO %in% center] <- "CENTER"
profile$IDEOLOGIA[profile$SG_PARTIDO %in% right] <- "RIGHT"

profile %>%
  ggplot(aes(x = PCT_ELEITORES_PERFIL_ESC_SUP, y = PCT_VOTOS_VER_TOTAL, color = IDEOLOGIA)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

profile %>%
  subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>% 
  ggplot(aes(x = PCT_ELEITORES_PERFIL_ESC_SUP, y = PCT_VOTOS_VER_TOTAL, color = IDEOLOGIA)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

profile %>%
  subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>% 
  ggplot(aes(x = G, y = D, color = IDEOLOGIA)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

mod <- lm(PCT_ELEITORES_PERFIL_ESC_FUND ~ IDEOLOGIA, data = profile)
summary(mod)
plot_model(mod, type = "pred", terms = "IDEOLOGIA")

eleitos <- profile %>%
  subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"))

set.seed(1)
km <- kmeans(select(eleitos, D, G), centers = 4, nstart = 25)
km

eleitos$cluster <- as.character(km$cluster)

eleitos %>%
  #subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>% 
  ggplot(aes(x = D, y = G, color = cluster)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()
