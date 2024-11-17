library(tidyverse)
library(rdrobust)
library(sjPlot)
library(sandwich) 
library(lmtest)
library(knitr)

candidatos <- read_delim("02_out/margins.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(ANO_ELEICAO = col_character(), 
                                                                              NR_CANDIDATO = col_character()), 
                         locale = locale(decimal_mark = ",", grouping_mark = "."), 
                         trim_ws = TRUE) 

indicadores <- read_delim("02_out/indicators.csv", 
                          delim = ";", escape_double = FALSE, col_types = cols(ANO_ELEICAO = col_character(), 
                                                                               NR_VOTAVEL = col_character()),
                          locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  select(ANO_ELEICAO, NM_MUNICIPIO, NR_VOTAVEL, D, HH, G)

perfil <- read_delim("02_out/profile.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                         grouping_mark = "."),
                     col_types = cols(ANO_ELEICAO = col_character(), 
                                      NR_VOTAVEL = col_character()), trim_ws = TRUE) %>% 
  select(ANO_ELEICAO, NM_MUNICIPIO, NR_VOTAVEL, PCT_ELEITORES_PERFIL_GENERO_FEM:PCT_ELEITORES_PERFIL_ESC_SUP)

distrito <- read_delim("02_out/dist_mag.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                           grouping_mark = "."),
                       col_types = cols(ANO_ELEICAO = col_character()), trim_ws = TRUE) %>% 
  select(ANO_ELEICAO, NM_MUNICIPIO, DIST_MAG)

candidatos <- candidatos %>% 
  left_join(indicadores, by = c("ANO_ELEICAO", "NM_MUNICIPIO", "NR_CANDIDATO" = "NR_VOTAVEL")) %>% 
  left_join(perfil, by = c("ANO_ELEICAO", "NM_MUNICIPIO", "NR_CANDIDATO" = "NR_VOTAVEL")) %>% 
  left_join(distrito, by = c("ANO_ELEICAO", "NM_MUNICIPIO"))

candidatos %>%
  subset(!is.na(D) & !is.na(ST_REELEICAO) & ST_REELEICAO != "Não divulgável") %>% 
  ggplot(aes(x = ST_REELEICAO, y = D, fill = ST_REELEICAO)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~ANO_ELEICAO)

candidatos %>%
  subset(!is.na(G) & !is.na(ST_REELEICAO) & ST_REELEICAO != "Não divulgável") %>% 
  ggplot(aes(x = ST_REELEICAO, y = G, fill = ST_REELEICAO)) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~ANO_ELEICAO)

median(candidatos$D[candidatos$ST_REELEICAO == "S"], na.rm = T)
median(candidatos$D[candidatos$ST_REELEICAO == "N"], na.rm = T)

median(candidatos$G[candidatos$ST_REELEICAO == "S"], na.rm = T)
median(candidatos$G[candidatos$ST_REELEICAO == "N"], na.rm = T)

df <- candidatos %>%
  subset(!is.na(NR_CPF_CANDIDATO)) %>%
  distinct(ANO_ELEICAO, NR_CPF_CANDIDATO, .keep_all = T) %>%
  subset(ANO_ELEICAO == "2020") %>% 
  left_join(subset(candidatos, ANO_ELEICAO == "2016", select = c("NR_CPF_CANDIDATO", "DS_SIT_TOT_TURNO", "QT_VOTOS", "MARGEM", "ST_REELEICAO",
                                                                 "D", "HH", "G",
                                                                 "PCT_ELEITORES_PERFIL_GENERO_FEM", "PCT_ELEITORES_PERFIL_GENERO_MASC",     
                                                                 "PCT_ELEITORES_PERFIL_FX_ETARIA_16A34", "PCT_ELEITORES_PERFIL_FX_ETARIA_35A59", 
                                                                 "PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS", "PCT_ELEITORES_PERFIL_ESC_FUND",        
                                                                 "PCT_ELEITORES_PERFIL_ESC_MED", "PCT_ELEITORES_PERFIL_ESC_SUP",
                                                                 "DIST_MAG")), by = "NR_CPF_CANDIDATO") %>% 
  rename("DS_SIT_TOT_TURNO_2020" = "DS_SIT_TOT_TURNO.x", "DS_SIT_TOT_TURNO_2016" = "DS_SIT_TOT_TURNO.y",
         "QT_VOTOS_2020" = "QT_VOTOS.x", "QT_VOTOS_2016" = "QT_VOTOS.y",
         "MARGEM_2020" = "MARGEM.x", "MARGEM_2016" = "MARGEM.y",
         "ST_REELEICAO_2020" = "ST_REELEICAO.x", "ST_REELEICAO_2016" = "ST_REELEICAO.y",
         "D_2020" = "D.x", "D_2016" = "D.y",
         "HH_2020" = "HH.x","HH_2016" = "HH.y",
         "G_2020" = "G.x", "G_2016" = "G.y",
         "PCT_ELEITORES_PERFIL_GENERO_FEM_2020" = "PCT_ELEITORES_PERFIL_GENERO_FEM.x",
         "PCT_ELEITORES_PERFIL_GENERO_FEM_2016" = "PCT_ELEITORES_PERFIL_GENERO_FEM.y",
         "PCT_ELEITORES_PERFIL_GENERO_MASC_2020" = "PCT_ELEITORES_PERFIL_GENERO_MASC.x",
         "PCT_ELEITORES_PERFIL_GENERO_MASC_2016" = "PCT_ELEITORES_PERFIL_GENERO_MASC.y",
         "PCT_ELEITORES_PERFIL_FX_ETARIA_16A34_2020" = "PCT_ELEITORES_PERFIL_FX_ETARIA_16A34.x",
         "PCT_ELEITORES_PERFIL_FX_ETARIA_16A34_2016" = "PCT_ELEITORES_PERFIL_FX_ETARIA_16A34.y",
         "PCT_ELEITORES_PERFIL_FX_ETARIA_35A59_2020" = "PCT_ELEITORES_PERFIL_FX_ETARIA_35A59.x",
         "PCT_ELEITORES_PERFIL_FX_ETARIA_35A59_2016" = "PCT_ELEITORES_PERFIL_FX_ETARIA_35A59.y",
         "PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS_2020" = "PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS.x",
         "PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS_2016" = "PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS.y",
         "PCT_ELEITORES_PERFIL_ESC_FUND_2020" = "PCT_ELEITORES_PERFIL_ESC_FUND.x",
         "PCT_ELEITORES_PERFIL_ESC_FUND_2016" = "PCT_ELEITORES_PERFIL_ESC_FUND.y",
         "PCT_ELEITORES_PERFIL_ESC_MED_2020" = "PCT_ELEITORES_PERFIL_ESC_MED.x",
         "PCT_ELEITORES_PERFIL_ESC_MED_2016" = "PCT_ELEITORES_PERFIL_ESC_MED.y",
         "PCT_ELEITORES_PERFIL_ESC_SUP_2020" = "PCT_ELEITORES_PERFIL_ESC_SUP.x",
         "PCT_ELEITORES_PERFIL_ESC_SUP_2016" = "PCT_ELEITORES_PERFIL_ESC_SUP.y",
         "DIST_MAG_2020" = "DIST_MAG.x",
         "DIST_MAG_2016" = "DIST_MAG.y") %>% 
  mutate(d_D_2020_2016 = D_2020 - D_2016,
         d_HH_2020_2016 = HH_2020 - HH_2016,
         d_G_2020_2016 = G_2020 - G_2016,
         TRATAMENTO_2016 = case_when(DS_SIT_TOT_TURNO_2016 %in% c("ELEITO POR QP", "ELEITO POR MÉDIA") ~ "1",
                                     DS_SIT_TOT_TURNO_2016 %in% c("SUPLENTE", "NÃO ELEITO") ~ "0"))

df %>%
  subset(!is.na(d_D_2020_2016) & !is.na(TRATAMENTO_2016)) %>% 
  ggplot(aes(x = TRATAMENTO_2016, y = d_D_2020_2016, fill = TRATAMENTO_2016)) +
  geom_boxplot() +
  theme_minimal()

df %>%
  subset(!is.na(d_G_2020_2016) & !is.na(TRATAMENTO_2016)) %>% 
  ggplot(aes(x = TRATAMENTO_2016, y = d_G_2020_2016, fill = TRATAMENTO_2016)) +
  geom_boxplot() +
  theme_minimal()

mean(df$d_D_2020_2016[df$TRATAMENTO_2016 == 1], na.rm = T)
mean(df$d_D_2020_2016[df$TRATAMENTO_2016 == 0], na.rm = T)

mean(df$d_G_2020_2016[df$TRATAMENTO_2016 == 1], na.rm = T)
mean(df$d_G_2020_2016[df$TRATAMENTO_2016 == 0], na.rm = T)

df %>%
  subset(!is.na(d_D_2020_2016)) %>% 
  ggplot(aes(x = MARGEM_2016, y = d_D_2020_2016, color = TRATAMENTO_2016)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

df %>%
  subset(!is.na(d_D_2020_2016)) %>% 
  ggplot(aes(x = D_2016, y = d_D_2020_2016, color = TRATAMENTO_2016)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

df %>%
  subset(!is.na(d_G_2020_2016)) %>% 
  ggplot(aes(x = MARGEM_2016, y = d_G_2020_2016, color = TRATAMENTO_2016)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

df %>%
  subset(!is.na(d_G_2020_2016)) %>% 
  ggplot(aes(x = G_2016, y = d_G_2020_2016, color = TRATAMENTO_2016)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

df_rdd <- df %>%
  subset(MARGEM_2016 <= 0.20 & MARGEM_2016 >= -0.20 & !is.na(d_D_2020_2016))
tab_balance <- data.frame()

var_balance <-  data.frame(nome = c("Percentage of female electorate (2016)",
                                    "Percentage of male electorate (2016)",
                                    "Percentage of electorate with middle school as highest education (2016)",
                                    "Percentage of electorate with high school as highest education (2016)",
                                    "Percentage of electorate with college as highest education (2016)",
                                    "Percentage of electorate with 16 to 34 years old (2016)",
                                    "Percentage of electorate with 35 to 59 years old (2016)",
                                    "Percentage of electorate with 60 years old or more (2016)"),
                           var = c("PCT_ELEITORES_PERFIL_GENERO_FEM_2016",
                                   "PCT_ELEITORES_PERFIL_GENERO_MASC_2016",
                                   "PCT_ELEITORES_PERFIL_ESC_FUND_2016",
                                   "PCT_ELEITORES_PERFIL_ESC_MED_2016",
                                   "PCT_ELEITORES_PERFIL_ESC_SUP_2016",
                                   "PCT_ELEITORES_PERFIL_FX_ETARIA_16A34_2016",
                                   "PCT_ELEITORES_PERFIL_FX_ETARIA_35A59_2016",
                                   "PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS_2016"))

for (i in var_balance$var){
  aux <- df_rdd %>% select(VARIABLE = paste(i), MARGEM_2016)
  out <- rdrobust(aux$VARIABLE, aux$MARGEM_2016)
  tab_balance <- rbind(tab_balance, cbind(Variable = var_balance$nome[var_balance$var == i],
                                          Bandwidth = round(out$bws[1, 1], 4),
                                          Coef = round(out$coef[3, 1], 4),
                                          `p-value` = round(out$pv[3, 1], 4),
                                          CI = str_c("[", round(out$ci[3, 1], 4), ", ", round(out$ci[3, 2], 4), "]"),
                                          `Effective N` = sum(out$N)))
  
}

kable(tab_balance, caption = "Balance Tests for Ecological Constituencies' Features. Source: author. Data: TSE.")

out <- rdplot(df_rdd$d_D_2020_2016, df_rdd$MARGEM_2016, nbins = c(20, 20), binselect = "es", x.label = "MARGEM_2016", y.label = "d_D_2020_2016")
out <- rdrobust(df_rdd$d_D_2020_2016, df_rdd$MARGEM_2016, kernel = "triangular", p = 1, bwselect = "mserd")
summary(out)
out <- rdrobust(df_rdd$d_D_2020_2016, df_rdd$MARGEM_2016, kernel = "triangular", p = 1, bwselect = "cerrd")
summary(out)

0.007/median(df$D_2016, na.rm = T)

out <- rdplot(df_rdd$d_G_2020_2016, df_rdd$MARGEM_2016, nbins = c(20, 20), binselect = "es", x.label = "MARGEM_2016", y.label = "d_D_2020_2016")
out <- rdrobust(df_rdd$d_G_2020_2016, df_rdd$MARGEM_2016, kernel = "triangular", p = 1, bwselect = "mserd")
summary(out)
out <- rdrobust(df_rdd$d_G_2020_2016, df_rdd$MARGEM_2016, kernel = "triangular", p = 1, bwselect = "cerrd")
summary(out)

0.028/median(df$G_2016, na.rm = T)

out <- rdbwselect(df_rdd$d_D_2020_2016, df_rdd$MARGEM_2016, kernel = "triangular", p = 1, all = TRUE)
summary(out)

df_rdd_bw <- subset(df_rdd, MARGEM_2016 <= 0.074 & MARGEM_2016 >= -0.074)

mod <- lm(d_D_2020_2016 ~ TRATAMENTO_2016*D_2016 + MARGEM_2016, data = df_rdd_bw)
summary(mod)
mod_cluster <- vcovCL(mod, cluster = df_rdd_bw$CD_MUNICIPIO)
coeftest(mod, vcov. = mod_cluster)
confint(mod, vcov. = mod_cluster)

df_rdd_bw %>% ggplot(aes(x = D_2016)) + geom_density()
median(df_rdd_bw$D_2016)

set_theme(theme_minimal())
plot_model(mod, type = "pred", terms = c("TRATAMENTO_2016", "D_2016 [0.010, 0.020]"))

out <- rdbwselect(df_rdd$d_G_2020_2016, df_rdd$MARGEM_2016, kernel = "triangular", p = 1, all = TRUE)
summary(out)

df_rdd_bw <- subset(df_rdd, MARGEM_2016 <= 0.057 & MARGEM_2016 >= -0.057)

mod <- lm(d_G_2020_2016 ~ TRATAMENTO_2016*G_2016 + MARGEM_2016, data = df_rdd_bw)
summary(mod)
mod_cluster <- vcovCL(mod, cluster = df_rdd_bw$CD_MUNICIPIO)
coeftest(mod, vcov. = mod_cluster)
confint(mod, vcov. = mod_cluster)

df_rdd_bw %>% ggplot(aes(x = G_2016)) + geom_density()
median(df_rdd_bw$G_2016)

set_theme(theme_minimal())
plot_model(mod, type = "pred", terms = c("TRATAMENTO_2016", "G_2016 [0.020, 0.040]"))