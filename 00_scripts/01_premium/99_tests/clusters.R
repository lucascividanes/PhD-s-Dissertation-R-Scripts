library(tidyverse)
library(sjPlot)
library(FactoMineR)
library(factoextra)

profile <- read_delim("02_out/profile.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE)

indicators <- read_delim("02_out/indicators.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE)

df <- profile %>%
  left_join(select(indicators, ANO_ELEICAO, NM_MUNICIPIO, NR_VOTAVEL, G, PCT_VOTOS_VER_TOTAL), by = c("ANO_ELEICAO", "NM_MUNICIPIO", "NR_VOTAVEL")) %>% 
  subset(NM_MUNICIPIO == "SÃO PAULO" & PCT_VOTOS_VER_TOTAL > 0.001)

left <- c("PC do B", "PCB", "PCO", "PDT", "PSB", "PSOL", "PSTU", "PT", "UP", "PPL")
center <- c("CIDADANIA", "MDB", "PATRIOTA", "PMB", "PMN", "PROS", "PSD", "PSDB", "PV", "REDE", "SOLIDARIEDADE", "PMDB", "PPS", "PRP", "SD", "PEN")
right <- c("AVANTE", "DC", "NOVO", "DEM", "PL", "PODE", "PP", "PRTB", "PSC", "PSL", "PTB", "PTC", "REPUBLICANOS", "PRB", "PTN", "PR", "PSDC", "PHS", "PT do B")

df$IDEOLOGIA <- NA
df$IDEOLOGIA[df$SG_PARTIDO %in% left] <- "LEFT"
df$IDEOLOGIA[df$SG_PARTIDO %in% center] <- "CENTER"
df$IDEOLOGIA[df$SG_PARTIDO %in% right] <- "RIGHT"

df_clust <- df %>% select(PCT_ELEITORES_PERFIL_GENERO_FEM,
                          PCT_ELEITORES_PERFIL_FX_ETARIA_16A34,
                          PCT_ELEITORES_PERFIL_ESC_FUND,
                          DIST,
                          G,
                          PCT_VOTOS_VER_TOTAL)

res.pca <- PCA(df_clust)
res.pca$eig 
res.pca <- PCA(df_clust, n = 2) ########ajustar n para 50% da variancia

res.hcpc <- HCPC(res.pca, nb.clust = 2) ########ajustar n para maior ganho de informacao
fviz_cluster(res.hcpc)

df <- cbind(df, res.hcpc$data.clust["clust"])

grupos <- df %>% 
  group_by(clust) %>% 
  summarise(N = n(),
            PCT_ESQUERDA = sum(IDEOLOGIA %in% c("LEFT"), na.rm = T) / N,
            PCT_ELEITO = sum(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP"), na.rm = T) / N,
            QT_VOTOS_NOMINAIS = median(QT_VOTOS_NOMINAIS, na.rm = T),
            PCT_ELEITORES_PERFIL_GENERO_FEM = median(PCT_ELEITORES_PERFIL_GENERO_FEM, na.rm = T),
            PCT_ELEITORES_PERFIL_GENERO_MASC = median(PCT_ELEITORES_PERFIL_GENERO_MASC, na.rm = T),
            PCT_ELEITORES_PERFIL_FX_ETARIA_16A34 = median(PCT_ELEITORES_PERFIL_FX_ETARIA_16A34, na.rm = T),
            PCT_ELEITORES_PERFIL_FX_ETARIA_35A59 = median(PCT_ELEITORES_PERFIL_FX_ETARIA_35A59, na.rm = T),
            PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS = median(PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS, na.rm = T),
            PCT_ELEITORES_PERFIL_ESC_FUND = median(PCT_ELEITORES_PERFIL_ESC_FUND, na.rm = T),
            PCT_ELEITORES_PERFIL_ESC_MED = median(PCT_ELEITORES_PERFIL_ESC_MED, na.rm = T),
            PCT_ELEITORES_PERFIL_ESC_SUP = median(PCT_ELEITORES_PERFIL_ESC_SUP, na.rm = T),
            DIST = median(DIST, na.rm = T),
            G = median(G, na.rm = T),
            PCT_VOTOS_VER_TOTAL = median(PCT_VOTOS_VER_TOTAL, na.rm = T))

grupos <- as.data.frame(t(grupos))
