library(tidyverse)

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

ano <- "2016"
mun <- "MANAUS"

voronoi <- read_delim("02_out/voronoi.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO == mun)

perfil_eleitor_secao <- read_delim(str_c("01_in/perfil_eleitor_secao_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"), 
                                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                       grouping_mark = ".", encoding = "latin1"), 
                                   trim_ws = TRUE, col_types = cols(CD_MUNICIPIO = col_double())) %>% 
  subset(NM_MUNICIPIO == mun) %>% 
  group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NR_ZONA, NR_SECAO, DIST) %>% 
  summarise(QT_ELEITORES = sum(QT_ELEITORES_PERFIL, na.rm = T),
            QT_ELEITORES_PERFIL_GENERO_FEM = sum(QT_ELEITORES_PERFIL[CD_GENERO == 4], na.rm = T),
            QT_ELEITORES_PERFIL_GENERO_MASC = sum(QT_ELEITORES_PERFIL[CD_GENERO == 2], na.rm = T),
            QT_ELEITORES_PERFIL_FX_ETARIA_16A34 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA %in% c(1600, 1700, 1800, 1900, 2000, 2124, 2529, 3034)], na.rm = T),
            QT_ELEITORES_PERFIL_FX_ETARIA_35A59 = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA %in% c(3539, 4044, 4549, 5054, 5559)], na.rm = T),
            QT_ELEITORES_PERFIL_FX_ETARIA_60MAIS = sum(QT_ELEITORES_PERFIL[CD_FAIXA_ETARIA %in% c(6064, 6569, 7074, 7579, 8084, 8589, 9094, 9599, 9999)], na.rm = T),
            QT_ELEITORES_PERFIL_ESC_FUND = sum(QT_ELEITORES_PERFIL[CD_GRAU_ESCOLARIDADE %in% c(1, 2, 3, 4)], na.rm = T),
            QT_ELEITORES_PERFIL_ESC_MED = sum(QT_ELEITORES_PERFIL[CD_GRAU_ESCOLARIDADE %in% c(5, 6)], na.rm = T),
            QT_ELEITORES_PERFIL_ESC_SUP = sum(QT_ELEITORES_PERFIL[CD_GRAU_ESCOLARIDADE %in% c(7, 8)], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(PCT_ELEITORES_PERFIL_GENERO_FEM = QT_ELEITORES_PERFIL_GENERO_FEM / QT_ELEITORES,
         PCT_ELEITORES_PERFIL_GENERO_MASC = QT_ELEITORES_PERFIL_GENERO_MASC / QT_ELEITORES,
         PCT_ELEITORES_PERFIL_FX_ETARIA_16A34 = QT_ELEITORES_PERFIL_FX_ETARIA_16A34 / QT_ELEITORES,
         PCT_ELEITORES_PERFIL_FX_ETARIA_35A59 = QT_ELEITORES_PERFIL_FX_ETARIA_35A59 / QT_ELEITORES,
         PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS = QT_ELEITORES_PERFIL_FX_ETARIA_60MAIS / QT_ELEITORES,
         PCT_ELEITORES_PERFIL_ESC_FUND = QT_ELEITORES_PERFIL_ESC_FUND / QT_ELEITORES,
         PCT_ELEITORES_PERFIL_ESC_MED = QT_ELEITORES_PERFIL_ESC_MED / QT_ELEITORES,
         PCT_ELEITORES_PERFIL_ESC_SUP = QT_ELEITORES_PERFIL_ESC_SUP / QT_ELEITORES,) %>% 
  select(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, DIST,
         PCT_ELEITORES_PERFIL_GENERO_FEM, PCT_ELEITORES_PERFIL_GENERO_MASC,
         PCT_ELEITORES_PERFIL_FX_ETARIA_16A34, PCT_ELEITORES_PERFIL_FX_ETARIA_35A59, PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS,
         PCT_ELEITORES_PERFIL_ESC_FUND, PCT_ELEITORES_PERFIL_ESC_MED, PCT_ELEITORES_PERFIL_ESC_SUP) %>% 
  mutate(NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
         NR_SECAO = str_pad(NR_SECAO, 5, "left", "0"))
