library(tidyverse)
library(sf)

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA",
                                    "MG", "AM", "PR", "PE",
                                    "GO", "RS", "PA",
                                    "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM",
                                     "GUARULHOS", "CAMPINAS", "SÃO LUÍS"),
                             CRS = c(31983, 31984, 31984, 31984,
                                     31983, 31981, 31982, 31985,
                                     31982, 31982, 31982, 31983, 31983, 31983),
                             LAT = c(-23.550219183230713, -22.901218546118674, -3.7328557776324276, -12.978121829863388, 
                                     -19.919075878899267, -3.1299440466612474, -25.437277365002902, -8.057924510350167, 
                                     -16.686968026339013, -30.03062423236882, -1.456397958622057, 
                                     -23.46789830632506, -22.910167339057764, -2.532123148298266),
                             LONG = c(-46.633923474372075, -43.17909134547906, -38.526630476279315, -38.51424239646277,
                                      -43.93865814451823, -60.02084126468137, -49.27017707823052, -34.88277823540448,
                                      -49.27073207988568, -51.22752843445819, -48.50128407325795,
                                      -46.53296203146622, -47.059341354563344, -44.300068812437175))

df_profile <- data.frame()

for (ano in c("2016", "2020")) {
  
  print(ano)
  
  for (mun in municipalities$MUN) { 
    
    print(mun)
    
    candidates <- read_delim(str_c("01_in/votacao_candidato_munzona_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                             trim_ws = TRUE, col_types = cols(CD_MUNICIPIO = col_double())) %>% 
      subset(NM_MUNICIPIO == mun & CD_TIPO_ELEICAO == 2 & CD_CARGO == 13) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_SIT_TOT_TURNO) %>% 
      summarise(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS, na.rm = T))
    
    votacao_secao <- read_delim(str_c("01_in/votacao_secao_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"), 
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                    grouping_mark = ".", encoding = "latin1"), 
                                trim_ws = TRUE, col_types = cols(CD_MUNICIPIO = col_double())) %>% 
      subset(NM_MUNICIPIO == mun & CD_TIPO_ELEICAO == 2 & CD_CARGO == 13) %>% 
      select(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, DS_LOCAL_VOTACAO_ENDERECO, NR_ZONA, NR_SECAO, NR_VOTAVEL, NM_VOTAVEL, QT_VOTOS) %>% 
      left_join(candidates, by = c("ANO_ELEICAO", "SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_VOTAVEL" = "NR_CANDIDATO")) %>%
      mutate(PESO = QT_VOTOS / QT_VOTOS_NOMINAIS,
             NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
             NR_SECAO = str_pad(NR_SECAO, 5, "left", "0"))
    
    poll <- read_delim(str_c("01_in/eleitorado_local_votacao_", ano, ".csv"), 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                       trim_ws = TRUE) %>% 
      subset(NM_MUNICIPIO == mun & NR_TURNO == 1) %>%
      distinct(NR_ZONA, NR_SECAO, .keep_all = T)
    
    poll_sf <- poll %>%
      subset(NR_LATITUDE != -1) %>% 
      st_as_sf(.,coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = 4326) %>%
      st_transform(municipalities$CRS[municipalities$MUN == mun])
    
    cc_sf <- data.frame(NR_LONGITUDE = municipalities$LONG[municipalities$MUN == mun],
                        NR_LATITUDE = municipalities$LAT[municipalities$MUN == mun]) %>% 
      st_as_sf(.,coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = 4326) %>%
      st_transform(municipalities$CRS[municipalities$MUN == mun])
    
    poll_sf$DIST <- as.numeric(st_distance(poll_sf, cc_sf))
    poll_sf <- poll_sf %>%
      st_drop_geometry() %>%
      select(NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO, DIST)
    
    poll <- poll %>%
      left_join(poll_sf, by = c("NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO")) %>%
      select(NR_ZONA, NR_SECAO, DIST)

    perfil_eleitor_secao <- read_delim(str_c("01_in/perfil_eleitor_secao_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"), 
                                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                           grouping_mark = ".", encoding = "latin1"), 
                                       trim_ws = TRUE, col_types = cols(CD_MUNICIPIO = col_double())) %>% 
      subset(NM_MUNICIPIO == mun) %>% 
      left_join(poll, by = c("NR_ZONA", "NR_SECAO")) %>% 
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
    
    aux_df_profile <- votacao_secao %>%
      left_join(perfil_eleitor_secao, by = c("ANO_ELEICAO", "SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO", "NR_ZONA", "NR_SECAO")) %>% 
      mutate(AUX_PCT_ELEITORES_PERFIL_GENERO_FEM = PCT_ELEITORES_PERFIL_GENERO_FEM * PESO,
             AUX_PCT_ELEITORES_PERFIL_GENERO_MASC = PCT_ELEITORES_PERFIL_GENERO_MASC * PESO,
             AUX_PCT_ELEITORES_PERFIL_FX_ETARIA_16A34 = PCT_ELEITORES_PERFIL_FX_ETARIA_16A34 * PESO,
             AUX_PCT_ELEITORES_PERFIL_FX_ETARIA_35A59 = PCT_ELEITORES_PERFIL_FX_ETARIA_35A59 * PESO,
             AUX_PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS = PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS * PESO,
             AUX_PCT_ELEITORES_PERFIL_ESC_FUND = PCT_ELEITORES_PERFIL_ESC_FUND * PESO,
             AUX_PCT_ELEITORES_PERFIL_ESC_MED = PCT_ELEITORES_PERFIL_ESC_MED * PESO,
             AUX_PCT_ELEITORES_PERFIL_ESC_SUP = PCT_ELEITORES_PERFIL_ESC_SUP * PESO,
             AUX_DIST = DIST * PESO) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_VOTAVEL, NM_URNA_CANDIDATO, SG_PARTIDO, DS_SIT_TOT_TURNO, QT_VOTOS_NOMINAIS) %>% 
      summarise(PCT_ELEITORES_PERFIL_GENERO_FEM = sum(AUX_PCT_ELEITORES_PERFIL_GENERO_FEM, na.rm = T),
                PCT_ELEITORES_PERFIL_GENERO_MASC = sum(AUX_PCT_ELEITORES_PERFIL_GENERO_MASC, na.rm = T),
                PCT_ELEITORES_PERFIL_FX_ETARIA_16A34 = sum(AUX_PCT_ELEITORES_PERFIL_FX_ETARIA_16A34, na.rm = T),
                PCT_ELEITORES_PERFIL_FX_ETARIA_35A59 = sum(AUX_PCT_ELEITORES_PERFIL_FX_ETARIA_35A59, na.rm = T),
                PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS = sum(AUX_PCT_ELEITORES_PERFIL_FX_ETARIA_60MAIS, na.rm = T),
                PCT_ELEITORES_PERFIL_ESC_FUND = sum(AUX_PCT_ELEITORES_PERFIL_ESC_FUND, na.rm = T),
                PCT_ELEITORES_PERFIL_ESC_MED = sum(AUX_PCT_ELEITORES_PERFIL_ESC_MED, na.rm = T),
                PCT_ELEITORES_PERFIL_ESC_SUP = sum(AUX_PCT_ELEITORES_PERFIL_ESC_SUP, na.rm = T),
                DIST = sum(AUX_DIST, na.rm = T)) %>% 
      subset(!is.na(NM_URNA_CANDIDATO))
    
    df_profile <- rbind(df_profile, aux_df_profile)
    
  }
  
}

df_profile <- df_profile %>% 
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO),
         CD_MUNICIPIO = as.character(CD_MUNICIPIO))

write.csv2(df_profile, "02_out/profile.csv", row.names = F, fileEncoding = "UTF-8")

