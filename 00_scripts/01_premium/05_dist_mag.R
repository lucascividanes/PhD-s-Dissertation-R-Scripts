library(tidyverse)

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

df_dist_mag <- data.frame()

for (ano in c("2016", "2020")) {
  
  print(ano)
  
  for (mun in municipalities$MUN) { 
    
    print(mun)
    
    candidates <- read_delim(str_c("01_in/votacao_candidato_munzona_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                             trim_ws = TRUE, col_types = cols(CD_MUNICIPIO = col_character())) %>% 
      subset(NM_MUNICIPIO == mun & CD_TIPO_ELEICAO == 2 & CD_CARGO == 13) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_SIT_TOT_TURNO) %>% 
      summarise(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS, na.rm = T))
    
    aux_df_dist_mag <- candidatos %>% group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO) %>% summarise(DIST_MAG = sum(DS_SIT_TOT_TURNO %in% c("ELEITO POR MÉDIA", "ELEITO POR QP"), na.rm = T))
    df_dist_mag <- rbind(df_dist_mag, aux_df_dist_mag)
    
  }
  
}

df_dist_mag <- df_dist_mag %>% 
  distinct() %>% 
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO))

write.csv2(df_dist_mag, "02_out/dist_mag.csv", row.names = F, fileEncoding = "UTF-8")
