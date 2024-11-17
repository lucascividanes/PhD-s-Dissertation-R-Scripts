library(tidyverse)

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                         MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                 "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                 "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

df_margins <- data.frame()

for (ano in c("2016", "2020")) {
  
  print(ano)
  
  for(mun in municipalities$MUN) {
    
    print(mun)
    
    candidates <- read_delim(str_c("01_in/votacao_candidato_munzona_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                             trim_ws = TRUE) %>% 
      subset(NM_MUNICIPIO == mun & CD_TIPO_ELEICAO == 2 & CD_CARGO == 13) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DS_SIT_TOT_TURNO) %>% 
      summarise(QT_VOTOS = sum(QT_VOTOS_NOMINAIS, na.rm = T)) %>% 
      ungroup()
    
    aux_last_elected <- candidates %>%
      subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>% 
      group_by(DS_COMPOSICAO_COLIGACAO) %>%
      mutate(RANK=rank(QT_VOTOS)) %>% 
      ungroup() %>% 
      subset(RANK == 1) %>% 
      select(DS_COMPOSICAO_COLIGACAO, QT_VOTOS) %>% 
      rename("QT_VOTOS_ULT_ELEITO_LISTA" = "QT_VOTOS") %>% 
      arrange(DS_COMPOSICAO_COLIGACAO)
    
    aux_first_not_elected <- candidates %>%
      subset(DS_SIT_TOT_TURNO %in% c("SUPLENTE")) %>% 
      group_by(DS_COMPOSICAO_COLIGACAO) %>%
      mutate(RANK=rank(-QT_VOTOS)) %>% 
      ungroup() %>% 
      subset(RANK == 1) %>% 
      select(DS_COMPOSICAO_COLIGACAO, QT_VOTOS) %>% 
      rename("QT_VOTOS_PRI_N_ELEITO_LISTA" = "QT_VOTOS") %>% 
      arrange(DS_COMPOSICAO_COLIGACAO)
    
    candidates <- candidates %>%
      left_join(aux_last_elected, by = "DS_COMPOSICAO_COLIGACAO") %>% 
      left_join(aux_first_not_elected, by = "DS_COMPOSICAO_COLIGACAO") %>% 
      mutate(MARGEM = case_when(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA") ~ (QT_VOTOS - QT_VOTOS_PRI_N_ELEITO_LISTA) / QT_VOTOS_PRI_N_ELEITO_LISTA,
                                DS_SIT_TOT_TURNO %in% c("SUPLENTE") ~ (QT_VOTOS - QT_VOTOS_ULT_ELEITO_LISTA) / QT_VOTOS_ULT_ELEITO_LISTA),
             AMOSTRA = case_when(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA") & QT_VOTOS == QT_VOTOS_ULT_ELEITO_LISTA ~ 1,
                                 DS_SIT_TOT_TURNO %in% c("SUPLENTE") & QT_VOTOS == QT_VOTOS_PRI_N_ELEITO_LISTA ~ 1,
                                 TRUE ~ 0))
    
    aux <- read_delim(str_c("01_in/consulta_cand_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"), 
                      delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                      trim_ws = TRUE) %>% 
      subset(NM_UE %in% municipalities$MUN & CD_TIPO_ELEICAO == 2 & CD_CARGO == 13) %>% 
      select(NR_CANDIDATO, NM_URNA_CANDIDATO, NR_CPF_CANDIDATO, ST_REELEICAO)
    
    candidates <- candidates %>% left_join(aux, by = c("NR_CANDIDATO", "NM_URNA_CANDIDATO"))
    
    df_margins <- rbind(df_margins, candidates)
    
  }
  
}

df_margins <- df_margins %>% 
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO),
         CD_MUNICIPIO = str_pad(CD_MUNICIPIO, 5, "left", "0"),
         NR_CANDIDATO = str_pad(NR_CANDIDATO, 5, "left", "0"),
         NR_CPF_CANDIDATO = str_pad(NR_CPF_CANDIDATO, 1, "left", "0"))

write.csv2(df_margins, "02_out/margins.csv", row.names = F, fileEncoding = "UTF-8")
