library(tidyverse)

municipios <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                         MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                 "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                 "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

df_ind <- data.frame()

for (ano in c("2016", "2020")) {
  
  print(ano)
  
  for (uf in municipios$UF) {
    
    print(unique(municipios$MUN[municipios$UF == uf]))
    
    votacao_secao <- read_delim(str_c("in/votacao_secao_", ano, "_", uf, ".csv"),
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                    grouping_mark = ".", encoding = "latin1"),
                                trim_ws = TRUE) %>% 
      subset(NM_MUNICIPIO %in% municipios$MUN & CD_TIPO_ELEICAO == 2 & CD_CARGO == 13) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, DS_LOCAL_VOTACAO_ENDERECO, NR_ZONA, NR_SECAO, NR_VOTAVEL, NM_VOTAVEL) %>% 
      summarise(QT_VOTOS = sum(QT_VOTOS, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
             NR_SECAO = str_pad(NR_SECAO, 5, "left", "0"))
    
    local <- read_delim("out/local.csv", 
                        delim = ";", escape_double = FALSE, col_types = cols(AA_ELEICAO = col_character(), NR_LOCAL_VOTACAO = col_character(), 
                                                                             NR_ZONA = col_character(), NR_SECAO = col_character(), 
                                                                             CD_APONDE = col_character()), locale = locale(decimal_mark = ",", 
                                                                                                                           grouping_mark = "."), trim_ws = TRUE) %>%
      subset(AA_ELEICAO == ano & NM_MUNICIPIO %in% municipios$MUN[municipios$UF == uf]) %>% 
      mutate(NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
             NR_SECAO = str_pad(NR_SECAO, 5, "left", "0")) %>% 
      select(NR_ZONA, NR_SECAO, CD_APONDE) %>% 
      arrange(NR_ZONA, NR_SECAO, CD_APONDE)
    
    votacao_secao <- votacao_secao %>%
      left_join(local, by = c("NR_ZONA", "NR_SECAO")) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_VOTAVEL, NM_VOTAVEL, CD_APONDE) %>%
      summarise(QT_VOTOS_VER_TER = sum(QT_VOTOS, na.rm = T)) %>% 
      ungroup()
    
    votacao_ver_ter <- votacao_secao %>% subset(NR_VOTAVEL >= 10000) %>% select(NR_VOTAVEL, NM_VOTAVEL, CD_APONDE, QT_VOTOS_VER_TER)
    votacao_ver_total <- votacao_ver_ter %>% group_by(NR_VOTAVEL, NM_VOTAVEL) %>% summarise(QT_VOTOS_VER_TOTAL = sum(QT_VOTOS_VER_TER, na.rm = T))
    votacao_ter <- votacao_secao %>% group_by(CD_APONDE) %>% summarise(QT_VOTOS_TER = sum(QT_VOTOS_VER_TER, na.rm = T)) %>% na.omit()
    
    aux_df_ind <- data.frame()
    
    for(j in sort(unique(votacao_ter$CD_APONDE))) {
      
      aux <- data.frame(ANO_ELEICAO = unique(votacao_secao$ANO_ELEICAO),
                        SG_UF = unique(votacao_secao$SG_UF),
                        CD_MUNICIPIO = unique(votacao_secao$CD_MUNICIPIO),
                        NM_MUNICIPIO = unique(votacao_secao$NM_MUNICIPIO),
                        NR_VOTAVEL = votacao_ver_total$NR_VOTAVEL,
                        NM_VOTAVEL = votacao_ver_total$NM_VOTAVEL,
                        CD_APONDE = j)
      aux_df_ind <- rbind(aux_df_ind, aux)
      rm(aux)
      
    }
    
    aux_df_ind <- aux_df_ind %>%
      left_join(votacao_ver_ter, by = c("NR_VOTAVEL", "NM_VOTAVEL", "CD_APONDE")) %>% 
      left_join(votacao_ver_total, by = c("NR_VOTAVEL", "NM_VOTAVEL")) %>% 
      left_join(votacao_ter, by = c("CD_APONDE")) %>% 
      mutate(QT_VOTOS_TOTAL = sum(votacao_secao$QT_VOTOS_VER_TER, na.rm = T))
    
    aux_df_ind$QT_VOTOS_VER_TER[is.na(aux_df_ind$QT_VOTOS_VER_TER)] <- 0
    
    #D indicator
    
    aux_df_ind$D_aux <- aux_df_ind$QT_VOTOS_VER_TER^2 / (aux_df_ind$QT_VOTOS_TER * aux_df_ind$QT_VOTOS_VER_TOTAL)
    
    #HH indicator
    
    aux_df_ind$HH_aux <- (aux_df_ind$QT_VOTOS_VER_TER / aux_df_ind$QT_VOTOS_VER_TOTAL)^2
    
    #G indicator
    
    aux_df_ind$G_aux <- (aux_df_ind$QT_VOTOS_VER_TER / aux_df_ind$QT_VOTOS_VER_TOTAL - aux_df_ind$QT_VOTOS_TER / aux_df_ind$QT_VOTOS_TOTAL)^2
    
    aux_df_ind <- aux_df_ind %>% group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_VOTAVEL, NM_VOTAVEL) %>%
      summarise(D = sum(D_aux, na.rm = T),
                HH = sum(HH_aux, na.rm = T),
                G = sum(G_aux, na.rm = T)) %>%
      left_join(votacao_ver_total, by = c("NR_VOTAVEL", "NM_VOTAVEL")) %>% 
      mutate(CD_MUNICIPIO = str_pad(CD_MUNICIPIO, 5, "left", "0"),
             NR_VOTAVEL = str_pad(NR_VOTAVEL, 5, "left", "0"))
    
    df_ind <- rbind(df_ind, aux_df_ind)
    
  }
  
}

df_ind <- df_ind %>% 
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO))

write.csv2(df_ind, "02 out/indicadores.csv", row.names = F, fileEncoding = "UTF-8")