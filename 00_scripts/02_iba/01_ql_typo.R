library(tidyverse)
library(sf)

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

municipalities <- data.frame(UF = c("SP"),
                             MUN = c("SÃO PAULO"))

df_ind <- data.frame()

for (ano in c("2016", "2020")) {
  
  print(ano)
  
  for (mun in municipalities$MUN) {
    
    print(mun)
    
    votacao_secao <- read_delim(str_c("01_in/TSE/votacao_secao_", ano, "_", municipalities$UF[municipalities$MUN == mun], ".csv"),
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                    grouping_mark = ".", encoding = "latin1"),
                                trim_ws = TRUE) %>% 
      subset(NM_MUNICIPIO == mun & CD_TIPO_ELEICAO == 2 & CD_CARGO == 13) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, DS_LOCAL_VOTACAO_ENDERECO, NR_ZONA, NR_SECAO, NR_VOTAVEL, NM_VOTAVEL) %>% 
      summarise(QT_VOTOS = sum(QT_VOTOS, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
             NR_SECAO = str_pad(NR_SECAO, 5, "left", "0"))
    
    poll <- read_delim("02_out/poll.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(AA_ELEICAO = col_character(), NR_LOCAL_VOTACAO = col_character(), 
                                                                            NR_ZONA = col_character(), NR_SECAO = col_character(), 
                                                                            CD_APONDE = col_character()), locale = locale(decimal_mark = ",", 
                                                                                                                          grouping_mark = "."), trim_ws = TRUE) %>%
      subset(AA_ELEICAO == ano & NM_MUNICIPIO == mun) %>% 
      mutate(NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
             NR_SECAO = str_pad(NR_SECAO, 5, "left", "0")) %>% 
      select(NR_ZONA, NR_SECAO, CD_APONDE) %>% 
      arrange(NR_ZONA, NR_SECAO, CD_APONDE)
    
    votacao_secao <- votacao_secao %>%
      left_join(poll, by = c("NR_ZONA", "NR_SECAO")) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_VOTAVEL, NM_VOTAVEL, CD_APONDE) %>%
      summarise(QT_VOTOS_VER_TER = sum(QT_VOTOS, na.rm = T)) %>% 
      ungroup()
    
    indicators <- read_delim("02_out/indicators.csv", 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                 grouping_mark = "."), trim_ws = TRUE) %>% 
      subset(ANO_ELEICAO == ano & NM_MUNICIPIO == mun)
    
    profile <- read_delim("02_out/profile.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                              grouping_mark = "."), trim_ws = TRUE) %>% 
      subset(ANO_ELEICAO == ano & NM_MUNICIPIO == mun) %>% 
      select(ANO_ELEICAO, NR_VOTAVEL, NM_URNA_CANDIDATO, DS_SIT_TOT_TURNO)
    
    indicators <- indicators %>%
      left_join(profile, by = c("ANO_ELEICAO", "NR_VOTAVEL")) %>%
      select(ANO_ELEICAO, NM_MUNICIPIO, NR_VOTAVEL, NM_URNA_CANDIDATO, D, G, QT_VOTOS_VER_TOTAL, PCT_VOTOS_VER_TOTAL, DS_SIT_TOT_TURNO)
    
    tipo <- indicators %>% subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA"))
    tipo$TYPO <- NA
    tipo$TYPO[tipo$D > mean(tipo$D, na.rm = T) &
              tipo$G > mean(tipo$G, na.rm = T)] <- "DOMINANT-CONCENTRATED"
    tipo$TYPO[tipo$D <= mean(tipo$D, na.rm = T) &
              tipo$G > mean(tipo$G, na.rm = T)] <- "SHARED-CONCENTRATED"
    tipo$TYPO[tipo$D > mean(tipo$D, na.rm = T) &
              tipo$G <= mean(tipo$G, na.rm = T)] <- "DOMINANT-SCATTERED"
    tipo$TYPO[tipo$D <= mean(tipo$D, na.rm = T) &
              tipo$G <= mean(tipo$G, na.rm = T)] <- "SHARED-SCATTERED"
    tipo <- tipo %>% select(ANO_ELEICAO, NM_MUNICIPIO, NR_VOTAVEL, TYPO, DS_SIT_TOT_TURNO)
    
    votacao_secao <- votacao_secao %>% left_join(tipo, by = c("ANO_ELEICAO", "NM_MUNICIPIO", "NR_VOTAVEL"))
    
    votacao_ver_ter <- votacao_secao %>% subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>% group_by(CD_APONDE, TYPO) %>% summarise(QT_VOTOS_VER_TER = sum(QT_VOTOS_VER_TER, na.rm = T))
    votacao_ver_total <- votacao_ver_ter %>% group_by(TYPO) %>% summarise(QT_VOTOS_VER_TOTAL = sum(QT_VOTOS_VER_TER, na.rm = T))
    votacao_ter <- votacao_secao %>% group_by(CD_APONDE) %>% summarise(QT_VOTOS_TER = sum(QT_VOTOS_VER_TER, na.rm = T)) %>% na.omit()
    
    aux_df_ind <- data.frame()
    
    for(j in sort(unique(votacao_ter$CD_APONDE))) {
      
      aux <- data.frame(ANO_ELEICAO = unique(votacao_secao$ANO_ELEICAO),
                        SG_UF = unique(votacao_secao$SG_UF),
                        CD_MUNICIPIO = unique(votacao_secao$CD_MUNICIPIO),
                        NM_MUNICIPIO = unique(votacao_secao$NM_MUNICIPIO),
                        TYPO = unique(votacao_secao$TYPO),
                        CD_APONDE = j)
      aux_df_ind <- rbind(aux_df_ind, aux)
      rm(aux)
      
    }
    
    aux_df_ind <- aux_df_ind %>%
      na.omit() %>% 
      left_join(votacao_ver_ter, by = c("TYPO", "CD_APONDE")) %>%
      left_join(votacao_ver_total, by = c("TYPO")) %>% 
      left_join(votacao_ter, by = c("CD_APONDE")) %>% 
      mutate(QT_VOTOS_TOTAL = sum(votacao_secao$QT_VOTOS_VER_TER, na.rm = T))
    
    aux_df_ind$QT_VOTOS_VER_TER[is.na(aux_df_ind$QT_VOTOS_VER_TER)] <- 0
    
    #QL indicator
    
    aux_df_ind$QL <- (aux_df_ind$QT_VOTOS_VER_TER / aux_df_ind$QT_VOTOS_VER_TOTAL) / (aux_df_ind$QT_VOTOS_TER / aux_df_ind$QT_VOTOS_TOTAL)
    
    shape <- read_sf(str_c("01_in/shapefiles/", iconv(mun, from = 'UTF-8', to = 'ASCII//TRANSLIT'), "_area_de_ponderacao.shp")) %>%
      st_transform(4326) %>% 
      rename(any_of(c("CD_APONDE" = "CD_APonde"))) %>% 
      st_make_valid() %>% 
      left_join(select(aux_df_ind, TYPO, CD_APONDE, QL), by = "CD_APONDE") %>% 
      subset(!is.na(TYPO))
    
    ggplot(shape) + 
      geom_sf(aes(fill = QL), size = 0) +
      scale_fill_gradient(low = "white", high = "darkred") +
      labs(title = "", 
           subtitle = "",
           caption = "",
           fill = "QL",
           x = "", y = "") +
      facet_wrap(~TYPO) +
      theme_void()
    ggsave(str_c("02_out/maps/", mun, "_", ano, ".png"), width = 16, height = 16, units = "cm", dpi = 600)
    
    df_ind <- rbind(df_ind, aux_df_ind)
    
  }
  
}

write.csv2(df_ind, "02_out/ql_typo_sp.csv", row.names = F, fileEncoding = "UTF-8")
