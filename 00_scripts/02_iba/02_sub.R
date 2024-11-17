library(tidyverse)
library(sf)
library(spdep)
library(classInt)

df_poll <- data.frame()

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

municipalities <- data.frame(UF = c("SP"),
                             MUN = c("SÃO PAULO"))

for(ano in c("2016", "2020")) {
  
  print(ano)
  
  for(mun in municipalities$MUN) {
  
    print(mun)
    
    poll <- read_delim(str_c("01_in/TSE/eleitorado_local_votacao_", ano, ".csv"), 
                       delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                       trim_ws = TRUE) %>% 
      subset(NM_MUNICIPIO == mun & NR_TURNO == 1) %>% 
      distinct(AA_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, NR_LATITUDE, NR_LONGITUDE, NR_ZONA, NR_SECAO)
    
    poll_sf <- poll %>% st_as_sf(.,coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = 4326)
    #poll_sf %>% ggplot() + geom_sf()
    
    shape <- read_sf(str_c("01_in/shapefiles/SIRGAS_SHP_subprefeitura_polygon.shp")) %>%
      st_transform(4326) %>% 
      #rename(any_of(c("CD_APONDE" = "CD_APonde"))) %>% 
      st_make_valid() %>% 
      mutate(sp_nome = str_c("SUBPREFEITURA ", sp_nome))
    inter <- poll_sf %>%
      st_intersection(shape) %>%
      select(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, sp_nome) %>%
      st_drop_geometry()
    
    aux_poll <- poll %>% left_join(inter, by = c("NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO")) %>% distinct()
    test <- nrow(aux_poll)
    
    aux_poll <- aux_poll %>% distinct(NR_ZONA, NR_SECAO, .keep_all = T)
    print((nrow(aux_poll) - test) / test)
    
    df_poll <- rbind(df_poll, aux_poll)
    
  }
  
}

print(nrow(df_poll) / nrow(poll))

df_poll <- df_poll %>%
  mutate(AA_ELEICAO = as.character(AA_ELEICAO),
         CD_MUNICIPIO = str_pad(CD_MUNICIPIO, 5, "left", "0"),
         NR_LOCAL_VOTACAO = str_pad(NR_LOCAL_VOTACAO, 5, "left", "0"),
         NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
         NR_SECAO = str_pad(NR_SECAO, 5, "left", "0"))

############################################

df_ind <- data.frame()
df_nec <- data.frame()

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
    
    poll <- df_poll %>% 
      subset(AA_ELEICAO == ano & NM_MUNICIPIO == mun) %>%
      mutate(NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
             NR_SECAO = str_pad(NR_SECAO, 5, "left", "0")) %>%
      select(NR_ZONA, NR_SECAO, sp_nome) %>%
      arrange(NR_ZONA, NR_SECAO, sp_nome)
    
    votacao_secao <- votacao_secao %>%
      left_join(poll, by = c("NR_ZONA", "NR_SECAO")) %>% 
      group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_VOTAVEL, NM_VOTAVEL, sp_nome) %>%
      summarise(QT_VOTOS_VER_TER = sum(QT_VOTOS, na.rm = T)) %>% 
      ungroup()
    
    votacao_ver_ter <- votacao_secao %>% subset(NR_VOTAVEL >= 10000) %>% select(NR_VOTAVEL, NM_VOTAVEL, sp_nome, QT_VOTOS_VER_TER)
    votacao_ver_total <- votacao_ver_ter %>% group_by(NR_VOTAVEL, NM_VOTAVEL) %>% summarise(QT_VOTOS_VER_TOTAL = sum(QT_VOTOS_VER_TER, na.rm = T))
    votacao_ter <- votacao_secao %>% group_by(sp_nome) %>% summarise(QT_VOTOS_TER = sum(QT_VOTOS_VER_TER, na.rm = T)) %>% na.omit()
    #votacao_total <- votacao_secao %>% group_by(NM_MUNICIPIO) %>% summarise(QT_VOTOS_MUN = sum(QT_VOTOS_VER_TER, na.rm = T))
    n_ver <- as.numeric(length(unique(votacao_ver_ter$NR_VOTAVEL)))
    
    aux_df_nec <- votacao_secao %>%
      subset(NR_VOTAVEL >= 10000) %>%
      select(ANO_ELEICAO, NR_VOTAVEL, NM_VOTAVEL, sp_nome, QT_VOTOS_VER_TER) %>% 
      group_by(sp_nome) %>% 
      mutate(QT_VOTOS_TER = sum(QT_VOTOS_VER_TER, na.rm  = T)) %>%
      ungroup() %>% 
      mutate(AUX_NEC = (QT_VOTOS_VER_TER / QT_VOTOS_TER)^2) %>% 
      group_by(ANO_ELEICAO, sp_nome) %>% 
      summarise(AUX_NEC = sum(AUX_NEC, na.rm = T)) %>% 
      na.omit() %>% 
      mutate(NEC = 1 / AUX_NEC)
    df_nec <- rbind(df_nec, aux_df_nec)
    
    aux_df_ind <- data.frame()
    
    for(j in sort(unique(votacao_ter$sp_nome))) {
      
      aux <- data.frame(ANO_ELEICAO = unique(votacao_secao$ANO_ELEICAO),
                        SG_UF = unique(votacao_secao$SG_UF),
                        CD_MUNICIPIO = unique(votacao_secao$CD_MUNICIPIO),
                        NM_MUNICIPIO = unique(votacao_secao$NM_MUNICIPIO),
                        NR_VOTAVEL = votacao_ver_total$NR_VOTAVEL,
                        NM_VOTAVEL = votacao_ver_total$NM_VOTAVEL,
                        sp_nome = j)
      aux_df_ind <- rbind(aux_df_ind, aux)
      rm(aux)
      
    }
    
    aux_df_ind <- aux_df_ind %>%
      left_join(votacao_ver_ter, by = c("NR_VOTAVEL", "NM_VOTAVEL", "sp_nome")) %>% 
      left_join(votacao_ver_total, by = c("NR_VOTAVEL", "NM_VOTAVEL")) %>% 
      left_join(votacao_ter, by = c("sp_nome")) %>% 
      #left_join(votacao_total, by = c("NM_MUNICIPIO")) %>%
      mutate(QT_VOTOS_TOTAL = sum(votacao_secao$QT_VOTOS_VER_TER, na.rm = T))
    
    aux_df_ind$QT_VOTOS_VER_TER[is.na(aux_df_ind$QT_VOTOS_VER_TER)] <- 0
    
    #QL indicator
    
    aux_df_ind$QL <- (aux_df_ind$QT_VOTOS_VER_TER / aux_df_ind$QT_VOTOS_VER_TOTAL) / (aux_df_ind$QT_VOTOS_TER / aux_df_ind$QT_VOTOS_TOTAL)
    
    #QD indicator
    
    aux_df_ind$QD <- (aux_df_ind$QT_VOTOS_VER_TER / aux_df_ind$QT_VOTOS_TER) / (aux_df_ind$QT_VOTOS_TER / (aux_df_ind$QT_VOTOS_TOTAL * n_ver))
    
    # #D indicator
    # 
    # aux_df_ind$D_aux <- aux_df_ind$QT_VOTOS_VER_TER^2 / (aux_df_ind$QT_VOTOS_TER * aux_df_ind$QT_VOTOS_VER_TOTAL)
    # 
    # #HH indicator
    # 
    # aux_df_ind$HH_aux <- (aux_df_ind$QT_VOTOS_VER_TER / aux_df_ind$QT_VOTOS_VER_TOTAL)^2
    # 
    # #G indicator
    # 
    # aux_df_ind$G_aux <- (aux_df_ind$QT_VOTOS_VER_TER / aux_df_ind$QT_VOTOS_VER_TOTAL - aux_df_ind$QT_VOTOS_TER / aux_df_ind$QT_VOTOS_TOTAL)^2
    
    profile <- read_delim("02_out/profile.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                              grouping_mark = "."), trim_ws = TRUE) %>% 
      subset(ANO_ELEICAO == ano & NM_MUNICIPIO == mun) %>% 
      select(ANO_ELEICAO, NR_VOTAVEL, NM_URNA_CANDIDATO, DS_SIT_TOT_TURNO)
    
    aux_df_ind <- aux_df_ind %>% left_join(profile, by = c("ANO_ELEICAO", "NR_VOTAVEL"))
    
    # aux_df_ind <- aux_df_ind %>%
    #   group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_VOTAVEL, NM_VOTAVEL, QT_VOTOS_TOTAL) %>%
    #   summarise(D = sum(D_aux, na.rm = T),
    #             HH = sum(HH_aux, na.rm = T),
    #             G = sum(G_aux, na.rm = T)) %>%
    #   left_join(votacao_ver_total, by = c("NR_VOTAVEL", "NM_VOTAVEL")) %>% 
    #   mutate(CD_MUNICIPIO = str_pad(CD_MUNICIPIO, 5, "left", "0"),
    #          NR_VOTAVEL = str_pad(NR_VOTAVEL, 5, "left", "0"),
    #          PCT_VOTOS_VER_TOTAL = QT_VOTOS_VER_TOTAL / QT_VOTOS_TOTAL)
    
    df_ind <- rbind(df_ind, aux_df_ind)
    
  }
  
}

df_ind <- df_ind %>% 
  mutate(NM_URNA_CANDIDATO = iconv(NM_URNA_CANDIDATO, from = 'UTF-8', to = 'ASCII//TRANSLIT'))

# df_ind %>% 
#   subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")) %>% 
#   ggplot(aes(x = QL, y = QD)) +
#   geom_point() +
#   geom_smooth()
 
write.csv2(df_ind, "02_out/ql_councilor_sp.csv", row.names = F, fileEncoding = "UTF-8")
write.csv2(df_nec, "02_out/nec_sp.csv", row.names = F, fileEncoding = "UTF-8")
 
#######################################################

df_lisa <- data.frame()

# ano <- "2016"
# mun <- "SÃO PAULO"

for (ano in c("2016", "2020")) {
  
  print(ano)
  
  for (mun in municipalities$MUN) {
    
    print(mun)
    
    # iba_sp <- read_delim("02_out/iba_sp.csv", 
    #                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
    #                                                                          grouping_mark = "."), trim_ws = TRUE) %>% 
    #   subset(election_year == ano & sub == 1)
    
    #LISA
    
    aux_df_lisa <- df_ind %>% 
      subset(NM_MUNICIPIO == mun &
               ANO_ELEICAO == ano) %>%  #&
               #NM_URNA_CANDIDATO %in% iba_sp$councilor) %>%
      #select(sp_nome, NM_URNA_CANDIDATO, QL, QD) %>% 
      select(sp_nome, NM_URNA_CANDIDATO, QL) %>% 
      arrange(NM_URNA_CANDIDATO, sp_nome) %>% 
      na.omit()
    
    sp <- read_sf("01_in/shapefiles/SIRGAS_SHP_subprefeitura_polygon.shp") %>% 
      mutate(sp_nome = str_c("SUBPREFEITURA ", sp_nome)) %>% 
      arrange(sp_nome)
    
    m_nbq <- poly2nb(sp)  #queen's neighborhood
    m_nbq_w <- nb2listw(m_nbq)
    
    for (councilor in unique(aux_df_lisa$NM_URNA_CANDIDATO)) {
      
      aux_councilor <- aux_df_lisa %>% 
        subset(NM_URNA_CANDIDATO == councilor)
      
      intervals <- classIntervals(aux_councilor$QL, 5, style = "sd")
      aux_councilor$SD <- cut(aux_councilor$QL, breaks = intervals$brks, include.lowest = T)
      aux_councilor$SD <- as.numeric(aux_councilor$SD)
      
      locm <- localmoran(aux_councilor$QL, m_nbq_w)  #calculate the local moran's I
      
      # manually make a moran plot standardized variables
      aux_councilor$QL_s <- scale(aux_councilor$QL)  #save to a new column
      
      # create a lagged variable
      aux_councilor$lag_i <- lag.listw(m_nbq_w, aux_councilor$QL_s)
      
      aux_councilor$LISA <- NA
      aux_councilor$LISA[(aux_councilor$QL_s >= 0 & aux_councilor$lag_i >= 0) & (locm[, 5] <= 0.05)] <- "Alto-Alto"
      aux_councilor$LISA[(aux_councilor$QL_s <= 0 & aux_councilor$lag_i <= 0) & (locm[, 5] <= 0.05)] <- "Baixo-Baixo"
      aux_councilor$LISA[(aux_councilor$QL_s >= 0 & aux_councilor$lag_i <= 0) & (locm[, 5] <= 0.05)] <- "Alto-Baixo"
      aux_councilor$LISA[(aux_councilor$QL_s <= 0 & aux_councilor$lag_i >= 0) & (locm[, 5] <= 0.05)] <- "Baixo-Alto"
      aux_councilor$LISA[(locm[,5] > 0.05)] <- "Não Significativo"  
      
      # # Set the breaks for the thematic map classes
      # breaks <- seq(1, 5, 1)
      # 
      # # see ?findInterval - This is necessary for making a map
      # np <- findInterval(variaveis$quad_sig, breaks)
      # np <- factor(np, levels = c("1", "2", "3", "4", "5"))
      # np <- recode_factor(np, "1" = "Alto-Alto", "2" = "Baixo-Baixo", "3" = "Alto-Baixo", "4" = "Baixo-Alto", "5" = "Não Significativo")
      
      # Assign colors to each map class
      # colors <- c("red", "blue", "lightpink", "skyblue2", "white")
      # 
      # ggplot(sp) +
      #   geom_sf(aes(fill=np), size = 0) +
      #   scale_fill_manual(values=c("Alto-Alto" = "red", "Baixo-Baixo" = "blue", "Alto-Baixo" = "lightpink", "Baixo-Alto" = "skyblue2", "Não Significativo" = "white")) +
      #   geom_sf_text(aes(label = NM_MUN), size = 1.0, color = "black", check_overlap = T) +
      #   labs(title = "", 
      #        subtitle = "",
      #        caption = "",
      #        fill = names[i],
      #        x = "", y = "") +
      #   theme_minimal()
      # ggsave(str_c("02_out/lisa_maps_variaveis/", reg, "_", names[i], ".png"), dpi = 600)
      
      aux_councilor$ANO_ELEICAO <- ano
    
      df_lisa <- rbind(df_lisa, select(aux_councilor, ANO_ELEICAO, sp_nome, NM_URNA_CANDIDATO, QL, SD, LISA))
      
    }
      
  }
  
}

write.csv2(df_lisa, "02_out/lisa_councilor_sp.csv", row.names = F, fileEncoding = "UTF-8")
