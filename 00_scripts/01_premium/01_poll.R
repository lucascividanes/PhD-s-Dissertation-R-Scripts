library(tidyverse)
library(sf)

df_poll <- data.frame()

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                         MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                 "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                 "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

for(ano in c("2016", "2020")) {
  
  print(ano)
  
  poll <- read_delim(str_c("01_in/TSE/eleitorado_local_votacao_", ano, ".csv"), 
                      delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                      trim_ws = TRUE) %>% 
    subset(NM_MUNICIPIO %in% municipalities$MUN & NR_TURNO == 1) %>% 
    distinct(AA_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, NR_LATITUDE, NR_LONGITUDE, NR_ZONA, NR_SECAO)
  
  poll_sf <- poll %>% st_as_sf(.,coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = 4326)
  #poll_sf %>% ggplot() + geom_sf()
  
  for(i in municipalities$MUN) {
    
    print(i)
    
    shape <- read_sf(str_c("01_in/shapefiles/", iconv(i, from = 'UTF-8', to = 'ASCII//TRANSLIT'), "_area_de_ponderacao.shp")) %>%
      st_transform(4326) %>% 
      rename(any_of(c("CD_APONDE" = "CD_APonde"))) %>% 
      st_make_valid()
    inter <- poll_sf %>%
      subset(NM_MUNICIPIO == i) %>%
      st_intersection(shape) %>%
      select(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, CD_APONDE) %>%
      st_drop_geometry()
    
    aux_poll <- poll %>% subset(NM_MUNICIPIO == i) %>% left_join(inter, by = c("NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO")) %>% distinct()
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

write.csv2(df_poll, "02_out/poll.csv", row.names = F, fileEncoding = "UTF-8")