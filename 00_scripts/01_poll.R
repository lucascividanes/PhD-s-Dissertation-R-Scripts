library(tidyverse)
library(sf)

df_local <- data.frame()

municipios <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                         MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                 "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                 "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"))

for(ano in c("2016", "2020")) {
  
  print(ano)
  
  local <- read_delim(str_c("in/eleitorado_local_votacao_", ano, ".csv"), 
                      delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                      trim_ws = TRUE) %>% 
    subset(NM_MUNICIPIO %in% municipios$MUN & NR_TURNO == 1) %>% 
    distinct(AA_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, NR_LATITUDE, NR_LONGITUDE, NR_ZONA, NR_SECAO)
  
  local_sf <- local %>% st_as_sf(.,coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = 4326)
  #local_sf %>% ggplot() + geom_sf()
  
  for(i in municipios$MUN) {
    
    print(i)
    
    shape <- read_sf(str_c("in/shapefiles/", iconv(i, from = 'UTF-8', to = 'ASCII//TRANSLIT'), "_area_de_ponderacao.shp")) %>%
      st_transform(4326) %>% 
      rename(any_of(c("CD_APONDE" = "CD_APonde"))) %>% 
      st_make_valid()
    inter <- local_sf %>%
      subset(NM_MUNICIPIO == i) %>%
      st_intersection(shape) %>%
      select(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, CD_APONDE) %>%
      st_drop_geometry()
    
    aux_local <- local %>% subset(NM_MUNICIPIO == i) %>% left_join(inter, by = c("NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO")) %>% distinct()
    teste <- nrow(aux_local)
    
    aux_local <- aux_local %>% distinct(NR_ZONA, NR_SECAO, .keep_all = T)
    print((nrow(aux_local) - teste) / teste)
    
    df_local <- rbind(df_local, aux_local)
    
  }
  
}

print(nrow(df_local) / nrow(local))

df_local <- df_local %>%
  mutate(AA_ELEICAO = as.character(AA_ELEICAO),
         CD_MUNICIPIO = str_pad(NR_ZONA, 5, "left", "0"),
         NR_LOCAL_VOTACAO = str_pad(NR_SECAO, 5, "left", "0"),
         NR_ZONA = str_pad(NR_ZONA, 5, "left", "0"),
         NR_SECAO = str_pad(NR_SECAO, 5, "left", "0"))

write.csv2(df_local, "02 out/local.csv", row.names = F, fileEncoding = "UTF-8")