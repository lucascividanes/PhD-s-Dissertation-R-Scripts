library(tidyverse)
library(sf)
library(writexl)

df_voronoi <- data.frame()

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA",
                                    #"MG",
                                    "AM", "PR", "PE",
                                    "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     #"BELO HORIZONTE",
                                     "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"),
                             COD = c(35, 33, 23, 29,
                                     #31,
                                     13, 41, 26,
                                     52, 43, 15, 35, 35, 21),
                             CRS = c(31983, 31984, 31984, 31984,
                                     #31983,
                                     31981, 31982, 31985,
                                     31982, 31982, 31982, 31983, 31983, 31983),
                             LAT = c(-23.550219183230713, -22.901218546118674, -3.7328557776324276, -12.978121829863388, 
                                     #-19.919075878899267,
                                     -3.1299440466612474, -25.437277365002902, -8.057924510350167, 
                                     -16.686968026339013, -30.03062423236882, -1.456397958622057, 
                                     -23.46789830632506, -22.910167339057764, -2.532123148298266),
                             LONG = c(-46.633923474372075, -43.17909134547906, -38.526630476279315, -38.51424239646277,
                                      #-43.93865814451823,
                                      -60.02084126468137, -49.27017707823052, -34.88277823540448,
                                      -49.27073207988568, -51.22752843445819, -48.50128407325795,
                                      -46.53296203146622, -47.059341354563344, -44.300068812437175))

ano <- "2020"

test <- read_delim(str_c("01_in/eleitorado_local_votacao_", ano, ".csv"), 
                   delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                   trim_ws = TRUE) %>% 
  subset(NM_MUNICIPIO %in% municipalities$MUN & NR_TURNO == 1) %>%
  distinct(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, NR_LATITUDE, NR_LONGITUDE, .keep_all = T) %>% 
  group_by(NM_MUNICIPIO) %>% 
  summarise(N_LOCAIS = n_distinct(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO),
            SEM_GEORREF = sum(NR_LATITUDE == -1, na.rm = T),
            PCT_SEM_GEORREF = SEM_GEORREF / N_LOCAIS)

for(ano in c("2016", "2020")) {
  
  for(mun in municipalities$MUN) {
    
    print(ano)
    print(mun)
    
    poll <- read_delim(str_c("01_in/eleitorado_local_votacao_", ano, ".csv"), 
                        delim = ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                        trim_ws = TRUE) %>% 
      subset(NM_MUNICIPIO == mun & NR_TURNO == 1) %>% 
      select(AA_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, DS_ENDERECO, NM_BAIRRO, NR_CEP, NR_LATITUDE, NR_LONGITUDE) %>%
      subset(NR_LONGITUDE != -1) %>% 
      distinct(NR_LATITUDE, NR_LONGITUDE, .keep_all = T)
    
    poll_sf <- poll %>%
      st_as_sf(.,coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = 4326) %>%
      st_transform(municipalities$CRS[municipalities$MUN == mun])
    
    cc_sf <- data.frame(NR_LONGITUDE = municipalities$LONG[municipalities$MUN == mun],
                        NR_LATITUDE = municipalities$LAT[municipalities$MUN == mun]) %>% 
      st_as_sf(.,coords = c("NR_LONGITUDE", "NR_LATITUDE"), crs = 4326) %>%
      st_transform(municipalities$CRS[municipalities$MUN == mun])
    
    poll_sf$DIST <- as.numeric(st_distance(poll_sf, cc_sf))
    
    voronoi_sf <-
      poll_sf |> 
      st_union() |> 
      st_voronoi() |> 
      st_collection_extract("POLYGON") |>
      st_sf(geometry = _)
    voronoi_sf <- voronoi_sf %>% mutate(id = c(1:nrow(voronoi_sf)))
    
    inter_poll <- st_intersection(voronoi_sf, poll_sf)
    
    shape <- read_sf(str_c("01_in/shapefiles/", municipalities$UF[municipalities$MUN == mun] ,"_Malha_Preliminar_2022.shp")) %>%
      subset(NM_MUN == str_to_title(mun)) %>% 
      st_transform(municipalities$CRS[municipalities$MUN == mun]) %>% 
      st_make_valid()
    
    inter_shape <- st_intersection(shape, voronoi_sf)
    inter_shape$area <- st_area(inter_shape)
    
    inter <- inter_shape %>%
      st_drop_geometry() %>%
      left_join(st_drop_geometry(inter_poll), by = "id") %>% 
      arrange(CD_SETOR, -area) %>% 
      distinct(CD_SETOR, .keep_all = T) %>% 
      select(AA_ELEICAO:DIST, CD_SETOR) %>% 
      distinct(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, CD_SETOR, .keep_all = T) %>% 
      arrange(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, CD_SETOR)
    
    df_voronoi <- rbind(df_voronoi, inter)
 
   }

}

write.csv2(df_voronoi, "02_out/voronoi.csv", row.names = F, fileEncoding = "UTF-8")

# shape <- shape %>% left_join(select(inter, CD_SETOR, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO), by = "CD_SETOR")
# shape$id <- str_c(shape$NR_LOCAL_VOTACAO, shape$NM_LOCAL_VOTACAO)
# 
# local_sf$id <- str_c(local_sf$NR_LOCAL_VOTACAO, local_sf$NM_LOCAL_VOTACAO)
#
# i <- unique(local_sf$NR_LOCAL_VOTACAO)[1]
#
# ggplot() +
#   geom_sf(data = subset(shape, id == i), aes(fill = NM_LOCAL_VOTACAO)) +
#   geom_sf(data = subset(local_sf, id == i)) +
#   geom_sf_text(data = subset(local_sf, id == i), aes(label = NM_LOCAL_VOTACAO), size = 2)