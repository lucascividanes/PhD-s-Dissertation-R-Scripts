library(tidyverse)
library(sf)

df_voronoi <- data.frame()

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA",
                                    "MG", "AM", "PR", "PE",
                                    "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"),
                             COD = c(35, 33, 23, 29,
                                     31, 13, 41, 26,
                                     52, 43, 15, 35, 35, 21),
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

for(mun in municipalities$MUN) {
    
  print(mun)
    
  poll <- read_csv("01_in/georref.csv", 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
    subset(NM_MUNICIPIO == mun) %>% 
    mutate(LAT = case_when(NR_LATITUDE == -1 ~ NR_LATITUDE_GEO,
                           TRUE ~ NR_LATITUDE),
           LONG = case_when(NR_LONGITUDE == -1 ~ NR_LONGITUDE_GEO,
                            TRUE ~ NR_LONGITUDE)) %>% 
    distinct(LAT, LONG, .keep_all = T)
  
  poll_sf <- poll %>%
    st_as_sf(.,coords = c("LONG", "LAT"), crs = 4326) %>%
    st_transform(municipalities$CRS[municipalities$MUN == mun])
  
  cc_sf <- data.frame(LONG = municipalities$LONG[municipalities$MUN == mun],
                      LAT = municipalities$LAT[municipalities$MUN == mun]) %>% 
    st_as_sf(.,coords = c("LONG", "LAT"), crs = 4326) %>%
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
  
  shape <- read_sf(str_c("01_in/shapefiles/", municipalities$COD[municipalities$MUN == mun], "SEE250GC_SIR.shp"),
                   options = "ENCODING=WINDOWS-1252") %>%
    subset(NM_MUNICIP == str_to_upper(mun)) %>%
    select(-NM_BAIRRO) %>% 
    st_transform(municipalities$CRS[municipalities$MUN == mun]) %>% 
    st_make_valid()
  
  inter_shape <- st_intersection(shape, voronoi_sf)
  inter_shape$area <- st_area(inter_shape)
  
  inter <- inter_shape %>%
    st_drop_geometry() %>%
    left_join(st_drop_geometry(inter_poll), by = "id") %>% 
    arrange(CD_GEOCODI, -area) %>% 
    distinct(CD_GEOCODI, .keep_all = T) %>% 
    select(AA_ELEICAO:NR_CEP, DIST, CD_GEOCODI) %>% 
    distinct(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, CD_GEOCODI, .keep_all = T) %>% 
    arrange(NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, CD_GEOCODI)
  
  df_voronoi <- rbind(df_voronoi, inter)
  
}

write.csv2(df_voronoi, "02_out/voronoi_2010.csv", row.names = F, fileEncoding = "UTF-8")