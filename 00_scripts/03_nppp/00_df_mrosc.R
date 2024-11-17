library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)

municipalities <- data.frame(UF = c("SP", "RJ", "CE", "BA", "MG", "AM", "PR", "PE", "GO", "RS", "PA", "SP", "SP", "MA"),
                             MUN = c("SÃO PAULO", "RIO DE JANEIRO", "FORTALEZA", "SALVADOR",
                                     "BELO HORIZONTE", "MANAUS", "CURITIBA", "RECIFE",
                                     "GOIÂNIA", "PORTO ALEGRE", "BELÉM", "GUARULHOS", "CAMPINAS", "SÃO LUÍS"),
                             COD_IBGE = c("3550308", "3304557", "2304400", "2927408",
                                          "3106200", "1302603", "4106902", "2611606",
                                          "5208707", "4314902", "1501402", "3518800", "3509502", "2111300"))

municipalities <- data.frame(UF = c("SP"),
                             MUN = c("SÃO PAULO"),
                             COD_IBGE = c("3550308"))

df_npo <- data.frame()

################### npos

npo <- read_excel("01_in/ipea/area_subarea.xlsx") %>% 
  clean_names() #%>% 
  #subset(edmu_cd_municipio %in% municipalities$COD_IBGE) %>% 
  #gather(key="area_subarea", value="sim", 13:50) %>% 
  #subset(!is.na(sim))

length(unique(npo$cd_identificador_osc))

# npo_cult <- npo %>% subset(area_subarea == "cultura_e_recrea_a_a_o")
# 
# npo_res <- read_excel("01_in/ipea/recursos.xls") %>%
#   clean_names() %>% 
#   subset(cd_identificador_osc %in% npo$cd_identificador_osc) %>% 
#   mutate(dt_ano_recursos_osc = year(dt_ano_recursos_osc))
# 
# length(unique(npo_res$cd_identificador_osc))
  
########################################### nppps culture 2019

mrosc <- read_excel("01_in/LAI/sao_paulo/46541_2019_mrosc.XLSX") %>% 
  clean_names() %>% 
  select(-x1)

valor <- sum(mrosc$valor)

############### projeto

mrosc$projeto <- iconv(mrosc$projeto, from = 'UTF-8', to = 'ASCII//TRANSLIT')
mrosc$projeto <- mrosc$projeto %>%
  str_trim() %>% 
  str_to_upper()

##############reg

mrosc$regionalizacao <- iconv(mrosc$regionalizacao, from = 'UTF-8', to = 'ASCII//TRANSLIT')
mrosc$regionalizacao <- mrosc$regionalizacao %>%
  str_trim() %>% 
  str_replace_all(" e ", ", ") %>% 
  str_to_upper()

c <- max(str_count(mrosc$regionalizacao, ", ")) + 1

mrosc <- mrosc %>%
  separate(regionalizacao, into = as.character(c(1:c)), sep = ", ") %>% 
  gather(key="index", value="dist", `1`:`19`) %>% 
  select(-index) %>% 
  subset(!is.na(dist))

aux_dist <- read_sf("01_in/shapefiles/SIRGAS_SHP_distrito.shp") %>% st_drop_geometry() %>% distinct(ds_nome)
aux_dist$ds_nome <- aux_dist$ds_nome %>% str_trim() %>% str_to_upper()

sort(unique(mrosc$dist[!(mrosc$dist %in% aux_dist$ds_nome)]))

mrosc$dist[mrosc$dist %in% c(" BUTANTA")] <- "BUTANTA"
mrosc$dist[mrosc$dist %in% c("CARAPICUIBA")] <- NA
mrosc$dist[mrosc$dist %in% c("CIDADER LIDER")] <- "CIDADE LIDER"
mrosc$dist[mrosc$dist %in% c("DIADEMA")] <- NA
mrosc$dist[mrosc$dist %in% c("GUAIANAZES")] <- "GUAIANASES"
mrosc$dist[mrosc$dist %in% c("HIGIENOPOLIS")] <- "CONSOLACAO"
mrosc$dist[mrosc$dist %in% c("IBIRAPUERA")] <- "MOEMA"
mrosc$dist[mrosc$dist %in% c("INTERLAGOS")] <- "JARAGUA"
mrosc$dist[mrosc$dist %in% c("JARAGUA,")] <- "JARAGUA"
mrosc$dist[mrosc$dist %in% c("JARDIM GUAPIRA")] <- "TREMEMBE"
mrosc$dist[mrosc$dist %in% c("JARDIM IBIRAPUERA")] <- "JARDIM SAO LUIS"
mrosc$dist[mrosc$dist %in% c("JARDIM LIMOEIRO")] <- "IGUATEMI"
mrosc$dist[mrosc$dist %in% c("JARDIM PAULISTANO")] <- "JARAGUA"
mrosc$dist[mrosc$dist %in% c("JARDIM PAULSITA")] <- "JARDIM PAULISTA"
mrosc$dist[mrosc$dist %in% c("JARDIM SILVA TELES")] <- "VILA CURUCA"
mrosc$dist[mrosc$dist %in% c("M'BOI MIRIM")] <- "JARDIM ANGELA"
mrosc$dist[mrosc$dist %in% c("OSASCO")] <- NA
mrosc$dist[mrosc$dist %in% c("TAIPAS")] <- "JARAGUA"
mrosc$dist[mrosc$dist %in% c("TIRADENTES")] <- "CIDADE TIRADENTES"
mrosc$dist[mrosc$dist %in% c("VILA CLEMENTINO")] <- "VILA MARIANA"
mrosc$dist[mrosc$dist %in% c("VILA ITAIM")] <- "JARDIM HELENA"
mrosc$dist[mrosc$dist %in% c("VILA JOANIZA")] <- "CIDADE ADEMAR"
mrosc$dist[mrosc$dist %in% c("VILA MADALENA")] <- "PINHEIROS"

mrosc$dist[!(mrosc$dist %in% aux_dist$ds_nome)]

############ councilor

mrosc$vereador_responsavel <- iconv(mrosc$vereador_responsavel, from = 'UTF-8', to = 'ASCII//TRANSLIT')
mrosc$vereador_responsavel <- mrosc$vereador_responsavel %>%
  str_trim() %>% 
  str_replace_all(" e ", ", ") %>% 
  str_to_upper()

c <- max(str_count(mrosc$vereador_responsavel, ", ")) + 1

mrosc <- mrosc %>%
  separate(vereador_responsavel, into = as.character(c(1:c)), sep = ", ") %>% 
  gather(key="index", value="ver", `1`:as.character(c)) %>% 
  select(-index) %>% 
  subset(!is.na(ver))

profile <- read_delim("02_out/profile.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(ANO_ELEICAO == 2016 & NM_MUNICIPIO == "SÃO PAULO")

profile$NM_URNA_CANDIDATO <- iconv(profile$NM_URNA_CANDIDATO, from = 'UTF-8', to = 'ASCII//TRANSLIT')

sort(unique(mrosc$ver[!(mrosc$ver %in% profile$NM_URNA_CANDIDATO)]))

mrosc$ver[mrosc$ver %in% c("CAIO MIRANDA")] <- "CAIO MIRANDA CARNEIRO"
mrosc$ver[mrosc$ver %in% c("CELSO GIANAZZI")] <- "CELSO GIANNAZI"
mrosc$ver[mrosc$ver %in% c("GILBERTO NASCIMENTO")] <- "GILBERTO NASCIMENTO JR"
mrosc$ver[mrosc$ver %in% c("ISAAC FELIX")] <- "ISAC FELIX"
mrosc$ver[mrosc$ver %in% c("JAIR TATO")] <- "JAIR TATTO"
mrosc$ver[mrosc$ver %in% c("KARINA PICCIOLLI")] <- NA
mrosc$ver[mrosc$ver %in% c("MASATAKA OTA")] <- "OTA"
mrosc$ver[mrosc$ver %in% c("MILTON FERREIRA")] <- "DR. MILTON FERREIRA"
mrosc$ver[mrosc$ver %in% c("PARCERIA DIRETA")] <- NA
mrosc$ver[mrosc$ver %in% c("PATRICIA BEZERRA.")] <- "PATRICIA BEZERRA"
mrosc$ver[mrosc$ver %in% c("REGINALDO TRIPOLI")] <- "TRIPOLI"
mrosc$ver[mrosc$ver %in% c("SONINHA FRANCINE")] <- "SONINHA"
mrosc$ver[mrosc$ver %in% c("ZE TURIM")] <- "ZE TURIN"

mrosc$ver[!(mrosc$ver %in% profile$NM_URNA_CANDIDATO)]

############# cnpj

mrosc$cnpj_da_entidade <- mrosc$cnpj_da_entidade %>%
  str_trim() %>%
  str_replace_all("\\.", "") %>% 
  str_replace_all("\\/", "") %>% 
  str_replace_all("-", "") %>% 
  str_replace_all(" ", "")

sum(unique(mrosc$cnpj_da_entidade) %in% npo$cd_identificador_osc)/length(unique(mrosc$cnpj_da_entidade))

############# corr_value

mrosc <- mrosc %>% 
  group_by(projeto) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  mutate(value_corr = valor/count)

sum(mrosc$value_corr, na.rm = T) == valor

############# index

mrosc <- mrosc %>% 
  group_by(projeto) %>% 
  mutate(index = str_c("2019_", str_pad(group_indices(), 5, "left", "0"))) %>%
  ungroup() %>% 
  select(index, projeto, ver, cnpj_da_entidade, dist, value_corr) %>% 
  arrange(ver, cnpj_da_entidade, index)

edges <- mrosc %>% 
  group_by(ver, cnpj_da_entidade) %>% 
  summarise(weight = sum(value_corr, na.rm = T)) %>%
  rename("from" = "ver", "to" = "cnpj_da_entidade") %>% 
  subset(!is.na(from)) #%>% 
  #spread(key = cnpj_da_entidade, value = value)

sum(edges$weight, na.rm = T) / valor

write.csv2(edges, "02_out/nppp_edges.csv", row.names = F, fileEncoding = "UTF-8")

indicators <- read_delim("02_out/indicators.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE) %>% 
  subset(ANO_ELEICAO == 2016 & NM_MUNICIPIO == "SÃO PAULO") %>% 
  left_join(select(profile, NR_VOTAVEL, NM_URNA_CANDIDATO, SG_PARTIDO, DS_SIT_TOT_TURNO), by = "NR_VOTAVEL")

left <- c("PC do B", "PCB", "PCO", "PDT", "PSB", "PSOL", "PSTU", "PT", "UP", "PPL")
center <- c("CIDADANIA", "MDB", "PATRIOTA", "PMB", "PMN", "PROS", "PSD", "PSDB", "PV", "REDE", "SOLIDARIEDADE", "PMDB", "PPS", "PRP", "SD", "PEN")
right <- c("AVANTE", "DC", "NOVO", "DEM", "PL", "PODE", "PP", "PRTB", "PSC", "PSL", "PTB", "PTC", "REPUBLICANOS", "PRB", "PTN", "PR", "PSDC", "PHS", "PT do B")

indicators$IDEOLOGY <- NA
indicators$IDEOLOGY[indicators$SG_PARTIDO %in% left] <- "LEFT"
indicators$IDEOLOGY[indicators$SG_PARTIDO %in% center] <- "CENTER"
indicators$IDEOLOGY[indicators$SG_PARTIDO %in% right] <- "RIGHT"

indicators$CLASS <- NA
indicators$CLASS[indicators$ANO_ELEICAO == 2016 &
                   indicators$D > mean(indicators$D[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T) &
                   indicators$G > mean(indicators$G[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T)] <- "DOMINANT-CONCENTRATED"
indicators$CLASS[indicators$ANO_ELEICAO == 2016 &
                   indicators$D <= mean(indicators$D[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T) &
                   indicators$G > mean(indicators$G[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T)] <- "SHARED-CONCENTRATED"
indicators$CLASS[indicators$ANO_ELEICAO == 2016 &
                   indicators$D > mean(indicators$D[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T) &
                   indicators$G <= mean(indicators$G[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T)] <- "DOMINANT-SCATTERED"
indicators$CLASS[indicators$ANO_ELEICAO == 2016 &
                   indicators$D <= mean(indicators$D[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T) &
                   indicators$G <= mean(indicators$G[indicators$ANO_ELEICAO == 2016 & indicators$DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA")], na.rm = T)] <- "SHARED-SCATTERED"

councilors <- indicators %>% 
  subset(DS_SIT_TOT_TURNO %in% c("ELEITO POR QP", "ELEITO POR MÉDIA") | 
           NM_URNA_CANDIDATO %in% mrosc$ver[!is.na(mrosc$ver)] |
           NM_URNA_CANDIDATO %in% c("BETO DO SOCIAL", "DALTON SILVANO")) %>% 
  select(NM_URNA_CANDIDATO, SG_PARTIDO, IDEOLOGY, DS_SIT_TOT_TURNO, D, G, PCT_VOTOS_VER_TOTAL, CLASS)

write.csv2(councilors, "02_out/nppp_councilors.csv", row.names = F, fileEncoding = "UTF-8")

npos <- mrosc %>% 
  distinct(cnpj_da_entidade) %>%
  left_join(select(npo, cd_identificador_osc, tx_razao_social_osc, edmu_nm_municipio, habita_a_a_o:outras_atividades_associativas), by = c("cnpj_da_entidade" =  "cd_identificador_osc")) %>% 
  arrange(cnpj_da_entidade)
  
write.csv2(npos, "02_out/nppp_npos.csv", row.names = F, fileEncoding = "UTF-8")

