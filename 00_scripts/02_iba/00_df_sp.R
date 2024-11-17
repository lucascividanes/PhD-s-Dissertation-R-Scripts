library(tidyverse)
library(readxl)
library(janitor)

####uniting data

df_iba_sp <- data.frame()

year <- "2017"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

year <- "2018"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

year <- "2019"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

year <- "2020"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

year <- "2021"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year) %>% 
  select(-processo_sei, -data_liberacao) %>% 
  select(1:3, 5, 4, 6:7)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

year <- "2022"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year) %>% 
  select(-processo_sei, -data_liberacao)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

year <- "2023"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year) %>% 
  select(-processo_sei, -data_liberacao)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

year <- "2024"

aux <- read_excel(str_c("01_in/iba/sao_paulo/", year, "_emendas.xlsx"), 
                  sheet = "emendas") %>% 
  clean_names() %>% 
  mutate(year = year) %>% 
  select(-item, -processo_sei, -data_da_liberacao) %>% 
  mutate(party = NA) %>% 
  select(1, 7, 2:6)

colnames(aux) <- c("councilor", "party", "object", "value", "executor", "status", "year")

df_iba_sp <- rbind(df_iba_sp, aux)

df_iba_sp <- df_iba_sp %>% subset(!is.na(councilor))

#### std data

df_iba_sp$councilor <- df_iba_sp$councilor %>%
  str_trim() %>%
  str_to_upper()
df_iba_sp$councilor <- iconv(df_iba_sp$councilor, from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(sort(df_iba_sp$councilor))

df_iba_sp$councilor[df_iba_sp$councilor == "ANTONIO DONATO"] <- "DONATO"
df_iba_sp$councilor[df_iba_sp$councilor == "CAIO MIRANDA"] <- "CAIO MIRANDA CARNEIRO"
df_iba_sp$councilor[df_iba_sp$councilor == "CARLOS BEZERRA JR"] <- "CARLOS BEZERRA JR."
df_iba_sp$councilor[df_iba_sp$councilor %in% c("GILBERTO NASCIMENTO", "GILBERTO NASCIMENTO JR.")] <- "GILBERTO NASCIMENTO JR"
df_iba_sp$councilor[df_iba_sp$councilor == "JOSE POLICE NETO"] <- "POLICE NETO"
df_iba_sp$councilor[df_iba_sp$councilor == "MILTON FERREIRA" & df_iba_sp$year %in% c(2017, 2018, 2019, 2020)] <- "DR. MILTON FERREIRA"
df_iba_sp$councilor[df_iba_sp$councilor == "MILTON FERREIRA" & df_iba_sp$year %in% c(2021, 2022, 2023, 2024)] <- "DR MILTON FERREIRA"
df_iba_sp$councilor[df_iba_sp$councilor == "PROFESSOR TONINHO VESPOLI"] <- "TONINHO VESPOLI"
df_iba_sp$councilor[df_iba_sp$councilor == "REGINALDO TRIPOLI"] <- "TRIPOLI"
df_iba_sp$councilor[df_iba_sp$councilor == "MARLON LUZ"] <- "MARLON DO UBER"
df_iba_sp$councilor[df_iba_sp$councilor == "SIDNEY CRUZ"] <- "DR SIDNEY CRUZ"
df_iba_sp$councilor[df_iba_sp$councilor == "XEXEU TRIPOLI"] <- "ROBERTO TRIPOLI"
df_iba_sp$councilor[df_iba_sp$councilor == "LUNA ZARATTINI"] <- "LUNA"
df_iba_sp$councilor[df_iba_sp$councilor == "BOMBEIRO MAJOR PALUMBO"] <- "MAJOR PALUMBO"
df_iba_sp$councilor[df_iba_sp$councilor == "ADRIANO SANTOS"] <- "DR ADRIANO SANTOS"
df_iba_sp$councilor[df_iba_sp$councilor == "JUSSARA BASSO"] <- "JUNTAS MULHERES SEM TETO"
df_iba_sp$councilor[df_iba_sp$councilor == "SONINHA FRANCINE" & df_iba_sp$year %in% c(2017, 2018, 2019, 2020)] <- "SONINHA"
df_iba_sp$councilor[df_iba_sp$councilor == "SONINHA" & df_iba_sp$year %in% c(2021, 2022, 2023, 2024)] <- "SONINHA FRANCINE"
df_iba_sp$councilor[df_iba_sp$councilor == "TONINHO VESPOLI"] <- "PROFESSOR TONINHO VESPOLI"
df_iba_sp$councilor[df_iba_sp$councilor == "SANDRA TADEU" & df_iba_sp$year %in% c(2021, 2022, 2023, 2024)] <- "DRA SANDRA TADEU"
df_iba_sp$councilor[df_iba_sp$councilor == "NUNES PEIXEIRO" & df_iba_sp$year %in% c(2021, 2022, 2023, 2024)] <- "DR NUNES PEIXEIRO"

unique(sort(df_iba_sp$councilor))

df_iba_sp$party <- df_iba_sp$party %>%
  str_trim() %>%
  str_to_upper()
df_iba_sp$party <- iconv(df_iba_sp$party, from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(sort(df_iba_sp$party))

df_iba_sp$party[df_iba_sp$party %in% c("#N/A", "SUB-PI", "SMS")] <- NA
unique(sort(df_iba_sp$party))

df_iba_sp$party[df_iba_sp$party == "PMDB"] <- "MDB"
df_iba_sp$party[df_iba_sp$party == "PRB"] <- "REPUBLICANOS"
df_iba_sp$party[df_iba_sp$party == "PSDC"] <- "DC"
df_iba_sp$party[df_iba_sp$party %in% c("PTN", "PHS", "PODEMOS")] <- "PODE"
df_iba_sp$party[df_iba_sp$party == "PR"] <- "PL"
df_iba_sp$party[df_iba_sp$party == "SD"] <- "SOLIDARIEDADE"
df_iba_sp$party[df_iba_sp$party == "PRP"] <- "PATRIOTA"
df_iba_sp$party[df_iba_sp$party == "PT do B"] <- "AVANTE"
df_iba_sp$party[df_iba_sp$party == "PPS"] <- "CIDADANIA"
df_iba_sp$party[df_iba_sp$party == "PPL"] <- "PC do B"
df_iba_sp$party[df_iba_sp$party %in% c("DEM", "PSL", "UNIAO BRASIL")] <- "UNIÃO BRASIL"

unique(sort(df_iba_sp$party))

df_iba_sp <- df_iba_sp %>% arrange(councilor, year) %>% fill(party) %>% arrange(year, councilor)

left <- c("PC do B", "PCB", "PCO", "PDT", "PSB", "PSOL", "PSTU", "PT", "UP")
center <- c("CIDADANIA", "MDB", "PATRIOTA", "PMB", "PMN", "PROS", "PSD", "PSDB", "PV", "REDE", "SOLIDARIEDADE")
right <- c("AVANTE", "DC", "NOVO", "UNIÃO BRASIL", "PL", "PODE", "PP", "PRTB", "PSC", "PTB", "PTC", "REPUBLICANOS")

df_iba_sp$ideology <- NA
df_iba_sp$ideology[df_iba_sp$party %in% left] <- "LEFT"
df_iba_sp$ideology[df_iba_sp$party %in% center] <- "CENTER"
df_iba_sp$ideology[df_iba_sp$party %in% right] <- "RIGHT"

df_iba_sp$object <- df_iba_sp$object %>% str_trim() %>% str_to_upper() 

df_iba_sp$value <- df_iba_sp$value %>% as.numeric()

df_iba_sp$executor <- df_iba_sp$executor %>% str_trim() %>% str_to_upper() %>% str_replace_all(" ", "") %>% str_replace_all("/", "")
unique(sort(df_iba_sp$executor))
df_iba_sp$executor <- iconv(df_iba_sp$executor, from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(sort(df_iba_sp$executor))
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("PREFEITURAREGIONAL", "SUBPREFEITURA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("PREFEITURAREGINALDOJABAQUARA", "SUBPREFEITURA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SUBPREFEITURADE", "SUBPREFEITURA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SUBPREFEITURADA", "SUBPREFEITURA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SUBPREFEITURADO", "SUBPREFEITURA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SUBPREFETURA", "SUBPREFEITURA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SECRETARIAMUNICIPAL", "SECRETARIA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SECRETARIAMUNICIPALDE", "SECRETARIA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SECRETARIAMUNICIPALDA", "SECRETARIA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SECRETARIADE", "SECRETARIA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SECRETARIADA", "SECRETARIA")
df_iba_sp$executor <- df_iba_sp$executor %>% str_replace_all("SECRETARIADO", "SECRETARIA")

unique(sort(df_iba_sp$executor))
df_iba_sp$executor[df_iba_sp$executor %in% c("CASACIVIL",
                                             "SECRETARIACASACIVIL")] <- "SECRETARIA CASA CIVIL"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAASSISTENCIAEDESENVOLVIMENTO",
                                             "SECRETARIAASSISTENCIAEDESENVOLVIMENTOSOCIAL",
                                             "SECRETARIAASSISTENCIASOCIAL",
                                             "SECRETARIADESENVOLVIMENTOEASSISTENCIASOCIAL",
                                             "SECRETARIAMUNCIPALDEASSISTENCIAEDESENVOLVIMENTO")] <- "SECRETARIA ASSISTENCIA E DESENVOLVIMENTO SOCIAL"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIACULTURA",
                                             "SECRETARIAMUNCIPALDECULTURA")] <- "SECRETARIA CULTURA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIADESENVOLVIMENTOECONOMICO,TRABALHOETURISMO",
                                             "SECRETARIADESENVOLVIMENTOECONOMICOETRABALHO",
                                             "SECRETARIATRABALHOEEMPREENDEDORISMO",
                                             "SECRETARIASENVOLVIMENTOECONOMICO,TRABALHOETURISMO",
                                             "SECRETARIASENVOLVIMENTOECONOMICOETRABALHO")] <- "SECRETARIA DESENVOLVIMENTO ECONOMICO E TRABALHO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIADIREITOHUMANOS",                          
                                             "SECRETARIADIREITOSHUMANOS",                          
                                             "SECRETARIADIREITOSHUMANOSECIDADANIA",
                                             "SECRETARIAMUNICPALDEDIREITOSHUMANOSECIDADANIA")] <- "SECRETARIA DIREITOS HUMANOS E CIDADANIA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAEDUCACAO")] <- "SECRETARIA EDUCACAO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAESPECIALDERELACOESGOVERNAMENTAIS")] <- "SECRETARIA RELACOES INSTITUCIONAIS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAESPECIALDERELACOESINTERNACIONAIS",
                                             "SECRETARIARELACOESINTERNACIONAIS")] <- "SECRETARIA RELACOES INTERNACIONAIS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAESPORTEELAZER",                            
                                             "SECRETARIAESPORTES",                                 
                                             "SECRETARIAESPORTESELAZER",
                                             "SECRETARIAMUNICPALDEESPORTEELAZER")] <- "SECRETARIA ESPORTES E LAZER"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAEXECUTIVADETRANSPORTEEMOBILIDADEURBANA")] <- "SECRETARIA TRANSPORTE E MOBILIDADE URBANA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAMOBILIDADEETRANSITO",                      
                                             "SECRETARIAMOBILIDADEETRANSPORTE")] <- "SECRETARIA MOBILIDADE E TRANSITO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAGOVERNO",                                  
                                             "SECRETARIAGOVERNOMUNICIPAL")] <- "SECRETARIA GOVERNO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAHABITACAO",
                                             "SECRETARIAMUNICPALDEHABITACAO")] <- "SECRETARIA HABITACAO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAINFRAESTRUTURAEOBRAS",                     
                                             "SECRETARIAINFRAESTRUTURAURBANAEOBRAS",
                                             "SECRETARIASERVICOSEOBRAS")] <- "SECRETARIA INFRAESTRUTURA E OBRAS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAINOVACAOETECNOLOGIA")] <- "SECRETARIA INOVACAO E TECNOLOGIA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAJUSTICA")] <- "SECRETARIA JUSTICA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAMUNICIPADESAUDE",                          
                                             "SECRETARIAMUNICPALDASAUDE",
                                             "SECRETARIAMUNICPALDESAUDE",
                                             "SECRETARIASAUDE")] <- "SECRETARIA SAUDE"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAPESSOACOMDEFEICIENCIA",                    
                                             "SECRETARIAPESSOACOMDEFICIENCIA",                     
                                             "SECRETARIAPESSOACOMDEFICIENCIAEMOBILIDADEREDUZIDA")] <- "SECRETARIA PESSOA COM DEFICIENCIA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIASEGURANCAPUBLICA",                        
                                             "SECRETARIASEGURANCAURBANA")] <- "SECRETARIA SEGURANCA URBANA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIASPREFEITURASREGIONAIS",                    
                                             "SECRETARIASSUBPREFEITURAS",
                                             "SUBPREFEITURA")] <- "SECRETARIA SUBPREFEITURAS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAURBANISMO",                                
                                             "SECRETARIAURBANISMOELICENCIAMENTO")] <- "SECRETARIA URBANISMO E LICENCIAMENTO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIAVERDEEDOMEIOAMBIENTE",                     
                                             "SECRETARIAVERDEEMEIOAMBIENTE")] <- "SECRETARIA VERDE E MEIO AMBIENTE"
df_iba_sp$executor[df_iba_sp$executor %in% c("SECRETARIATETURISMO",                              
                                             "SECRETARIATURISMO")] <- "SECRETARIA TURISMO"

df_iba_sp$executor[df_iba_sp$executor %in% c("SERVICOFUNERARIODOMUNICIPIODESAOPAULO")] <- "SERVICO FUNERARIO"
df_iba_sp$executor[df_iba_sp$executor %in% c("CONTROLADORIAGERALDOMUNICIPIO")] <- "CONTROLADORIA GERAL"
df_iba_sp$executor[df_iba_sp$executor %in% c("COHAB",                                             
                                             "COHAB-COMPANHIAMETROPOLITANADEHABITACAODESAOPAULO")] <- "COHAB"

df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAARICANDUVA",
                                             "SUBPREFEITURAARICANDUVACARRAOFORMOSA",
                                             "SUBPREFEITURAARICANDUVAFORMOSACARRAO")] <- "SUBPREFEITURA ARICANDUVA-FORMOSA-CARRAO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURABUTANTA")] <- "SUBPREFEITURA BUTANTA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURACAMPOLIMPO")] <- "SUBPREFEITURA CAMPO LIMPO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURACAPELADOSOCORRO")] <- "SUBPREFEITURA CAPELA DO SOCORRO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURACASAVERDE",
                                             "SUBPREFEITURACASAVERDE,LIMAOECACHOEIRINHA",
                                             "SUBPREFEITURACASAVERDECACHOEIRINHA")] <- "SUBPREFEITURA CASA VERDE-CACHOEIRINHA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURACIDADEADEMAR")] <- "SUBPREFEITURA CIDADE ADEMAR"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURACIDADETIRADENTES")] <- "SUBPREFEITURA CIDADE TIRADENTES"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAERMELINOMATARAZO",
                                             "SUBPREFEITURAERMELINOMATARAZZO")] <- "SUBPREFEITURA ERMELINO MATARAZZO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAFREGUESIA",
                                             "SUBPREFEITURAFREGUESIABRASILANDIA",
                                             "SUBPREFEITURAFREGUESIADOO")] <- "SUBPREFEITURA FREGUESIA-BRASILANDIA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAGUAIANASES",
                                             "SUBPREFEITURAGUAIANAZES",
                                             "SUBPREFEITURAGUIANASES")] <- "SUBPREFEITURA GUAIANASES"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAIPIRANGA")] <- "SUBPREFEITURA IPIRANGA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAITAIMPAULISTA")] <- "SUBPREFEITURA ITAIM PAULISTA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAITAQUERA")] <- "SUBPREFEITURA ITAQUERA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAJABAQUARA")] <- "SUBPREFEITURA JABAQUARA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAJACANA",
                                             "SUBPREFEITURAJACANATREMEMBE")] <- "SUBPREFEITURA JACANA-TREMEMBE"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURALAPA")] <- "SUBPREFEITURA LAPA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUB-M'BOIMIRIM",
                                             "SUBPREFEITURAM'BOIMIRIM",
                                             "SUBPREFEITURAMBOIMIRIM")] <- "SUBPREFEITURA M BOI MIRIM"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAMOOCA")] <- "SUBPREFEITURA MOOCA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAPARELHEIROS",
                                             "SUBPEFEITURADEPARELHEIROS")] <- "SUBPREFEITURA PARELHEIROS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAPENHA")] <- "SUBPREFEITURA PENHA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAPERUS")] <- "SUBPREFEITURA PERUS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAPINHEIROS")] <- "SUBPREFEITURA PINHEIROS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAPIRITUBA",
                                             "SUBPREFEITURAPIRITUBAJARAGUA")] <- "SUBPREFEITURA PIRITUBA-JARAGUA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURASANTANA",
                                             "SUBPREFEITURASANTANATUCURUVI")] <- "SUBPREFEITURA SANTANA-TUCURUVI"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURASANTOAMARO")] <- "SUBPREFEITURA SANTO AMARO"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURASAOMATEUS",
                                             "SUBPREFEITURASAOMATHEUS")] <- "SUBPREFEITURA SAO MATEUS"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURASAOMIGUELPAULISTA")] <- "SUBPREFEITURA SAO MIGUEL"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURASAPOPEMBA")] <- "SUBPREFEITURA SAPOPEMBA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURASE")] <- "SUBPREFEITURA SE"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAVILAMARIA",
                                             "SUBPREFEITURAVILAMARIAVILAGUILHERME")] <- "SUBPREFEITURA VILA MARIA-VILA GUILHERME"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAVILAMARIANA")] <- "SUBPREFEITURA VILA MARIANA"
df_iba_sp$executor[df_iba_sp$executor %in% c("SUBPREFEITURAVILAPRUDENTE")] <- "SUBPREFEITURA VILA PRUDENTE"
unique(sort(df_iba_sp$executor))

political <- c("SECRETARIA CASA CIVIL", "SECRETARIA RELACOES INSTITUCIONAIS", "SECRETARIA GOVERNO")
urban <- c("SECRETARIA HABITACAO", "SECRETARIA MOBILIDADE E TRANSITO", "SECRETARIA TRANSPORTE E MOBILIDADE URBANA",
           "SECRETARIA INFRAESTRUTURA E OBRAS", "SECRETARIA SUBPREFEITURAS", "SECRETARIA URBANISMO E LICENCIAMENTO",
           "SUBPREFEITURA ARICANDUVA-FORMOSA-CARRAO",        
           "SUBPREFEITURA BUTANTA",                          
           "SUBPREFEITURA CAMPO LIMPO",                      
           "SUBPREFEITURA CAPELA DO SOCORRO",                
           "SUBPREFEITURA CASA VERDE-CACHOEIRINHA",          
           "SUBPREFEITURA CIDADE ADEMAR",                    
           "SUBPREFEITURA CIDADE TIRADENTES",                
           "SUBPREFEITURA ERMELINO MATARAZZO",               
           "SUBPREFEITURA FREGUESIA-BRASILANDIA",            
           "SUBPREFEITURA GUAIANASES",                       
           "SUBPREFEITURA IPIRANGA",                         
           "SUBPREFEITURA ITAIM PAULISTA",                   
           "SUBPREFEITURA ITAQUERA",                         
           "SUBPREFEITURA JABAQUARA",                        
           "SUBPREFEITURA JACANA-TREMEMBE",                  
           "SUBPREFEITURA LAPA",                             
           "SUBPREFEITURA M BOI MIRIM",                      
           "SUBPREFEITURA MOOCA",                            
           "SUBPREFEITURA PARELHEIROS",                      
           "SUBPREFEITURA PENHA",                            
           "SUBPREFEITURA PERUS",                            
           "SUBPREFEITURA PINHEIROS",                        
           "SUBPREFEITURA PIRITUBA-JARAGUA",                 
           "SUBPREFEITURA SANTANA-TUCURUVI",                
           "SUBPREFEITURA SANTO AMARO",                      
           "SUBPREFEITURA SAO MATEUS",                       
           "SUBPREFEITURA SAO MIGUEL",                       
           "SUBPREFEITURA SAPOPEMBA",                        
           "SUBPREFEITURA SE",                               
           "SUBPREFEITURA VILA MARIA-VILA GUILHERME",        
           "SUBPREFEITURA VILA MARIANA",                     
           "SUBPREFEITURA VILA PRUDENTE",
           "COHAB")
social <- c("SECRETARIA ASSISTENCIA E DESENVOLVIMENTO SOCIAL",
            "SECRETARIA CULTURA", "SECRETARIA DIREITOS HUMANOS E CIDADANIA",
            "SECRETARIA EDUCACAO", "SECRETARIA ESPORTES E LAZER",
            "SECRETARIA PESSOA COM DEFICIENCIA", "SECRETARIA SAUDE")
other <- c("CONTROLADORIA GERAL", "SECRETARIA DESENVOLVIMENTO ECONOMICO E TRABALHO",
           "SECRETARIA INOVACAO E TECNOLOGIA", "SECRETARIA JUSTICA", "SECRETARIA RELACOES INTERNACIONAIS",
           "SECRETARIA SEGURANCA URBANA", "SECRETARIA VERDE E MEIO AMBIENTE", "SECRETARIA TURISMO",
           "SERVICO FUNERARIO")

df_iba_sp$policy <- NA
df_iba_sp$policy[df_iba_sp$executor %in% political] <- "POLITICAL"
df_iba_sp$policy[df_iba_sp$executor %in% urban] <- "URBAN"
df_iba_sp$policy[df_iba_sp$executor %in% social] <- "SOCIAL"
df_iba_sp$policy[df_iba_sp$executor %in% other] <- "OTHER"
unique(sort(df_iba_sp$policy))

df_iba_sp$sub <- 0
df_iba_sp$sub[df_iba_sp$executor %in% c("SUBPREFEITURA ARICANDUVA-FORMOSA-CARRAO",        
                                        "SUBPREFEITURA BUTANTA",                          
                                        "SUBPREFEITURA CAMPO LIMPO",                      
                                        "SUBPREFEITURA CAPELA DO SOCORRO",                
                                        "SUBPREFEITURA CASA VERDE-CACHOEIRINHA",          
                                        "SUBPREFEITURA CIDADE ADEMAR",                    
                                        "SUBPREFEITURA CIDADE TIRADENTES",                
                                        "SUBPREFEITURA ERMELINO MATARAZZO",               
                                        "SUBPREFEITURA FREGUESIA-BRASILANDIA",            
                                        "SUBPREFEITURA GUAIANASES",                       
                                        "SUBPREFEITURA IPIRANGA",                         
                                        "SUBPREFEITURA ITAIM PAULISTA",                   
                                        "SUBPREFEITURA ITAQUERA",                         
                                        "SUBPREFEITURA JABAQUARA",                        
                                        "SUBPREFEITURA JACANA-TREMEMBE",                  
                                        "SUBPREFEITURA LAPA",                             
                                        "SUBPREFEITURA M BOI MIRIM",                      
                                        "SUBPREFEITURA MOOCA",                            
                                        "SUBPREFEITURA PARELHEIROS",                      
                                        "SUBPREFEITURA PENHA",                            
                                        "SUBPREFEITURA PERUS",                            
                                        "SUBPREFEITURA PINHEIROS",                        
                                        "SUBPREFEITURA PIRITUBA-JARAGUA",                 
                                        "SUBPREFEITURA SANTANA-TUCURUVI",                
                                        "SUBPREFEITURA SANTO AMARO",                      
                                        "SUBPREFEITURA SAO MATEUS",                       
                                        "SUBPREFEITURA SAO MIGUEL",                       
                                        "SUBPREFEITURA SAPOPEMBA",                        
                                        "SUBPREFEITURA SE",                               
                                        "SUBPREFEITURA VILA MARIA-VILA GUILHERME",        
                                        "SUBPREFEITURA VILA MARIANA",                     
                                        "SUBPREFEITURA VILA PRUDENTE")] <- 1

df_iba_sp$status <- df_iba_sp$status %>% str_trim() %>% str_to_upper() 
unique(sort(df_iba_sp$status))
df_iba_sp$status[df_iba_sp$status == "LIBERADO"] <- "LIBERADA"
df_iba_sp$status[df_iba_sp$status %in% c("CANCELADO", "EM CONSULTA", "EM CORREÇÃO",
                                         "EM SF", "NÃO LIBERADO", "NEGADO",
                                         "PMO E ANUÊNCIA")] <- "NAO LIBERADA"
unique(sort(df_iba_sp$status))

df_iba_sp$election_year <- NA
df_iba_sp$election_year[df_iba_sp$year %in% c(2017, 2018, 2019, 2020)] <- "2016"
df_iba_sp$election_year[df_iba_sp$year %in% c(2021, 2022, 2023, 2024)] <- "2020"

df_iba_sp$councilor[df_iba_sp$councilor == "PROFESSOR TONINHO VESPOLI"] <- "TONINHO VESPOLI"

n1 <- c("SUBPREFEITURA CASA VERDE-CACHOEIRINHA", "SUBPREFEITURA FREGUESIA-BRASILANDIA",
        "SUBPREFEITURA PERUS", "SUBPREFEITURA PIRITUBA-JARAGUA")
n2 <- c("SUBPREFEITURA JACANA-TREMEMBE", "SUBPREFEITURA SANTANA-TUCURUVI",
        "SUBPREFEITURA VILA MARIA-VILA GUILHERME")
s1 <- c("SUBPREFEITURA IPIRANGA", "SUBPREFEITURA JABAQUARA", "SUBPREFEITURA VILA MARIANA")
s2 <- c("SUBPREFEITURA CAMPO LIMPO", "SUBPREFEITURA CAPELA DO SOCORRO",
        "SUBPREFEITURA CIDADE ADEMAR", "SUBPREFEITURA M BOI MIRIM",
        "SUBPREFEITURA PARELHEIROS", "SUBPREFEITURA SANTO AMARO")
e1 <- c("SUBPREFEITURA ARICANDUVA-FORMOSA-CARRAO", "SUBPREFEITURA MOOCA",
        "SUBPREFEITURA PENHA", "SUBPREFEITURA SAPOPEMBA", "SUBPREFEITURA VILA PRUDENTE")
e2 <- c("SUBPREFEITURA CIDADE TIRADENTES", "SUBPREFEITURA ERMELINO MATARAZZO",
        "SUBPREFEITURA GUAIANASES", "SUBPREFEITURA ITAIM PAULISTA",
        "SUBPREFEITURA ITAQUERA", "SUBPREFEITURA SAO MATEUS",  "SUBPREFEITURA SAO MIGUEL")
we <- c("SUBPREFEITURA LAPA", "SUBPREFEITURA BUTANTA", "SUBPREFEITURA PINHEIROS")
ce <- c("SUBPREFEITURA SE")

df_iba_sp$region_8 <- NA
df_iba_sp$region_8[df_iba_sp$executor %in% n1] <- "NORTH 1"
df_iba_sp$region_8[df_iba_sp$executor %in% n2] <- "NORTH 2"
df_iba_sp$region_8[df_iba_sp$executor %in% s1] <- "SOUTH 1"
df_iba_sp$region_8[df_iba_sp$executor %in% s2] <- "SOUTH 2"
df_iba_sp$region_8[df_iba_sp$executor %in% e1] <- "EAST 1"
df_iba_sp$region_8[df_iba_sp$executor %in% e2] <- "EAST 2"
df_iba_sp$region_8[df_iba_sp$executor %in% we] <- "WEST"
df_iba_sp$region_8[df_iba_sp$executor %in% ce] <- "CENTER"

df_iba_sp$region_5 <- NA
df_iba_sp$region_5[df_iba_sp$executor %in% c(n1, n2)] <- "NORTH"
df_iba_sp$region_5[df_iba_sp$executor %in% c(s1, s2)] <- "SOUTH"
df_iba_sp$region_5[df_iba_sp$executor %in% c(e1, e2)] <- "EAST"
df_iba_sp$region_5[df_iba_sp$executor %in% we] <- "WEST"
df_iba_sp$region_5[df_iba_sp$executor %in% ce] <- "CENTER"

df_iba_sp_lib <- subset(df_iba_sp, status == "LIBERADA")

write.csv2(df_iba_sp_lib, "02_out/iba_sp.csv", row.names = F, fileEncoding = "UTF-8")
