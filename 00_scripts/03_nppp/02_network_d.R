library(tidyverse)
library(igraph)
library(ggnetwork)
library(network)
library(intergraph)
library(ggraph)
library(janitor)
library(readxl)

nppp_edges <- read_delim("02_out/nppp_edges.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE)

g <- graph_from_data_frame(nppp_edges, directed = T)

nppp_councilors <- read_delim("02_out/nppp_councilors.csv", 
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                  grouping_mark = "."), trim_ws = TRUE)

nppp_npos <- read_delim("02_out/nppp_npos.csv", 
                              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                  grouping_mark = "."), trim_ws = TRUE)

aux_councilors <- data.frame(vertex = c(unique(nppp_edges$from))) %>% 
  mutate(TYPE = "CC")

aux_npos <- data.frame(vertex = c(unique(nppp_edges$to))) %>% 
  mutate(TYPE = "NPO")

aux_vertex <- rbind(aux_councilors, aux_npos)

aux_vertex <- aux_vertex %>% 
  left_join(select(nppp_councilors, NM_URNA_CANDIDATO, IDEOLOGY, D, G, PCT_VOTOS_VER_TOTAL, CLASS), by = c("vertex" = "NM_URNA_CANDIDATO"))

#V(g)$type <- ifelse(V(g)$name %in% nppp_edges$from, 0, 1)

V(g)$type <- aux_vertex$TYPE
V(g)$ideology <- aux_vertex$IDEOLOGY
V(g)$class <- aux_vertex$CLASS

###################teste bipartite

# ggraph(g, layout = "bipartite") +
#   geom_edge_link(aes(width = weight),  # Use weight for edge thickness
#                  arrow = arrow(length = unit(4, 'mm')), 
#                  end_cap = circle(3, 'mm')) +  # Adds directed edges with arrows
#   geom_node_point(aes(color = factor(type)), size = 5) +  # Colors nodes based on type
#   geom_node_text(aes(label = name), vjust = -1) +  # Adds labels to the nodes
#   scale_edge_width(range = c(0.5, 2)) +  # Adjusts the range of edge thickness
#   theme_minimal() +  # A minimal theme for a clean look
#   labs(title = "Bipartite Directed Network with Weights") 

###################plot

net <- asNetwork(g)
net %e% "weight" <- E(g)$weight
net %v% "type" <- V(g)$type
net %v% "ideology" <- V(g)$ideology
net %v% "class" <- V(g)$class

set.seed(1234)
ggplot(ggnetwork(net, layout = "fruchtermanreingold"), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(size = weight), color = "gray50", arrow = arrow(type = "closed", length = unit(6, "pt")), curvature = 0.2) +
  geom_nodes(aes(color = ideology, shape = type), size = 9) +
  geom_nodelabel(aes(label = vertex.names), color = "black", fontface = "bold", size = 2) +
  theme_blank() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Directed Network with Edge Weights") +
  scale_size_continuous(range = c(0.5, 3)) +
  scale_color_manual(values = c("LEFT" = "salmon", "CENTER" = "lightgreen", "RIGHT" = "lightblue"), na.value = "gray") +
  scale_shape_manual(values = c("CC" = 16, "NPO" = 17))

set.seed(1234)
ggplot(ggnetwork(net, layout = "fruchtermanreingold"), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(size = weight), color = "gray50", arrow = arrow(type = "closed", length = unit(6, "pt")), curvature = 0.2) +
  geom_nodes(aes(color = class, shape = type), size = 9) +
  geom_nodelabel(aes(label = vertex.names), color = "black", fontface = "bold", size = 2) +
  theme_blank() +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Directed Network with Edge Weights") +
  scale_size_continuous(range = c(0.5, 3)) +
  scale_color_manual(values = c("DOMINANT-CONCENTRATED" = "salmon", "DOMINANT-SCATTERED" = "lightgreen", "SHARED-CONCENTRATED" = "gold", "SHARED-SCATTERED" = "lightblue"), na.value = "gray") +
  scale_shape_manual(values = c("CC" = 16, "NPO" = 17))

############################ descriptive statistics

aux_vertex$in_degree <- degree(g, mode = "in")
aux_vertex$out_degree <- degree(g, mode = "out")

aux_out_degree <- read_excel("01_in/iba/sao_paulo/2019_emendas_R01.xlsx", 
                                skip = 1) %>% 
  clean_names() %>% 
  distinct(vereador_a) %>% 
  mutate(vereador_a = str_to_upper(vereador_a) %>% str_trim(),
         vereador_a = iconv(vereador_a, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         vereador_a = case_when(vereador_a == "ANTONIO DONATO" ~ "DONATO",
                                vereador_a == "GILBERTO NASCIMENTO" ~ "GILBERTO NASCIMENTO JR",
                                vereador_a == "REGINALDO TRIPOLI" ~ "TRIPOLI",
                                vereador_a == "SONINHA FRANCINE" ~ "SONINHA",
                                vereador_a == "JOSE POLICE NETO" ~ "POLICE NETO",
                                vereador_a == "MILTON FERREIRA" ~ "DR. MILTON FERREIRA",
                                T ~ vereador_a)) %>%
  left_join(select(nppp_councilors, NM_URNA_CANDIDATO, CLASS), by  = c("vereador_a" = "NM_URNA_CANDIDATO")) %>% 
  left_join(select(aux_vertex, vertex, out_degree), by = c("vereador_a" = "vertex")) %>% 
  mutate(out_degree = case_when(is.na(out_degree) ~ 0,
                               T ~ out_degree)) %>% 
  group_by(CLASS) %>% 
  summarise(N = n(),
            N_ties_nppp = sum(out_degree != 0),
            mean_out_degree = mean(out_degree, na.rm = T))

aux_in_degree <- nppp_edges %>% 
  left_join(select(aux_vertex, vertex, in_degree), by = c("to" = "vertex")) %>% 
  left_join(select(aux_vertex, vertex, CLASS), by = c("from" = "vertex")) %>% 
  group_by(CLASS) %>% 
  summarise(mean_in_degree = mean(in_degree, na.rm = T))

aux <- aux_out_degree %>% 
  left_join(aux_in_degree, by = c("CLASS")) %>% 
  select(1, 4:5)

density <- sum(degree(g, mode = "in"))/(nrow(aux_councilors)*nrow(aux_npos))
density_value <- sum(degree(g, mode = "in"))/(nrow(aux_councilors)*nrow(aux_npos))

global_transitivity <- transitivity(g, type = "global")
local_transitivity <- transitivity(g, type = "local")
average_path_length <- mean_distance(g, directed = TRUE)
diameter_value <- diameter(g, directed = TRUE)
strong_components <- components(g, mode = "strong")
weak_components <- components(g, mode = "weak")
assortativity_value <- assortativity_degree(g, directed = TRUE)
aux_vertex$page_rank_values <- page_rank(g)$vector
aux_vertex$betweenness_values <- betweenness(g, directed = TRUE)

teste <- nppp_edges %>% 
  left_join(select(nppp_councilors, NM_URNA_CANDIDATO, CLASS), by = c("from" = "NM_URNA_CANDIDATO")) %>% 
  left_join(select(aux_vertex, vertex, in_degree), by = c("to" = "vertex")) %>% 
  group_by(CLASS) %>% 
  summarise(avg_in_degree = mean(in_degree, na.rm = T))



