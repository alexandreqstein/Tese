#### PhD Thesis - Spatializing and grids
#### PhD Thesis - Spatializing and grids
#### PhD Thesis - Spatializing and grids
#### PhD Thesis - Spatializing and grids

# Author: Alexandre de Queiroz Stein
# Starting date: 18/06/2023


#### Libraries and options ----------------

library(tidyverse)
library(data.table)
library(sf)
library(aopdata)

options(scipen = 999L)

rm(list = ls(all = TRUE))


st_intersection_faster <- function(x,y,...){
    #faster replacement for st_intersection(x, y,...)
    
    y_subset <-
        st_intersects(x, y) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        {y[.,]}
    
    st_intersection(x, y_subset,...)
}


#### Loading files ----

tamanho_hexagonos <- read_rds("1_data/tamanho_hexagonos.RDS")

rais.loc <- st_read("1_data/rais_loc_insideRMBH_cci.gpkg") %>% 
    distinct()

shape.rmbh <- st_read("1_data/shape_RMBH.gpkg")

centroides.rmbh <- st_read("1_data/shape_centroides_rmbh.gpkg")

grid <- st_read(paste0("1_data/grid_RMBH_", tamanho_hexagonos, ".gpkg"))



rais_ano <- rais.loc %>% 
    filter(ano == 2010) %>% 
    distinct()

loc2 <- rais_ano %>% 
    select(id_estab)

loc3 <- loc2[c(1:1000),]


'ESTÃO SAINDO INTERSEÇÕES DUPLICADAS. PROVAVEL QUE NÃO HOUVE FILTRO PELO SCORE PARA DEIXAR SOMENTE UM PONTO.
AUMENTAR TAMANHO DOS HEXAGONOS. ESTÃO MUITO PEQUENOS. VER TAMANHO EM OUTROS TRABALHOS E PENSAR SEM HEXAGONO.'

location_correct <- st_intersection_faster(grid, loc3)

location_correct2 <- st_intersection_faster(loc3, grid)








teste <- location_correct %>% 
    st_drop_geometry() %>% 
    group_by(id_estab) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(n >1)

teste2 <- location_correct %>% 
    filter(id_estab %in% teste$id_estab) %>% 
    group_by(ID) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)

teste3 <- location_correct %>% 
    filter(ID == 	
               "ID13129")

ggplot()+
    geom_sf(data = filter(location_correct, id_estab == "00110540240067"),
            size = 0.01, color = "red")+
    geom_sf(data = filter(grid, ID == "ID22341"), fill = NA)


ggsave("3_results/teste.pdf", height = 30, width = 30,
       dpi = 800, plot = last_plot())






st_within()

sample <- sample(c(1:nrow(rais.loc)), 1000)
rais.loc2 <- rais.loc[sample,]

hexagono <- grid %>% 
    slice(1380:1400)


hexagono <- grid[1,]

ggplot()+
    geom_sf(data = shape.rmbh)+
    geom_sf(data = hexagono)+
    geom_sf(data = rais.loc2, color = "red")

'O ST_INTERSECTS RESOLVE, ENTRETANTO TENHO QUE APRENDER MELHOR A MANIPULAR AS LISTAS,
PEGANDO POR EXEMPLO O NÚMERO DO ELEMENTO DA LISTA, QUE É A LINHA DE RAIS.LOC, E ENTÃO ASSOCIAR
AO QUE APARECE EM VALUE, QUE SÃO EM QUAIS HEXAGONOS CADA PONTO ESTÁ CONTIDO.'

a <- st_intersects(rais.loc2, hexagono, sparse = TRUE)

a <- st_intersects(rais.loc2, hexagono) %>% lengths > 0


b <- rais.loc2[st_intersects(rais.loc2, hexagono) %>% lengths > 0,]



a <- unlist(st_intersects(shape.rmbh, rais.loc))

a <- st_intersection(x = rais.loc2, y = grid)

b <- do.call(rbind, a)









