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


#tamanho_hexagonos <- 0.01
tamanho_hexagonos <- 0.02


#0.0033635 para mesma área do ipea: 0,11km2,
#0.003398 ou 0.0034 para mesmo número de hexagonos que do ipea: 3049 em BH
# Porém o que parece razoável é 0.007


#### Loading shapefile RMBH ----

rais.loc <- st_read("1_data/rais_loc_cci.gpkg")

# Note that between Fortuna de Minas and Inhaúma there's a black space of municipalities
# that are not considered metropolitan region
shape.rmbh <- geobr::read_metro_area() %>% 
    filter(name_metro == "RM Belo Horizonte")

'st_write(shape.rmbh, "1_data/shape_RMBH.gpkg", delete_dsn = TRUE)'

centroides.rmbh <- geobr::read_municipal_seat() %>% 
    mutate(code_muni = str_sub(code_muni, 1, 6),
           coordx = map_dbl(geom, 1),
           coordy = map_dbl(geom, 2)) %>% 
    filter(code_muni %in% rais.loc$code_muni_loc | code_muni %in% rais.loc$code_muni_rais) 

'st_write(centroides.rmbh, "1_data/shape_centroides_rmbh.gpkg", delete_dsn = TRUE)'

# Adjusting crs
crs.usado <- st_crs(shape.rmbh)
st_crs(rais.loc) <- crs.usado
rais.loc <- st_transform(rais.loc, crs.usado)

st_crs(rais.loc)
st_crs(shape.rmbh)


# Filtering points outside RMBH


a <- unlist(st_intersects(shape.rmbh, rais.loc))

rais.loc <- rais.loc[a,]
    
st_write(rais.loc, "1_data/rais_loc_insideRMBH_cci.gpkg", delete_dsn = TRUE)


# Making hexagonal grid ---------------------------------------------------

grid <- st_make_grid(shape.rmbh, cellsize = tamanho_hexagonos, square = FALSE)
grid <- sf::st_sf(grid)
grid$ID <- paste("ID", c(1:length(grid$grid)), sep = "")
b <- unlist(st_intersects(shape.rmbh, grid))
grid <- grid[b,] %>% 
    select(ID, grid) %>% 
    rename(geom = grid)


st_write(grid, paste0("1_data/grid_RMBH_", tamanho_hexagonos, ".gpkg"), delete_dsn = TRUE)


saveRDS(tamanho_hexagonos, "1_data/tamanho_hexagonos.RDS")
    
# Discard -------
'sample <- sample(c(1:nrow(rais.loc)), 1000)
rais.loc <- rais.loc[sample,]'

'grid_aop <- aopdata::read_grid(city = "bho") %>% 
mutate(area = st_area(.))

plot(grid_aop$geom)

st_crs(grid_aop)
st_crs(grid)'
    
    'O valor de cell size 0.003363 deixa muito parecido com o do IPEA quando
utilizamos o shape de municípios do IBE. Entretanto, os CRS são diferentes,
de forma que os resultados não serão iguais. Área aproximada de 0,11km2.

0.003398

0.0034

shape.bh <- shape.rmbh %>% 
    filter(name_muni == "Belo Horizonte")

'



'shape.rmbh <- st_read("C:/backup_arquivos/Outros/shapes/BR_Municipios_2022.shp") %>%  #geobr::read_municipality() %>% 
select(CD_MUN, NM_MUN) %>% 
rename(code_muni = CD_MUN,
       name_muni = NM_MUN) %>% 
mutate(code_muni = str_sub(code_muni, 1, 6)) %>% 
filter(code_muni %in% rais.loc$code_muni_loc | code_muni %in% rais.loc$code_muni_rais)'