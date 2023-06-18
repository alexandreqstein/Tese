library(tidyverse)
library(geobr)
library(data.table)
library(sf)

setwd("C:/Users/queir/Meu Drive/Economia/3 - Doutorado/Tese/1_dados")

rm(list = ls(all = TRUE))

# CNAES Compatibility

cnae <- fread("cnae_2.csv",
              colClasses = list("character" = 1:8)) %>% 
    select(divisao, secao, descricao_secao) %>% 
    distinct()


# Loading location data
loc <- data.table::fread("raisgeo_2010_2019.csv",
                         colClasses = c(id_estab = "character",
                                        codemun = "character")) %>% 
    filter(ano == 2019) %>% 
    distinct()

# Loading RAIS data
rais <- data.table::fread("C:/bases/RAIS/RAIS2019ID_estab/Estb2019ID.txt",
                          nrows = Inf, encoding = "Latin-1",
                          colClasses = c(`CNPJ Raiz` = "character",
                                         `CNPJ / CEI` = "character",
                                         Município = "character")) %>%
    janitor::clean_names()

rais2 <- rais %>% 
    filter(municipio %in% loc$codemun) %>% 
    distinct(cnpj_cei, .keep_all = TRUE)
    

gc()

# Joining location and rais
rais_loc <- loc %>% 
    filter(ano == 2019) %>% 
    left_join(rais2, by = c("id_estab" = "cnpj_cei")) %>% 
    mutate(cnae_divisao = str_sub(cnae_2_0_classe, 1, 2)) %>% 
    left_join(cnae, by = c("cnae_divisao" = "divisao"))

rais_bh_loc <- rais_loc %>% 
    filter(name_muni == "Belo Horizonte") %>% 
    slice(1:10000) %>% 
    st_as_sf(coords = c("lon","lat"), remove = FALSE)

# Loading shapefile RMBH



shape_bh <- geobr::read_municipality(code_muni = 31)
shape_bh <- shape_bh %>% 
    filter(name_muni == "Belo Horizonte")

st_crs(rais_bh_loc) <- st_crs(shape_bh)

rais_bh_loc <- mutate(rais_bh_loc, BH = as.integer(st_intersects(rais_bh_loc, shape_bh))) %>% 
    filter(!is.na(BH))
                              

ggplot()+
    geom_sf(data = shape_bh)+
    geom_jitter(data = rais_bh_loc, 
               aes(x = lon, y = lat, color = secao),
               size = 0.3,
               show.legend = FALSE)+
    scale_color_brewer(palette = "Paired")+
    scale_x_continuous(limits = c(-44.06334, -43.85658))+
    scale_y_continuous(limits = c(-20.05970, -19.77659))+
    theme_grey(base_size = 12)+
    theme(panel.background = element_rect(fill = "white"))+
    labs(x = NULL,
         y = NULL,
         title = "Companies Location - Sample n = 10000",
         caption = "Source: RAIS (2019)")

ggsave(filename = "C:/Users/queir/Meu Drive/Economia/3 - Doutorado/Tese/3_results/sample_empresas_bh.png", 
       plot = last_plot(),
       height = 25, width = 17, units = "cm")



# Grid

grid <-  st_make_grid(shape_bh, cellsize = 0.007, square = FALSE)
grid <- sf::st_sf(grid)
grid_bh <- sf::st_intersection(x = shape_bh, y = grid)

ggplot()+
    geom_sf(data = shape_bh)+
    geom_jitter(data = rais_bh_loc, 
                aes(x = lon, y = lat, color = secao),
                size = 0.3,
                show.legend = FALSE)+
    geom_sf(data = grid_bh, fill = NA)+
    scale_color_brewer(palette = "Paired")+
    scale_x_continuous(limits = c(-44.06334, -43.85658))+
    scale_y_continuous(limits = c(-20.05970, -19.77659))+
    theme_grey(base_size = 12)+
    theme(panel.background = element_rect(fill = "white"))+
    labs(x = NULL,
         y = NULL,
         title = "Companies Location - Sample n = 10000",
         caption = "Source: RAIS (2019)")

ggsave(filename = "C:/Users/queir/Meu Drive/Economia/3 - Doutorado/Tese/3_results/sample_empresas_bh_grid.png", 
       plot = last_plot(),
       height = 25, width = 17, units = "cm")

