#### PhD Thesis - Preparing database
#### PhD Thesis - Preparing database
#### PhD Thesis - Preparing database
#### PhD Thesis - Preparing database

# Author: Alexandre de Queiroz Stein
# Starting date: 18/06/2023

#### Libraries and options ----------------

library(tidyverse)
library(geobr)
library(data.table)
library(sf)

rm(list = ls(all = TRUE))



#### Loading Data --------------

    # CNAES Compatibility -------------

    cnae <- fread("1_data/attrs_cnae.csv") %>% 
        filter(str_length(id) == 6) %>% 
        select(id, name_en, name_pt) %>% 
        rename(cnae_subclasse = id,
               cnae_subclasse_name = name_en,
               cnae_subclasse_name_pt = name_pt) %>% 
        mutate(cnae_secao = str_sub(cnae_subclasse, 1, 1),
               cnae_subclasse = str_sub(cnae_subclasse, 2, str_length(cnae_subclasse))) %>% 
    distinct()

    cnae_secao <- fread("1_data/attrs_cnae.csv") %>% 
        filter(str_length(id) == 1) %>% 
        select(id, name_en, name_pt, color) %>% 
        rename(cnae_secao = id,
               cnae_secao_name = name_en,
               cnae_secao_name_pt = name_pt) %>% 
        distinct()
    
    cnaes <- cnae %>% 
        left_join(cnae_secao)
    
    cnae_comp <- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cnae_2.csv")
    
    
    
    rm(cnae, cnae_secao)
    
    # Municipality codes
    
    municipios <- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/municipio.csv") %>% 
        select(id_municipio_6, nome) %>% 
        rename(municipio = id_municipio_6,
               municipio_nome = nome) %>% 
        mutate(municipio = as.character(municipio))


    # Loading location data ----------------
    loc <- data.table::fread("1_data/raisgeo_2010_2019.csv",
                             colClasses = c(id_estab = "character",
                                            codemun = "character")) %>% 
        arrange(desc(score)) %>% # assuming score is getting quality of location
        distinct(id_estab, ano, .keep_all = TRUE) 
    
    
    # CCI ----------------------------
    
    cci2010 <- read_rds("C:/Users/queir/Meu Drive/Economia/3 - Doutorado/Tese/Complexidade_empresas/3_results/cci_2010.RDS") %>% 
        mutate(ano = 2010)
    
    cci2019 <- read_rds("C:/Users/queir/Meu Drive/Economia/3 - Doutorado/Tese/Complexidade_empresas/3_results/cci_2019.RDS") %>% 
        mutate(ano = 2019)
    
    cci <- rbind(cci2010, cci2019)
    
    
    loc2 <- loc %>% 
        left_join(cci, by = c("id_estab" = "cnpj_cei",
                              "ano" = "ano")) %>% 
        filter(!is.na(cci) & !is.na(lon) & !is.na(lat)) %>% 
        select(ano, everything()) %>% 
        st_as_sf(coords = c("lon","lat"), remove = FALSE)
    
    st_write(loc2, "1_data/rais_loc_cci.gpkg", delete_dsn = TRUE)
    
    
    
    
    
    '# Loading RAIS data ------------
    rais10 <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/Estb2019ID.txt",
                                nrows = Inf, encoding = "Latin-1",
                                colClasses = list("character" = c(1:19, 26:29))) %>%
        janitor::clean_names() %>% 
        mutate(ano = 2010)

    rais19 <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/Estb2019ID.txt",
                            nrows = Inf, encoding = "Latin-1",
                            colClasses = list("character" = c(1:19, 26:29))) %>%
        janitor::clean_names()%>% 
        mutate(ano = 2019)
    
    
    # Preparando para join
    rais <- rbind(rais10, rais19) %>% 
        filter(municipio %in% loc$codemun & !is.na(cnpj_cei) & cnpj_cei != 0) %>% 
        distinct() %>% 
        select(cei_vinculado, cep_estab, cnpj_cei, municipio, qtd_vinculos_ativos, tamanho_estabelecimento, cnae_2_0_subclasse, ano)
    
    loc <- loc %>% 
        select(id_estab, lon, lat, codemun, name_muni, ano, score)
    
    rm(rais10, rais19)
    
    gc()
    
    
#### Joining RAIS and locations -------------------
    
    
    
    # Joining location and rais
    
    rais_loc <- rais %>% 
        left_join(loc, by = c("cnpj_cei" = "id_estab",
                              "ano" = "ano")) %>% 
        filter(!is.na(lon) & !is.na(lat)) %>% 
        left_join(municipios) %>% 
        select(ano, codemun, name_muni, municipio, municipio_nome, everything()) %>% 
        rename(code_muni_loc = codemun,
               name_muni_loc = name_muni,
               code_muni_rais = municipio,
               name_muni_rais = municipio_nome,
               cnae_subclasse = cnae_2_0_subclasse) %>% 
        mutate(cnae_subclasse = str_sub(cnae_subclasse, 1, 5)) %>% 
        left_join(cnaes) %>% 
    st_as_sf(coords = c("lon","lat"), remove = FALSE)
    
    st_write(rais_loc, "1_data/rais_loc.gpkg", delete_dsn = TRUE)'

    

    