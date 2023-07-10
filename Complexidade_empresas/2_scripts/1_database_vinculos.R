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
library(EconGeo)
library(economiccomplexity)
library(foreach)
library(doParallel)
library(classInt)

rm(list = ls(all = TRUE))



#### Side data --------------

    # CBOS
    
    cbos <- fread("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cbo_2002.csv",
                  colClasses = list(character = 1:10)) %>%
        select(cbo_2002, descricao) %>% 
        mutate(cbo_2002 = as.character(cbo_2002))

    loc <- data.table::fread("1_data/raisgeo_2010_2019.csv",
                             colClasses = c(id_estab = "character",
                                            codemun = "character")) %>% 
        arrange(desc(score)) %>% # assuming score is getting quality of location
        distinct(id_estab, ano, .keep_all = TRUE) 
    
    
#### Loading RAIS data ------------
    rais19 <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/RAIS_VINC_ID_MG_ES_RJ.txt",
                            nrows = Inf, encoding = "Latin-1",
                            colClasses = list("character" = c(1:22, 31:42, 60:66)),
                            sep = ";", dec = ",",
                            select = c("CNPJ / CEI",
                                       "Município",
                                       "Vínculo Ativo 31/12",
                                       "CBO Ocupação 2002",
                                       "CNAE 2.0 Classe",
                                       "Vl Remun Dezembro Nom")) %>% 
        janitor::clean_names() %>% 
        mutate(ano = 2019) %>% 
        filter(municipio %in% loc$codemun & 
                   !is.na(cnpj_cei) & 
                   cnpj_cei != 0 &
                   vinculo_ativo_31_12 == "1" &
                   substr(cnae_2_0_classe, 1, 2) != 84) %>% 
        distinct() %>% 
        mutate(id_vinc = c(1:nrow(.))) %>% 
        select(id_vinc, everything())
    
    lista_vinculos <- list()

    cbos_unique <- unique(rais19$cbo_ocupacao_2002)
    
        
        
    for(i in seq_along(cbos_unique)){
    
        print(i)
    
        df <- rais19 %>% 
            select(id_vinc, cnpj_cei, cbo_ocupacao_2002, vl_remun_dezembro_nom) %>% 
            filter(cbo_ocupacao_2002 == cbos_unique[i])
        
        if(nrow(df) <= 3 | # Aqui acabamos por retirar cbos que tem menos de 3 vínculos
           (max(df$vl_remun_dezembro_nom) - min(df$vl_remun_dezembro_nom) < 10)){ # aqui retiramos cbos com variacao da remuneracao menor que 10 reais
            next
        } else {
        
        intervalo <- classIntervals(var = df$vl_remun_dezembro_nom,
                                              n = 3,
                                              style = "fisher")$brks
        
        lista_vinculos[[i]] <- df %>% 
            mutate(cbo_nova = cut(vl_remun_dezembro_nom,
                                  breaks = intervalo,
                                  labels = c(paste0(cbos_unique[i], "_A"), 
                                             paste0(cbos_unique[i], "_B"),
                                             paste0(cbos_unique[i], "_C")),
                                     include.lowest = TRUE))
        }
    }
        
    
    
    lista_vinculos <- do.call(rbind, lista_vinculos)
    
    
    # Municipality codes
    
    municipios <- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/municipio.csv") %>% 
        select(id_municipio_6, nome) %>% 
        rename(municipio = id_municipio_6,
               municipio_nome = nome) %>% 
        mutate(municipio = as.character(municipio))
    
    
    # Companies names
    nomes_emp <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/Estb2019ID.txt",
                                   nrows = Inf, encoding = "Latin-1",
                                   select = c("CNPJ / CEI",
                                              "Razão Social",
                                              "Qtd Vínculos Ativos",
                                              "Qtd Vínculos CLT"),
                                   colClasses = list("character" = c(1:19, 26:29))) %>%
        janitor::clean_names()
    
    emp_poucos_vinc <- nomes_emp %>% 
        filter(qtd_vinculos_ativos < 5) %>%
        select(cnpj_cei) %>% 
        pull(cnpj_cei)



#### Preparing data ------------
    
    data <- lista_vinculos %>%
        select(-cbo_ocupacao_2002) %>% 
        rename(cbo_ocupacao_2002 = cbo_nova) %>% 
        select(cnpj_cei, cbo_ocupacao_2002, vl_remun_dezembro_nom) %>% 
        group_by(cnpj_cei, cbo_ocupacao_2002) %>% 
        summarise(value = sum(vl_remun_dezembro_nom, na.rm = TRUE)) %>% 
        ungroup() %>%
        rename(country = cnpj_cei,
               product = cbo_ocupacao_2002) %>% 
        filter(!country %in% emp_poucos_vinc)
    
    # Removing some outliers products and countries
    ocupacao_unica <- data %>%
        group_by(country) %>%
        mutate(proc = ifelse(value > 0,
                             yes = 1,
                             no = 0)) %>%
        summarise(n.proc = sum(proc, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(n.proc < 2) %>%
        pull(country)
    
    
    emp_unica <- data %>%
        group_by(product) %>%
        mutate(proc = ifelse(value > 0,
                             yes = 1,
                             no = 0)) %>%
        summarise(n.proc = sum(proc, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(n.proc < 2) %>%
        pull(product)
    
    
    muns_zero <- data %>%
        group_by(country) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 1500) %>%
        pull(country)
    
    
    products_zero <- data %>%
        group_by(product) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 1500) %>%
        pull(product)
    
    
    data <- data %>%
        distinct() %>% 
        filter(!country %in% muns_zero & 
                   !product %in% products_zero & 
                   !country %in% ocupacao_unica &
                   !product %in% emp_unica) %>%
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0) %>%
        {.->> data_mat} %>%
        pivot_longer(cols = 2:last_col(),
                     values_to = "value",
                     names_to = "product")
    
    # Preparing data
    data_mat <- data_mat %>%
        column_to_rownames(var = "country")
    
    nomes <- list(names1 = rownames(data_mat),
                  names2 = colnames(data_mat))
    
    data_mat <- as.matrix(x = data_mat)
    dimnames(data_mat) <- nomes
    
    gc()
    
    
    # Diversity and ubiquity --------------------------------------------------
    
    mat.rca <- location.quotient(mat = data_mat, binary = TRUE)
    
    rca_lista <- mat.rca %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "country") %>% 
        pivot_longer(cols = 2:last_col(),
                     names_to = "produtc",
                     values_to = "value")
    
    coocorrencias <- co.occurrence(mat = t(mat.rca), diagonal = TRUE) #c?lculo da coocorr?ncias - exige a matriz transposta
    
    proximidades <- relatedness(mat = coocorrencias, method = "cosine")
    
    diversificacao <- data.frame(diversificacao = EconGeo::diversity(mat = mat.rca, RCA = FALSE)) %>%
        rownames_to_column(var = "country") %>%
        left_join(nomes_emp, by = c("country" = "cnpj_cei"))
    
    ubiquidade <- data.frame(ubiquidade = EconGeo::ubiquity(mat = mat.rca, RCA = FALSE)) %>%
        rownames_to_column(var = "product") %>%
        mutate(cbo = str_sub(product, 1, 6),
               nivel_cbo = str_sub(product, -1, -1)) %>% 
        left_join(cbos, by = c("cbo" = "cbo_2002"))
    
    
    mcp <- Matrix(data = mat.rca, sparse = TRUE)
    
    
    
    # Complexity indexes
    
    complexity <- complexity_measures(mcp, iterations = 20, method = "reflections")
    
    
    product_complexity <- data.frame(pci = complexity[[2]]) %>%
        mutate(pci = pci) %>%
        rownames_to_column(var = "product") %>%
        left_join(ubiquidade) 
    
    regions_complexity <- data.frame(eci = complexity[[1]]) %>%
        mutate(eci = eci) %>% 
        rownames_to_column(var = "country") %>%
        left_join(diversificacao) 
    
    
    ggplot(filter(regions_complexity, eci > -15))+
        geom_point(aes(x = log(qtd_vinculos_ativos), y = eci))
    
    ggplot(product_complexity)+
        geom_point(aes(x = log(ubiquidade), y = pci))


    teste <- data %>% 
        filter(country == "00119010021406")
    
    teste <- rca_lista %>% 
        filter(country == "00119010021406")
    
    
    


    saveRDS(regions_complexity, str_c("3_results_6d/ECI_", anos[k], ".RDS"))
    saveRDS(product_complexity, str_c("3_results_6d/PCI_", anos[k], ".RDS"))
    saveRDS(regions_complexity, str_c("3_results_6d_cnae/ECI_", anos[k], ".RDS"))
    saveRDS(product_complexity, str_c("3_results_6d_cnae/PCI_", anos[k], ".RDS"))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
    
    st_write(rais_loc, "1_data/rais_loc.gpkg", delete_dsn = TRUE)

    

    