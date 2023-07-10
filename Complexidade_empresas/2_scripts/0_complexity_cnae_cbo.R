#### Sase article - data cleaning
#### Sase article - data cleaning
#### Sase article - data cleaning

# Start: 09/05/2023
# Author: Alexandre Stein

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(economiccomplexity)
library(EconGeo)
library(Matrix)
library(stringi)
library(foreach)
library(doParallel)


rm(list = ls(all = TRUE))

# Side data -----------------------------------------------------------


cnaes <- read.csv("1_data/attrs_cnae.csv") %>%
    filter(nchar(id) == 6) %>% 
    select(id, desc_en) %>%
    mutate(cnae = str_sub(id, 2, 6)) %>% 
    select(cnae, desc_en)

cbos <- fread("1_data/attrs_cbo.csv",
              colClasses = list(character = 1:10)) %>%
    filter(nchar(id) == 4) %>% 
    select(id, desc_en) %>%
    rename(cbo_2002 = id)


# Data --------------------------------------------------------------------

files <- list.files(path = "C:/Users/queir/Meu Drive/Economia/Outros/Gustavo Britto/sase_2023_rio/digital_techs/1_dados", 
                    pattern = "cnae20",
                    full.names = TRUE)
anos <- str_sub(files, -8, -5)

# Defining paralellization ----------------------------------------------------

# Define number of clusters
parallel::detectCores()
n.cores <- parallel::detectCores() - 10

# Make a cluster
my.cluster <- parallel::makeCluster(
    n.cores, 
    type = "PSOCK"
)


print(my.cluster)

# Registering cluster
doParallel::registerDoParallel(cl = my.cluster)


# Checking 
foreach::getDoParRegistered()
foreach::getDoParWorkers()


# Complaxity measures -----------------------------------------------------

foreach( k = seq_along(files),
         .packages = c("tidyverse",
                       "data.table",
                       "economiccomplexity",
                       "EconGeo",
                       "Matrix")) %dopar% {
    
    # Load data
    dados<- readRDS(files[k]) %>% 
        filter(substr(cnae, 1, 2) != 84)
    

    data <- dados %>%
        mutate(cnae = substr(cnae, 1, 5),
               cbo_2002 = substr(cbo_2002, 1, 4)) %>% 
        group_by(cnae, cbo_2002) %>% 
        summarise(qtd_vinc = sum(qtd_vinc, na.rm = TRUE)) %>% 
        ungroup() %>% 
        rename(country = cnae,
               product = cbo_2002,
               value = qtd_vinc) %>%
        pivot_wider(names_from = product,
                    values_from = value,
                    values_fill = 0) %>%
        pivot_longer(cols = 2:last_col(),
                     names_to = "product",
                     values_to = "value")
    
    # Removing some outliers products and countries
    procs_unicos <- data %>%
        group_by(country) %>%
        mutate(proc = ifelse(value > 0,
                             yes = 1,
                             no = 0)) %>%
        summarise(n.proc = sum(proc, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(n.proc < 0) %>%
        pull(country)
    
    muns_zero <- data %>%
        group_by(country) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 101) %>% #removed sectors with less then 100 jobs
        pull(country)
    
    products_zero <- data %>%
        group_by(product) %>%
        summarise(total = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(total < 101) %>%
        pull(product)
    
    data <- data %>%
        filter(!country %in% muns_zero & !product %in% products_zero & !country %in% procs_unicos) %>%
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
    
    
    # Diversity and ubiquity --------------------------------------------------
    
    mat.rca <- location.quotient(mat = data_mat, binary = TRUE)
    
    coocorrencias <- co.occurrence(mat = t(mat.rca), diagonal = TRUE) #c?lculo da coocorr?ncias - exige a matriz transposta
    
    proximidades <- relatedness(mat = coocorrencias, method = "cosine")
    
    diversificacao <- data.frame(diversificacao = EconGeo::diversity(mat = mat.rca, RCA = FALSE)) %>%
        rownames_to_column(var = "country") %>%
        left_join(cnaes, by = c("country" = "cnae")) %>%
        select(country, desc_en, diversificacao)
    
    ubiquidade <- data.frame(ubiquidade = EconGeo::ubiquity(mat = mat.rca, RCA = FALSE)) %>%
        rownames_to_column(var = "product") %>%
        left_join(cbos, by = c("product" = "cbo_2002"))
    
    
    mcp <- Matrix(data = mat.rca, sparse = TRUE)
    
    
    
    # Complexity indexes
    
    complexity <- complexity_measures(mcp, iterations = 4, method = "reflections")
    
    
    product_complexity <- data.frame(pci = complexity[[2]]) %>%
        mutate(pci = pci,
               ano = anos[k]) %>%
        rownames_to_column(var = "product") %>%
        left_join(ubiquidade) %>% 
        select(ano, everything()) %>% 
        ungroup()
    
    regions_complexity <- data.frame(eci = complexity[[1]]) %>%
        mutate(eci = eci,
               ano = anos[k]) %>% 
        rownames_to_column(var = "country") %>%
        left_join(diversificacao) %>% 
        filter(!is.na(desc_en)) %>% 
        select(ano, everything()) %>% 
        ungroup()
    
    saveRDS(regions_complexity, str_c("3_results/ECI_", anos[k], ".RDS"))
    saveRDS(product_complexity, str_c("3_results/PCI_", anos[k], ".RDS"))
    
    
  
    
}

parallel::stopCluster(cl = my.cluster)
