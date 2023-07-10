#### Acessing Companies Complecity Index
#### Acessing Companies Complecity Index
#### Acessing Companies Complecity Index


# Author: Alexandre de Queiroz Stein
# Date: 29/06/2023


rm(list = ls(all = TRUE))

# Libraries and options ---------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------
municipios <- read.csv("1_data/municipio.csv") %>% 
    select(id_municipio_6, nome) %>% 
    mutate(id_municipio_6 = as.character(id_municipio_6)) %>% 
    rename(municipio = id_municipio_6,
           nome_municipio = nome)



cci <- read_rds("3_results/cci.RDS")

sample <- sample(c(1:nrow(cci)), 10000)
cci_s <- cci[sample,]


# Setores de maior cci
cci %>% 
    group_by(cnae_2_0_classe, cnae_desc) %>% 
    summarise(cci = mean(cci, na.rm = TRUE)) %>% 
    ungroup() %>% 
    slice_max(cci, n = 15) %>% 
    ggplot()+
    geom_col(aes(y = reorder(cnae_desc, cci), x = cci))




# Municípios de maior cci
cci %>% 
    left_join(municipios) %>% 
    group_by(municipio, nome_municipio) %>% 
    summarise(cci = mean(cci, na.rm = TRUE)) %>% 
    ungroup() %>% 
    slice_max(cci, n = 15) %>% 
    ggplot()+
    geom_col(aes(y = reorder(nome_municipio, cci), x = cci))





ggplot(cci_s)+
    geom_point(aes(x = qtd_vinculos_ativos, y = cci))

