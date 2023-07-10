#### PhD Thesis - Preparing database
#### PhD Thesis - Preparing database
#### PhD Thesis - Preparing database
#### PhD Thesis - Preparing database

# Author: Alexandre de Queiroz Stein
# Starting date: 18/06/2023

#### Libraries and options ----------------

library(tidyverse)
library(data.table)

rm(list = ls(all = TRUE))



#### Side data --------------

# CBOS

cbos <- fread("C:/Users/queir/Meu Drive/Economia/Outros/Tabelas de Compatibilizacoes/cbo_2002.csv",
              colClasses = list(character = 1:10)) %>%
    select(cbo_2002, descricao) %>% 
    distinct()


attrs_cnae<- read.csv("C:/Users/queir/Meu Drive/Economia/Outros/Projeto João Romero SEBRAE 2023/relatorios/1_dados/attrs_cnae.csv", encoding = "Latin1") %>% 
    arrange(name_pt) %>% 
    {.->> colors_secs} %>% 
    filter(nchar(id) == 6 & !(name_pt %in% c("Não Declarado", "Confidential"))) %>% 
    mutate(id = str_sub(id, 2, 6),
           name_pt = str_replace_all(name_pt, "Atacato", "Atacado")) %>% 
    select(id, name_pt) %>%
    rename(setor = id,
           cnae_desc = name_pt)


#### Loading RAIS data ------------


# 2010
rais2010 <- read_rds("C:/backup_arquivos/RAIS/dados_rais_tese/2010/rais_2010_pronta.RDS") %>% 
    filter(
        !is.na(cnpj_cei) &
            cnpj_cei != 0 &
            vinculo_ativo_31_12 == "1" & 
            substr(cnae_2_0_classe, 1, 2) != 84
    ) %>% 
    distinct() %>% 
    mutate(id_vinc = c(1:nrow(.))) %>% 
    select(id_vinc, everything())




# 2019
rais_SE <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/2019/RAIS_VINC_ID_MG_ES_RJ.txt",
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
    filter(
        !is.na(cnpj_cei) &
            cnpj_cei != 0 &
            vinculo_ativo_31_12 == "1" & 
            substr(cnae_2_0_classe, 1, 2) != 84
    ) %>% 
    distinct() %>% 
    mutate(id_vinc = c(1:nrow(.))) %>% 
    select(id_vinc, everything())


# SP
rais_SP <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/2019/RAIS_VINC_ID_SP.txt",
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
    filter(
        !is.na(cnpj_cei) &
            cnpj_cei != 0 &
            vinculo_ativo_31_12 == "1" & 
            substr(cnae_2_0_classe, 1, 2) != 84
    ) %>% 
    distinct() %>% 
    mutate(id_vinc = c(1:nrow(.))) %>% 
    select(id_vinc, everything())



rais2019 <- rbind(rais_SE, rais_SP)



# Nomes das empresas

# 2010
nomes_emp_2010 <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/2019/Estb2019ID.txt",
                                    nrows = Inf, encoding = "Latin-1",
                                    select = c("CNPJ / CEI",
                                               "Razão Social"),
                                    colClasses = list("character" = c(1:19, 26:29))) %>%
    janitor::clean_names() %>% 
    arrange(cnpj_cei) %>% 
    distinct() %>% 
    mutate(ano = 2010)




# 2019
nomes_emp_2019 <- data.table::fread("C:/backup_arquivos/RAIS/dados_rais_tese/2019/Estb2019ID.txt",
                                    nrows = Inf, encoding = "Latin-1",
                                    select = c("CNPJ / CEI",
                                               "Razão Social"),
                                    colClasses = list("character" = c(1:19, 26:29))) %>%
    janitor::clean_names() %>% 
    arrange(cnpj_cei) %>% 
    distinct() %>% 
    mutate(ano = 2019)




# Puttin in lists
rais <- list(rais10 = rais2010, rais19 = rais2019)

nomes_emp <- list(nomes_emp_2010 = nomes_emp_2010,
                  nomes_emp_2019 = nomes_emp_2019)

rm(rais_SP, rais_SE, rais2010, rais2019, nomes_emp_2010, nomes_emp_2019)

gc()

#### Loading complexity data --------------------------------


# Occupation complexity
files <- list.files(path = "3_results",
                    pattern = "6d_PCI",
                    full.names = TRUE)

pci_cbo <- map_dfr(.x = files, .f = readRDS) %>% 
    group_by(product, desc_en) %>% 
    summarise(pci_cbo = mean(pci, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(cbo_desc = desc_en) %>% 
    filter(!is.na(cbo_desc)) %>% 
    mutate(cbo_desc = case_when(
        product == "1418" ~ "Civil aviation operational managers", 
        product == "2527" ~ "Planning, programming, and logistics control professionals", 
        product == "2534" ~ "Digital media professionals and related fields", 
        product == "2545" ~ "Urban activities inspection professionals", 
        product == "2619" ~ "TV assistant directors and continuity supervisors", 
        product == "3148" ~ "Industrial inspection specialists", 
        product == "4242" ~ "Test proctors and related occupations", 
        product == "5115" ~ "Tourism drivers", 
        product == "2123" ~ "IT Administrators",
        TRUE ~ cbo_desc),
        pci_cbo = ((pci_cbo - min(pci_cbo))/(max(pci_cbo) - min(pci_cbo)))+1)


# Sector complexity
files <- list.files(path = "3_results",
                    pattern = "6d_ECI",
                    full.names = TRUE)


pci_cnae <- map_dfr(.x = files, .f = readRDS) %>% 
    group_by(country, desc_en) %>% 
    summarise(pci_cnae = mean(eci, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(cnae_desc = desc_en) %>% 
    mutate(pci_cnae = ((pci_cnae - min(pci_cnae))/(max(pci_cnae) - min(pci_cnae)))+1)


# Size complexity

lista_pci_vinc <- list()

lista_pci_vinc[["pci_vinc_2010"]] <- rais[[1]] %>% 
    group_by(cnpj_cei) %>% 
    summarise(qtd_vinculos_ativos = n()) %>% 
    ungroup() %>% 
    mutate(qtd_vinculos_ativos = log(qtd_vinculos_ativos),
           pci_vinc = ((qtd_vinculos_ativos - min(qtd_vinculos_ativos))/
                           (max(qtd_vinculos_ativos) - min(qtd_vinculos_ativos)))+1) %>% 
    select(-qtd_vinculos_ativos)

lista_pci_vinc[["pci_vinc_2019"]] <- rais[[2]] %>% 
    group_by(cnpj_cei) %>% 
    summarise(qtd_vinculos_ativos = n()) %>% 
    ungroup() %>% 
    mutate(qtd_vinculos_ativos = log(qtd_vinculos_ativos),
           pci_vinc = ((qtd_vinculos_ativos - min(qtd_vinculos_ativos))/
                           (max(qtd_vinculos_ativos) - min(qtd_vinculos_ativos)))+1) %>% 
    select(-qtd_vinculos_ativos)


#### Computing company complexity

lista_cci <- list()

for (i in seq_along(rais)){

    # First removing duplicated cnae
    cnaes_emps <- rais[[i]] %>% 
        group_by(cnpj_cei, cnae_2_0_classe) %>%
        summarise(n = n()) %>% 
        arrange(cnpj_cei, -n) %>% 
        ungroup() %>% 
        distinct(cnpj_cei, .keep_all = TRUE) %>%  # Keep cnae class with greateast number of employees (happened for 35 companies)
        rename(cnae_correta = cnae_2_0_classe)
    
    
    # Computing number of employees by cnae
    lista_cci[[i]] <- rais[[i]] %>% 
        left_join(cnaes_emps) %>% # Join correct cnaes
        group_by(cnpj_cei, cnae_correta, cbo_ocupacao_2002) %>% 
        summarise(n.cbos = n()) %>% #number os cbo types by cnpj
        ungroup() %>%
        left_join(pci_cbo, by = c("cbo_ocupacao_2002" = "product")) %>% # join pci_cbo
        left_join(pci_cnae, by = c("cnae_correta" = "country")) %>% # join pci_cnae
        left_join(lista_pci_vinc[[i]]) %>% # join pci_vinculos
        mutate(pci_cbo_prev = n.cbos*pci_cbo) %>% #occupations complexity by company
        filter(!is.na(pci_cbo_prev)) %>%  # remove missings
        group_by(cnpj_cei, cnae_correta, cnae_desc) %>% 
        summarise(pci_cbo = sum(pci_cbo_prev, na.rm = TRUE),
                  pci_cnae = mean(pci_cnae, na.rm = TRUE),
                  pci_vinc = mean(pci_vinc, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(pci_cbo = ((pci_cbo - min(pci_cbo))/
                              (max(pci_cbo) - min(pci_cbo)))+1,
               cci = pci_cbo*pci_cnae, #considering just cnaes and cbo
               cci_media = (pci_cbo+pci_cnae)/2,
               cci_rank = rank(-cci),
               cci_media_rank = rank(-cci_media)) %>% 
        left_join(nomes_emp[[i]], multiple = "any") %>% 
        select(cnpj_cei, 
               razao_social, 
               cnae_correta, 
               cnae_desc, 
               pci_cbo, 
               pci_cnae, 
               pci_vinc,
               cci, 
               cci_media, 
               cci_rank, 
               cci_media_rank)
    
}


saveRDS(lista_cci[[1]], "3_results/cci_2010.RDS")
saveRDS(lista_cci[[2]], "3_results/cci_2019.RDS")




