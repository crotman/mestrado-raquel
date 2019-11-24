
library(tidyverse)
library(reactable)


acerto_faixas <- tibble(
    faixa_censo = c(
        "0 a 4 anos",
        "5 a 9 anos",
        "10 a 14 anos",
        "15 a 19 anos",
        "20 a 24 anos",
        "25 a 29 anos",
        "30 a 34 anos",
        "35 a 39 anos",
        "40 a 44 anos",
        "45 a 49 anos",
        "50 a 54 anos",
        "55 a 59 anos",
        "60 a 64 anos",
        "65 a 69 anos",
        "70 a 74 anos",
        "75 a 79 anos",
        "80 anos ou mais",
        "80 a 84 anos",
        "85 a 89 anos",
        "90 a 94 anos",
        "95 a 99 anos",
        "100 anos ou mais",
        "Idade ignorada"        
    ),
    faixa = c("0-4",
               "5-9",
               "10-14",
               "15-19",
               "20-24",
               "25-29",
               "30-34",
               "35-39",
               "40-44",
               "45-49",
               "50-54",
               "55-59",
               "60-64",
               "65-69",
               "70-74",
               "75-79",
               "80-84",
               "80-84",
               "85-89",
               "90+",
               "90+",
               "90+",
               "Ignorada"),

    classe = c("Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Não-idosos",
              "Jovens idosos",
              "Jovens idosos",
              "Idosos médios",
              "Idosos médios",
              "Idosos velhos",
              "Idosos velhos",
              "Idosos velhos",
              "Idosos velhos",
              "Idosos velhos",
              "Idosos velhos",
              "Ignorada")
    
        
    )



censo <- read_csv2("dados/tabela200.csv", skip = 2) %>% 
    select(
        ano = 3,
        faixa_censo = 4,
        sexo = 5,
        populacao = 6
    ) %>% 
    left_join(acerto_faixas, by = c("faixa_censo")) %>% 
    select(-faixa_censo) %>% 
    filter(populacao != "...") %>% 
    mutate(fonte = "Censo") %>% 
    mutate(
        populacao = as.numeric(populacao)
    )

proj_homens <- read_csv2("dados/proj_homens.csv") %>% 
    rename(faixa = 1) %>% 
    select(-X53) %>% 
    filter(faixa != "Total") %>% 
    pivot_longer(-faixa, names_to = "ano", values_to = "populacao") %>% 
    mutate(sexo = "Homens") %>% 
    mutate(fonte = "Projeção") %>% 
    mutate(ano = as.numeric(ano)) %>% 
    filter(ano != 2010) %>% 
    inner_join(acerto_faixas, by = c("faixa"))

proj_mulheres <- read_csv2("dados/proj_mulher.csv") %>% 
    rename(faixa = 1) %>% 
    select(-X53) %>% 
    filter(faixa != "Total") %>% 
    pivot_longer(-faixa, names_to = "ano", values_to = "populacao") %>% 
    mutate(sexo = "Mulheres") %>% 
    mutate(fonte = "Projeção") %>% 
    mutate(ano = as.numeric(ano)) %>% 
    filter(ano != 2010) %>% 
    inner_join(acerto_faixas, by = c("faixa"))

tudo <- censo %>% 
    bind_rows(proj_homens) %>% 
    bind_rows(proj_mulheres)


tudo_classe <- tudo %>% 
    group_by(ano, sexo, classe, fonte) %>%
    summarise(populacao = sum(populacao)) %>% 
    ungroup() %>% 
    mutate(classe = fct_relevel(classe, "Não-idosos", "Jovens idosos", "Idosos médios", "Idosos velhos", "Ignorada"))

tudo_sem_sexo <- tudo_classe %>% 
    select(-sexo, fonte) %>% 
    group_by(ano, classe) %>% 
    summarise(populacao = sum(populacao)) %>% 
    pivot_wider(names_from = ano, values_from = populacao)
    





