---
title: "Tipos de julgamento"
author: "Jessica Voigt"
date: "15 de maio de 2020"
output: 
  html_document:
    theme: paper 
    toc: true
    toc_depth: 3
    code_folding: hide
    toc_float: true
---

## Níveis de julgamento

Falei com o Denizar e ele me explicou que os municípios, quando lançam o edital de licitação, decidem se o julgamento (TP_NIVEL_JULGAMENTO) será por item, por lote ou global.

- **Julgamentos por item:** casos em que o preço vária muito entre itens, por exemplo na compra de carros oficiais em que é comprado mais de um modelo. Então um fornecedor poderia levar o item 1 (caminhonete) e outro o item 2 (ambulância)

- **Julgamento por lote:** prefeitura divide os itens a serem licitados em lotes de produtos semelhantes entre si (por exemplo, um lote com laticínios, outro lote com biscoitos, outro lote com verduras). Então um fornecedor poderia levar os laticínios e outro as verduras;

- **Julgamento global:** um único fornecedor vai levar todos os itens daquela licitação.

Nas palavras do Denizar "Nos casos das licitações por lote ou por item, é como se cada lote ou cada item fossem uma licitação diferente englobadas em um mesmo edital", então temos que trata-las como tal.

Como até o momento essa não era uma categoria para a qual estávamos prestando atenção, vou observar o que temos de dados sobre elas baseada nos dados de merenda:



```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE
                      , comment = NA
                      , warning = FALSE
                      , error = FALSE
                      , message = FALSE
                      , tidy = TRUE)

knitr::opts_knit$set(root.dir = "C:/Users/coliv/Documents/brazilian_funds_db/banco_final/merenda/rs")

```

## Distribuição do nível de julgamento nas licitações:

```{r}
library(tidyverse)
library(data.table)
library(readr)
library(janitor)
library(xlsx)
library(readxl)
library(ggplot2)
library(ggrepel)
```

```{r}
# Licitações de merenda adjudicadas:

licitacoes <- data.frame()

ano <- c("2017", "2018", "2019")

for (i in 1:length(ano)){
  
  a <- fread(paste0("C:/Users/coliv/licitacoes_merenda_rs/dbs/", ano[i], "_licitacoes/licitacao.csv"), 
             encoding = "UTF-8", colClasses=list(character=1:61))
  
  a <- a %>%
    mutate(DS_OBJETO = iconv(DS_OBJETO, from="UTF-8", to="ASCII//TRANSLIT")) %>%
    filter(CD_TIPO_MODALIDADE == "CPP" |
             grepl("^.*(genero.*aliment|aliment.*esc|genero.*agric.*famil|merenda|pnae).*$",
                   DS_OBJETO),
           CD_TIPO_FASE_ATUAL == "ADH")    # adjudicadas
  
  licitacoes <- rbind(licitacoes, a)  
  rm(a)
}
```

### 1. Quantas licitaçoes de merenda existem em cada tipo de julgamento?
```{r}
theme_set(theme_classic())

pie <- licitacoes %>%
  group_by(TP_NIVEL_JULGAMENTO) %>%
  summarise(total_licitacoes = n()) %>%
  ungroup() %>%
  mutate(TP_NIVEL_JULGAMENTO = ifelse(TP_NIVEL_JULGAMENTO == "G", "Global",
                                      ifelse(TP_NIVEL_JULGAMENTO == "L", "Por lote", "Por item")),
         perc = paste0(round(total_licitacoes/2876, 2)*100, "%")) %>%
  ggplot(aes(x = "", y=total_licitacoes, fill = factor(TP_NIVEL_JULGAMENTO))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="TP_NIVEL_JULGAMENTO", 
       x=NULL, 
       y=NULL, 
       title="Tipo nível julgamento")

pie + coord_polar(theta = "y", start=0) +
   geom_label_repel(aes(label = perc), size=5, show.legend = F, nudge_x = 1) +
       guides(fill = guide_legend(title = "Group"))         

```

Como já foi mencionado, apenas as licitações tipo Global ```TP_NIVEL_JULGAMENTO == "G"```  terão o campo ```VL_HOMOLOGADO``` preenchido. 
Alternativamente, temos o campo ```VL_LICITACAO``` cuja descrição no manual é:

> Nesse campo deverá ser informado o valor total estimado para a licitação, ou o
> valor total mínimo de arrematação, ou o valor total da remuneração/prêmio ou,
> ainda, o valor total contratado, de acordo com o tipo de licitação/forma de
> contratação.

Entrei em contato novamente com o TCE-RS para ter mais esclarecimentos de quando o esse campo será o valor total contratado e aguardo retorno. 



