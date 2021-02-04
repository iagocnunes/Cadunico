setwd("C:/Users/IJSN/Desktop/cadúnico")
rm(list = ls())
options(survey.lonely.psu = "adjust")
options(OutDec=",")
library(h2o)
library(lime)
library(dplyr);library(survey);library(srvyr)
library(haven);library(knitr);library(kableExtra);library(tidyr);library(ggplot2)
library(readr);library(DBI);library(fst)
base_amostra_pessoa_201812 <- read_delim("C:/Users/IJSN/Desktop/cadúnico/base_amostra_pessoa_201812.csv",
                                         ";", escape_double = FALSE, col_types = cols(cd_ibge = col_character(),
                                                                                      estrato = col_number(), classf = col_number(),
                                                                                      id_familia = col_number(), id_pessoa = col_number(),
                                                                                      cod_sexo_pessoa = col_number(), idade = col_number(),
                                                                                      cod_parentesco_rf_pessoa = col_number(),
                                                                                      cod_raca_cor_pessoa = col_number(),
                                                                                      cod_local_nascimento_pessoa = col_number(),
                                                                                      cod_certidao_registrada_pessoa = col_number(),
                                                                                      cod_deficiencia_memb = col_number(),
                                                                                      cod_sabe_ler_escrever_memb = col_number(),
                                                                                      ind_frequenta_escola_memb = col_number(),
                                                                                      cod_escola_local_memb = col_number(),
                                                                                      cod_curso_frequenta_memb = col_number(),
                                                                                      cod_ano_serie_frequenta_memb = col_number(),
                                                                                      cod_curso_frequentou_pessoa_memb = col_number(),
                                                                                      cod_ano_serie_frequentou_memb = col_number(),
                                                                                      cod_concluiu_frequentou_memb = col_number(),
                                                                                      cod_trabalhou_memb = col_number(),
                                                                                      cod_afastado_trab_memb = col_number(),
                                                                                      cod_agricultura_trab_memb = col_number(),
                                                                                      cod_principal_trab_memb = col_number(),
                                                                                      val_remuner_emprego_memb = col_number(),
                                                                                      cod_trabalho_12_meses_memb = col_number(),
                                                                                      qtd_meses_12_meses_memb = col_number(),
                                                                                      val_renda_bruta_12_meses_memb = col_number(),
                                                                                      val_renda_doacao_memb = col_number(),
                                                                                      val_renda_aposent_memb = col_number(),
                                                                                      val_renda_seguro_desemp_memb = col_number(),
                                                                                      val_renda_pensao_alimen_memb = col_number(),
                                                                                      val_outras_rendas_memb = col_number(),
                                                                                      peso.fam = col_number(), peso.pes = col_number()),
                                         trim_ws = TRUE)
base_amostra_pessoa_201812$one <- 1

base_amostra_pessoa_201812$peso.pess <- ifelse(base_amostra_pessoa_201812$peso.pes>99999999999999, (base_amostra_pessoa_201812$peso.pes / 100000000000000),
                                ifelse(base_amostra_pessoa_201812$peso.pes>9999999999999, (base_amostra_pessoa_201812$peso.pes / 10000000000000),
                                       ifelse(base_amostra_pessoa_201812$peso.pes>999999999999, (base_amostra_pessoa_201812$peso.pes / 1000000000000),
                                              ifelse(base_amostra_pessoa_201812$peso.pes>99999999999, (base_amostra_pessoa_201812$peso.pes / 100000000000), 0))))

cadunico_plano <-     
  svydesign(            
    ids = ~ id_pessoa,
    strata = ~ estrato,
    weights = ~ peso.pess,
    data = base_amostra_pessoa_201812
  )

saveRDS(cadunico_plano,"cadunic_pess_2018")
rm(list = ls())

cadunic_pess_2018 <- readRDS(file="cadunic_pess_2018") 
cadunic_pess_2018 <- as_survey_design(cadunic_pess_2018)
options(scipen=999)
svytotal(~ one, cadunic_pess_2018)

svytotal(~cod_sabe_ler_escrever_memb, cadunic_pess_2018)
