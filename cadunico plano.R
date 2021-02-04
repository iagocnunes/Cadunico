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

cadunic_ES <- subset(cadunic_pess_2018, cd_ibge=="3200102"|cd_ibge=="3200136"|cd_ibge=="3200169"|cd_ibge=="3200201"|cd_ibge=="3200300"|cd_ibge=="3200359"|cd_ibge=="3200409"|cd_ibge=="3200508"|cd_ibge=="3200607"|cd_ibge=="3200706"|cd_ibge=="3200805"|cd_ibge=="3200904"|cd_ibge=="3201001"|cd_ibge=="3201100"|cd_ibge=="3201159"|cd_ibge=="3201209"|cd_ibge=="3201308"|cd_ibge=="3201407"|cd_ibge=="3201506"|cd_ibge=="3201605"|cd_ibge=="3201704"|cd_ibge=="3201803"|cd_ibge=="3201902"|cd_ibge=="3202009"|cd_ibge=="3202108"|cd_ibge=="3202207"|cd_ibge=="3202256"|cd_ibge=="3202306"|cd_ibge=="3202405"|cd_ibge=="3202454"|cd_ibge=="3202504"|cd_ibge=="3202553"|cd_ibge=="3202603"|cd_ibge=="3202652"|cd_ibge=="3202702"|cd_ibge=="3202801"|cd_ibge=="3202900"|cd_ibge=="3203007"|cd_ibge=="3203056"|cd_ibge=="3203106"|cd_ibge=="3203130"|cd_ibge=="3203163"|cd_ibge=="3203205"|cd_ibge=="3203304"|cd_ibge=="3203320"|cd_ibge=="3203346"|cd_ibge=="3203353"|cd_ibge=="3203403"|cd_ibge=="3203502"|cd_ibge=="3203601"|cd_ibge=="3203700"|cd_ibge=="3203809"|cd_ibge=="3203908"|cd_ibge=="3204005"|cd_ibge=="3204054"|cd_ibge=="3204104"|cd_ibge=="3204203"|cd_ibge=="3204252"|cd_ibge=="3204302"|cd_ibge=="3204351"|cd_ibge=="3204401"|cd_ibge=="3204500"|cd_ibge=="3204559"|cd_ibge=="3204609"|cd_ibge=="3204658"|cd_ibge=="3204708"|cd_ibge=="3204807"|cd_ibge=="3204906"|cd_ibge=="3204955"|cd_ibge=="3205002"|cd_ibge=="3205010"|cd_ibge=="3205036"|cd_ibge=="3205069"|cd_ibge=="3205101"|cd_ibge=="3205150"|cd_ibge=="3205176"|cd_ibge=="3205200"|cd_ibge=="3205309")
rm(cadunic_pess_2018)
svytotal(~ one, cadunic_ES)

svytotal(~cod_sabe_ler_escrever_memb, cadunic_ES, na.rm=T)
