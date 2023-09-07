#-----------------
#- SETUP INICIAL -
#-----------------

# Limpando arquivos armazenados na memória
rm(list=ls(all=TRUE))

# Definindo limite de mem?ria para compilação do programa
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=50000)
options(warn=aviso)
rm(aviso)

# Definindo tempo de espera para obtenção de resposta do servidor
aviso <- getOption("warn")
options(warn=-1)
options(timeout=600)
options(warn=aviso)
rm(aviso)

# Definindo op??o de codificação dos caracteres e linguagem
aviso <- getOption("warn")
options(warn=-1)
options(encoding="latin1")
options(warn=aviso)
rm(aviso)

# Definindo op??o de exibição de números sem representação em exponencial
aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Definindo op??o de repositório para instalação dos pacotes necessários
aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)

# Definindo diret?rio de trabalho

setwd("E:/TPE/base_dados/")
caminho <- getwd()

# Definindo Time-Out para download do arquivo
getOption('timeout')
options(timeout=600000)

# Carregando pacotes necess?rios para obten??o da estimativa desejada
pacman::p_load(PNADcIBGE, SIPDIBGE, tidyverse,dplyr, tibble,survey, janitor, lubridate,  writexl, openxlsx, rJava, xlsx)



#---------------------------------------------------------
#- IMPORTAÇÃO DE DADOS E CRIAÇÃO DE VARIÉVEIS AUXILIARES -
#---------------------------------------------------------


# Importação dos microdados
arquivo_16 <- PNADcIBGE::get_pnadc(year=2016, topic=2, design=FALSE)
arquivo_17 <- PNADcIBGE::get_pnadc(year=2017, topic=2, design=FALSE)
arquivo_18 <- PNADcIBGE::get_pnadc(year=2018, topic=2, design=FALSE)
arquivo_19 <- PNADcIBGE::get_pnadc(year=2019, topic=2, design=FALSE)
arquivo_22 <- PNADcIBGE::get_pnadc(year=2022, topic=2, design=FALSE)


# Função para o cálculo da renda domiciliar per capita
funcao_renda_per_capita <- function(df){
  # 1º passo: criar uma variável ID_DOMICILIO, ou seja, uma chave única para cada domicílio da base de dados
  df <- df %>% mutate(ID_DOMICILIO=paste0(UPA,V1008,V1014, sep=""))
  # 2º passo: filtro de pessoas que estão incluídas no cálculo (exceto Pensionista, Empregado(a) doméstico(a) e 
  # Parente do(a) empregado(a) doméstico(a))
  df <- df %>% mutate(VD4020_real=ifelse(is.na(VD4020) | V2005==17 | V2005==18 | V2005==19, 0, VD4020*Efetivo),
                      V2001_rendimento=ifelse(V2005==17 | V2005==18 | V2005==19, 0, 1))
  # 3º passo: divisão de rendimento pela quantidade de pessoas no domicílio
  df_rend <- df %>% group_by(ID_DOMICILIO) %>% summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=T),
                                                         rendimento_todos_trabalhos=sum(VD4020_real, na.rm=T))
  df_rend <- df_rend %>% mutate(REND_PER_CAPITA=rendimento_todos_trabalhos/moradores_rendimento)
  # 4º passo: inclusão da variável de rendimento per capita na base pnad_2020
  df <- merge(x=df, y=df_rend, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=T, all.y=F)
  rm(df_rend)
  df <- df %>% mutate(df, REND_PER_CAPITA=ifelse(V2005==17 | V2005==18 | V2005==19, NA, REND_PER_CAPITA))
}


summary(arquivo_22$REND_PER_CAPITA)

# Cálculo da renda domiciliar per capita
arquivo_16 <- funcao_renda_per_capita(arquivo_16)
arquivo_17 <- funcao_renda_per_capita(arquivo_17)
arquivo_18 <- funcao_renda_per_capita(arquivo_18)
arquivo_19 <- funcao_renda_per_capita(arquivo_19)
arquivo_22 <- funcao_renda_per_capita(arquivo_22)

# Função para criar variável da idade escolar (com base em 31 de março do ano de referência)
funcao_idade_escolar <- function(df, ano){
  df <- df %>% mutate(dt_nasc=dmy(paste0(V2008, V20081, V20082, sep="/")),
                      dt_ref=dmy(paste0("31","03",Ano,sep="/")),
                      idade=floor(ifelse(V2008=="99" & V20081=="99" & V20082=="9999",
                                         V2009,
                                         (dt_nasc %--% dt_ref) / dyears(1))))
}


# Cálculo da idade escolar
arquivo_16 <- funcao_idade_escolar(arquivo_16)
arquivo_17 <- funcao_idade_escolar(arquivo_17)
arquivo_18 <- funcao_idade_escolar(arquivo_18)
arquivo_19 <- funcao_idade_escolar(arquivo_19)
arquivo_22 <- funcao_idade_escolar(arquivo_22)




#Inclusão do Plano Amostral
PA_16 <- pnadc_design(dplyr::as_tibble(arquivo_16))
PA_17 <- pnadc_design(dplyr::as_tibble(arquivo_17))
PA_18 <- pnadc_design(dplyr::as_tibble(arquivo_18))
PA_19 <- pnadc_design(dplyr::as_tibble(arquivo_19))
PA_22 <- pnadc_design(dplyr::as_tibble(arquivo_22))


#############################################
#### Frequencia #############################
#############################################

# Cálculo da estimativa de crianças de 4 a 5 anos que frequentam e que não frequentam a escola
criancas_45_16 <- survey::svymean(x=~V3002,
                                  design=subset(PA_16, idade %in% c(4,5)),
                                  na.rm=T)
criancas_45_17 <- survey::svymean(x=~V3002,
                                  design=subset(PA_17, idade %in% c(4,5)),
                                  na.rm=T)
criancas_45_18 <- survey::svymean(x=~V3002,
                                  design=subset(PA_18, idade %in% c(4,5)),
                                  na.rm=T)
criancas_45_19 <- survey::svymean(x=~V3002,
                                  design=subset(PA_19, idade %in% c(4,5)),
                                  na.rm=T)
criancas_45_22 <- survey::svymean(x=~V3002,
                                  design=subset(PA_22, idade %in% c(4,5)),
                                  na.rm=T)

criancas_45_16 
criancas_45_17
criancas_45_18
criancas_45_19
criancas_45_22


write.xlsx(criancas_45_16, file = "freq_escola_45.xlsx", sheetName="16", append=TRUE)
write.xlsx(criancas_45_17, file = "freq_escola_45.xlsx", sheetName="17", append=TRUE)
write.xlsx(criancas_45_18, file = "freq_escola_45.xlsx", sheetName="18", append=TRUE)
write.xlsx(criancas_45_19, file = "freq_escola_45.xlsx", sheetName="19", append=TRUE)
write.xlsx(criancas_45_22, file = "freq_escola_45.xlsx", sheetName="22", append=TRUE)



# Cálculo da estimativa de crianças de 0 a 3 anos que frequentam e que não frequentam a escola
criancas_03_16 <- survey::svymean(x=~V3002,
                                  design=subset(PA_16, idade <4),
                                  na.rm=T)
criancas_03_17 <- survey::svymean(x=~V3002,
                                  design=subset(PA_17, idade <4),
                                  na.rm=T)
criancas_03_18 <- survey::svymean(x=~V3002,
                                  design=subset(PA_18, idade <4),
                                  na.rm=T)
criancas_03_19 <- survey::svymean(x=~V3002,
                                  design=subset(PA_19, idade <4),
                                  na.rm=T)
criancas_03_22 <- survey::svymean(x=~V3002,
                                  design=subset(PA_22, idade <4),
                                  na.rm=T)

criancas_03_16 
criancas_03_17
criancas_03_18
criancas_03_19
criancas_03_22

write.xlsx(criancas_03_16, file = "freq_escola_03.xlsx", sheetName="16", append=TRUE)
write.xlsx(criancas_03_17, file = "freq_escola_03.xlsx", sheetName="17", append=TRUE)
write.xlsx(criancas_03_18, file = "freq_escola_03.xlsx", sheetName="18", append=TRUE)
write.xlsx(criancas_03_19, file = "freq_escola_03.xlsx", sheetName="19", append=TRUE)
write.xlsx(criancas_03_22, file = "freq_escola_03.xlsx", sheetName="22", append=TRUE)


#############################################
#### Frequencia + Raca ######################
#############################################


# Populacao de 0 a 3 anos por raca/cor e frequencia na escola
freq_03_16 <- survey::svyby(~V3002, by=~V2010, subset(PA_16, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_17 <- survey::svyby(~V3002, by=~V2010, subset(PA_17, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_18 <- survey::svyby(~V3002, by=~V2010, subset(PA_18, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_19 <- survey::svyby(~V3002, by=~V2010, subset(PA_19, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_22 <- survey::svyby(~V3002, by=~V2010, subset(PA_22, idade<4),FUN=svymean, na.rm=TRUE)


write.xlsx(freq_03_16, file = "freq_raca_03.xlsx", sheetName="16", append=TRUE)
write.xlsx(freq_03_17, file = "freq_raca_03.xlsx", sheetName="17", append=TRUE)
write.xlsx(freq_03_18, file = "freq_raca_03.xlsx", sheetName="18", append=TRUE)
write.xlsx(freq_03_19, file = "freq_raca_03.xlsx", sheetName="19", append=TRUE)
write.xlsx(freq_03_22, file = "freq_raca_03.xlsx", sheetName="22", append=TRUE)


# Populacao de 4 a 5 anos por raca/cor e frequencia na escola
freq_45_16 <- survey::svyby(~V3002, by=~V2010, subset(PA_16, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_17 <- survey::svyby(~V3002, by=~V2010, subset(PA_17, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_18 <- survey::svyby(~V3002, by=~V2010, subset(PA_18, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_19 <- survey::svyby(~V3002, by=~V2010, subset(PA_19, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_22 <- survey::svyby(~V3002, by=~V2010, subset(PA_22, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)

freq_45_16
freq_45_17
freq_45_18
freq_45_19
freq_45_22

write.xlsx(freq_45_16, file = "freq_raca_45.xlsx", sheetName="16", append=TRUE)
write.xlsx(freq_45_17, file = "freq_raca_45.xlsx", sheetName="17", append=TRUE)
write.xlsx(freq_45_18, file = "freq_raca_45.xlsx", sheetName="18", append=TRUE)
write.xlsx(freq_45_19, file = "freq_raca_45.xlsx", sheetName="19", append=TRUE)
write.xlsx(freq_45_22, file = "freq_raca_45.xlsx", sheetName="22", append=TRUE)


#############################################
#### Motivos para nao ir a escola  ##########
#############################################

motivo_03_19 <- survey::svymean(x=~V3033A,
                                  design=subset(PA_19, idade <4),
                                  na.rm=T)
motivo_03_22 <- survey::svymean(x=~V3033A,
                                  design=subset(PA_22, idade <4),
                                  na.rm=T)

motivo_03_19
motivo_03_22


motivo_45_19 <- survey::svymean(x=~V3033A,
                                  design=subset(PA_19, idade %in% c(4,5)),
                                  na.rm=T)
motivo_45_22 <- survey::svymean(x=~V3033A,
                                  design=subset(PA_22, idade %in% c(4,5)),
                                  na.rm=T)

motivo_45_19
motivo_45_22


write.xlsx(motivo_03_19 , file ="motivo.xlsx", sheetName="03_19", append=TRUE)
write.xlsx(motivo_03_22, file = "motivo.xlsx", sheetName="03_22", append=TRUE)
write.xlsx(motivo_45_19, file = "motivo.xlsx", sheetName="45_19", append=TRUE)
write.xlsx(motivo_45_22, file = "motivo.xlsx", sheetName="45_22", append=TRUE)




#Motivos de não frequentar a escola (a partir de 2019) 0 a 3 anos + raca
mot_03_19 <- survey::svyby(~V3033A, by=~V2010, subset(PA_19, idade<4),FUN=svymean, na.rm=TRUE)
mot_03_22 <- survey::svyby(~V3033A, by=~V2010, subset(PA_22, idade<4),FUN=svymean, na.rm=TRUE)

mot_03_19
mot_03_22


#Motivos de não frequentar a escola (a partir de 2019) 4 a 5 anos + raca
mot_45_19 <- survey::svyby(~V3033A, by=~V2010, subset(PA_19, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE) # Sem valores para amostra
mot_45_22 <- survey::svyby(~V3033A, by=~V2010, subset(PA_22, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE) # Sem valores para amostra


write.xlsx(mot_03_19 , file ="motivo_raca.xlsx", sheetName="03_19", append=TRUE)
write.xlsx(mot_03_22, file = "motivo_raca.xlsx", sheetName="03_22", append=TRUE)


table(arquivo_22$VDI5009)


names(arquivo_16)

#Renda x Acesso
# Renda


#Renda x Acesso

# Populacao de 0 a 3 anos por renda/cor e frequencia na escola
freq_03_16 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_16, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_17 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_17, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_18 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_18, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_19 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_19, idade<4),FUN=svymean, na.rm=TRUE)
freq_03_22 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_22, idade<4),FUN=svymean, na.rm=TRUE)


freq_03_16

write.xlsx(freq_03_16, file = "freq_renda_03.xlsx", sheetName="16", append=TRUE)
write.xlsx(freq_03_17, file = "freq_renda_03.xlsx", sheetName="17", append=TRUE)
write.xlsx(freq_03_18, file = "freq_renda_03.xlsx", sheetName="18", append=TRUE)
write.xlsx(freq_03_19, file = "freq_renda_03.xlsx", sheetName="19", append=TRUE)
write.xlsx(freq_03_22, file = "freq_renda_03.xlsx", sheetName="22", append=TRUE)


# Populacao de 4 a 5 anos por renda/cor e frequencia na escola
freq_45_16 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_16, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_17 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_17, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_18 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_18, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_19 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_19, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)
freq_45_22 <- survey::svyby(~V3002, by=~VDI5009, subset(PA_22, idade %in% c(4,5)),FUN=svymean, na.rm=TRUE)

freq_45_16
freq_45_17
freq_45_18
freq_45_19
freq_45_22

write.xlsx(freq_45_16, file = "freq_renda_45.xlsx", sheetName="16", append=TRUE)
write.xlsx(freq_45_17, file = "freq_renda_45.xlsx", sheetName="17", append=TRUE)
write.xlsx(freq_45_18, file = "freq_renda_45.xlsx", sheetName="18", append=TRUE)
write.xlsx(freq_45_19, file = "freq_renda_45.xlsx", sheetName="19", append=TRUE)
write.xlsx(freq_45_22, file = "freq_renda_45.xlsx", sheetName="22", append=TRUE)



#Renda X motivo de nao frequentar
#Motivos de não frequentar a escola (a partir de 2019) 0 a 3 anos + raca
mot_03_19 <- survey::svyby(~V3033A, by=~VDI5009, subset(PA_19, idade<4),FUN=svymean, na.rm=TRUE)
mot_03_22 <- survey::svyby(~V3033A, by=~VDI5009, subset(PA_22, idade<4),FUN=svymean, na.rm=TRUE)

mot_03_22

write.xlsx(mot_03_19 , file ="motivo_renda.xlsx", sheetName="03_19", append=TRUE)
write.xlsx(mot_03_22, file = "motivo_renda.xlsx", sheetName="03_22", append=TRUE)


#Renda x motivo de nao frequentar x Raca


#Renda X motivo de nao frequentar
#Motivos de não frequentar a escola (a partir de 2019) 0 a 3 anos + raca
mot_03_19 <- survey::svyby(~V3033A, by=~VDI5009 + V2010, subset(PA_19, idade<4),FUN=svymean, na.rm=TRUE)
mot_03_22 <- survey::svyby(x=~interaction(V3033A,V2010), by=~VDI5009, design=subset(PA_22, idade<4),FUN=svymean, na.rm=TRUE)



mot_03_22

write.xlsx(mot_03_19 , file ="motivo_renda.xlsx", sheetName="03_19", append=TRUE)
write.xlsx(mot_03_22, file = "motivo_renda.xlsx", sheetName="03_22", append=TRUE)
