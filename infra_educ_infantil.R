rm(list = ls())

pacman::p_load(tidyverse,data.table, janitor, gtsummary, flextable, haven, basedosdados, writexl, xlsx)

set_billing_id("932818983325")


censo_educ_infantil <-  "SELECT  ano,sigla_uf,id_municipio,id_escola,rede,
tipo_situacao_funcionamento,poder_publico_parceria, conveniada_poder_publico,tipo_convenio_poder_publico,
agua_potavel,agua_rede_publica,agua_inexistente,energia_rede_publica,esgoto_rede_publica,
lixo_servico_coleta,area_verde,banheiro,banheiro_pne,banheiro_educacao_infantil,
biblioteca,biblioteca_sala_leitura,dormitorio_aluno,parque_infantil,quadra_esportes,
refeitorio,internet,alimentacao,material_pedagogico_infantil,etapa_ensino_infantil,
etapa_ensino_infantil_creche,etapa_ensino_infantil_pre_escola,
quantidade_matricula_infantil,quantidade_matricula_infantil_creche,
quantidade_matricula_infantil_pre_escola,quantidade_matricula_idade_0_3,
quantidade_matricula_idade_4_5, quantidade_matricula_infantil_integral,
quantidade_matricula_infantil_creche_integral,
quantidade_matricula_infantil_pre_escola_integral,quantidade_turma_infantil,
quantidade_turma_infantil_creche,quantidade_turma_infantil_pre_escola,
quantidade_docente_infantil,quantidade_docente_infantil_creche,
quantidade_docente_infantil_pre_escola
FROM `basedosdados.br_inep_censo_escolar.escola` 
WHERE ano >=2007"

censo_educ_infantil <- basedosdados::read_sql(censo_educ_infantil)



infra_infantil <-  censo_educ_infantil |> 
  filter (rede !=4,
          tipo_situacao_funcionamento ==1,
          etapa_ensino_infantil==1) |>
  group_by(ano) |> 
  summarise (
    escolas = n(),
    agua_potavel= mean(agua_potavel =="1",na.rm = T),
    agua_rede_publica= mean(agua_rede_publica=="1",na.rm = T),
    agua_inexistente= mean(agua_inexistente=="1",na.rm = T),
    energia_rede_publica= mean(energia_rede_publica=="1",na.rm = T),
    esgoto_rede_publica= mean(esgoto_rede_publica=="1",na.rm = T),
    lixo_servico_coleta= mean(lixo_servico_coleta=="1",na.rm = T),
    area_verde= mean(area_verde=="1",na.rm = T),
    banheiro= mean(banheiro=="1",na.rm = T),
    banheiro_pne= mean(banheiro_pne=="1",na.rm = T),
    banheiro_educacao_infantil= mean(banheiro_educacao_infantil=="1",na.rm = T),
    biblioteca= mean(biblioteca=="1",na.rm = T),
    biblioteca_sala_leitura= mean(biblioteca_sala_leitura=="1",na.rm = T),
    dormitorio_aluno= mean(dormitorio_aluno=="1",na.rm = T),
    parque_infantil= mean(parque_infantil=="1",na.rm = T),
    quadra_esportes= mean(quadra_esportes=="1",na.rm = T),
    refeitorio= mean(refeitorio=="1",na.rm = T),
    internet= mean(internet=="1",na.rm = T),
    alimentacao= mean(alimentacao=="1",na.rm = T),
    material_pedagogico_infantil= mean(material_pedagogico_infantil=="1",na.rm = T),
    quantidade_matricula_infantil=sum(quantidade_matricula_infantil,na.rm = T),
    quantidade_matricula_infantil_creche=sum(quantidade_matricula_infantil_creche,na.rm = T),
    quantidade_matricula_infantil_pre_escola=sum(quantidade_matricula_infantil_pre_escola,na.rm = T),
    quantidade_matricula_idade_0_3=sum(quantidade_matricula_idade_0_3,na.rm = T),
    quantidade_matricula_idade_4_5=sum(quantidade_matricula_idade_4_5,na.rm = T),
    quantidade_matricula_infantil_integral=sum(quantidade_matricula_infantil_integral,na.rm = T),
    quantidade_matricula_infantil_creche_integral=sum(quantidade_matricula_infantil_creche_integral,na.rm = T),
    quantidade_matricula_infantil_pre_escola_integral=sum(quantidade_matricula_infantil_pre_escola_integral,na.rm = T),
    quantidade_turma_infantil=sum(quantidade_turma_infantil,na.rm = T),
    quantidade_turma_infantil_creche=sum(quantidade_turma_infantil_creche,na.rm = T),
    quantidade_turma_infantil_pre_escola=sum(quantidade_turma_infantil_pre_escola,na.rm = T),
    quantidade_docente_infantil=sum(quantidade_docente_infantil,na.rm = T),
    quantidade_docente_infantil_creche=sum(quantidade_docente_infantil_creche,na.rm = T),
    quantidade_docente_infantil_pre_escola=sum(quantidade_docente_infantil_pre_escola,na.rm = T),
    ) |>  ungroup()


write.xlsx2(infra_infantil, file = "E:/TPE/infra_infantil.xlsx", sheetName = "publica")

#Percentual de alunos sem infra
infra_infantil <-  censo_educ_infantil |> 
  filter (rede !=4,
          tipo_situacao_funcionamento ==1,
          etapa_ensino_infantil==1) 

#Percentual por pergunta:
agua_potavel <-  infra_infantil |> 
  filter (agua_potavel != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    agua_potavel=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

  agua_rede_publica <- infra_infantil |> 
  filter (agua_rede_publica != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    agua_rede_publica=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

agua_inexistente <- infra_infantil |> 
  filter (agua_inexistente != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    agua_inexistente =sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

energia_rede_publica <- infra_infantil |> 
  filter (energia_rede_publica != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    energia_rede_publica=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

esgoto_rede_publica <- infra_infantil |> 
  filter (esgoto_rede_publica != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    esgoto_rede_publica=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

lixo_servico_coleta <- infra_infantil |> 
  filter (lixo_servico_coleta != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    lixo_servico_coleta=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

area_verde <- infra_infantil |> 
  filter (area_verde != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    area_verde=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

banheiro <- infra_infantil |> 
  filter (banheiro != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    banheiro=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

banheiro_pne <- infra_infantil |> 
  filter (banheiro_pne != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    banheiro_pne=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

banheiro_educacao_infantil <- infra_infantil |> 
  filter (banheiro_educacao_infantil != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    banheiro_educacao_infantil=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

biblioteca <- infra_infantil |> 
  filter (biblioteca != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    biblioteca=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

biblioteca_sala_leitura <- infra_infantil |> 
  filter (biblioteca_sala_leitura != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    biblioteca_sala_leitura=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

dormitorio_aluno <- infra_infantil |> 
  filter (dormitorio_aluno != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    dormitorio_aluno=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

parque_infantil <- infra_infantil |> 
  filter (parque_infantil != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    parque_infantil=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

quadra_esportes <- infra_infantil |> 
  filter (quadra_esportes != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    quadra_esportes=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

refeitorio <- infra_infantil |> 
  filter (refeitorio != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    refeitorio=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()


internet <- infra_infantil |> 
  filter (internet != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    internet=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

alimentacao <- infra_infantil |> 
  filter (alimentacao != "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    alimentacao=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()

material_pedagogico_infantil <- infra_infantil |> 
  filter (material_pedagogico_infantil!= "1") |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    material_pedagogico_infantil=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()


quantidade_matricula_infantil <- infra_infantil |> 
  group_by(ano) |> 
  summarise (
    escolas = n(),
    quantidade_matricula_infantil=sum(quantidade_matricula_infantil,na.rm = T),
  ) |>  ungroup()



write.xlsx2(agua_potavel , file = "E:/TPE/infra_infantil.xlsx", sheetName = "agua_potavel", append =T)
write.xlsx2(agua_rede_publica , file = "E:/TPE/infra_infantil.xlsx", sheetName = "agua_rede_publica", append =T)
write.xlsx2(agua_inexistente , file = "E:/TPE/infra_infantil.xlsx", sheetName = "agua_inexistente", append =T)
write.xlsx2(energia_rede_publica, file = "E:/TPE/infra_infantil.xlsx", sheetName = "energia_rede_publica", append =T)
write.xlsx2(esgoto_rede_publica, file = "E:/TPE/infra_infantil.xlsx", sheetName = "esgoto_rede_publica", append =T)
write.xlsx2(lixo_servico_coleta, file = "E:/TPE/infra_infantil.xlsx", sheetName = "lixo_servico_coleta", append =T)
write.xlsx2(area_verde, file = "E:/TPE/infra_infantil.xlsx", sheetName = "area_verde", append =T)
write.xlsx2(banheiro, file = "E:/TPE/infra_infantil.xlsx", sheetName = "banheiro", append =T)
write.xlsx2(banheiro_pne, file = "E:/TPE/infra_infantil.xlsx", sheetName = "banheiro_pne", append =T)
write.xlsx2(banheiro_educacao_infantil, file = "E:/TPE/infra_infantil.xlsx", sheetName = "banheiro_educacao_infantil", append =T)
write.xlsx2(biblioteca, file = "E:/TPE/infra_infantil.xlsx", sheetName = "biblioteca", append =T)
write.xlsx2(biblioteca_sala_leitura, file = "E:/TPE/infra_infantil.xlsx", sheetName = "biblioteca_sala_leitura", append =T)
write.xlsx2(dormitorio_aluno, file = "E:/TPE/infra_infantil.xlsx", sheetName = "dormitorio_aluno", append =T)
write.xlsx2(parque_infantil, file = "E:/TPE/infra_infantil.xlsx", sheetName = "parque_infantil", append =T)
write.xlsx2(quadra_esportes, file = "E:/TPE/infra_infantil.xlsx", sheetName = "quadra_esportes", append =T)
write.xlsx2(refeitorio, file = "E:/TPE/infra_infantil.xlsx", sheetName = "refeitorio", append =T)
write.xlsx2(internet, file = "E:/TPE/infra_infantil.xlsx", sheetName = "internet", append =T)
write.xlsx2(alimentacao, file = "E:/TPE/infra_infantil.xlsx", sheetName = "alimentacao", append =T)
write.xlsx2(material_pedagogico_infantil, file = "E:/TPE/infra_infantil.xlsx", sheetName = "material_pedagogico_infantil", append =T)
write.xlsx2(quantidade_matricula_infantil, file = "E:/TPE/infra_infantil.xlsx", sheetName = "quantidade_matricula_infantil", append =T)














