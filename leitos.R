# CARREGANDO BIBLIOTECAS ---------------

library(rio)
library(tidyverse)
library(reshape2)

#DIRETORIO

setwd("~/Documentos/jfemdados")

# CRIANDO FUNCOES E VETORES

# nomes de colunas da tabela de leitos

vetor_leitos <- c("municipio", "adm_federal", "adm_estadual", "adm_municipal", "adm_pub_outras",
  "sociedade_mista", "empresarial", "s_fim_lucrativo", "total")

vetor_nome_mapa <- c("municipio", "id_municipio", "ano_ref",
                     "leidecriacao", "area", "geometry")

vetor_zonadamata <- c("313670", #jf
                      "314390", #muriae     
                      "316990", #ubá
                      "316070", #santos dummont
                      "313940", #manhuaçu
                      "311530", #cataguases
                      "315210", #ponte nova
                      "317130", #viçosa
                      "313840", #leopoldina
                      "310150") #além paraíba

# função para transformar NA em 0 

zeradora <- function(x){
 ifelse(x == "-", 0, as.double(x)  )
}

corrigena <- function(x){
  if_else(is.na(x), 0, as.numeric(x))
}

# fun??o para calcular e arredondar valores em porcentagem

geradora_de_porcento <- function (x,y){
  round((x/y)*100, 2)
}

# ABRINDO AS BASE -------------------------

# populacao residente em minas gerais - estimativas do IBGE para 2020
pop_estimada <- read.csv("leitos/pop_estimada.csv", header=FALSE)%>%
  filter( str_starts(V1, pattern = "31"))%>%
  rename(id_municipio = V1, municipio = V2, pop2020 = V3)%>%
  mutate(pop2020 = as.numeric(pop2020),
         id_municipio = str_sub(id_municipio,1,6))%>%
  select(-municipio)

#CALCULANDO O NUMERO DE LEITOS

# leitos - hospitalar internacao  
organizadora <- function(nomedocsv, tipodeleito) {
  import(nomedocsv)%>%
  set_names("municipio", "leitos_total", "leitos_sus", "leitos_nao_sus")%>%
  mutate(id_municipio = str_sub(municipio, 1,6),
         across(-id_municipio, zeradora),
         across(-id_municipio, as.numeric),
         tipo_leito = tipodeleito)%>%
  select(-municipio)%>%
  filter(! id_municipio == "Total")
}


leitos_municipio_sep <- organizadora('leitos/leito-hosp-int.csv', tipodeleito = 'hospitalar internação')%>%
  bind_rows(organizadora('leitos/leitos-hosp-comp.csv', 'hospitalar complementar'))
  
leitos_municipio <- leitos_municipio_sep%>%
  group_by(id_municipio)%>%
  summarise(leitos_total = sum(leitos_total),
            leitos_sus = sum(leitos_sus),
            leitos_nao_sus = sum(leitos_nao_sus))

  
# JUNTANDO AS BASES -------------------------

basefinal <- leitos_municipio%>%
left_join(pop_estimada, by = "id_municipio")%>%
#criando colunas de interesse
          mutate(leito_total_pcapita = (leitos_total/pop2020)*1000,
                 total_leitos_zm = sum(leitos_total),
                 prop_leitos_zm = (leitos_total/total_leitos_zm)*100)


#PEGANDO QUAIS MUNICIPIOS REPRESENTAM TODA A ZONA DA MATA

nomes_zm <- rio::import("vacina/mapa_vacina.xlsx")%>%
  filter(Nome_da_meso  == "Zona da Mata")%>%
  mutate(code_muni = str_sub(code_muni,1,6))%>%
  pull(code_muni)%>%
  as.character()


# MAPEANDO LEITOS


library(ggplot2)
library(geobr)
geoms_mg<- geobr::read_municipality(code_muni = 'MG')%>%
                    mutate(id_municipio = str_sub(code_muni,1,6))%>%
                    filter(id_municipio %in% nomes_zm)%>%
  left_join(basefinal, by= "id_municipio")%>%
  sf::st_simplify(dTolerance = 0.005)%>%
  mutate(name_muni= str_to_title(abjutils::rm_accent(name_muni)),
         across(leitos_total:prop_leitos_zm, corrigena),
         recomendacao_oms = case_when(leito_total_pcapita > 4 ~ "Mínimo segundo a OMS",
                                      leito_total_pcapita < 4 ~ "Abaixo do recomendado"),
         prop_leitos_zm_grupos =  cut(prop_leitos_zm, c(0,0,1,5,10,100)))




ggplot() + geom_sf(data=geoms_mg , size= .15, 
                   show.legend = F) + geom_sf(data=geoms_mg, aes(fill =prop_leitos_zm_grupos ), color = "white")+
  theme_void() + scale_fill_manual(name = "Proporção de Leitos/ Habitante da Zona da Mata em cada município",
                                   label= c("0% - Sem leito hospitalar", "Menos de 1%", "Entre 1% e 5%","Entre 5% e 10%","Mais que 10%"),
                                    values= c("black", "red4","indianred1", "yellow", "limegreen"))













