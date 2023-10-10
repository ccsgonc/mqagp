# O objetivo deste tutorial é explorar os dados da Relação Anual de
# Informações Sociais - RAIS.

# Contextualização

## Relação Anual de Informações Sociais (RAIS)

# Instituída pelo Decreto nº 76.900, de 23 de dezembro de 1975 e regida
# atualmente pelo Decreto nº 10.854, de 10 de novembro de 2021, a RAIS tem
# por objetivo:
#   
#   -   o suprimento às necessidades de controle da atividade trabalhista no
# País,
# -   o provimento de dados para a elaboração de estatísticas do trabalho,
# -   a disponibilização de informações do mercado de trabalho às
# entidades governamentais.
# 
# A RAIS é um Registro Administrativo e possui periodicidade anual e
# apresenta informações sobre todos os estabelecimentos formais e vínculos
# celetistas e estatutários no Brasil. Constitui um instrumento
# imprescindível para o cumprimento das normas legais, como também é de
# fundamental importância para o acompanhamento e a caracterização do
# mercado de trabalho formal brasileiro.
# 
# Fonte:
#   <http://pdet.mte.gov.br/images/RAIS/2021/2-Sumário_Executivo_RAIS_2021.pdf>
  
  # Base de dados
  
  ## Leitura dos dados
  
#   Defina o diretório para o local onde está a base de dados e importe a
# base completa. Os microdados estão disponíveis no seguinte endereço
# eletrônico: <ftp://ftp.mtps.gov.br/pdet/microdados/>
  
# setwd("insira o caminho")
options(scipen = 999) # opção para não exibir números científicos
library(readxl)
library(tidyverse)
library(data.table)

Sys.setlocale(category = "LC_ALL", locale = "Portuguese_Brazil.1252")
vinc <- fread("RAIS_VINC_PUB_MG_ES_RJ.txt",dec=",")
saveRDS(vinc,"RAIS_VINC_PUB_MG_ES_RJ.RData")

vinc <- fread("RAIS_VINC_PUB_NORDESTE.txt",dec=",")
saveRDS(vinc,"RAIS_VINC_PUB_NORDESTE.RData")

vinc <- fread("RAIS_VINC_PUB_NORTE.txt",dec=",")
saveRDS(vinc,"RAIS_VINC_PUB_NORTE.RData")

base <- readRDS("RAIS_VINC_PUB_MG_ES_RJ.RData")

# visualizando as variáveis
colnames(vinc) 
colnames(vinc)[43] <-"Tipo Estab_2"


# filtra a base
# seleciona as variáveis de interesse 
base.vinc <- vinc %>% mutate (one = 1,
                              UF = substr(Município,1,2)) %>% 
  filter(UF == "31" & `Vínculo Ativo 31/12`==1)
# rm(vinc)


# Visualizando dados duplicados:
base.vinc2<- distinct(base.vinc)
nrow(base.vinc2)-nrow(base.vinc)

## Verificando variáveis

# O pacote
# [DataMaid](https://cran.r-project.org/web/packages/dataMaid/index.html)
# é uma ferramenta valiosa no ambiente R para a verificação, limpeza e
# organização eficiente de conjuntos de dados. Desenvolvido com o objetivo
# de aprimorar a qualidade e confiabilidade dos dados, o DataMaid oferece
# funcionalidades que auxiliam na identificação de valores ausentes, erros
# de digitação, inconsistências e outras anomalias nos dados. Além disso,
# permite a criação de relatórios detalhados, facilitando a compreensão
# das estruturas e conteúdo dos datasets, promovendo assim uma exploração
# mais assertiva. Com suas capacidades automatizadas de geração de
# sumários e gráficos descritivos, o DataMaid é uma ferramenta para
# cientistas de dados e analistas que desejam garantir a qualidade e
# integridade dos seus dados, tornando todo o processo de análise mais
# eficiente e confiável.

library(dataMaid)
# makeDataReport(base.vinc,replace = TRUE)

## Indicadores

# 1.  Total de vínculos
# 2.  Total de vínculos por municípios
# 3.  Total de vínculos por atividade econômica
# 4.  Total de vínculo por sexo
# 5.  Total de vínculo por idade
# 6.  Total de vínculo por escolaridade

### UF e municípios ####

# total da UF
tabela01 <- base.vinc %>% 
  summarise(t.vinc = sum(one))
tabela01

# total por municípios
nomes_mun <- read_excel("Nomes_municipios.xlsx")
colnames(nomes_mun)
nomes_mun <- nomes_mun %>%  mutate(
  Município = as.numeric(substr(`Código do Município`,1,6))
)

# junta as duas bases
base.vinc <- left_join(base.vinc, nomes_mun)

# sintetiza tabela
tabela02 <- base.vinc %>%  group_by(`Nome do Município`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

arrange(tabela02,desc(tabela02$t.vinc))[1:10,]
arrange(tabela02,(tabela02$t.vinc))[1:10,]

### Atividade econômica ####
cnae20 <- read_excel("cnae_2_0.xlsx")
colnames(cnae20)

# junta as duas bases
base.vinc <- left_join(base.vinc, cnae20)

# total da UF
tabela03 <- base.vinc %>% 
  group_by(`CNAE 2.0 Subclasse_descricao`) %>% 
  summarise(t.vinc = sum(one))

arrange(tabela03,desc(tabela03$t.vinc))[1:10,]

# total por municípios
# sintetiza tabela
tabela04 <- base.vinc %>%  
  group_by(`Nome do Município`,`CNAE 2.0 Subclasse_descricao`) %>% 
  summarise(t.vinc = sum(one))

# escolhendo uma atividade
tabela04a <- tabela04 %>%  
  filter(`CNAE 2.0 Subclasse_descricao`=="Construção de Edifícios")

arrange(tabela04a,desc(tabela04a$t.vinc))[1:10,]

# escolhendo um município
tabela04m <- tabela04 %>%  
  filter(`Nome do Município`=="Sete Lagoas")

arrange(tabela04m,desc(tabela04m$t.vinc))[1:10,]


### Sexo ####
# inserindo labels
base.vinc$`Sexo Trabalhador` <- as.factor(base.vinc$`Sexo Trabalhador`)
levels(base.vinc$`Sexo Trabalhador`) <- c("Masculino","Feminino")

# total da UF
tabela05 <- base.vinc %>%
  group_by(`Sexo Trabalhador`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

tabela05

# total por municípios
# sintetiza tabela
tabela06 <- base.vinc %>%  
  group_by(`Nome do Município`,`Sexo Trabalhador`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

# separando as bases
tabela06m <- tabela06 %>%  filter(`Sexo Trabalhador`=="Masculino")
tabela06f <- tabela06 %>%  filter(`Sexo Trabalhador`=="Feminino")

arrange(tabela06m,desc(tabela06m$t.vinc))[1:10,]
arrange(tabela06f,desc(tabela06f$t.vinc))[1:10,]

arrange(tabela06m,desc(tabela06m$p.vinc))[1:10,]
arrange(tabela06f,desc(tabela06f$p.vinc))[1:10,]


### Sexo e Atividade Econômica ####
# escolhendo uma atividade
tabela07 <- base.vinc %>%  
  filter(`CNAE 2.0 Subclasse_descricao`=="Construção de Edifícios") %>% 
  group_by(`Nome do Município`,`Sexo Trabalhador`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))
  
arrange(tabela07,desc(tabela07$p.vinc))[1:10,]

tabela08 <- base.vinc %>%  
  filter(`CNAE 2.0 Subclasse_descricao`=="Comércio Varejista de Artigos do Vestuário e Acessórios") %>% 
  group_by(`Nome do Município`,`Sexo Trabalhador`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

arrange(tabela08,desc(tabela08$p.vinc))[1:10,]

# escolhendo um município
tabela09 <- base.vinc %>%  
  filter(`Nome do Município`=="Sete Lagoas")%>% 
  group_by(`CNAE 2.0 Subclasse_descricao`,`Sexo Trabalhador`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

# separando as bases
tabela09m <- tabela09 %>%  filter(`Sexo Trabalhador`=="Masculino")
tabela09f <- tabela09 %>%  filter(`Sexo Trabalhador`=="Feminino")

arrange(tabela09m,desc(tabela09m$t.vinc))[1:10,]
arrange(tabela09f,desc(tabela09f$t.vinc))[1:10,]

arrange(tabela09m,desc(tabela09m$p.vinc))[1:10,]
arrange(tabela09f,desc(tabela09f$p.vinc))[1:10,]

### Idade ####
# criando a variável idade
base.vinc <- base.vinc %>% mutate(
faixa_idade =  cut(Idade,c(-Inf,13,17,24,29,44,64,Inf),
                   labels = c("_menor que 13 anos",
                              "14 a 17 anos",
                              "18 a 24 anos",
                              "25 a 29 anos",
                              "30 a 44 anos",
                              "45 a 64 anos",
                              "65 anos ou mais")))

# total da UF
tabela10 <- base.vinc %>%
  group_by(faixa_idade) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

tabela10

### Sexo e Idade ####

# sintetiza tabela
tabela11 <- base.vinc %>%  
  group_by(faixa_idade,`Sexo Trabalhador`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

# separando as bases
tabela11m <- tabela11 %>%  filter(`Sexo Trabalhador`=="Masculino")
tabela11f <- tabela11 %>%  filter(`Sexo Trabalhador`=="Feminino")

tabela11m
tabela11f

### Idade e Atividade Econômica ####
# escolhendo uma atividade
tabela12 <- base.vinc %>%  
  filter(`CNAE 2.0 Subclasse_descricao`=="Construção de Edifícios") %>% 
  group_by(faixa_idade) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

tabela12

tabela13 <- base.vinc %>%  
  filter(`CNAE 2.0 Subclasse_descricao`=="Comércio Varejista de Artigos do Vestuário e Acessórios") %>% 
  group_by(faixa_idade) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

tabela13

### Escolaridade ####

# Faz o total de vínculos de escolaridade para a UF


### Agrupamento de atividades ####

# organizando o turismo
base.vinc <- base.vinc %>% 
mutate(g1=ifelse((`CNAE 2.0 Subclasse`==4912401)|
                   (`CNAE 2.0 Subclasse`==4922101)|
                   (`CNAE 2.0 Subclasse`==4922102)|
                   (`CNAE 2.0 Subclasse`==4922103)|
                   (`CNAE 2.0 Subclasse`==4923001)|
                   (`CNAE 2.0 Subclasse`==4923002)|
                   (`CNAE 2.0 Subclasse`==4929902)|
                   (`CNAE 2.0 Subclasse`==4929904)|
                   (`CNAE 2.0 Subclasse`==4929999)|
                   (`CNAE 2.0 Subclasse`==4950700),1,0),
       g2=ifelse((`CNAE 2.0 Subclasse`==5011402)|
                   (`CNAE 2.0 Subclasse`==5012202)|
                   (`CNAE 2.0 Subclasse`==5022002)|
                   (`CNAE 2.0 Subclasse`==5091202)|
                   (`CNAE 2.0 Subclasse`==5099801)|
                   (`CNAE 2.0 Subclasse`==5099899),1,0),
       g3=ifelse((`CNAE 2.0 Subclasse`==5111100)|
                   (`CNAE 2.0 Subclasse`==5112901)|
                   (`CNAE 2.0 Subclasse`==5112999),1,0),
       g4=ifelse((`CNAE 2.0 Subclasse`== 5222200)|
                   (`CNAE 2.0 Subclasse`==5229001)|
                   (`CNAE 2.0 Subclasse`==5229099)|
                   (`CNAE 2.0 Subclasse`==5240101)|
                   (`CNAE 2.0 Subclasse`==5240199),1,0),
       g5=ifelse((`CNAE 2.0 Subclasse`==5510801)|
                   (`CNAE 2.0 Subclasse`==5510802)|
                   (`CNAE 2.0 Subclasse`==5590601)|
                   (`CNAE 2.0 Subclasse`==5590602)|
                   (`CNAE 2.0 Subclasse`==5590603)|
                   (`CNAE 2.0 Subclasse`==5590699),1,0),
       g6=ifelse((`CNAE 2.0 Subclasse`==5611201)|
                   (`CNAE 2.0 Subclasse`==5611202)|
                   (`CNAE 2.0 Subclasse`==5611204)|
                   (`CNAE 2.0 Subclasse`==5611205)|
                   (`CNAE 2.0 Subclasse`==5611203)|
                   (`CNAE 2.0 Subclasse`==5612100),1,0),
       g7=ifelse((`CNAE 2.0 Subclasse`==7711000)|
                   (`CNAE 2.0 Subclasse`==7721700),1,0),
       g8=ifelse((`CNAE 2.0 Subclasse`==7911200)|
                   (`CNAE 2.0 Subclasse`==7912100)|
                   (`CNAE 2.0 Subclasse`==7990200),1,0),
       g9=ifelse((`CNAE 2.0 Subclasse`==9001901)|
                   (`CNAE 2.0 Subclasse`==9001902)|
                   (`CNAE 2.0 Subclasse`==9001903)|
                   (`CNAE 2.0 Subclasse`==9001904)|
                   (`CNAE 2.0 Subclasse`==9001905)|
                   (`CNAE 2.0 Subclasse`==9001999)|
                   (`CNAE 2.0 Subclasse`==9002701)|
                   (`CNAE 2.0 Subclasse`==9102301)|
                   (`CNAE 2.0 Subclasse`==9103100)|
                   (`CNAE 2.0 Subclasse`==9200301)|
                   (`CNAE 2.0 Subclasse`==9200302)|
                   (`CNAE 2.0 Subclasse`==9200399)|
                   (`CNAE 2.0 Subclasse`==9319101)|
                   (`CNAE 2.0 Subclasse`==9321200)|
                   (`CNAE 2.0 Subclasse`==9329801)|
                   (`CNAE 2.0 Subclasse`==9329802)|
                   (`CNAE 2.0 Subclasse`==9329803)|
                   (`CNAE 2.0 Subclasse`==9329804)|
                   (`CNAE 2.0 Subclasse`==9329899),1,0),
       act = 1* (g1==1 | g2==1 | g3==1 | g4==1 | g5==1 |
                   g6==1 | g7==1 | g8==1 | g9==1))

# total da UF
tabela14 <- base.vinc %>%
  filter(act==1) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))
tabela14

#total de municípios
tabela15 <- base.vinc %>%
  filter(act==1) %>% 
  group_by(`Nome do Município`) %>% 
  summarise(t.vinc = sum(one)) %>% 
  mutate(p.vinc = round(t.vinc/sum(t.vinc)*100,2))

arrange(tabela15,desc(tabela15$t.vinc))[1:10,]
