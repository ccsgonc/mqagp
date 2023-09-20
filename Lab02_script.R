# ---
#   title: "Indicadores no R: pirâmides etárias com o agregado de setores - Censo 
# Demográfico 2010"
# author: "Caio Gonçalves - caio.goncalves@fjp.mg.gov.br"
# date: "30-agosto-2023"
# ---


# O objetivo deste tutorial produzir pirâmides etárias e calcular indicadores 
# demográficos relacionados.

# Contextualização

## Pirâmides etárias
# As pirâmides etárias desempenham um papel fundamental ao retratar visualmente 
# a distribuição da população por faixas etárias em uma determinada sociedade. 
# Essas representações gráficas oferecem insights valiosos para os planejadores 
# governamentais, economistas e especialistas em políticas públicas, permitindo 
# uma compreensão mais profunda das tendências demográficas, do envelhecimento 
# da população e das necessidades sociais. As pirâmides etárias também são 
# ferramentas cruciais para antecipar desafios relacionados à saúde, previdência, 
# educação e mercado de trabalho, auxiliando na formulação de estratégias que 
# visem atender às demandas específicas de cada faixa etária e promover um 
# desenvolvimento sustentável a longo prazo. 


## Requisitos

# A representação gráfica das pirâmides etárias ilustra uma distribuição 
# demográfica reveladora: 
#   * os segmentos mais jovens compõem a base, enquanto os grupos mais idosos 
# estão no ápice; 
# * a construção desse gráfico essencial requer apenas informações sobre a 
# quantidade de indivíduos em uma população, discriminados por gênero e faixa 
# etária; 
# * pode ser categorizados de forma simples ou em intervalos de cinco anos; 
# * em geral, homens são dispostos à esquerda e mulheres à direita; 
# * é possível optar por apresentar dados em idade simples ou agrupada; 
# * Ao recorrer a percentagens populacionais, em vez de números absolutos, as 
# pirâmides proporcionam uma comparação eficaz entre diferentes representações, 
# assegurando que homens e mulheres totalizem conjuntamente 100%.

## Identificando setores

# Escolha um setor em: https://censo2010.ibge.gov.br/painel/ e salve o número 
# do setor escolhido no objeto `setor_escolhido`:
  
setor_escolhido <- 310620005680043

# Quais dados são necessários para a construção da pirâmide? 
#   
#   Para responder essa pergunta, é necessário conhecer a documentação disponível 
# (https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/). Depois disso, o passo seguinte é importar os dados.

# Base de dados

# A base de dados de agregados por setores do IBGE é uma fonte robusta e 
# abrangente de informações socioeconômicas e demográficas que oferece um 
# panorama detalhado das características das áreas geográficas em seu menor 
# nível territorial. A granularidade dessas informações tornam a base de dados 
# de agregados por setores do IBGE uma ferramenta crucial para a formulação de
# políticas, estudos acadêmicos e análises de mercado.

## Leitura dos dados 

# Importando os dados dos arquivos *Básico*,*Pessoa11* e *Pessoa12* por existir 
# o interesse em variáveis disponíveis nessas duas bases de dados. Em sequência 
# organiza-se em um único dataframe *df*.

library(readxl)
library(tidyverse)
df.basico <- read_excel("Basico-MG.xls") %>% select("Cod_setor","Cod_municipio",
                                                    "Nome_do_municipio",
                                                    "Cod_bairro",
                                                    "Nome_do_bairro",
                                                    "Cod_subdistrito",
                                                    "Nome_do_subdistrito") %>% 
  mutate(Cod_setor=as.character(Cod_setor))
df.pessoa11 <- read_excel("Pessoa11_MG.xls", 
                          col_types = c("text", "text", "numeric", 
                                        rep("numeric",133)))
df.pessoa12 <- read_excel("Pessoa12_MG.xls", 
                          col_types = c("text", "text", "numeric", 
                                        rep("numeric",133)))

df.h <- full_join(df.basico,df.pessoa11)
df.m <- full_join(df.basico,df.pessoa12)
rm(df.pessoa11,df.pessoa12,df.basico)

## Verificando variáveis

# O pacote [DataMaid](https://cran.r-project.org/web/packages/dataMaid/index.html) 
# é uma ferramenta valiosa no ambiente R para a verificação, limpeza e organização 
# eficiente de conjuntos de dados. Desenvolvido com o objetivo de aprimorar a 
# qualidade e confiabilidade dos dados, o DataMaid oferece funcionalidades que 
# auxiliam na identificação de valores ausentes, erros de digitação, 
# inconsistências e outras anomalias nos dados. Além disso, permite a 
# criação de relatórios detalhados, facilitando a compreensão das estruturas e 
# conteúdo dos datasets, promovendo assim uma exploração mais assertiva. 
# Com suas capacidades automatizadas de geração de sumários e gráficos 
# descritivos, o DataMaid é uma ferramenta para cientistas de dados e analistas 
# que desejam garantir a qualidade e integridade dos seus dados, tornando todo 
# o processo de análise mais eficiente e confiável.

# Observe que no código anterior, algumas alterações já foram feitas.

library(dataMaid)
makeDataReport(df.h,replace = TRUE)
makeDataReport(df.m,replace = TRUE)


# O que foi possível verificar com o relatório produzido anteriormente? 
#   Anote para implementar na sequência, porém, em conjunto com essas mudanças, 
# planeje os indicadores a serem calculados e identifique as alterações
# necessárias para tal.

## Modificando variáveis

# Pensando em cada um dos seguintes indicadores: 
#   
# 1. Total populacional e a proporção por sexo e idade (base para a pirâmide 
#                                                         etária)
# 2. Razão de dependência
# 3. Índice de envelhecimento
# 
# Qual variáveis precisam ser manipuladas para obter essas informações? Realize 
# as modificações na base necessárias para isso:
  
# para assegurar que alguns NA fiquei com zero
df.h <- replace(df.h, is.na(df.h), 0)
df.m <- replace(df.m, is.na(df.m), 0)

# Para montar a pirâmide, é necessário montar o data.frame com as idades e sexo, 
# totais populacionais e também a distribuição populacional:
  
library(tidyselect)

# Fazendo para os homens
df.hh <- df.h %>% mutate(
  x00_04= V022 + V035 + V036 + V037 + V038,
  x05_09= V039 + V040 + V041 + V042 + V043,
  x10_14= V044 + V045 + V046 + V047 + V048,
  x15_19= V049 + V050 + V051 + V052 + V053,
  x20_24= V054 + V055 + V056 + V057 + V058,
  x25_29= V059 + V060 + V061 + V062 + V063,
  x30_34= V064 + V065 + V066 + V067 + V068,
  x35_39= V069 + V070 + V071 + V072 + V073,
  x40_44= V074 + V075 + V076 + V077 + V078,
  x45_49= V079 + V080 + V081 + V082 + V083,
  x50_54= V084 + V085 + V086 + V087 + V088,
  x55_59= V089 + V090 + V091 + V092 + V093,
  x60_64= V094 + V095 + V096 + V097 + V098,
  x65_69= V099 + V100 + V101 + V102 + V103,
  x70_74= V104 + V105 + V106 + V107 + V108,
  x75_79= V109 + V110 + V111 + V112 + V113,
  x80_84= V114 + V115 + V116 + V117 + V118,
  x85_mais= V119 + V120 + V121 + V122 + V123 +
    V124 + V125 + V126 + V127 + V128 +
    V129 + V130 + V131 + V132 + V133 + V134
) %>% select(Cod_setor,Cod_municipio ,     
             Nome_do_municipio,Cod_bairro ,        
             Nome_do_bairro,Cod_subdistrito ,   
             Nome_do_subdistrito,x00_04,  x05_09,             
             x10_14,              x15_19,             
             x20_24,              x25_29,             
             x30_34,              x35_39,             
             x40_44,              x45_49,             
             x50_54,              x55_59,             
             x60_64,              x65_69,             
             x70_74,              x75_79,             
             x80_84,              x85_mais)

# Fazendo para as mulheres
df.mm <- df.m %>% mutate(
  x00_04= V022 + V035 + V036 + V037 + V038,
  x05_09= V039 + V040 + V041 + V042 + V043,
  x10_14= V044 + V045 + V046 + V047 + V048,
  x15_19= V049 + V050 + V051 + V052 + V053,
  x20_24= V054 + V055 + V056 + V057 + V058,
  x25_29= V059 + V060 + V061 + V062 + V063,
  x30_34= V064 + V065 + V066 + V067 + V068,
  x35_39= V069 + V070 + V071 + V072 + V073,
  x40_44= V074 + V075 + V076 + V077 + V078,
  x45_49= V079 + V080 + V081 + V082 + V083,
  x50_54= V084 + V085 + V086 + V087 + V088,
  x55_59= V089 + V090 + V091 + V092 + V093,
  x60_64= V094 + V095 + V096 + V097 + V098,
  x65_69= V099 + V100 + V101 + V102 + V103,
  x70_74= V104 + V105 + V106 + V107 + V108,
  x75_79= V109 + V110 + V111 + V112 + V113,
  x80_84= V114 + V115 + V116 + V117 + V118,
  x85_mais= V119 + V120 + V121 + V122 + V123 +
    V124 + V125 + V126 + V127 + V128 +
    V129 + V130 + V131 + V132 + V133 + V134
) %>% select(Cod_setor,Cod_municipio ,     
             Nome_do_municipio,Cod_bairro ,        
             Nome_do_bairro,Cod_subdistrito ,   
             Nome_do_subdistrito,x00_04,  x05_09,             
             x10_14,              x15_19,             
             x20_24,              x25_29,             
             x30_34,              x35_39,             
             x40_44,              x45_49,             
             x50_54,              x55_59,             
             x60_64,              x65_69,             
             x70_74,              x75_79,             
             x80_84,              x85_mais)


# Preparando os dados para a pirâmide
library(tidyr)
df.h_long <- gather(df.hh, faixas, t.pop, x00_04:x85_mais) %>% mutate(Sex = "homem")
df.m_long <- gather(df.mm, faixas, t.pop, x00_04:x85_mais) %>% mutate(Sex = "mulher")
df.long <- rbind(df.h_long,df.m_long)
rm(df.h_long,df.m_long)

df.long <- df.long %>% 
  group_by(Cod_setor, Cod_municipio,
           Nome_do_municipio, Cod_bairro,
           Nome_do_bairro, Cod_subdistrito,
           Nome_do_subdistrito) %>%
  mutate(p.pop = t.pop/sum(t.pop)*100)


# Análise dos dados

## Pirâmides etárias

# As pirâmides etárias estão representadas em três recortes geográficos: setor, 
# bairro e município. Reflita sobre a questão do tamanho populacional e seu 
# impacto no desenho da pirâmide etária.

library(DescTools)

# setor censitário escolhido
df.piramide01 <- filter(df.long, Cod_setor==setor_escolhido)

h.pop<- subset(df.piramide01,df.piramide01$Sex=="homem")$p.pop
m.pop<- subset(df.piramide01,df.piramide01$Sex=="mulher")$p.pop
age <- c("0-4","5-9","10-14","15-19","20-24","25-29",
         "30-34","35-39","40-44","45-49","50-54",
         "55-59","60-64","65-69","70-74","75-79","80-44","85+")

PlotPyramid(h.pop, 
            m.pop,
            ylab = age, space = 0, col = c("cornflowerblue", "indianred"),
            xlim=c(-5,5),
            main="Pirâmide etária",
            lxlab="Homem", rxlab="Mulher", gapwidth=0, ylab.x=-5 )

# bairro escolhido
bairro_escolhido <- df.h$Nome_do_bairro[df.h$Cod_setor==setor_escolhido]
df.piramide02 <- df.long%>% filter(Nome_do_bairro==bairro_escolhido) %>%
  group_by(Nome_do_bairro, faixas ,Sex) %>% summarise(total.pop = sum(t.pop)) %>%
  group_by(Nome_do_bairro) %>% mutate(p.pop = total.pop/sum(total.pop)*100)

h.pop<- subset(df.piramide02,df.piramide02$Sex=="homem")$p.pop
m.pop<- subset(df.piramide02,df.piramide02$Sex=="mulher")$p.pop
age <- c("0-4","5-9","10-14","15-19","20-24","25-29",
         "30-34","35-39","40-44","45-49","50-54",
         "55-59","60-64","65-69","70-74","75-79","80-44","85+")

PlotPyramid(h.pop, 
            m.pop,
            ylab = age, space = 0, col = c("cornflowerblue", "indianred"),
            xlim=c(-5,5),
            main="Pirâmide etária",
            lxlab="Homem", rxlab="Mulher", gapwidth=0, ylab.x=-5 )

# município escolhido
municipio_escolhido <- df.h$Nome_do_municipio[df.h$Cod_setor==setor_escolhido]
df.piramide03 <- df.long%>% filter(Nome_do_municipio==municipio_escolhido) %>%
  group_by(Nome_do_municipio, faixas ,Sex) %>% summarise(total.pop = sum(t.pop)) %>%
  group_by(Nome_do_municipio) %>% mutate(p.pop = total.pop/sum(total.pop)*100)

h.pop<- subset(df.piramide03,df.piramide03$Sex=="homem")$p.pop
m.pop<- subset(df.piramide03,df.piramide03$Sex=="mulher")$p.pop
age <- c("0-4","5-9","10-14","15-19","20-24","25-29",
         "30-34","35-39","40-44","45-49","50-54",
         "55-59","60-64","65-69","70-74","75-79","80-44","85+")

PlotPyramid(h.pop, 
            m.pop,
            ylab = age, space = 0, col = c("cornflowerblue", "indianred"),
            xlim=c(-5,5),
            main="Pirâmide etária",
            lxlab="Homem", rxlab="Mulher", gapwidth=0, ylab.x=-5 )

# Como você classificaria essas pirâmides etárias:
#   1. Base alargada
# População “jovem” e crescimento populacional rápido.
# 
# 2. Centro alargado
# População em transição e crescimento populacional mais lento.
# 
# 3. Topo alargado
# População envelhecida e crescimento populacional nulo ou negativo.

## Proporção por grupo etário, razão de dependência e índice de envelhecimento

# Razão de dependência mostra a relação entre as pessoas que são dependentes 
# (muito jovens e idosos) e aquelas que supostamente deveriam sustentá-las. Pode 
# ser dividido entre razão de dependência das crianças e razão de dependência dos 
# idosos. Já o índice de envelhecimento é a razão entreo número de pessoas idosas 
# e o número de crianças.
# 
# Por se tratar de um novo agrupamento, cria-se uma nova variável:
  
df.long <- df.long %>% mutate(grupos = case_when(faixas=="x00_04" | faixas=="x05_09" | faixas=="x10_14" ~ "0_crianças",
                                                 faixas=="x15_19" | faixas=="x20_24"| faixas=="x25_29" | faixas=="x30_34"| faixas=="x35_39" | faixas=="x40_44"| faixas=="x45_49"| faixas=="x50_54" | faixas=="x55_59" ~ "1_adultos",
                                                 faixas=="x60_64" | faixas=="x65_69" | faixas=="x70_74" | faixas=="x75_79"| faixas=="x80_84" | faixas=="x85_mais" ~ "2_idosos"))

# setor censitário escolhido
df.01 <- filter(df.long, Cod_setor==setor_escolhido) %>% group_by(grupos) %>% 
  summarise(t.pop=sum(t.pop)) %>% mutate(p.pop = t.pop/sum(t.pop)*100)
df.01

r.dep.crian <- df.01$t.pop[df.01$grupos=="0_crianças"]/df.01$t.pop[df.01$grupos=="1_adultos"]*100
r.dep.idos  <- df.01$t.pop[df.01$grupos=="2_idosos"]/df.01$t.pop[df.01$grupos=="1_adultos"]*100
r.dep.total <-(df.01$t.pop[df.01$grupos=="0_crianças"]+df.01$t.pop[df.01$grupos=="2_idosos"])/df.01$t.pop[df.01$grupos=="1_adultos"]*100
ind.envelh <- df.01$t.pop[df.01$grupos=="2_idosos"]/df.01$t.pop[df.01$grupos=="0_crianças"]*100
r.dep.crian
r.dep.idos
r.dep.total
ind.envelh

# bairro escolhido
bairro_escolhido <- df.h$Nome_do_bairro[df.h$Cod_setor==setor_escolhido]
df.02 <- df.long %>% filter(Nome_do_bairro==bairro_escolhido) %>%
  group_by(grupos) %>% summarise(t.pop = sum(t.pop))  %>% mutate(p.pop = t.pop/sum(t.pop)*100)
df.02

r.dep.crian <- df.02$t.pop[df.02$grupos=="0_crianças"]/df.02$t.pop[df.02$grupos=="1_adultos"]*100
r.dep.idos  <- df.02$t.pop[df.02$grupos=="2_idosos"]/df.02$t.pop[df.02$grupos=="1_adultos"]*100
r.dep.total <-(df.02$t.pop[df.02$grupos=="0_crianças"]+df.02$t.pop[df.02$grupos=="2_idosos"])/df.02$t.pop[df.02$grupos=="1_adultos"]*100
ind.envelh <- df.02$t.pop[df.02$grupos=="2_idosos"]/df.02$t.pop[df.02$grupos=="0_crianças"]*100
r.dep.crian
r.dep.idos
r.dep.total
ind.envelh

# município escolhido
municipio_escolhido <- df.h$Nome_do_municipio[df.h$Cod_setor==setor_escolhido]
df.03 <- df.long %>% filter(Nome_do_municipio==municipio_escolhido) %>%
  group_by(grupos) %>% summarise(t.pop = sum(t.pop)) %>%
  mutate(p.pop = t.pop/sum(t.pop)*100)
df.03

r.dep.crian <- df.03$t.pop[df.03$grupos=="0_crianças"]/df.03$t.pop[df.03$grupos=="1_adultos"]*100
r.dep.idos  <- df.03$t.pop[df.03$grupos=="2_idosos"]/df.03$t.pop[df.03$grupos=="1_adultos"]*100
r.dep.total <-(df.03$t.pop[df.03$grupos=="0_crianças"]+df.03$t.pop[df.03$grupos=="2_idosos"])/df.03$t.pop[df.03$grupos=="1_adultos"]*100
ind.envelh <- df.03$t.pop[df.03$grupos=="2_idosos"]/df.03$t.pop[df.03$grupos=="0_crianças"]*100
r.dep.crian
r.dep.idos
r.dep.total
ind.envelh

# Para refletir: poderia ser usada uma outra classificação de idosos:
#   
## Considerações finais
#   
#   Produzir dados mais desagregados é algo requisitado para conhecer realidades 
# locais, porém é necessário ter cautela nos cálculos dos indicadores dado que 
# com populações pequenas, alguns deles não informam o que deveriam informam e 
# perdem sentido. Ficar atento a esse tipo de situação.