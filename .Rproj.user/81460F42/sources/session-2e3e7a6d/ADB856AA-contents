# ---
#   title: "Indicadores no R: registros de mortalidade e as projeções populacionais"
# author: "Caio Gonçalves - caio.goncalves@fjp.mg.gov.br"
# date: "30-agosto-2023"
# ---
  
# O objetivo deste tutorial é realizar comparações entre os registros de óbitos e
# nascimentos e as projeções desses dois componentes. Nesse caso trabalha-se com 
# o nível geográfico UF.

# Contextualização

# O Sistema de Informações da Mortalidade (SIM) e oSistema de Informações sobre 
# Nascidos Vivos (SINASC) desempenha um papel relevante na captação e registro 
# de dados sobre os óbitos e nascimentos do país. O sistema reune informações 
# detalhadas sobre cada óbito como local de residência, data, sexo e sobre o 
# recém-nascido, como local de nascimento, data, sexo, tipo de parto e 
# características da mãe. Já as projeções populacionais realizadas pelo 
# IBGE são essenciais para o planejamento de políticas públicas, alocação 
# de recursos e desenvolvimento sustentável, uma vez que oferecem insights 
# sobre as tendências demográficas futuras, permitindo que os governos e as 
# instituições tomem decisões informadas para atender às necessidades em 
# constante evolução das populações.

# Estatísticas vitais

## SIM

# 1. Instale e carregue o pacote "microdatasus":

library(remotes)
if (!require("microdatasus")){
  install.packages("remotes")
  remotes::install_github("rfsaldanha/microdatasus")
  library(microdatasus)
}

# 2. Acesse os microdados de óbitos:
  
  # Vamos supor que você queira acessar os microdados de registros de óbitos 
  # da dados_sim de dados "SIM-DO". Use o seguinte código para isso:
  
dados_sim <- fetch_datasus(year_start = 2000, 
                           year_end = 2021, 
                           uf = "DF", 
                           information_system = "SIM-DO",
                           vars=c("DTOBITO","DTNASC","IDADE","SEXO","CODMUNRES"))

# 3. Processa os dados

dados_sim <- process_sim(dados_sim)

# pacote [dataMaid](https://cran.r-project.org/web/packages/dataMaid/index.html)
if (!require("dataMaid")){
  install.packages("dataMaid")
  library(dataMaid)
}
makeDataReport(dados_sim,replace = TRUE)

library(tidyverse)
dados_sim<- dados_sim %>% mutate(
  obt = 1,
  ano = as.numeric(substr(DTOBITO,1,4)),
  ano_nasc = as.numeric(substr(DTNASC,1,4)),
  IDADEanos = as.numeric(IDADEanos),
  faixa_idade =  cut(IDADEanos,c(-Inf,0,4,9,14,19,24,29,34,39,
                                 44,49,54,59,64,69,74,79,84,Inf),
                     labels = c("_menor que 1 ano",
                                "01 a 04 anos",
                                "05 a 09 anos",
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
                                "80 a 84 anos",
                                "85 anos ou mais")))




# total de óbitos para DF
aggregate(data=dados_sim,obt~ano, FUN= sum) # problema identificado com criação de NA em 2000

# correção dos NA presente na dados_sim de 2000
dados_sim$ano[which(is.na(dados_sim$ano))]<-2000
# total de óbitos para DF
aggregate(data=dados_sim,obt~ano, FUN= sum) # ok com http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/obt10df.def

# tabela 1 - total de obitos e por sexo
tab1 <-dados_sim  %>%
  group_by(ano) %>%
  summarise(obt = n())
tab1

tab1a<-dados_sim  %>%
  filter(SEXO=="Masculino")%>%
  group_by(ano) %>%
  summarise(obt_masc = n())
tabela1<-merge(tab1,tab1a,by="ano")
rm(tab1,tab1a)
tabela1

tab1b<-dados_sim  %>%
  filter(SEXO=="Feminino")%>%
  group_by(ano) %>%
  summarise(obt_fem = n())
tabela1<-merge(tabela1,tab1b,by="ano")
rm(tab1b)
tabela1

# ajuste com NA
tabela1$obt_masc_ajust<-round(tabela1$obt_masc/(tabela1$obt_masc+tabela1$obt_fem)*tabela1$obt)
tabela1$obt_fem_ajust<-round(tabela1$obt_fem/(tabela1$obt_masc+tabela1$obt_fem)*tabela1$obt)
# verifica se a soma é verdadeira
tabela1$obt_fem_ajust+tabela1$obt_masc_ajust==tabela1$obt
tabela1

# exportando tabelas
library(writexl)
write_xlsx(tabela1,"Tabela  1a - Total de obitos.xlsx")

# produzir gráfico
tabela1<-ts(tabela1,start = 2000,frequency = 1)
library(zoo)
tabela1m<-rollmean(x = tabela1,k = 3)
tabela1m

write_xlsx(as.data.frame(tabela1m),"Tabela  1b - Total de obitos com ma.xlsx")

ggplot(as.data.frame(tabela1m), aes(x=ano, y=obt)) +
  geom_line( color="steelblue") +
  geom_point() +
  xlab("Ano")+
  ylab("Óbitos")+
  ggtitle("Total de obitos - DF - 2000-2020 ")

# leitura dos pacotes utilizados
library(readxl)

# leitura da base
proj_ibge<- read_excel("indicadores_projecoes_DF_IBGE.xlsx")

# alterações para produzir gráficos
proj_ibge<-ts(proj_ibge,start = 2010,frequency = 1)
proj_ibge<-window(proj_ibge,start = 2010,end=2030,frequency = 1)

ggplot(as.data.frame(proj_ibge), aes(x=ano, y=obt)) +
  geom_line( color="steelblue") +
  geom_point() +
  xlab("Ano")+
  ylab("Óbitos")+
  ggtitle("Total de óbitos - DF - 2000-2020 ")

ano1<-2010:2030
var1<-c(round(as.data.frame(tabela1)$obt[11:21]),rep(NA,10))
var2<-c(round(as.data.frame(tabela1m)$obt[10:19]),rep(NA,11))
var3<-as.data.frame(proj_ibge)$obt
data1<-data.frame(ano1,var1,var2,var3)

p1<-ggplot(data1, aes(x=ano1)) +
  geom_line(aes(y=var1, col="SIM"),linetype=1) +
  geom_line(aes(y=var2, col="SIM-média móvel"),linetype=1) +
  geom_line(aes(y=var3, col="Proj. IBGE"),linetype=1) +
  labs(title="Total de óbitos por fonte de dados - DF - 2010-2030", y="Total de óbitos",x=element_blank()) +
  scale_color_manual(name="",
                     values = c("SIM"="coral4",
                                "SIM-média móvel"="coral3",
                                "Proj. IBGE"="darkblue")) +
  # ylim(0,NA)+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks=ano1, labels=ano1)
p1

png(filename = "Total de óbitos por fonte de dados - DF - 2010-2030.png",
    width = 15, height = 8,
    units = "cm",res = 1200)
p1
dev.off()

write_xlsx(data1,"Tabela  1c - Total de óbitos por fontes.xlsx")

# taxa bruta de mortalidade
ano2<-2010:2030
var1<-c(round(as.data.frame(tabela1)$obt[11:21]),rep(NA,10))/
  as.data.frame(proj_ibge)$pop_total*1000
var2<-c(round(as.data.frame(tabela1m)$obt[10:19]),rep(NA,11))/
  as.data.frame(proj_ibge)$pop_total*1000
var3<-as.data.frame(proj_ibge)$tbm
data2<-data.frame(ano2,var1,var2,var3)

p2<-ggplot(data2, aes(x=ano2)) +
  geom_line(aes(y=var1, col="SIM"),linetype=1,size=1.2) +
  geom_line(aes(y=var2, col="SIM-média móvel"),linetype=1,size=1.2) +
  geom_line(aes(y=var3, col="Proj. IBGE"),linetype=1,size=1.2) +
  labs(title="Taxa Bruta de Mortalidade por fonte de dados - DF - 2010-2030",
       y="TBM",x=element_blank()) +
  scale_color_manual(name="",
                     values = c("SIM"="coral4",
                                "SIM-média móvel"="coral3",
                                "Proj. IBGE"="darkblue")) +
  # ylim(0,NA)+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks=ano2, labels=ano2)
p2

png(filename = "Taxa Bruta de Mortalidade por fonte de dados - DF - 2010-2030.png",
    width = 15, height = 8,
    units = "cm",res = 1200)
p2
dev.off()



write_xlsx(data2,"Tabela  1d - Taxa Bruta de Mortalidade por fontes.xlsx")



# total de obitos para DF por faixa etaria
tab2 <- dados_sim %>%
  group_by(ano,faixa_idade) %>%
  summarise(
    obt = n())

tab2a <- dados_sim %>%
  filter(SEXO=="Masculino")%>%
  group_by(ano,faixa_idade) %>%
  summarise(
    obt_h = n())

tabela2<-merge(tab2,tab2a,by=c("ano","faixa_idade"))
rm(tab2,tab2a)

tab2b <- dados_sim %>%
  filter(SEXO=="Feminino")%>%
  group_by(ano,faixa_idade) %>%
  summarise(
    obt_m = n())

tabela2<-merge(tabela2,tab2b,by=c("ano","faixa_idade"))
rm(tab2b)

subset(tabela2,ano==2019) # conferencia ok com http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/obt10df.def
subset(tabela2,ano==2020)

# faz ajuste de NA - faixa da idade
table1<-as.data.frame(tabela1[,c("ano","obt")])
colnames(table1)<-c("ano","obt_total")
tabela2<-merge(tabela2,table1,by=c("ano"))
rm(table1)
tabela2

table2 <- tabela2 %>%
  filter(!is.na(faixa_idade))%>%
  group_by(ano) %>%
  summarise(
    obt_sem_na = sum(obt))

tabela2<-merge(tabela2,table2,by=c("ano"))
tabela2

tabela2$part<-tabela2$obt/tabela2$obt_sem_na
tabela2$obt_ajust<-round(tabela2$part*tabela2$obt_total)

#  arredondou para todos
tabela2 %>%
  filter(!is.na(faixa_idade))%>%
  group_by(ano) %>%
  summarise(
    obt_conf = sum(obt_ajust))

tabela2
tabela2$obt_h_ajust<-round(tabela2$obt_h/(tabela2$obt_h+tabela2$obt_m)*tabela2$obt_ajust)
tabela2$obt_m_ajust<-round(tabela2$obt_m/(tabela2$obt_h+tabela2$obt_m)*tabela2$obt_ajust)
# conferencia
tabela2$obt_h_ajust+tabela2$obt_m_ajust==tabela2$obt_ajust

tabela2

# população geral
# traz dados de população por faixa etária
pop<-read_excel("pop_faixa_etaria.xlsx",sheet="total")
colnames(pop)

library(tidyr)
pop_wide<-as.data.frame(pop)

pop_long <- pop_wide %>% gather(faixa_idade, pop, -c(ano)) 
tabela2<-merge(tabela2,pop_long,by=c("ano","faixa_idade"))
tabela2 


# Taxas Específicas de Mortalidade (TEM) por idade (nMx)
tabela2$tem<-tabela2$obt_ajust/tabela2$pop

#fazendo a média móvel
tabela2[,c("ano","faixa_idade","obt_ajust")]

table2<-spread(tabela2[,c("ano","faixa_idade","obt_ajust")],faixa_idade,obt_ajust)

table2<-ts(table2,start = 2010,frequency = 1)
library(zoo)
library(ggplot2)
table2m<-rollmean(x = table2,k = 3)

table2m<-as.data.frame(table2m) %>% gather(faixa_idade, obt_ajust_m, -c(ano)) # para fazer o merge coloca o nome faixa_idade
tabela2<-merge(tabela2,table2m,by=c("ano","faixa_idade"),all.x = TRUE)


colnames(tabela2)
tabela2$tem_m<-tabela2$obt_ajust_m/tabela2$pop

tabela2$ano<-as.character(tabela2$ano)

data<-subset(tabela2,ano!=2010&ano!=2020)
p4<-ggplot(data, aes(x=faixa_idade, y=log(tem_m),group=ano,color=ano)) +
  geom_line(linetype=1,size=1.2) +
  labs(title="Taxas Específicas de Mortalidade por ano - DF - 2010-2020",
       y="ln(TEM)",x=element_blank()) +
  # ylim(0,NA)+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))


p4

png(filename = "Taxas Específicas de Mortalidade por ano - DF - 2010-2020.png",
    width = 15, height = 12,
    units = "cm",res = 1200)
p4
dev.off()

write_xlsx(tabela2,"Tabela  1e - Taxas Específicas de Mortalidade.xlsx")

# população homens
# traz dados de população por faixa etária
pop_h<-read_excel("pop_faixa_etaria.xlsx",sheet="homens")
colnames(pop_h)

library(tidyr)
pop_h_wide<-as.data.frame(pop_h)

pop_h_long <- pop_h_wide %>% gather(faixa_idade, pop_h, -c(ano)) # para fazer o merge coloca o nome faixa_idade_mae_15_49

tabela2<-merge(tabela2,pop_h_long,by=c("ano","faixa_idade"))

# Taxas Específicas de Mortalidade (TEM) masculina por idade (nMx)
tabela2$tem_h<-tabela2$obt_h_ajust/tabela2$pop_h

#fazendo a média móvel
table2h<-spread(tabela2[,c("ano","faixa_idade","obt_h_ajust")],faixa_idade,obt_h_ajust)

table2h$ano<-as.numeric(table2h$ano)
table2h<-ts(table2h,start = 2010,frequency = 1)
library(zoo)
library(ggplot2)
table2hm<-rollmean(x = table2h,k = 3)
table2hm<-as.data.frame(table2hm)

table2hm<-table2hm %>% gather(faixa_idade, obt_h_ajust_m, -c(ano)) # para fazer o merge coloca o nome faixa_idade
tabela2<-merge(tabela2,table2hm,by=c("ano","faixa_idade"),all.x = TRUE)

colnames(tabela2)
tabela2$tem_h_m<-tabela2$obt_h_ajust_m/tabela2$pop_h

tabela2$ano<-as.character(tabela2$ano)

data<-subset(tabela2,ano!=2010&ano!=2020)
p4<-ggplot(data, aes(x=faixa_idade, y=log(tem_h_m),group=ano,color=ano)) +
  geom_line(linetype=1,size=1.2) +
  labs(title="Taxas Específicas de Mortalidade dos homens por ano - DF - 2010-2020",
       y="ln(TEM)",x=element_blank()) +
  # ylim(0,NA)+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))


p4

png(filename = "Taxas Específicas de Mortalidade dos homens por ano - DF - 2010-2020.png",
    width = 15, height = 12,
    units = "cm",res = 1200)
p4
dev.off()

# população mulheres
library(readxl)
# traz dados de população por faixa etária
pop_m<-read_excel("pop_faixa_etaria.xlsx",sheet="mulheres")
colnames(pop_m)

library(tidyr)
pop_m_wide<-as.data.frame(pop_m)

pop_m_long <- pop_m_wide %>% gather(faixa_idade, pop_m, -c(ano)) # para fazer o merge coloca o nome faixa_idade_mae_15_49

tabela2<-merge(tabela2,pop_m_long,by=c("ano","faixa_idade"))

# Taxas Específicas de Mortalidade (TEM) feminina por idade (nMx)
tabela2$tem_mu<-tabela2$obt_m_ajust/tabela2$pop_m

#fazendo a média móvel
table2<-spread(tabela2[,c("ano","faixa_idade","obt_m_ajust")],faixa_idade,obt_m_ajust)
table2$ano<-as.numeric(table2$ano)

table2<-ts(table2,start = 2010,frequency = 1)
library(zoo)
library(ggplot2)
table2m<-rollmean(x = table2,k = 3)
table2m<-as.data.frame(table2m)


table2m<-table2m %>% gather(faixa_idade, obt_m_ajust_m, -c(ano)) # para fazer o merge coloca o nome faixa_idade
tabela2<-merge(tabela2,table2m,by=c("ano","faixa_idade"),all.x = TRUE)


colnames(tabela2)
tabela2$tem_m_m<-tabela2$obt_m_ajust_m/tabela2$pop_m

tabela2$ano<-as.character(tabela2$ano)

data<-subset(tabela2,ano!=2010&ano!=2020)
p4<-ggplot(data, aes(x=faixa_idade, y=log(tem_m_m),group=ano,color=ano)) +
  geom_line(linetype=1,size=1.2) +
  labs(title="Taxas Específicas de Mortalidade das mulheres por ano - DF - 2010-2020",
       y="ln(TEM)",x=element_blank()) +
  # ylim(0,NA)+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90))


p4

png(filename = "Taxas Específicas de Mortalidade das mulheres por ano - DF - 2010-2020.png",
    width = 15, height = 12,
    units = "cm",res = 1200)
p4
dev.off()


write_xlsx(tabela2,"Tabela  1f - Taxas Específicas de Mortalidade por sexo.xlsx")


