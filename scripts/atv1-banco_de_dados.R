

###Atividade 01

library(tidyverse)
library(dplyr)
library(lubridate)

#################### Importação e organização tabela Victor ################

tab_VC <- read.csv("data/atividade1_Vitor_Figueira_Arueira.csv", sep = ";")

## Renomeado as colunas
names(tab_VC)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_VC)
tab_VC2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_VC2)
################################ FIM #######################################



################### Importação e organização tabela Lorrana ################


tab_Lorr <- read.csv("data/Atividade1_Lorrana.csv", sep = ";")

## Renomeado as colunas
names(tab_VC)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_Lorr)
tab_Lorr2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_Lorr2)
################################ FIM #######################################


#################### Importação e organização tabela Marina ################

tab_MA <- read.csv("data/atividade1_MARINA.csv", sep = ";")

## Renomeado as colunas
names(tab_MA)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_MA)
tab_MA2 = cbind(amostra, spp, data,site,latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_MA2)
################################ FIM #######################################


################ Importação e organização tabela Henrique ##################

tab_HS <- read.csv("data/atividade1_HenriqueSimfrone.csv", sep = ";")
## Renomeado as colunas
names(tab_HS)[1:10] <- c("amostra", "spp", "tamanho_sepala","largura_sepala", "tamanho_petala", "largura_petala", "site", "longitude", "latitude","data")

## Reordenando as colunas
attach(tab_HS)
tab_HS2 = cbind(amostra, spp, data,site,latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

view(tab_HS2)

#################################### FIM ###################################


################ Importação e organização tabela Mariana  ##################

tab_MB <- read.csv("data/atividade1_MARIANA-BURATO.csv", sep = ";")

## Renomeado as colunas
names(tab_MB)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

## Reordenando as colunas
attach(tab_MB)
tab_MB2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

#################################### FIM ###################################

################ Importação e organização tabela Jonatha  ##################

tab_JR <- read.csv("data/atividade1_JonathaR.csv", sep = ";")

## Renomeado as colunas
names(tab_JR)[1:10] <- c("amostra", "spp", "tamanho_sepala", "largura_sepala", "tamanho_petala", "largura_petala", "site","latitude", "longitude", "data")

## Reordenando as colunas
attach(tab_JR)
tab_JR2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

#################################### FIM ###################################



#### Unindo todas as tabelas em um único arquivo
dados <- rbind(tab_HS2, tab_Lorr2, tab_MA2, tab_MB2, tab_VC2, tab_JR2)

#### Transformando em data.frame
dados <- as.data.frame(dados)


#### Padronizando os nomes de cada espécie e o local

dados$spp[dados$spp == "IRIS_VERSICOLOR"] <- "Iris versicolor"
dados$spp[dados$spp == "IRIS_VIRGINICA"] <- "Iris virginica"
dados$spp[dados$spp == "IRIS_SETOSA"] <- "Iris setosa"
dados$spp[dados$spp == "iris_versicolor"] <- "Iris versicolor"
dados$spp[dados$spp == "iris_virginica"] <- "Iris virginica"
dados$spp[dados$spp == "iris_setosa"] <- "Iris setosa"
dados$spp[dados$spp == "Iris_versicolor"] <- "Iris versicolor"
dados$spp[dados$spp == "Iris_virginica"] <- "Iris virginica"
dados$spp[dados$spp == "Iris_setosa"] <- "Iris setosa"


dados$site[dados$site == "S3"] <- "Site3"
dados$site[dados$site == "S2"] <- "Site2"
dados$site[dados$site == "S1"] <- "Site1"
dados$site[dados$site == "site3"] <- "Site3"
dados$site[dados$site == "site2"] <- "Site2"
dados$site[dados$site == "site1"] <- "Site1"


#### Padronizando o formato das datas

dados$data[dados$data == "01/12/1929"] <- "01_12_1929"
dados$data[dados$data == "13/02/1930"] <- "13_02_1930"
dados$data[dados$data == "1929_12_01"] <- "01_12_1929"
dados$data[dados$data == "1930_02_13"] <- "13_02_1930"

#### Convertendo a coluna data com classe "factor" para "date"; padronizando o formato para YYY-MM-DD

dados$data <- dmy(dados$data)
is.Date(dados$data)
class(dados$data)

#### Ordenando a tabela pela coluna "spp"


dados<-dados[order(dados$spp),]

View(dados)

#### Exportando o arquivo em formato .csv

write.csv2(dados, "dados_unidos.csv")
