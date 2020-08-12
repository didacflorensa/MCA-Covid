library(readr)
library(openxlsx)
library(plotly)
library(epiDisplay)
library(janitor)

#-------------------Only exitus---------------------#
covid.data.exitus <- read.csv("~/RelacioSimptomes-TOTS-NomesExitus.csv")
covid.data.exitus
data.frame(table(covid.data.exitus$Edat))
data.frame(table(covid.data.exitus$Sexe))

tab1(covid.data.exitus$Sexe)
tab1(covid.data.exitus$Edat)

covid.data <- read.csv("~/RelacioSimptomes-Lleida-01062020-31072020-R-BO.csv")
covid.data

data.frame(table(covid.data$Edat))
data.frame(table(covid.data$Sexe))
data.frame(table(covid.data$Febre))
data.frame(table(covid.data$Tos))
data.frame(table(covid.data$Pneumonia))
data.frame(table(covid.data$Maldecoll))
data.frame(table(covid.data$Calfreds))
data.frame(table(covid.data$Disnea))
table(covid.data$Disnea)


library(janitor)
tabyl(covid.data$Disnea, sort = TRUE)

library(epiDisplay)
tab1(covid.data$Sexe)
tab1(covid.data$Edat)
tab1(covid.data$Ingreshopitalari)
tab1(covid.data$IngresUCI)
tab1(covid.data$Defuncio)

tab1(covid.data$Febre)
tab1(covid.data$Tos)
tab1(covid.data$Pneumonia)
tab1(covid.data$Maldecoll)
tab1(covid.data$Calfreds)
tab1(covid.data$Disnea)
tab1(covid.data$Vòmits)
tab1(covid.data$Diarrea)
tab1(covid.data$SDR)
tab1(covid.data$IRA)


tab1(covid.data$Diabetis)
tab1(covid.data$MalaltiaCardiovascular)
tab1(covid.data$MNeurologica)
tab1(covid.data$MalaltiaPulmonarCrónica)


#-----Chi squared Sexe-------#
dataTable <- matrix(c(1014,2457,1217,3139),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("MAle","Female")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Edat-------#
dataTable <- matrix(c(72,344,362,1744, 291, 1143, 385, 1045, 308, 605, 195, 283, 252, 202, 366, 230),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("0-14","15-34", "35-44", "45-54", "55-64", "65-74", "75-84" ,"85+")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Ingres hospitalari-------#
dataTable <- matrix(c(551,74,1680,5522),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Ingres UCI-------#
dataTable <- matrix(c(60,10,2171,5586),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)


#-----Chi squared Defuncio-------#
dataTable <- matrix(c(134,46,2097,5550),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Febre-------#
dataTable <- matrix(c(1704,1334,527,4252),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Tos-------#
dataTable <- matrix(c(1385,1149,846,4447),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Mal de coll-------#
dataTable <- matrix(c(672,801,1559,4795),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Pneumonia-------#
dataTable <- matrix(c(444,89,1787,5507),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Calfreds-------#
dataTable <- matrix(c(768,548,1463,5048),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Disnea-------#
dataTable <- matrix(c(642,400,1589,5196),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Vomits-------#
dataTable <- matrix(c(155,209,2076,5387),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Diarrea-------#
dataTable <- matrix(c(533,683,1689,4913),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)


#-----Chi squared SDR-------#
dataTable <- matrix(c(49,19,2182,5577),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Insuficiencia renal aguda-------#
dataTable <- matrix(c(26,11,2205,5585),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared Diabetes-------#
dataTable <- matrix(c(286,145,1945,5451),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)


#-----Chi squared M. Cardiovascular-------#
dataTable <- matrix(c(462,87,1769,5099),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared M. Pulmonar-------#
dataTable <- matrix(c(246,121,1985,5475),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)

#-----Chi squared M. Neuralgica-------#
dataTable <- matrix(c(277,0,1954,5596),ncol=2,byrow=TRUE)
colnames(dataTable) <- c("1r outbreak","2n outbreak")
rownames(dataTable) <- c("Yes","No")
dataTable <- as.table(dataTable)
dataTable
chisq.test(dataTable)



#----------------------Age Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Edat)),
              y = as.numeric(table(covid.data$Edat)),
              text = as.numeric(table(covid.data$Edat)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Edat",
              type = "bar") %>% 
  layout(title = "Age distribution",
         xaxis = list(title = "Age groups",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Sexe Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Sexe)),
              y = as.numeric(table(covid.data$Sexe)),
              text = as.numeric(table(covid.data$Sexe)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Sexe",
              type = "bar") %>% 
  layout(title = "Sexe distribution",
         xaxis = list(title = "Sexe groups",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Febre Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Febre)),
              y = as.numeric(table(covid.data$Febre)),
              text = as.numeric(table(covid.data$Febre)),
              textposition = 'auto',
              marker = list(color = 'rgb(135,206,250)',
                            line = list(color = 'rgb(70,130,180)', width = 1.5)),
              name = "Febre",
              type = "bar") %>% 
  layout(title = "Febre distribution",
         xaxis = list(title = "Febre groups",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#----------------------Tos Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Tos)),
              y = as.numeric(table(covid.data$Tos)),
              text = as.numeric(table(covid.data$Tos)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Tos",
              type = "bar") %>% 
  layout(title = "Tos distribution",
         xaxis = list(title = "Tos groups",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Pneumonia Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Pneumonia)),
              y = as.numeric(table(covid.data$Pneumonia)),
              text = as.numeric(table(covid.data$Pneumonia)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Pneumonia",
              type = "bar") %>% 
  layout(title = "Pneumonia distribution",
         xaxis = list(title = "Pneumonia",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#----------------------Mal de coll Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Maldecoll)),
              y = as.numeric(table(covid.data$Maldecoll)),
              text = as.numeric(table(covid.data$Maldecoll)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Maldecoll",
              type = "bar") %>% 
  layout(title = "Maldecoll distribution",
         xaxis = list(title = "Maldecoll",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Calfreds Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Calfreds)),
              y = as.numeric(table(covid.data$Calfreds)),
              text = as.numeric(table(covid.data$Calfreds)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Calfreds",
              type = "bar") %>% 
  layout(title = "Calfreds distribution",
         xaxis = list(title = "Calfreds",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Disnea Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Disnea)),
              y = as.numeric(table(covid.data$Disnea)),
              text = as.numeric(table(covid.data$Disnea)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Disnea",
              type = "bar") %>% 
  layout(title = "Disnea distribution",
         xaxis = list(title = "Disnea",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Vòmits Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Vomits)),
              y = as.numeric(table(covid.data$Vomits)),
              text = as.numeric(table(covid.data$Vomits)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Vòmits",
              type = "bar") %>% 
  layout(title = "Vòmits distribution",
         xaxis = list(title = "Vòmits",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Diarrea Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Diarrea)),
              y = as.numeric(table(covid.data$Diarrea)),
              text = as.numeric(table(covid.data$Diarrea)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Diarrea",
              type = "bar") %>% 
  layout(title = "Diarrea distribution",
         xaxis = list(title = "Diarrea",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#----------------------SDR Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$SDR)),
              y = as.numeric(table(covid.data$SDR)),
              text = as.numeric(table(covid.data$SDR)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "SDR",
              type = "bar") %>% 
  layout(title = "Sindrome Distres Respiratori distribution",
         xaxis = list(title = "SDR",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#----------------------IRA Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$InsuficienciaRenalAguda)),
              y = as.numeric(table(covid.data$InsuficienciaRenalAguda)),
              text = as.numeric(table(covid.data$InsuficienciaRenalAguda)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Insuficiència Renal Aguda",
              type = "bar") %>% 
  layout(title = "InsuficienciaRenalAguda distribution",
         xaxis = list(title = "InsuficienciaRenalAguda",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#----------------------Diabetis Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Diabetis)),
              y = as.numeric(table(covid.data$Diabetis)),
              text = as.numeric(table(covid.data$Diabetis)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Diabetis",
              type = "bar") %>% 
  layout(title = "Diabetis distribution",
         xaxis = list(title = "Diabetis",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#----------------------Malaltia Cardiovascular Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$MalaltiaCardiovascular)),
              y = as.numeric(table(covid.data$MalaltiaCardiovascular)),
              text = as.numeric(table(covid.data$MalaltiaCardiovascular)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "MalaltiaCardiovascular",
              type = "bar") %>% 
  layout(title = "Malaltia Cardiovascular distribution",
         xaxis = list(title = "MalaltiaCardiovascular",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#----------------------Hepatopatia Cronica Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Hepatopaticacronica)),
              y = as.numeric(table(covid.data$Hepatopaticacronica)),
              text = as.numeric(table(covid.data$Hepatopaticacronica)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Hepatopatiacronica",
              type = "bar") %>% 
  layout(title = "Hepatopatia Cronica distribution",
         xaxis = list(title = "Hepatopatiacronica",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#---------------------Malaltia Neurologica Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Mneurologica)),
              y = as.numeric(table(covid.data$Mneurologica)),
              text = as.numeric(table(covid.data$Mneurologica)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "MNeurologica",
              type = "bar") %>% 
  layout(title = "Malaltia Neurologia distribution",
         xaxis = list(title = "MNeurologica",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1




#---------------------Malaltia Pulmonar Crònica Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$MalaltiaPulmonarCrónica)),
              y = as.numeric(table(covid.data$MalaltiaPulmonarCrónica)),
              text = as.numeric(table(covid.data$MalaltiaPulmonarCrónica)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "MalaltiaPulmonarCrónica",
              type = "bar") %>% 
  layout(title = "Malaltia Pulmonar Crònica distribution",
         xaxis = list(title = "MalaltiaPulmonarCrónica",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#---------------------Immunodeficiència Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Immunodeficiencia)),
              y = as.numeric(table(covid.data$Immunodeficiencia)),
              text = as.numeric(table(covid.data$Immunodeficiencia)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Immunodeficiencia",
              type = "bar") %>% 
  layout(title = "Immunodeficiencia distribution",
         xaxis = list(title = "Immunodeficiencia",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1



#---------------------Ingrés Hospitalari Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Ingreshopitalari)),
              y = as.numeric(table(covid.data$Ingreshopitalari)),
              text = as.numeric(table(covid.data$Ingreshopitalari)),
              textposition = 'auto',
              marker = list(color = 'rgb(135,206,250)',
                            line = list(color = 'rgb(70,130,180)', width = 1.5)),
              name = "Ingrés hospitalari",
              type = "bar") %>% 
  layout(title = "Ingrés hospitalari distribution",
         xaxis = list(title = "Ingrés hospitalari",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#---------------------Ingrés UCI Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$IngresUCI)),
              y = as.numeric(table(covid.data$IngresUCI)),
              text = as.numeric(table(covid.data$IngresUCI)),
              textposition = 'auto',
              marker = list(color = 'rgb(135,206,250)',
                            line = list(color = 'rgb(70,130,180)', width = 1.5)),
              name = "Ingrés a la UCI",
              type = "bar") %>% 
  layout(title = " Ingrés a la UCI distribution",
         xaxis = list(title = "Ingrés a la UCI",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1


#---------------------Defuncio UCI Distribution----------------------#
p1 <- plot_ly(x = names(table(covid.data$Defuncio)),
              y = as.numeric(table(covid.data$Defuncio)),
              text = as.numeric(table(covid.data$Defuncio)),
              textposition = 'auto',
              marker = list(color = 'rgb(0,204,102)',
                            line = list(color = 'rgb(0,153,0)', width = 1.5)),
              name = "Defuncio",
              type = "bar") %>% 
  layout(title = " Defuncio distribution",
         xaxis = list(title = "Defuncio",
                      zeroline = FALSE),
         yaxis = list(title = "Frequency",
                      zeroline = FALSE))
p1

