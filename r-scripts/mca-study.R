#' MCA Study
#'
#' Study to understand the relations between COVID19 symptoms
#' areas with the cancer.
#'
#' The Multiple correspondence analysis (MCA) is an extension of the simple correspondence analysis (chapter @ref(correspondence-analysis)) 
# for summarizing and visualizing a data table containing more than two categorical variables. It can also be seen as a generalization of 
# principal component analysis when the variables to be analyzed are categorical instead of quantitative (Abdi and Williams 2010).
#'


library("FactoMineR")
library("factoextra")
library('ggplot2')
library(readr)
library(plotly)


#' @details 
#' This CSV is cleaned and contains the following columns AGE_GROUP, POPULATION, CANCER and GENDER
#' * AGE is a numerical variable.
#' * GENDER is a categorical variable with includes a set with the names of the most important cancers.
#' * FEBRE is a categorical variable with YES or NO.
#' * TOS is a categorical variable with YES or NO.
#' * PNEUMONIA is a categorical variable with YES or NO.
#' * MAL DE COLL is a categorical variable with YES or NO.
#' * CALFREDS is a categorical variable with YES or NO.
#' * DISNEA is a categorical variable with YES or NO.
#' * VÒMITS is a categorical variable with YES or NO.
#' * DIARREA is a categorical variable with YES or NO.
#' * SDR is a categorical variable with YES or NO.
#' * INSUFICIENCIA RENAL AGUDA is a categorical variable with YES or NO.
#' * FACTORS DE RISC is a categorical variable with YES or NO.
#' * DIABETIS is a categorical variable with YES or NO.
#' * MALALTIA CARDIOVASCULAR is a categorical variable with YES or NO.
#' * HEPATOPATIA CRONICA is a categorical variable with YES or NO.
#' * MALALTIA NEUROLOGICA is a categorical variable with YES or NO.
#' * IMMUNODEFICIENCIAis a categorical variable with YES or NO.
#' * CANCER is a categorical variable with YES or NO.


covid.data=read_csv("/Users/Didac/OneDrive - Generalitat de Catalunya/COVID19/COVID_SIMPTOMES_ESTUDI/MCA-Covid/data/private/RelacioSimptomes-Cleaning-AgeString-FebreY.csv")
covid.data
summary(covid.data)

covid.data.analisis = covid.data[1:20]
#summary(covid.data.analisis$Edat)
covid.data.analisis$Factorrisc <- NULL
covid.data.analisis$SDR <- NULL
covid.data.analisis$Hepatopatiacronica <- NULL
covid.data.analisis$InsuficienciaRenalAguda <- NULL
covid.data.analisis$Immunodeficiencia <- NULL
covid.data.analisis$Cancer <- NULL

#NULL PROVISIONAL
#covid.data.analisis$Diabetis <- NULL
#covid.data.analisis$MalaltiaCardiovascular <- NULL
#covid.data.analisis$MNeurologica <- NULL
#covid.data.analisis$MalaltiaPulmonarCrónica <- NULL

#Simptomes provisionals NULL
covid.data.analisis$Febre <- NULL
#covid.data.analisis$Disnea <- NULL
#covid.data.analisis$Vòmits <- NULL
#covid.data.analisis$Diarrea <- NULL

#
covid.data.analisis$Sexe <- NULL



a <- table(covid.data.analisis$Edat)
a
as.data.frame(a)





head(covid.data.analisis)

summary(covid.data.analisis)

res.mca1 <- MCA(covid.data.analisis, graph = FALSE)
fviz_screeplot(res.mca1, addlabels = TRUE)
fviz_mca_var(res.mca1, choice = "mca.cor", 
             repel = TRUE, 
             ggtheme = theme_minimal())


fviz_mca_var(res.mca1, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


# Color by cos2 values: quality on the factor map
fviz_mca_var(res.mca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

fviz_mca_var(res.mca1, choice = "mca.cor",
             repel = TRUE)


# Cos2 of variable categories on Dim.1 and Dim.2
fviz_cos2(res.mca1, choice = "var", axes = 1:2)





boxplot_datacleaning <- function(cancer.data, filtre) {
  head(cancer.data)
  df1 <- filter(cancer.data, Cancer == "cancer_y")
  df2 <- filter(cancer.data, Cancer == "cancer_n")
  
  y_title <- list(
    title = "age"
  )
  
  p <- plot_ly(df1, x = df1[["Cancer"]], y = df1[["Edat"]], type  = "box", name = 'Yes')%>%
    add_trace(df2, x = df2[["Cancer"]], y = df2[["Edat"]], name = 'No') %>%
    layout(yaxis = y_title) %>%
    layout(title = paste("Cancer by gender -", filtre))
  p
}
