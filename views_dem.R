###Conversion of databases###
library(haven)
WVSBR2006 <- read_sav("WVSBR2006.sav")
WVSBR2014 <- read_sav("WVSBR2014.sav")
WVSBR2018 <- read_sav("WVSBR2018.sav")

###Removal of labels###
library(sjlabelled)
WVSBR2006 <- remove_all_labels(WVSBR2006)
WVSBR2014 <- remove_all_labels(WVSBR2014)
WVSBR2018 <- remove_all_labels(WVSBR2018)

### Dependent Variables###
##2006##
# Social View
WVSBR2006$Soc_Dem1 <- WVSBR2006$V152
WVSBR2006$Soc_Dem2 <- WVSBR2006$V155
WVSBR2006$Soc_Dem <- WVSBR2006$Soc_Dem1+WVSBR2006$Soc_Dem2
WVSBR2006$Soc_Dem01 <- 
  WVSBR2006$Soc_Dem/max(WVSBR2006$Soc_Dem, na.rm = TRUE) 
WVSBR2006$Soc_Dem10 <- WVSBR2006$Soc_Dem01*10
# Liberal View
WVSBR2006$Lib_Dem1 <- WVSBR2006$V154
WVSBR2006$Lib_Dem2 <- WVSBR2006$V157
WVSBR2006$Lib_Dem3 <- WVSBR2006$V161
WVSBR2006$Lib_Dem <- 
  WVSBR2006$Lib_Dem1+WVSBR2006$Lib_Dem2+WVSBR2006$Lib_Dem3
WVSBR2006$Lib_Dem01 <- 
  WVSBR2006$Lib_Dem/max(WVSBR2006$Lib_Dem, na.rm = TRUE) 
WVSBR2006$Lib_Dem10 <- WVSBR2006$Lib_Dem01*10
# Participacionist View
library(memisc)
WVSBR2006$Part_Dem1 <- recode(WVSBR2006$V69, 2 <- 3, 0 <- c(1,2,4))
WVSBR2006$Part_Dem2 <- recode(WVSBR2006$V70, 1 <- 3, 0 <- c(1,2,4))
WVSBR2006$Part_Dem3 <- recode(WVSBR2006$V71, 2 <- 2, 0 <- c(1,3,4))
WVSBR2006$Part_Dem4 <- recode(WVSBR2006$V72, 1 <- 2, 0 <- c(1,3,4))
WVSBR2006$Part_Dem <- 
  WVSBR2006$Part_Dem1+WVSBR2006$Part_Dem2+WVSBR2006$Part_Dem3+WVSBR2006$Part_Dem4
WVSBR2006$Part_Dem01 <- 
  WVSBR2006$Part_Dem/max(WVSBR2006$Part_Dem, na.rm = TRUE) 
WVSBR2006$Part_Dem10 <- WVSBR2006$Part_Dem01*10
# Iliberal View
WVSBR2006$Ilib_Dem10 <- WVSBR2006$V156
# Firm Liberal View 
WVSBR2006<- WVSBR2006%>% 
  mutate(LibConvB = case_when((Lib_Dem10 >= 8 & Ilib_Dem10 <= 2) ~ 1,
                              (Lib_Dem10 <= 8 & Ilib_Dem10 >= 2) ~ 0,
                              TRUE ~ 0))

##2014
# Social View
WVSBR2014$Soc_Dem1 <- WVSBR2014$V131
WVSBR2014$Soc_Dem2 <- WVSBR2014$V134
WVSBR2014$Soc_Dem <- WVSBR2014$Soc_Dem1+WVSBR2014$Soc_Dem2
WVSBR2014$Soc_Dem01 <- 
  WVSBR2014$Soc_Dem/max(WVSBR2014$Soc_Dem, na.rm = TRUE) 
WVSBR2014$Soc_Dem10 <- WVSBR2014$Soc_Dem01*10
#Liberal View
WVSBR2014$Lib_Dem1 <- WVSBR2014$V133
WVSBR2014$Lib_Dem2 <- WVSBR2014$V136
WVSBR2014$Lib_Dem3 <- WVSBR2014$V139
WVSBR2014$Lib_Dem <- 
  WVSBR2014$Lib_Dem1+WVSBR2014$Lib_Dem2+WVSBR2014$Lib_Dem3
WVSBR2014$Lib_Dem01 <- 
  WVSBR2014$Lib_Dem/max(WVSBR2014$Lib_Dem, na.rm = TRUE) 
WVSBR2014$Lib_Dem10 <- WVSBR2014$Lib_Dem01*10
#Participacionist View
WVSBR2014$Part_Dem1 <- recode(WVSBR2014$V60, 2 <- 3, 0 <- c(1,2,4))
WVSBR2014$Part_Dem2 <- recode(WVSBR2014$V61, 1 <- 3, 0 <- c(1,2,4))
WVSBR2014$Part_Dem3 <- recode(WVSBR2014$V62, 2 <- 2, 0 <- c(1,3,4))
WVSBR2014$Part_Dem4 <- recode(WVSBR2014$V63, 1 <- 2, 0 <- c(1,3,4))
WVSBR2014$Part_Dem <- 
  WVSBR2014$Part_Dem1+WVSBR2014$Part_Dem2+WVSBR2014$Part_Dem3+WVSBR2014$Part_Dem4
WVSBR2014$Part_Dem01 <- 
  WVSBR2014$Part_Dem/max(WVSBR2014$Part_Dem, na.rm = TRUE) 
WVSBR2014$Part_Dem10 <- WVSBR2014$Part_Dem01*10
#Iliberal View
WVSBR2014$Ilib_Dem10 <- WVSBR2014$V135

#Firm Liberal View
WVSBR2014<- WVSBR2014%>% 
  mutate(LibConvB = case_when((Lib_Dem10 >= 8 & Ilib_Dem10 <= 2) ~ 1,
                              (Lib_Dem10 <= 8 & Ilib_Dem10 >= 2) ~ 0,
                              TRUE ~ 0))

##2018
# Social View
WVSBR2018$Soc_Dem1 <- WVSBR2018$Q241
WVSBR2018$Soc_Dem2 <- WVSBR2018$Q244
WVSBR2018$Soc_Dem <- WVSBR2018$Soc_Dem1+WVSBR2018$Soc_Dem2
WVSBR2018$Soc_Dem01 <- 
  WVSBR2018$Soc_Dem/max(WVSBR2018$Soc_Dem, na.rm = TRUE) 
WVSBR2018$Soc_Dem10 <- WVSBR2018$Soc_Dem01*10
#Liberal View
WVSBR2018$Lib_Dem1 <- WVSBR2018$Q243
WVSBR2018$Lib_Dem2 <- WVSBR2018$Q246
WVSBR2018$Lib_Dem3 <- WVSBR2018$Q249
WVSBR2018$Lib_Dem <- 
  WVSBR2018$Lib_Dem1+WVSBR2018$Lib_Dem2+WVSBR2018$Lib_Dem3
WVSBR2018$Lib_Dem01 <-
  WVSBR2018$Lib_Dem/max(WVSBR2018$Lib_Dem, na.rm = TRUE) 
WVSBR2018$Lib_Dem10 <- WVSBR2018$Lib_Dem01*10
#Participacionist View
WVSBR2018$Part_Dem1 <- recode(WVSBR2018$Q152, 2 <- 3, 0 <- c(1,2,4))
WVSBR2018$Part_Dem2 <- recode(WVSBR2018$Q153, 1 <- 3, 0 <- c(1,2,4))
WVSBR2018$Part_Dem3 <- recode(WVSBR2018$Q154, 2 <- 2, 0 <- c(1,3,4))
WVSBR2018$Part_Dem4 <- recode(WVSBR2018$Q155, 1 <- 2, 0 <- c(1,3,4))
WVSBR2018$Part_Dem <-
  WVSBR2018$Part_Dem1+WVSBR2018$Part_Dem2+WVSBR2018$Part_Dem3+WVSBR2018$Part_Dem4
WVSBR2018$Part_Dem01 <- 
  WVSBR2018$Part_Dem/max(WVSBR2018$Part_Dem, na.rm = TRUE) 
WVSBR2018$Part_Dem10 <- WVSBR2018$Part_Dem01*10
#Iliberal View
WVSBR2018$Ilib_Dem10 <- WVSBR2018$Q245
#Firm Liberal View
WVSBR2018<- WVSBR2018%>% 
  mutate(LibConvB = case_when((Lib_Dem10 >= 8 & Ilib_Dem10 <= 2) ~ 1,
                              (Lib_Dem10 <= 8 & Ilib_Dem10 >= 2) ~ 0,
                              TRUE ~ 0))

###Independent Variables
#Age
WVSBR2006$Idade <- WVSBR2006$V237
WVSBR2014$Idade <- WVSBR2014$V242
WVSBR2018$Idade <- WVSBR2018$Q262
#Education
WVSBR2006$Ed <- WVSBR2006$V238
WVSBR2014$Ed <- WVSBR2014$V248
WVSBR2018$Ed <- WVSBR2018$Q275
#Gender
WVSBR2006$Sexo <- memisc::recode(WVSBR2006$V235, 1 <- 1, 0 <- 2)
WVSBR2014$Sexo <- memisc::recode(WVSBR2014$V240, 1 <- 2, 0 <- 1)
WVSBR2018$Sexo <- memisc::recode(WVSBR2018$Q260, 1 <- 1, 0 <- 2)
#Unemployed
WVSBR2006$Desemp <- memisc::recode(WVSBR2006$V241, 1 <- 7, 0 <- c(1:6, 8))
WVSBR2014$Desemp <- memisc::recode(WVSBR2014$V229, 1 <- 2, 0 <- c(1, 3:8))
WVSBR2018$Desemp <- memisc::recode(WVSBR2018$Q279, 1 <- 7, 0 <- c(1:6, 8))
#Subjective Social Status
WVSBR2006$ClasSub <- memisc::recode(WVSBR2006$V252, 0 <- 5, 1 <- 4, 2 <- 3, 
                                    3 <- 2, 4 <- 1)
WVSBR2014$ClasSub <- memisc::recode(WVSBR2014$V238, 0 <- 5, 1 <- 4, 2 <- 3, 
                                    3 <- 2, 4 <- 1)
WVSBR2018$ClasSub <- memisc::recode(WVSBR2018$Q287, 0 <- 5, 1 <- 4, 2 <- 3, 
                                    3 <- 2, 4 <- 1)
#Interpersonal Confidence
WVSBR2006$ConfInt <- memisc::recode(WVSBR2006$V126, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2014$ConfInt <- memisc::recode(WVSBR2014$V103, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2018$ConfInt <- memisc::recode(WVSBR2018$Q59, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Political Confidence (Federal Gov., Parties e Congr.)
WVSBR2006$ConfPol_Gov <- memisc::recode(WVSBR2006$V138,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2006$ConfPol_Part <- memisc::recode(WVSBR2006$V139,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2006$ConfPol_Cong <- memisc::recode(WVSBR2006$V140,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2006$ConfPol <- 
  WVSBR2006$ConfPol_Gov+WVSBR2006$ConfPol_Part+WVSBR2006$ConfPol_Cong
WVSBR2014$ConfPol_Gov <- memisc::recode(WVSBR2014$V115,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2014$ConfPol_Part <- memisc::recode(WVSBR2014$V116,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2014$ConfPol_Cong <- memisc::recode(WVSBR2014$V117,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2014$ConfPol <- 
  WVSBR2014$ConfPol_Gov+WVSBR2014$ConfPol_Part+WVSBR2014$ConfPol_Cong
WVSBR2018$ConfPol_Gov <- memisc::recode(WVSBR2018$Q71,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2018$ConfPol_Part <- memisc::recode(WVSBR2018$Q72,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2018$ConfPol_Cong <- memisc::recode(WVSBR2018$Q73, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2018$ConfPol <- 
  WVSBR2018$ConfPol_Gov+WVSBR2018$ConfPol_Part+WVSBR2018$ConfPol_Cong
#Ideology
WVSBR2006$Ideol <- WVSBR2006$V114
WVSBR2014$Ideol <- WVSBR2014$V95
WVSBR2018$Ideol <- WVSBR2018$Q240
#Extremismo Ideológico (0=centro, 1=extrema esq e 2=extrema direita)
WVSBR2006$ExtrIdeol <- recode(WVSBR2006$V114, 0 <- c(3:8), 1 <- c(1,2), 2 <- c(9,10))
WVSBR2006$ExtrIdeol <- factor(WVSBR2006$ExtrIdeol2, 
                              labels = c("moderado", "extremista de esquerda", 
                                         "extremista de direita"))
WVSBR2014$ExtrIdeol <- recode(WVSBR2014$V95, 0 <- c(3:8), 1 <- c(1,2), 2 <- c(9,10))
WVSBR2014$ExtrIdeol <- factor(WVSBR2014$ExtrIdeol2, 
                              labels = c("moderado", "extremista de esquerda", 
                                         "extremista de direita"))
WVSBR2018$ExtrIdeol <- recode(WVSBR2018$Q240, 0 <- c(3:8), 1 <- c(1,2), 2 <- c(9,10))
WVSBR2018$ExtrIdeol <- factor(WVSBR2018$ExtrIdeol2, 
                              labels = c("moderado", "extremista de esquerda", 
                                         "extremista de direita"))
#Interest in politics
WVSBR2006$Int <- memisc::recode(WVSBR2006$V95, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2014$Int <- memisc::recode(WVSBR2014$V84, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2018$Int <- memisc::recode(WVSBR2018$Q199, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Economic Attitudes
WVSBR2006$Econ <- WVSBR2006$V118
WVSBR2014$Econ <- WVSBR2014$V98
WVSBR2018$Econ <- WVSBR2018$Q108
#Cultural Attitudes
WVSBR2006$Cult <- WVSBR2006$V202+WVSBR2006$V203+WVSBR2006$V204+
  WVSBR2006$V205
WVSBR2006$Cult01 <- WVSBR2006$Cult/max(WVSBR2006$Cult, na.rm=TRUE) 
WVSBR2006$Cult10 <- WVSBR2006$Cult01*10
WVSBR2014$Cult <- WVSBR2014$V203+WVSBR2014$V203A+WVSBR2014$V204+
  WVSBR2014$V205
WVSBR2014$Cult01 <- WVSBR2014$Cult/max(WVSBR2014$Cult, na.rm=TRUE) 
WVSBR2014$Cult10 <- WVSBR2014$Cult01*10
WVSBR2018$Cult <- WVSBR2018$Q182+WVSBR2018$Q183+WVSBR2018$Q184+
  WVSBR2018$Q185
WVSBR2018$Cult01 <- WVSBR2018$Cult/max(WVSBR2018$Cult, na.rm=TRUE) 
WVSBR2018$Cult10 <- WVSBR2018$Cult01*10

#####Models
## Social View
#2006
Mod.SocDem2006a <- lm(Soc_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + Ideol + Int + Econ +
                        Cult10, data = WVSBR2006)
#2014
Mod.SocDem2014a <- lm(Soc_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + Ideol + Int + Econ +
                        Cult10, data = WVSBR2014)
#2018
Mod.SocDem2018a <- lm(Soc_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + Ideol + Int + Econ +
                        Cult10, data = WVSBR2018)


tab_model(Mod.SocDem2006a, Mod.SocDem2014a, Mod.SocDem2018a, 
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Ideology",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")

#2006
Mod.SocDem2006b <- lm(Soc_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                        Cult10, data = WVSBR2006)
#2014
Mod.SocDem2014b <- lm(Soc_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                        Cult10, data = WVSBR2014)
#2018
Mod.SocDem2018b <- lm(Soc_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                        Cult10, data = WVSBR2018)


tab_model(Mod.SocDem2006b, Mod.SocDem2014b, Mod.SocDem2018b, 
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Right-wing extremism", "Left-wing extremism",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")


#Liberal
Mod.LibDem2006a <- lm(Lib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + Ideol + Int + Econ +
                        Cult10, data = WVSBR2006)
Mod.LibDem2014a <- lm(Lib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + Ideol + Int + Econ +
                        Cult10, data = WVSBR2014)
Mod.LibDem2018a <- lm(Lib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + Ideol + Int + Econ +
                        Cult10, data = WVSBR2018)

tab_model(Mod.LibDem2006a, Mod.LibDem2014a, Mod.LibDem2018a,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Ideology",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")

Mod.LibDem2006b <- lm(Lib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                        Cult10, data = WVSBR2006)
Mod.LibDem2014b <- lm(Lib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                        Cult10, data = WVSBR2014)
Mod.LibDem2018b <- lm(Lib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                        Cult10, data = WVSBR2018)

tab_model(Mod.LibDem2006b, Mod.LibDem2014b, Mod.LibDem2018b,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Right-wing extremism","Left-wing extremism",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")

#Participacionist
Mod.PartDem2006a <- lm(Part_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + Ideol + Int + Econ +
                         Cult10, data = WVSBR2006)
Mod.PartDem2014a <- lm(Part_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + Ideol + Int + Econ +
                         Cult10, data = WVSBR2014)
Mod.PartDem2018a <- lm(Part_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + Ideol + Int + Econ +
                         Cult10, data = WVSBR2018)

tab_model(Mod.PartDem2006a, Mod.PartDem2014a, Mod.PartDem2018a,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Ideology",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")


Mod.PartDem2006b <- lm(Part_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                         Cult10, data = WVSBR2006)
Mod.PartDem2014b <- lm(Part_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                         Cult10, data = WVSBR2014)
Mod.PartDem2018b <- lm(Part_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                         Cult10, data = WVSBR2018)

tab_model(Mod.PartDem2006b, Mod.PartDem2014b, Mod.PartDem2018b,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Right-wing extremism","Left-wing extremism",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")
#Iliberal
Mod.IlibDem2006a <- lm(Ilib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + Ideol + Int + Econ +
                         Cult10, data = WVSBR2006)
Mod.IlibDem2014a <- lm(Ilib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + Ideol + Int + Econ +
                         Cult10, data = WVSBR2014)
Mod.IlibDem2018a <- lm(Ilib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + Ideol + Int + Econ +
                         Cult10, data = WVSBR2018)

tab_model(Mod.IlibDem2006a, Mod.IlibDem2014a, Mod.IlibDem2018a,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Ideology",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")

Mod.IlibDem2006b <- lm(Ilib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                         Cult10, data = WVSBR2006)
Mod.IlibDem2014b <- lm(Ilib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                         Cult10, data = WVSBR2014)
Mod.IlibDem2018b <- lm(Ilib_Dem10 ~ Idade + Ed + Sexo + Desemp + ClasSub +
                         ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                         Cult10, data = WVSBR2018)

tab_model(Mod.IlibDem2006b, Mod.IlibDem2014b, Mod.IlibDem2018b,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Right-wing extremism","Left-wing extremism",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")

#Firm Liberal
Mod.LibConvBDem2006a <- glm(LibConvB ~ Idade + Ed + Sexo + Desemp + ClasSub +
                              ConfInt + ConfPol + Ideol + Int + Econ +
                              Cult10, data = WVSBR2006, family = binomial(link = logit))
Mod.LibConvBDem2014a <- glm(LibConvB ~ Idade + Ed + Sexo + Desemp + ClasSub +
                              ConfInt + ConfPol + Ideol + Int + Econ +
                              Cult10, data = WVSBR2014, family = binomial(link = logit))
Mod.LibConvBDem2018a <- glm(LibConvB ~ Idade + Ed + Sexo + Desemp + ClasSub +
                              ConfInt + ConfPol + Ideol + Int + Econ +
                              Cult10, data = WVSBR2018, family = binomial(link = logit))

tab_model(Mod.LibConvBDem2006a, Mod.LibConvBDem2014a, Mod.LibConvBDem2018a,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Ideology",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")

Mod.LibConvBDem2006b <- glm(LibConvB ~ Idade + Ed + Sexo + Desemp + ClasSub +
                              ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                              Cult10, data = WVSBR2006, family = binomial(link = logit))
Mod.LibConvBDem2014b <- glm(LibConvB ~ Idade + Ed + Sexo + Desemp + ClasSub +
                              ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                              Cult10, data = WVSBR2014, family = binomial(link = logit))
Mod.LibConvBDem2018b <- glm(LibConvB ~ Idade + Ed + Sexo + Desemp + ClasSub +
                              ConfInt + ConfPol + ExtrIdeol + Int + Econ +
                              Cult10, data = WVSBR2018, family = binomial(link = logit))

tab_model(Mod.LibConvBDem2006b, Mod.LibConvBDem2014b, Mod.LibConvBDem2018b,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercept", "Age", "Education", "Gender",
                          "Unemployed", "Subjective Social Status", 
                          "Interpersonal Confidence", "Political Confidence",
                          "Right-wing extremism","Left-wing extremism",
                          "Interest in politics", "Economic Attitude", 
                          "Cultural Attitude"), string.pred = "Preditors")

#Regression plots
library(ggplot2)
library(ggsci)
library(ggprism)
library(jtools)

plotSocDema <- plot_summs(Mod.SocDem2006a, Mod.SocDem2014a, Mod.SocDem2018a, 
                          model.names = c("2006", "2014", "2018"), legend.title = "Anos",
                          inner_ci_level = .9,
                          point.shape = FALSE)

plotSocDema + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Ideology",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))

plotSocDemb <- plot_summs(Mod.SocDem2006b, Mod.SocDem2014b, Mod.SocDem2018b, 
                          model.names = c("2006", "2014", "2018"), legend.title = "Anos",
                          inner_ci_level = .9,
                          point.shape = FALSE)

plotSocDemb + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))

plotLibDema <- plot_summs(Mod.LibDem2006a, Mod.LibDem2014a, Mod.LibDem2018a, 
                          model.names = c("2006", "2014", "2018"),
                          legend.title = "Anos",
                          inner_ci_level = .9,
                          point.shape = FALSE)

plotLibDema + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Ideology",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))

plotLibDemb <- plot_summs(Mod.LibDem2006b, Mod.LibDem2014b, Mod.LibDem2018b, 
                          model.names = c("2006", "2014", "2018"),
                          legend.title = "Anos",
                          inner_ci_level = .9,
                          point.shape = FALSE)

plotLibDemb + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))

plotPartDema <- plot_summs(Mod.PartDem2006a, Mod.PartDem2014a, Mod.PartDem2018a, 
                           model.names = c("2006", "2014", "2018"),
                           legend.title = "Anos",
                           inner_ci_level = .9,
                           point.shape = FALSE)

plotPartDema + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Ideology",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))


plotPartDemb <- plot_summs(Mod.PartDem2006b, Mod.PartDem2014b, Mod.PartDem2018b, 
                           model.names = c("2006", "2014", "2018"),
                           legend.title = "Anos",
                           inner_ci_level = .9,
                           point.shape = FALSE)

plotPartDemb + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))

plotIlibDema <- plot_summs(Mod.IlibDem2006a, Mod.IlibDem2014a, Mod.IlibDem2018a, 
                           model.names = c("2006", "2014", "2018"),
                           legend.title = "Anos",
                           inner_ci_level = .9,
                           point.shape = FALSE)

plotIlibDema + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Ideology",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))

plotIlibDemb <- plot_summs(Mod.IlibDem2006b, Mod.IlibDem2014b, Mod.IlibDem2018b, 
                           model.names = c("2006", "2014", "2018"),
                           legend.title = "Anos",
                           inner_ci_level = .9,
                           point.shape = FALSE)

plotIlibDemb + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))


plotLibConvBDema <- plot_summs(Mod.LibConvBDem2006a, Mod.LibConvBDem2014, Mod.LibConvBDem2018, 
                               model.names = c("2006", "2014", "2018"),
                               legend.title = "Anos",
                               inner_ci_level = .9,
                               point.shape = FALSE)

plotLibConvBDema + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Ideology",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))

plotLibConvBDemb <- plot_summs(Mod.LibConvBDem2006b, Mod.LibConvBDem2014b, Mod.LibConvBDem2018b, 
                               model.names = c("2006", "2014", "2018"),
                               legend.title = "Anos",
                               inner_ci_level = .9,
                               point.shape = FALSE)

plotLibConvBDemb + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressivism",
                             "Individualism",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education",
                             "Age"))
