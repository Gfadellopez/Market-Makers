#Author: Jasjeet Singh Sekhon (Yale University)

install.packages("arm")
install.packages("Matching")
install.packages("rgenoud")
install.packages("writexl")


Data_Paper_Market_Makers <- read_excel("C:/Users/jorge.sabat/Dropbox/Market Maker Chile/Data Paper Market Makers.xlsx", 
                                       sheet = "Tercero")
Data_Paper_Market_Makers = na.omit(Data_Paper_Market_Makers)
  
library("writexl")
library(arm)
library(Matching)
library(rgenoud)
#data("lalonde")
#Calculates a univariate test on variable "re75" by Treat and Control

foo  <- balanceUV(Data_Paper_Market_Makers$ROE[MM==1],Data_Paper_Market_Makers$ROE[MM!=1])

#The covariates we want to match on
X = cbind(Data_Paper_Market_Makers$ROE, Data_Paper_Market_Makers$PB, Data_Paper_Market_Makers$PE, Data_Paper_Market_Makers$año)
#The covariates we want to obtain balance on
#BalanceMat <- cbind(Data_Paper_Market_Makers$ROE, Data_Paper_Market_Makers$PB, Data_Paper_Market_Makers$PE, Data_Paper_Market_Makers$año)
# Optimal weights that balance Treat/Control group by observables
genout <- GenMatch(Tr=Data_Paper_Market_Makers$MM, X=X, BalanceMatrix=X, estimand="ATE", M=1,
                   pop.size=1000, max.generations=10, wait.generations=1)                    
df_matches=data.frame(genout$matches)
write_xlsx(df_matches,"C:/Users/jorge.sabat/Dropbox/Market Maker Chile/matches.xlsx")
