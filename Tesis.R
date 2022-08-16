
install.packages('Rcpp')
install.packages('ggplot2')
install.packages('sjPlot')
install.packages('sjmisc')
#install.packages("coefplot2", repos="http://www.math.mcmaster.ca/bolker/R", type="source")
install.packages("coefplot")
install.packages("arm")
install.packages("Rcpp") 

install.packages("lfe")
install.packages("bacondecomp")
install.packages("data.table")
install.packages("foreign")
install.packages("zoo")
install.packages("interactions")
install.packages("fixest")

install.packages("plotly")
install.packages("dplyr")

#.rs.restartR()

library(dplyr)
library(plotly)
library(haven)
library(tidyverse)
library(stargazer)
library(estimatr)
library(lfe)
library(bacondecomp)
library(Rcpp)
library(fixest)
library(zoo)
library(ggplot2)
library(interactions)
library(sjPlot)
library(sjmisc)
library(plyr)
library(dplyr)
library(arm)
library(ggplot2)
library(readxl)

Data_Paper_Market_Makers <- read_excel("C:/Users/Soporte/Desktop/Data Paper Market Makers.xlsx", 
                                       sheet = "Segundos datos")



MM <- Data_Paper_Market_Makers


#fecha por año mes: 
MM$Fecha <- as.Date(MM$Fecha, tz = "UTC", "%Y-%m-%d")

año <- format (MM$Fecha, format="%Y")
mes <- format (MM$Fecha, format="%m")
ym <- format(MM$Fecha, format="%y%m") 

MM$time = ym

#Binaria:
MM$post = ifelse((MM$time >= 1201), 1, 0)

#bid-ask
#na.omit(x)
MM$Bid [ MM$Bid == "#N/A N/A" ] <- NA
MM$Ask [ MM$Ask == "#N/A N/A" ] <- NA

MM$Bid <- as.numeric(MM$Bid)
MM$Ask <- as.numeric(MM$Ask)


MM$media <- rowMeans (MM [, c (4,5)], na.rm = TRUE )                
MM$desv <- (abs(MM$Bid - MM$media) + abs(MM$Ask - MM$media))/2


MM$nuevodif <- MM$desv/MM$Precio

# Treated firms

MM$treated = ifelse(MM$Empresa == "ILC CC Equity" | 
                      MM$Empresa == "ANDROMAC CI Equity" | 
                      MM$Empresa == "AUSTRALI CI Equity"|
                      MM$Empresa == "BANVI CI Equity"|
                      MM$Empresa == "BLUMAR CI Equity"|
                      MM$Empresa == "CAMPOS CI Equity"|
                      MM$Empresa == "CEMENT CI Equity"|
                      MM$Empresa == "EDELPA CI Equit"|
                      MM$Empresa == "EISA CI Equity"|
                      MM$Empresa == "ENAEX CI Equity"|
                      MM$Empresa == "INDISA CI Equity"|
                      MM$Empresa == "CONDES CI Equity"|
                      MM$Empresa == "POTASIOA CI Equity"|
                      MM$Empresa == "PUCOBRE CI Equity"|
                      MM$Empresa == "VSPT CI Equity"|
                      MM$Empresa == "SOQUIC CI Equity"|
                      MM$Empresa == "VENTANA CI Equity"|
                      MM$Empresa == "WATTS CI Equity"|
                      MM$Empresa == "ZOFRI CI Equity", 1, 0)


#Regresion:

didreg <- lm_robust(nuevodif ~ treated*time + Empresa + time, data = MM, weights = NULL, clusters = Empresa)
summary(didreg)


didreg2 <- lm_robust(log(Turnover) ~ treated*time + Empresa + time, data = MM, weights = NULL, clusters = Empresa)
summary(didreg2)
 


#GRAFICAS 

dates <- seq.Date(from = as.Date("2011-06-01"), length.out = 31, by = "month")
format(dates, "%B %Y")

class(date)

MM_plot <- tibble(
  sd = didreg$std.error[97:127],
  mean = didreg$coefficients[97:127],
  year = dates)


Grafico1 <-MM_plot %>% 
  ggplot(aes(x = year, y = mean)) + 
  #geom_rect(aes(xmin=15150, xmax=16100, ymin=-Inf, ymax=Inf), fill = "light cyan", alpha = 0.01)+
  geom_point()+
  xlab("") + ylab("")+
  #scale_x_date( date_breaks = "1 month", date_labels = "%b-%Y")+
  #geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05)) +
  labs( x = "fecha",y = " %") + 
  ggtitle(label = " Bid-ASK ") +
  theme(plot.title = element_text(hjust = 0.5, size= 10))

Grafico1
ggsave( plot = Grafico1, filename = "bidask.png", width = 16, height = 11, dpi = 300)


MM_plot2 <- tibble(
  sd = didreg2$std.error[97:127],
  mean = didreg2$coefficients[97:127],
  year = dates)


Grafico2 <- MM_plot2 %>% 
  ggplot(aes(x = dates, y = mean)) + 
  geom_rect(aes(xmin=15150, xmax=16100, ymin=-Inf, ymax=Inf), fill = "light cyan", alpha = 0.01)+
  geom_point()+
  xlab("") + ylab("")+
  #scale_x_date( date_breaks = "1 month", date_labels = "%b-%Y")+
  #geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0) +
  #scale_x_yearmon(break= function(range) seq(range[1], range[2], by =1), date_labels= "%m/%Y")+
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                position = position_dodge(0.05)) +
  labs(title= "Turnover ")

Grafico2
ggsave( plot = Grafico2, filename = "turnover.png", width = 16, height = 11, dpi = 300)











###############################################################################################

  gg<- ggplot(data= MM, aes(x = year, y = mean, type = "l"))+
  geom_line()+
  geom_rect(aes(xmin= "1201", xmax= "1212", ymin= -Inf, ymax= Inf), fill = "light cyan",alpha=0.1 )+
  geom_point()+
  #geom_smooth( method = "lm")+
  geom_text(aes(label = year), hjust=-0.002, vjust = -0.03, size = 0.1)+
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = mean - sd*1.96, ymax = mean + sd*1.96), width = 0.2,
                  position = position_dodge(0.05)) +
  #geom_abline(didreg2) + 
  labs( x = "año",y = " Bid - Ask") + 
  ggtitle(label = "Diferencia Bid-ASK DESDE 2011 A 2013") +
  theme(plot.title = element_text(hjust = 0.5, size= 10))


gg  
ggsave( plot = gg, filename = "dif.png", width = 16, height = 11, dpi = 300)



  
  
  