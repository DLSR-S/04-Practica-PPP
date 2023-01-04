rm(list=ls())
# install.packages("tidyverse")
# Instala el paquete dplyr o tydyverse,
# en caso de cambiar de compu
library(readxl)
library(dplyr)
library(ggplot2)
PPP <- read_excel("PPP_Activity.xlsx")
View(PPP)
PPP <- PPP %>%
  mutate(diff_inf=pi_mx-pi_us,
       ppp_for=((1+pi_mx)/(1+pi_us))-1)
View(PPP)
PPP %>%
  ggplot(aes(x=diff_inf,
             y=e))+
  theme_bw()+
  labs(title="PPP Line",
       subtitle="MXN/USD PPP",
       y=expression(paste("%",Delta,e[f])),
       x=expression(paste(pi[l]," - ",pi[f])))+
  geom_point(col="blue",
             size=1)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_x_continuous(labels=scales::percent,limits=c(-0.05,0.05))+
  scale_y_continuous(labels=scales::percent,limits=c(-0.05,0.05))+
  geom_smooth(method="lm",
              se=F,
              fullrange=T,
              col="red")

LM_PPP_1 <- lm(e~diff_inf,data=PPP)
summary(LM_PPP_1)

LM_PPP_2 <- lm(e~0+diff_inf,data=PPP)
summary(LM_PPP_2)

