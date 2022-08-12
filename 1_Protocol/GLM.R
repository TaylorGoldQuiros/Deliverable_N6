#### Loading Packages ####

library(multcomp)
library(readr)
library(mgcv)
require(gam)
library(gplots)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidymv)
library(emmeans)
library(multcompView) # making posthoc tests easier to view and plot
library(car)
library(ggpubr)
library(cowplot)
library(readxl)

#create a theme for all plots

theme_set(theme_bw() + 
            theme(panel.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x = element_text( size=15),
                  axis.title.y = element_text( size=15),
                  axis.text.x = element_text(size = 12,
                                             angle=45, 
                                             vjust=1, 
                                             hjust=1),
                  axis.text.y = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  legend.text=element_text(size=12),
                  strip.text = element_text(size = 12)))

# Get rid of scientific notation

options(scipen = 999)

# Onboarding data

smallfish <- data.frame(read_excel("0_Data/Smallfish_CLEAN.xlsx"))

#Subsetting by species

#smallfish<-subset(smallfish, Species=="RSSH")

# Level order for sites x axis

level.order<-c("WS","DL","GC","BG")

#### GLMs ####

Small_Fish_GLM <- glm(log_n_effort ~ SITE * AREA,
                 data = smallfish,
                 family = Gamma(link="log"),
                 na.action=na.omit)


anova(Small_Fish_GLM)

#plot(Small_Fish_GLM)

#estimating marginal mean w/in model. showing means (response column) standard error and confidence intervals  

smallfish.emm2 <- emmeans(Small_Fish_GLM, ~ SITE:AREA, type = "response")

smallfish_SITE.cld2 <- cld(smallfish.emm2, by = "SITE", alpha = 0.05, Letters = letters)

smallfish_AREA.cld2 <- cld(smallfish.emm2, by = "AREA", alpha = 0.05, Letters = letters)


#this is to allow for tidy plotting and to easily pull from output 

smallfish_SITE.cld2$.group <- gsub(" ", "", smallfish_SITE.cld2$.group)
smallfish_SITE.cld2 <- subset(smallfish_SITE.cld2)

smallfish_AREA.cld2$.group <- gsub(" ", "", smallfish_AREA.cld2$.group)
smallfish_AREA.cld2 <- subset(smallfish_AREA.cld2)



dev.new()

#PLOT Habitats X Sites

(plotasglm.a.res.2.ln <- ggplot() +
    geom_jitter(data = smallfish, 
                aes(x = factor(SITE, level = level.order), y = log_n_effort), size = 1.5, width = 0.2, 
                color = "gray") +
    geom_errorbar(data = smallfish_AREA.cld2,
                  aes(x = factor(SITE, level = level.order), ymin = lower.CL, ymax = upper.CL), width = 0) +
    geom_point(data = smallfish_AREA.cld2,
               aes(x = factor(SITE, level = level.order), y = response), shape = 1, size = 7) +
    geom_text(data = smallfish_AREA.cld2, 
              aes(x = factor(SITE, level = level.order), y = response,
                  label = .group, 
                  vjust = 0, hjust = 0), 
              position = position_dodge(0.75),
              size = 6,) +
    #theme(axis.title.x=element_blank()) +
    scale_fill_brewer(palette = "Spectral") +
    labs(x= "Habitat", y = expression(Abundance~(log[10]))) +
    scale_x_discrete()+
#    scale_y_log10() +
    facet_wrap(~AREA))

#PLOT Sites X Habitats

(plotasglm.a.res.2.ln <- ggplot() +
    geom_jitter(data = smallfish, 
                aes(x = AREA, y = log_n_effort), size = 1.5, width = 0.2, 
                color = "gray") +
    geom_errorbar(data = smallfish_SITE.cld2,
                  aes(x = AREA, ymin = lower.CL, ymax = upper.CL), width = 0) +
    geom_point(data = smallfish_SITE.cld2,
               aes(x = AREA, y = response), shape = 1, size = 7) +
    geom_text(data = smallfish_SITE.cld2, 
              aes(x = AREA, y = response,
                  label = .group, 
                  vjust = 0, hjust = 0), 
              position = position_dodge(0.75),
              size = 6,) +
    #theme(axis.title.x=element_blank()) +
    scale_fill_brewer(palette = "Spectral") +
    labs(x= "Habitat", y = expression(Abundance~(log[10]))) +
    scale_x_discrete()+
#    scale_y_log10() +
    facet_wrap(~factor(SITE, level = level.order)))


# Saving plot
#save_plot("./3_Output/GLM_smallfish_FULL.png", plotasglm.a.res.2.ln, base_width = 10, 
          base_height = 8)


##########







