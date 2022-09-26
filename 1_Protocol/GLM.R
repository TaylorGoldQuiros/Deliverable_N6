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

smallfish <- data.frame(read_excel("0_Data/Smallfish_CLEAN.xlsx")) %>%
    mutate(SITE = ordered(SITE, levels = c("WS", "DL", "GC", "BG"))) %>%
    mutate(AREA = ordered(AREA, levels = c("Backwater", "Inner meander bend",
                          "Outer meander bend", "Sidechannel", "Riffle"))) 

new_smallfish <- pivot_wider(smallfish, names_from = "Species", values_from = "n") %>%
    select(2:5, 8:13) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(sum = Dace + LNDC + LNSU + LSSU + RMCOT + RSSH) %>%
    mutate(n_effort = sum/X..of.pulls) %>%
    mutate(log_n_effort = log(n_effort + 1))
#Subsetting by species

#smallfish<-subset(smallfish, Species=="RSSH")

# Level order for sites x axis


#### GLMs ####

fish.glm <- glm(n_effort ~ SITE * AREA,
                 data = new_smallfish,
                 family = Gamma(link="log"))

Anova(fish.glm)

par(mfrow = c(2,2))
plot(fish.glm)

fish.glm2 <- glm(log_n_effort ~ SITE * AREA,
                      data = new_smallfish,
                      family = Gamma(link="log"))

Anova(fish.glm2)

par(mfrow = c(2,2))
plot(fish.glm2)


#estimating marginal mean w/in model. showing means (response column) standard error and confidence intervals  

Small_Fish_GLM2 <- glm(log_n_effort ~ SITE + AREA,
                      data = new_smallfish,
                      family = Gamma(link="identity"),
                      na.action=na.omit)

Anova(Small_Fish_GLM2)
plot(Small_Fish_GLM2)

smallfish.emm2 <- emmeans(Small_Fish_GLM2, ~ SITE, type = "response")

smallfish_SITE.cld2 <- cld(smallfish.emm2, alpha = 0.05, Letters = letters)

smallfish.emm3 <- emmeans(Small_Fish_GLM2, ~ AREA, type = "response")

smallfish_AREA.cld3 <- cld(smallfish.emm3, alpha = 0.05, Letters = letters)


#this is to allow for tidy plotting and to easily pull from output 

smallfish_SITE.cld2$.group <- gsub(" ", "", smallfish_SITE.cld2$.group)
smallfish_SITE.cld2 <- subset(smallfish_SITE.cld2)

smallfish_AREA.cld3$.group <- gsub(" ", "", smallfish_AREA.cld3$.group)
smallfish_AREA.cld3 <- subset(smallfish_AREA.cld3)

#PLOT Sites

(plot_smallfish_site <- ggplot() +
    geom_jitter(data = new_smallfish, 
                aes(x = SITE, y = exp(log_n_effort)-1), size = 1.5, width = 0.2, 
                color = "gray") +
    geom_errorbar(data = smallfish_SITE.cld2,
                  aes(x = SITE, ymin = (exp(lower.CL)-1), ymax = 
                          (exp(upper.CL))-1), width = 0) +
    geom_point(data = smallfish_SITE.cld2,
               aes(x = SITE, y = (exp(emmean))- 1), shape = 1, size = 7) +
    geom_text(data = smallfish_SITE.cld2, 
              aes(x = SITE, y = exp(emmean) - 1,
                  label = .group, 
                  vjust = -0.5, hjust = -0.5), 
              position = position_dodge(0.75),
              size = 6,) +
    #theme(axis.title.x=element_blank()) +
    labs(x= "Habitat", y = "CPUE (Fish/seine pull)")+
    scale_x_discrete()+
    scale_y_log10())

#PLOT Habitat

(plot_smallfish_habitat <- ggplot() +
    geom_boxplot(data = new_smallfish, aes(x = AREA, y = n_effort), 
                 width = 0.5, outlier.shape = NA) +
    geom_jitter(data = new_smallfish, 
                    aes(x = AREA, y = (n_effort)), size = 1.5, width = 0.2, 
                    color = "gray") +
    # geom_errorbar(data = smallfish_AREA.cld3,
    #               aes(x = AREA, ymin = (exp(lower.CL)-1), ymax = (exp(upper.CL)-1)), width = 0) +
    # geom_point(data = smallfish_AREA.cld3,
    #            aes(x = AREA, y = (exp(emmean)-1)), shape = 1, size = 7) +
    # geom_text(data = smallfish_AREA.cld3, 
    #           aes(x = AREA, y = (exp(emmean)-1),
    #               label = .group, 
    #               vjust = 0, hjust = 0), 
    #           position = position_dodge(0.75),
    #           size = 6,) +
    #theme(axis.title.x=element_blank()) +
    labs(x= "Habitat", y = "CPUE (Fish/seine pull)") +
    scale_x_discrete() +
    scale_y_log10())


# Saving plot
save_plot("./3_Output/plot_smallfish_site.png", plot_smallfish_site, 
          base_width = 5, 
          base_height = 4)

save_plot("./3_Output/plot_smallfish_habitat.png", plot_smallfish_habitat, 
          base_width = 5, 
          base_height = 4)


##########

bigfish <- data.frame(read_excel("0_Data/Browntrout length frequency.xlsx", 
                                 skip = 1)) %>%
    mutate(Site = ordered(Site, levels = c("WS", "DL", "GC", "BG")),
           Avg.length.mm. = "ave_length", .keep = "unused") 






