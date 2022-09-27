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

###Small fish###----
## Onboarding data
# Import excel file, change SITE to an ordered factor, then change AREA to
# an ordered factor:
smallfish <- data.frame(read_excel("0_Data/Smallfish_CLEAN.xlsx")) %>%
    mutate(SITE = ordered(SITE, levels = c("WS", "DL", "GC", "BG"))) %>%
    mutate(AREA = ordered(AREA, levels = c("Backwater", "Inner meander bend",
                          "Outer meander bend", "Sidechannel", "Riffle"))) 


# Rearrange to a wider format, remove unneeded rows, replace NA with 0 for math
# purposes, add all the fish, divide by effort, and create a log transformed 
# value of CPUE

new_smallfish <- pivot_wider(smallfish, names_from = "Species", values_from = "n") %>%
    select(2:5, 8:13) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(sum = Dace + LNDC + LNSU + LSSU + RMCOT + RSSH) %>%
    mutate(n_effort = sum/X..of.pulls) %>%
    mutate(log_n_effort = log(n_effort + 1))

##Modeling and exploring data

fish.glm <- glm(n_effort ~ SITE + AREA,
                 data = new_smallfish,
                 family = Gamma(link="log"))

par(mfrow = c(2,2)) # Makes diagnostic plots show as 2 x 2 grid
plot(fish.glm) # Diagnostic plots
Anova(fish.glm) # ANOVA

fish.emm <- emmeans(fish.glm, ~ SITE, type = "response")
fish.cld <- cld(fish.emm, alpha = 0.05, Letters = letters)

(plot.fish <- ggplot() +
    geom_jitter(data = new_smallfish, 
                aes(x = SITE, y = n_effort), size = 1.5, width = 0.2, 
                color = "gray") +
    geom_errorbar(data = fish.cld,
                  aes(x = SITE, ymin = lower.CL, ymax = upper.CL, width = 0)) +
    geom_point(data = fish.cld,
               aes(x = SITE, y = response), shape = 1, size = 7) +
    geom_text(data = fish.cld, 
              aes(x = SITE, 
                  y = response,
                  label = .group, 
                  vjust = -0.5, hjust = -0.5), 
              position = position_dodge(0.75),
              size = 6,) +
    #theme(axis.title.x=element_blank()) +
    labs(x= "Habitat", y = "CPUE (Fish/seine pull)")+
    scale_x_discrete()+
    scale_y_log10())

# Diagnostic plots show residuals are uneven around the axis. May be driven by 
# outliers, but still is concerning. Also, when taking the model forward to
# emmeans and cld, it results in a plot where the means are outside of where
# the majority of the data are residing. Looks very suspect if your central 
# tendency isn't central. 

fish.glm2 <- glm(log_n_effort ~ SITE + AREA,
                      data = new_smallfish,
                      family = Gamma(link="log"))

plot(fish.glm2) # Looks better, but not great. Will do emm and cld for reference
Anova(fish.glm2)

fish.emm2 <- emmeans(fish.glm2, ~ SITE, type = "response")

fish.cld2 <- cld(fish.emm2, alpha = 0.05, Letters = letters)



fish.glm3 <- glm(log_n_effort ~ SITE + AREA,
                      data = new_smallfish,
                      family = Gamma(link = "identity"))

plot(fish.glm3) # Residuals look good. Going with this as log link on log 
                # transformed data feels like a lot of log.
Anova(fish.glm3)

fish.emm3 <- emmeans(fish.glm3, ~ SITE, type = "response")

fish.cld3 <- cld(fish.emm3, alpha = 0.05, Letters = letters)

#PLOT Sites

(plot_smallfish_site <- ggplot() +
    geom_jitter(data = new_smallfish, 
                aes(x = SITE, y = n_effort), size = 1.5, width = 0.2, 
                color = "gray") +
    geom_errorbar(data = fish.cld3,
                  aes(x = SITE, ymin = (exp(lower.CL)-1), ymax = 
                          (exp(upper.CL))-1), width = 0) +
    geom_point(data = fish.cld3,
               aes(x = SITE, y = (exp(emmean))- 1), shape = 1, size = 7) +
    geom_text(data = fish.cld3, 
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


####Stats on linear models for bigfish data######
# Onboard data
bigfish <- data.frame(read_excel("0_Data/Browntrout length frequency.xlsx", 
                                 skip = 1)) %>%
    mutate(Site = ordered(Site, levels = c("WS", "MR", "BM"))) 

# Run regressions for each site using lapply to make a list by applying a 
# function, "function(x)" to let it know you're going to do a function with
# arguments, lm to call a linear model, then a generic version of the function 
# for the bigfish object, and namely, for each Site in the object. Then, pull 
# out the stats using lapply again.

regression_list <- lapply(unique(bigfish$Site),
                         function(x) lm(Avg.length..mm. ~ Year, bigfish[bigfish$Site==x,]))


regression_stats <- lapply(regression_list, summary)
names(regression_stats) <- c("WS", "MR", "BM")
regression_stats
