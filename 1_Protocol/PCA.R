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
library(factoextra)
library(readxl)
#### PCA ####



CPUE_HIstorical <- read_excel("0_Data/Smallfish_PCA.xlsx")

pca9.ln <- prcomp(CPUE_HIstorical[c(3:8)],  scale = TRUE)

plot(pca9.ln)


#pca9.ln$rotation

aload <- abs(pca9.ln$rotation)
results <- sweep(aload, 2, colSums(aload), "/")
dev.new()
factoextra::fviz_pca_biplot(pca9.ln, 
                            habillage=CPUE_HIstorical$AREA,
                            geom.ind = c("point"), 
                            #numbers removed use point only
                            col.var="black", #change colors of arrow variables
#                            addEllipses = TRUE, 
                            repel = FALSE,
                            title = NULL) +
                            xlim(-5, 5) + ylim (-2.5, 2.5)
 
