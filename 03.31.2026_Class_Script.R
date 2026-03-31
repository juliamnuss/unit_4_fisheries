# 03.31.2026
# JMN
# Class Script
# Unit 4
# 4.4 Principle Component Analysis

library(tidyverse)
library(palmerpenguins)

head(penguins)

# you use PCA when you have a lot of variables in your data
# one method of data compression to better understand the data in it
# results can be difficult to interpret

# remove the NAs - they dont work well with PCAs
pen_drop_na = penguins %>%
  drop_na()

# isolate the variables I want to use in my PCA
# PCA only works with numbers not characters
pen_num = pen_drop_na %>% 
  select(ends_with("mm"), body_mass_g)
# metadata with remaining info
pen_meta = pen_drop_na %>%
  select(species, sex, island, year)

# run PCA; set parameters to normalize the data
pen_pca = prcomp(pen_num, scale. = TRUE, center = TRUE)
summary(pen_pca)
print(pen_pca) # shows standard deviations adn loadings
str(pen_pca) # examine structure of the variable

pen_pca$sdev # look at standard deviations of each PCA
str(summary(pen_pca)) # shows that summary stats are calculated and stored in $importance
summary(pen_pca)$importance[2, ] # extract proportion of variance from summary()
pen_pca$sdev^2/sum(pen_pca$sdev^2) # calculate proportion of variance manually

pen_pca$rotation # loading
head(pen_pca$x) # contains the principal componenets
dim(pen_pca$x)
# x muliplied by rotation = original data

# scree plot
plot(pen_pca) # plots std dev squared for each PC

# traditional scree plots show the proportion of variance explained by each PC
# do it yourself with ggplot:
pca_scree = data.frame(pc = c(1:4),
                        var = summary(pen_pca)$importance[2, ])
ggplot(aes(x = pc, y = var), data = pca_scree) +
  geom_col() + 
  geom_point(size = 4) + 
  geom_line() +
  ggtitle("Scree Plot") + ylab("Proportion Variance Explained") + xlab("PC") +
  theme_bw()

# biplot
# plotting PCs against eachother
pen_pca_meta = cbind(pen_pca$x, pen_meta) # join PCs and metadata into a single table for easier plotting
ggplot() + 
  geom_point(aes(x = PC1, y = PC2, color = species, shape = sex), data = pen_pca_meta) +
  coord_fixed(ratio = 1) + # scale if PC1 is the same as PC2
  theme_bw()

# same kind of plot but with different packages for different aesthetics
# install.packages("devtools")
# install.packages("rlang")
# install.packages("ggbiplot")
library(devtools)
library(ggbiplot)

biplot(pen_pca) # points are row numbers (from stats package)
ggbiplot(pen_pca)

# plot PC1 vs PC2 (classic biplot)
ggbiplot(pen_pca, scale = 1, obs.scale = 1, groups = pen_meta$species, ellipse = T)

# change point shape to reflect penguins sex
ggbiplot(pen_pca, scale = 1, obs.scale = 1, groups = pen_meta$species, ellipse = T, alpha = 0) + 
  geom_point(aes(color = pen_meta$species, shape = pen_meta$sex)) +
  theme_bw()

# plot PC3 vs PC4 to see if there is more interesting variation (there isnt)
ggbiplot(pen_pca, scale = 1, obs.scale = 1, groups = pen_meta$species, ellipse = T, choices = c(3,4))
