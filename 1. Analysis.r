setwd("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Advanced Data Driven Decision Making/Case Study 2")
factors = fread("Data File_Case Study_Factor Analysis_Variables.csv")
md = fread("Data File_Case_Study_Factor Analysis_MD.csv")
qds = md[,9:41]

library(rcompanion)   #Histogram and Normal Curve
library(nortest) #Kolmogorov-Smirnov-Test
library(corrplot) #correlation matrix plot
library(olsrr)  #VIF and Tolerance Values
library(dplyr)
library(pastecs)
library(REdaS) #Bartelett's Test
library(psych)
library(reshape) # Melt
library(ggplot2)

# Run a factor analysis with the quality items qd_1 to qd_33. In this first step run a principal axis factoring and use varimax rotation.
# Go through the steps of a factor analysis that you have learned i.e.
# start with looking at the correlation matrix
# check whether the data set and all of its variables are suitable for factor analysis etc. 

# correlation matrix, check whether the data set and all of its variables are suitable for factor analysis
qds = qds[complete.cases(qds), ]
kmo = KMOS(qds)
corrplot(kmo$cormat)
sort(kmo$MSA) # qd4 and qd23 have low values, this can also be seen in the correlation plot
kmo$KMO

bart_spher(qds) # p-Value very close to 0, H0 can be rejected

# Make appropriate decisions and decide on a final factor solution.
############### Principal Axis Analysis

af = fa(kmo$cormat, scores = TRUE, rotate = "varimax")
# af = factor.pa(kmo$cormat, nfactors = 9, rotate = "varimax")
print(af$Structure, cutoff=0.3, sort=TRUE)
plot(af$values) + abline(h = 1)

af = fa(kmo$cormat, nfactors = 5, rotate = "varimax")
# I would leave it as it is without removing anything.

# Interpret the factors based on the literature review on quality dimensions (see table 1).
colnames(af$Structure) <- c("Serviceability", "Performance", "Aesthetics/Appearance", "Ease of use", "Reliability/Flawlessness", "Features/Versatility", "Durability", "Distinctiveness/Prestige", "Conformance")
af$Structure

# Do all of the variables in your final solution show clear loading patterns?
print(af$Structure, cutoff=0.15, sort=TRUE)
# - no zeros
# - according 

# For your final solution run a principal component analysis and compare the results. 

############### Principal Component Analysis

pc1 = principal(qds, rotate = "varimax", scores = TRUE)
plot(pc1$values) + abline(h=1) # one should select 8 factors, Kaiser-criterion shows 8 factors with an eigenvalue > 1

pc2 = principal(qds, rotate = "varimax", nfactors = 9, scores = TRUE)

kable(sort(pc2$communality)) # only variable 23 has a lower value and should be handled carefully during the further analysis. qd4 has ahigher commulaity value

################

# Total Variance Explained
total_var_explained = data.table("eigenvalue" = numeric(length(qds)),
                               "variance" = 0,
                               "total_variance" = 0)

total_var_explained$eigenvalue = pc1$values
total_var_explained$variance = (total_var_explained$eigenvalue / length(qds)) * 100
total_var_explained$total_variance = cumsum(total_var_explained$eigenvalue / length(qds))
total_var_explained

# Explanation:
# The first variable has an eigenvalue of 15.9 that can explain 48.2% of the overall variance
# the first 8 factors to be selected can explain about 80% of the variance.

print(pc2$loadings, cutoff=0.3,sort=TRUE)

# qd28 is loading low on factor 1, so is qd8.
# qg32 is loading low on factor 5.
# qd26 is loading low on factor 3.

qds_new = qds[,-c(8, 32)]
pc3 = principal(qds_new, rotate = "varimax", nfactors = 8, scores = TRUE)

plot(pc3$values) + abline(h=1)

total_var_explained = data.table("eigenvalue" = numeric(length(qds_new)),
                                 "variance" = 0,
                                 "total_variance" = 0)

total_var_explained$eigenvalue = pc3$values
total_var_explained$variance = (total_var_explained$eigenvalue / length(qds)) * 100
total_var_explained$total_variance = cumsum(total_var_explained$eigenvalue / length(qds))

# 6 factors with eigenvalue at least 1

pc4 = principal(qds_new, rotate = "varimax", nfactors = 6, scores = TRUE)

print(pc4$loadings, cutoff=0.3, sort=TRUE)

# Interpret the factors based on the literature review on quality dimensions (see table 1).
# Merging with the scores and changing column names
qds = cbind(qds, pc2$scores)
colnames[,34:42] = c("Serviceability", "Performance", "Durability", "Aesthetics/Appearance", "Ease of use", "Features/Versatility", "Reliability/Flawlessness", "Distinctiveness/Prestige")

# Are there differences to your solution based on principal axis factoring?

      # ...

# What do the eigenvalues of the factors/quality dimensions tell us about their relevance for repurchase behaviour? Please explain!

      # ...














