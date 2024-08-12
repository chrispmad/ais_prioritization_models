#### Have to test the parameters before using them for modelling purposes
###For example, zero inflation of the response variable in GLM may cause biased
### parameter estimates. Temporal or spatial correlations acan increase type I errors.


#### How would an outlier affect the model output?

###############################################
## 1. outliers - box plot and cleveland dotplot
###############################################

# does an unusual observation have an undue effect on analysis? - omission or transformation?

library(ggplot2)

###data
boxplot(NA_ph_res, maxpixels = 100000, main = "pH in British Columbia")
boxplot(NA_calc_res, maxpixels = 100000, main = "Calcium in British Columbia")
boxplot(elev_res, maxpixels = 100000, main = "Elevation in British Columbia")
boxplot(pred_bioc_c$Annual_Mean_Temperature, maxpixels = 100000, main = "Annual mean temperature in British Columbia")
### Cleveland dotplots


###############################################
## 2. Homogeneity - 
###############################################



###############################################
## 3. Are the data normally distributed?
###############################################

# make histograms of residuals to get the impression of normality



###############################################
## 4. Are there lots of zeroes?
###############################################

###############################################
## 5. Colinearity
###############################################

#variance inflation factor - drop covariates until this value falls below a certain threshold (eg 3 Zuur 2009)
#Pairwise scatterplots of covariates 
# spatial variables - lat and long with temp, rainfall etc. Should always plot all covariates against temporal and 
# spatial covariates
# drop based on common sense or biological knowledge


