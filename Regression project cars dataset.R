#Removing from memory, packages
rm(list=ls())
library(Matrix) # For matrix computations
library(olsrr) # For Variable selection algorithms
library(car) # For VIF
library(mctest) # For variance decomposition algoritms


#reading data
library(readxl)
data=read_excel("D:/MSc/sem/Sem 2/Zz_Project_Regression/CarPrice_Assignment.xlsx",col_names=TRUE)
data=data.frame(data)
attach(data)
n=length(price)


#define response, predictors, design matrix, cs matrix
y=data[,14]
y=log(y) ##### Idea of Box cox method was used
data_pred = data[,-c(14)]
p=length(data_pred[1,]) #equals 13 (variable selection needed)
X = cbind(rep(1,n),as.matrix(data_pred))
Xcs = data.frame(sqrt(1/(n-1))*scale.default(data_pred, center=TRUE,scale=TRUE))
Xcs=as.matrix(Xcs)
det(t(Xcs)%*%Xcs) # equals 3.3e-07 (suspecting multicollinearity)


#pairwise scatterplot
par(mfrow=c(3,5))
for(i in 1:13)
{	plot(data[,i],y,xlab=names(data)[i],ylab="Price")	}


#fitting
fit=lm(y~.,data=data_pred) 
summary(fit) # r2=0.8792



#####################################################


# Evaluating all possible subset linear regression models is not advisable since total no. would be 2^13-1

# Forward selection method
f = ols_step_forward_p(fit, details = FALSE) 
summary(f$model) # r2=0.8789


# backward elimination method
b = ols_step_backward_p(fit, details=FALSE)
summary(b$model) # r2=0.8789


# Step-wise selection method
s = ols_step_both_p(fit, details=FALSE)
summary(s$model) # r2=0.8781

# g = r2(M)/r2(whole) 
# So for forward/backward selection, g is close to 1


#####################################################


# we drop the boreratio,carlength,carheight, updating X,Xcs
data_pred2=subset(data_pred,select=-c(carlength,boreratio,carheight))
X = cbind(rep(1,n),as.matrix(data_pred2))
Xcs = data.frame(sqrt(1/(n-1))*scale.default(data_pred2, center=TRUE,scale=TRUE))
Xcs=as.matrix(Xcs)
ycs=(y-mean(y))/((n-1)*(sd(y))) 


# fitting MLRM with CS data
scaled_fit = lm(ycs~.-1,data=data.frame(Xcs))
summary(scaled_fit)

# correlation matrix
corr_mat=t(Xcs)%*%(Xcs)
corr_mat # high pairwise correlation between predictors is present (suspecting multicollinearity)


# Determinant of X'X
det(t(Xcs)%*%Xcs) # equals 1.35e-05 (suspecting multicollinearity)

# Computing VIF
round(vif(scaled_fit),2)
# We use  cut-off 10.
# Multi-collinearity is suspected and the regression coefficients corresponding to curbweight, highwaympg, citympg are suffering.


# Condition indices
e = eigen(corr_mat)$values  # eigen(A) computes both eigenvalues and eigenvectors
Condition_indices = array(0)
for(i in 1:length(e))
{
  Condition_indices[i] = max(e)/e[i] 
}
round(Condition_indices,2)
# We use cut-off 25.
# Last four condition indices are larger than 25.
# Last four principal components of the predictor variables are suspected to be responsible for multi-collinearity.


# Calculating measures based on variance decomposition
scaled_fit_intercept = lm(y ~ ., data = data.frame(Xcs))
VP = eigprop(scaled_fit_intercept, Inter=FALSE)
VP$pi[c(7,9,8,3,10),]
# Regression coefficient correspond to :
# carwidth is suffering from the 7th pc. The measure is 0.79
# curbweight is suffering from the 9th pc. The measure is 0.97
# stroke is suffering from the 3rd pc. The measure is 0.81
# horsepower is suffering from the 8th pc. The measure is 0.73
# citympg is suffering from the 10th pc. The measure is 0.93
# highwaympg is suffering from the 10th pc. The measure is 0.92




###########	PC REGRESSION ############
pca = prcomp(Xcs)
pca  
# prcomp(A) performs principal component analysis on the data matrix A.

pc_variances = summary(pca) # summary(prcomp()) prints sample variance of the proncipal components.
pc_variances 
pc = pca$x # The i-th column of prcomp(A)$x provides the i-th principal component of the data matrix A.
pc_data = data.frame(pc)
summary(lm(ycs~.-1,pc_data)) # Fit MLRM for the standarized response on the principal components obtained in the last step.
fit_good=lm(ycs~.-1,pc_data[,c(1,4,5,10)])
summary(fit_good) # Fit final multiple linear regression model with significant principal components.
round(pca$rotation[,c(1,4,5,10)],2) # This provides the linear combination for the significant principal component.



############# Assumption Verification ###################
# Normality assumption
ols_plot_resid_qq(fit_good) # Outliers present and hence we can note departure from normality
ols_test_normality(fit_good) # All tests result in rejection of null hypothesis; therefore normality assumption is violated 

# Homoscedasticity assumption
ols_plot_resid_fit(fit_good) # Clustering is observed  and hence homoscedasticity assumption does not hold

# Assumption of random errors being uncorrelated
acf(fit_good$residuals,plot=TRUE) # Autocorrelation is present


############ Outlier detection ####################

lev1=ols_plot_resid_lev(fit_good) # Computing leverage measure for all observations

# leverage points
which(lev1$data$color=="leverage")

# outliers
which(lev1$data$color=="outlier")

# outlier and leverage
which(lev1$data$color=="outlier & leverage")


lev2=ols_plot_cooksd_bar(fit_good) # Computing Cook's distance statistics
which(lev2$data$color=="outlier")

lev3=ols_plot_dffits(fit_good) # Computing DFFFITS statistics
which(lev3$data$color=="outlier")

ols_plot_dfbetas(fit_good)# Computing DFBETAS statistics




