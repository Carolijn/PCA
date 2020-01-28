# change object folder to folder where the data is
folder <- 'C:/Users/carol/Documents/Key docs/Master Econometrics/Seminar/BBP'
xls_input_file <-  file.path(folder, "dec20_pad01_bvar.xls")
xls_output_file <- file.path(folder, "bvar_dec2020_14nov.xlsx")

# first and last period for realisations in output-file
# forecast for coming and next year
first_last_per <- "2017q1/2019q3"
first_per_forecast <- "2019q4"
last_per_forecast <- "2020q4"

###########################################################################################
#                                   Data preparation                                      #
###########################################################################################
library(xlsx)
library(regts)
# Import data for BVAR (only from 2001q1 onwards)
data_raw <- read_ts_xlsx(xls_input_file, na_string = "NA")
data <- data_raw["2001q1/",]
end <- nrow(data)

# Transform data (logarithms)
data[,c(1:9,12,17,23,27:29)] = log(data[,c(1:9,12,17,23,27:29)]);

# Swap output data to the first place
data <- data[,c(2,1,3:29)]

# Variable selection (new model contains 22 variables)
#data <- data[,-c(15,18,19,24,26,27,28,29)]

#taking first differences
data <- diff(data)

#standardize the X variables
Yt = data[,c(1,2,4,5,6,7,8)]
Xt = data[,-c(1,2,4,5,6,7,8)]
#Xt = Xt[1:73,]
Xt = Xt[,-c(20,21)]
Xt <- scale(Xt)

# Create variable for GDP growth
real_gdp_growth <- regts(c(NA,data[2:end,"YBMAN___"]-data[1:end-1,"YBMAN___"]),start="2001q1")

#principal component analysis
prcomp(Xt)
pca <- prcomp(~ ., data=Xt, na.action=na.omit, scale=TRUE)
summary(pca)
plot(pca)

loadings = pca$rotation
pc1 = loadings[,1]

# Computation of Bai Information Criteria
N = dim(Xt)[2]
T = dim(Xt)[1]
C_nt_squared = min(N,T)

k=3
F = pca$x[,1:k]
loading = pca$rotation[,1:k]
V = sum((Xt - F%*%t(loading))^2)/(N*T)
test = Xt - F%*%t(loading)
test2 = (Xt - F%*%t(loading))^2
test3 = sum(test2[,1:20])

V = function(k){
  F = pca$x[,1:k]
  loading = pca$rotation[,1:k]
  V = sum((Xt - F%*%t(loading))^2)/(N*T)
  return(V)
  }

var.hat = V(dim(Xt)[2])

PC_1 = function(k){ V(k) + 0.25*k*((N+T)/(N*T))*log(N*T/(N+T))}
PC_1 = Vectorize(PC_1); curve(PC_1, 0, 20)
PC_2 = function(k){ V(k) + k*var.hat*((N+T)/(N*T))*log(C_nt_squared)}
PC_2 = Vectorize(PC_2); curve(PC_2, 0, 20)

IC_1 = function(k){ log(V(k)) + k*((N+T)/(N*T))*log((N+T)/(N*T))}
IC_1 = Vectorize(IC_1); curve(IC_1, 0, 20)
IC_2 = function(k){ log(V(k)) + k*((N+T)/(N*T))*log(C_nt_squared)}
IC_2 = Vectorize(IC_2); curve(IC_2, 0, 20)

#########
## PCA ##
#########

#Xt_m <- as.matrix(Xt)
#cov_xt <- t(yt_m) %*% yt_m / T ## Defining the 128x128 covariance matrix
## Eigen decomposition, identical to SVD here (symmetric square matrix)
#eig = eigen(cov_xt) 
#eig_val <- as.data.frame(eig[1]$values) ## Eigenvalue
#colnames(eig_val) <- "Eigenvalue"
#rownames(eig_val) <- 1:nrow(eig_val)
#eig_vec <- as.data.frame(eig[2]$vectors) ## Eigenvector
#colnames(eig_vec) <- 1:ncol(eig_vec)
#rownames(eig_val) <- 1:nrow(eig_vec)

#Factor_loadings <- sqrt(ncol(yt_z)) * eig_vec
#Factor_loadings <-as.data.frame(Factor_loadings)
#Factor_components <- as.matrix(yt_z) %*% as.matrix(Factor_loadings/ncol(yt_z))
#Factor_components <- as.data.frame(Factor_components)

#Fhat <- Factor_components[,1:7] ## Using the first 7 factors as indicated by the IC2