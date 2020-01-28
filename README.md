# PCA
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

##################################  Added Data Preperation  ##################################
# Create first differences of data
data_new = diff(data)
Xt = data_new[,-c(1,2,4,5,6,7,8)] 
Xt = Xt[,-c(20,21)]#Remove 20 and 21 because they obtain missing values in final quarter. Alternative is to remove final quarter.. 
Yt = data_new[,c(1,2,4,5,6,7,8)]
# Standardize X_t data 
Xt = scale(Xt)

################################ Principal component analysis ##################################
# Note that prcomp uses the singular value decomposition, we could also do spectral decomposition using princomp (difference??)
pca <- prcomp(~ ., data=Xt, na.action=na.omit, scale=TRUE)
summary(pca)
plot(pca)

# Computation of Bai Information Criteria
N = dim(Xt)[2]
T = dim(Xt)[1]
C_nt_squared = min(N,T)

V = function(k){
F = pca$x[,1:k]
loading = pca$rotation[,1:k]
V = sum((Xt - F%*%t(loading))^2)/(N*T)}

var.hat = V(dim(Xt)[2])
PC_1 = function(k){ V(k) + k*var.hat*((N+T)/(N*T))*log((N*T)/(N+T))}
PC_1 = Vectorize(PC_1); curve(PC_1, 0, 20)
PC_2 = function(k){ V(k) + k*var.hat*((N+T)/(N*T))*log(C_nt_squared)}
PC_2 = Vectorize(PC_2); curve(PC_2, 0, 20)

IC_1 = function(k){ log(V(k)) + k*((N+T)/(N*T))*log((N*T)/(N+T))}
IC_1 = Vectorize(IC_1); curve(IC_1, 0, 20)
IC_2 = function(k){ log(V(k)) + k*((N+T)/(N*T))*log(C_nt_squared)}
IC_2 = Vectorize(IC_2); curve(IC_2, 0, 20)
