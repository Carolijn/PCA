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

#taking first differences
data <- diff(data)

#standardize the X variables
Xt = data[,2:length(data[2,])]
Xt <- scale(Xt)

# Create variable for GDP growth
real_gdp_growth <- regts(c(NA,data[2:end,"YBMAN___"]-data[1:end-1,"YBMAN___"]),start="2001q1")

#principal component analysis
prcomp(Xt)
pca <- prcomp(~ ., data=Xt, na.action=na.omit, scale=TRUE)
summary(pca)
plot(pca)
