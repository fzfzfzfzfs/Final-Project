pacman::p_load(tidyverse, openxlsx, GauPro)

set.seed(2024)
# Read the data into R
data <- read.xlsx("2024-03-19-WIF-tis4d.xlsx")

# Clean the data
data$cell_line <- gsub("CELL-TYPE 101", "Cell-type 101", data$cell_line)
data$cell_line <- gsub("WILD-TYPE", "Wild-type", data$cell_line)
data$treatment <- gsub("activating factor 42", "Activating factor 42", data$treatment)
data$treatment <- gsub("placebo", "Placebo", data$treatment)
data$name <- gsub("Gl-Rjs", "GL-rjS", data$name)
data$name <- gsub("Gl-Xib", "GL-XIb", data$name)
data$name <- gsub("Gl-Zhw", "GL-ZHw", data$name)
data$name <- gsub("Gl-Cwn", "GL-cwN", data$name)

# Plot all the data
x = 0:1:10
XIb = data$gene_expression[data$name == "GL-XIb"]
cDZ = data$gene_expression[data$name == "GL-cDZ"]
rjS = data$gene_expression[data$name == "GL-rjS"]
Xik = data$gene_expression[data$name == "GL-Xik"]
cwN = data$gene_expression[data$name == "GL-cwN"]
kYH = data$gene_expression[data$name == "GL-kYH"]
ZHw = data$gene_expression[data$name == "GL-ZHw"]
MFA = data$gene_expression[data$name == "GL-MFA"]


# GL-XIb
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), XIb, kernel=kern, parallel=FALSE)
plot(gpk)

# GL-cDZ
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), cDZ, kernel=kern, parallel=FALSE)
plot(gpk)

# GL-rjS
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), rjS, kernel=kern, parallel=FALSE)
plot(gpk)

# GL-Xik
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), Xik, kernel=kern, parallel=FALSE)
plot(gpk)

# GL-cwN
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), cwN, kernel=kern, parallel=FALSE)
plot(gpk)

# GL-kYH
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), kYH, kernel=kern, parallel=FALSE)
plot(gpk)

# GL-ZHw
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), ZHw, kernel=kern, parallel=FALSE)
plot(gpk)

# GL-MFA
kern <- Matern52$new(0)
gpk <- GauPro_kernel_model$new(matrix(x, ncol=1), MFA, kernel=kern, parallel=FALSE)
plot(gpk)
