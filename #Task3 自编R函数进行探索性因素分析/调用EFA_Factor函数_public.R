
source(file.choose())

# Ì½Ë÷ĞÔÒòËØ·ÖÎöEFA
# use file.choose() to select a file
data <- read.csv(file.choose(),header=TRUE)
head(data)
data <- data[,2:ncol(data)]
# install.packages('psych')
# install.packages('GPArotation')
a <- EFA_factor(data = data)

# fa.parallel(data, fm = 'minres', fa = 'fa')