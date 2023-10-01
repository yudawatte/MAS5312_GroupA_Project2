# Helper packages
library(caTools)

# Set working directory
setwd("C:/Personal/Books/MSc. in Applied Statistics/Semester_3/MAS5312_Statistical_Machine_Learning/Project_2")

# Loading data sets
df <- read.csv('data_sets/data.csv')

dim(df)
colnames(df)
set.seed(1)
sample <- sample.split(df$Response, SplitRatio = 0.8)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)


dim(df)
dim(train)
dim(test)
write.csv(train,file='data_sets/train.csv', row.names = FALSE)
write.csv(test,file='data_sets/test.csv', row.names = FALSE)
