# Helper packages
library(tidyverse)
library(dplyr)

# Set working directory
setwd("C:/Personal/Books/MSc. in Applied Statistics/Semester_3/MAS5312_Statistical_Machine_Learning/Project_2")

# Loading data sets
train_df <- read.csv('data_sets/train.csv')
test_df <- read.csv('data_sets/test.csv')
region_cat_df <- read.csv('data_sets/region_categories.csv')
sal_ch_cat_df <- read.csv('data_sets/sales_channel_categories.csv')

# Label encoding existing categories
train_df <- train_df %>%  mutate(
  id = as.character(id),
  Gender = ifelse(Gender == "Male", 0, 1),
  Vehicle_Damage = ifelse(Vehicle_Damage == "Yes", 1, 0),
  Vehicle_Age = case_when(
    Vehicle_Age == "< 1 Year" ~ 0,
    Vehicle_Age == "1-2 Year" ~ 1,
    Vehicle_Age == "> 2 Years" ~ 2
  )) 

test_df <- test_df %>%  mutate(
  id = as.character(id),
  Gender = ifelse(Gender == "Male", 0, 1),
  Vehicle_Damage = ifelse(Vehicle_Damage == "Yes", 1, 0),
  Vehicle_Age = case_when(
    Vehicle_Age == "< 1 Year"~0,
    Vehicle_Age == "1-2 Year"~1,
    Vehicle_Age == "> 2 Years"~2
  ))

# Introduce region categories to both train set and test set
region_cat_df <- select(region_cat_df, -c("responses","positive_rate"))

train_df <- train_df %>% left_join(region_cat_df, 
                             by=c('Region_Code'))

test_df <- test_df %>% left_join(region_cat_df,
                                 by=c('Region_Code'))

# Check for null records after merging
sum(is.na(test_df))

# Introduce sales channel categories to both train set and test set
sal_ch_cat_df <- select(sal_ch_cat_df, -c("responses","positive_rate"))

train_df <- train_df %>% left_join(sal_ch_cat_df, 
                                   by=c('Policy_Sales_Channel'))

test_df <- test_df %>% left_join(sal_ch_cat_df,
                                 by=c('Policy_Sales_Channel'))

# Check for null records after merging
sum(is.na(test_df))

test_df <- test_df %>% drop_na()

# Introduce gender specific age categories
# Modify train set
train_df_m <- train_df %>% filter(Gender==0)
print("Age quartiles for males")
print(quantile(train_df_m$Age))

train_df_m <- train_df_m %>%  mutate(
  Age_Category = case_when(
    Age <= 26 ~ 0,
    Age <= 41 ~ 1,
    Age <= 52 ~ 2,
    TRUE ~ 3
  ))

train_df_f <- train_df %>% filter(Gender==1)
print("Age quartiles for females")
print(quantile(train_df_f$Age))

train_df_f <- train_df_f %>%  mutate(
  Age_Category = case_when(
    Age <= 24 ~ 0,
    Age <= 30 ~ 1,
    Age <= 46 ~ 2,
    TRUE ~ 3
  ))

train_df = rbind(train_df_m,train_df_f)

# Modify test set
test_df_m <- test_df %>% filter(Gender==0)
test_df_m <- test_df_m %>%  mutate(
  Age_Category = case_when(
    Age <= 26 ~ 0,
    Age <= 41 ~ 1,
    Age <= 52 ~ 2,
    TRUE ~ 3
  ))

test_df_f <- test_df %>% filter(Gender==1)
test_df_f <- test_df_f %>%  mutate(
  Age_Category = case_when(
    Age <= 24 ~ 0,
    Age <= 30 ~ 1,
    Age <= 46 ~ 2,
    TRUE ~ 3
  ))

test_df = rbind(test_df_m,test_df_f)

# Drop unnecessary columns
colnames(train_df)
train_df <- select(train_df, -c("id","Age","Region_Code","Policy_Sales_Channel"))
train_df <- train_df %>% relocate(Response)

colnames(test_df)
test_df <- select(test_df, -c("id","Age","Region_Code","Policy_Sales_Channel"))
test_df <- test_df %>% relocate(Response)

# Save preprocessed data (both train set and test set)
write.csv(train_df,file='data_sets/train_prep.csv', row.names = FALSE)
write.csv(test_df,file='data_sets/test_prep.csv', row.names = FALSE)
