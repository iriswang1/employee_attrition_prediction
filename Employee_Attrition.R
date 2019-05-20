# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# April 9, 2018, Siwei Wang
# Explore the relationship between Attrition and other factors
# BIA 650 Group Project
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(tidyverse)

employee_data <- read_csv(file = "/Users/iriswang/Google Drive/Stevens/BIA Spring 2018/BIA 650/Individual project/Group project/WA_Fn-UseC_-HR-Employee-Attrition.csv")

employee_data <- employee_data %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0), 
                                          Gender = if_else(Gender == "Male", 1, 0), 
                                          OverTime = if_else(OverTime == "Yes", 1, 0), 
                                          BusinessTravel = 
                                            if_else(BusinessTravel == "Travel_Frequently", 1, 0),
                                          MaritalStatus = if_else(MaritalStatus == "Married", 1, 0))


#correlation 
x <- employee_data$Attrition
y <- employee_data %>% select(Age, 
                              BusinessTravel, 
                              Education, 
                              MonthlyIncome, 
                              TrainingTimesLastYear, 
                              OverTime,
                              DailyRate,
                              DistanceFromHome,
                              EnvironmentSatisfaction,
                              HourlyRate,
                              JobInvolvement,
                              JobLevel,
                              JobSatisfaction,
                              MonthlyRate,
                              NumCompaniesWorked,
                              PercentSalaryHike,
                              PerformanceRating,
                              RelationshipSatisfaction,
                              StockOptionLevel,
                              TotalWorkingYears,
                              WorkLifeBalance,
                              YearsAtCompany,
                              YearsInCurrentRole,
                              YearsSinceLastPromotion,
                              YearsWithCurrManager)

cor_df <- cor(x, y, method = 'kendall') %>% as.data.frame()
cor_df_t <- t(cor_df)%>% as.data.frame()

cor_df_t$Variables <- row.names(cor_df_t)

names(cor_df_t) <- c("Correlation with Attrition", "Variables")

# 1 means variables we can manipulat, 0 means we cannot
cor_df_t$DV <- as.factor(c(0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0))

#new df with a new column for changable variables


# correlation barchart
library(ggplot2)

employee_correlation_barchart <- ggplot(data = cor_df_t, aes(x = reorder(Variables, -`Correlation with Attrition`), y = `Correlation with Attrition`, fill = DV)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Variables") +
 theme_minimal() +  theme(legend.position='top', legend.title=element_blank()) + 
  scale_fill_discrete (labels=c("Unchangable Variable", "Changable Variable")) # mark the varaibles that can be manipulated

employee_correlation_barchart 

#regression model using the first half of employee data
employee_reg <- read_csv(file = "/Users/iriswang/Google Drive/Stevens/BIA Spring 2018/BIA 650/Individual project/Group project/Data/R_WA_Fn-UseC_-HR-Employee-Attrition.csv")

employee_reg <- employee_reg %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0), 
                                          Gender = if_else(Gender == "Male", 1, 0), 
                                          OverTime = if_else(OverTime == "Yes", 1, 0), 
                                          BusinessTravel = 
                                            if_else(BusinessTravel == "Travel_Frequently", 1, 0),
                                          MaritalStatus = if_else(MaritalStatus == "Married", 1, 0))

reg_model <- lm(Attrition ~ BusinessTravel + MonthlyIncome + TrainingTimesLastYear + OverTime + EnvironmentSatisfaction + JobLevel + StockOptionLevel, data = employee_reg)
summary(reg_model)


#k-means clustering for O_data
library(dplyr)
employee_data_cluster <- read_csv(file = "/Users/iriswang/Google Drive/Stevens/BIA Spring 2018/BIA 650/Individual project/Group project/Data/O_WA_Fn-UseC_-HR-Employee-Attrition.csv")

employee_data_cluster <- employee_data_cluster %>% mutate(Attrition = if_else(Attrition == "Yes", 1, 0), 
                                          Gender = if_else(Gender == "Male", 1, 0), 
                                          OverTime = if_else(OverTime == "Yes", 1, 0), 
                                          BusinessTravel = 
                                            if_else(BusinessTravel == "Travel_Frequently", 1, 0),
                                          MaritalStatus = if_else(MaritalStatus == "Married", 1, 0))

# correct cluster analysis
employee_data_var_to_cluster <- employee_data_cluster %>% select(OverTime, BusinessTravel, TrainingTimesLastYear, MonthlyIncome, StockOptionLevel)
cluster_solution <- kmeans(employee_data_var_to_cluster, centers = 4)

employee_data_var_to_cluster$cluster <- cluster_solution$cluster
# radar chart
library(radarchart)
library(scales)
employee_radar_data <- employee_data_var_to_cluster %>% group_by(cluster) %>% summarise_all(mean) %>% ungroup()
employee_radar_data <- employee_radar_data %>% mutate_at(vars(-cluster),funs(rescale), to=c(1,5)) %>% mutate(cluster = as.character(cluster)) %>% select(-cluster) %>% t %>% data.frame()
employee_radar_data <- rownames_to_column(employee_radar_data)
employee_radar_data
chartJSRadar(scores = employee_radar_data , maxScale = 5, showToolTipLabel=TRUE)


