library(dplyr)
library(tidyverse)
library(reshape2)
library(xtable)
library(GGally)


setwd("/Users/zhanranl/Desktop/Wharton/Courses/statME/hw2-chris_lin/")
getwd()

# (a)
# read the data
crime_data <- read_tsv("./Statewide_crime.tsv", col_types = "ifdd")

# population data (online) and cleaning
population_data <- read_csv("./us-state-populations.csv", show_col_types = FALSE)
pop_data_sort <- population_data[order(population_data$code), ]
colnames(pop_data_sort) <- c('STATE','state','Pop')
pop_data_sort <- pop_data_sort %>%
  select(-state)
crime_data$STATE <- pop_data_sort$STATE

# new columns
df <- left_join(crime_data, pop_data_sort, by = "STATE")
df$Violent = as.numeric(df$Violent)
df$Pop = as.numeric(df$Pop)
df <- df %>%
  mutate(CrimeRate = Violent/Pop)


# (b)

# summary
table <- summary(df[, c("CrimeRate", "Metro", "HighSchool", "Poverty")])
latex_table <- xtable(table, caption = "Summary Table")
print(latex_table, include.rownames = FALSE)

# variance
variance_per_column <- sapply(df[, c("CrimeRate", "Metro", "HighSchool", "Poverty")], var)
print(variance_per_column)
variance_per_column <- as.data.frame(variance_per_column)
latex_table1 <- xtable(variance_per_column, caption = "Variance")
print(latex_table1)

# correlation
correlation_matrix <- cor(df[, c("CrimeRate", "Metro", "HighSchool", "Poverty")])
pic1 <- ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  labs(title = "Correlation Matrix") + 
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('./figures-and-tables/3-2-correlation.png', plot = pic1)

pic2 <- ggpairs(df, columns=c(4,5,6,8), aes(color="black")) + 
  ggtitle("Relationships")+
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('./figures-and-tables/3-2-scatterplot.png', plot = pic2)

# (c)

model_M <- lm(CrimeRate ~ Metro, data = df)
latex_tableM <- xtable(summary(model_M), caption = "Variance")
print(xtable(format(latex_tableM)))

model_H <- lm(CrimeRate ~ HighSchool, data = df)
latex_tableH <- xtable(summary(model_H), caption = "Variance")
print(xtable(format(latex_tableH)))

model_P <- lm(CrimeRate ~ Poverty, data = df)
latex_tableP <- xtable(summary(model_P), caption = "Variance")
print(xtable(format(latex_tableP)))

model_all <- lm(CrimeRate ~ Poverty + HighSchool + Metro, data = df)
latex_tableA <- xtable(summary(model_all), caption = "Variance")
print(xtable(format(latex_tableA)))

