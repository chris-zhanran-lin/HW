library(tidyverse)
library(kableExtra)
library(xtable)

# path
setwd("/Users/zhanranl/Desktop/Wharton/Courses/statME/hw1-chris_lin/")
getwd()

# (a)
# read the data
data <- read_tsv("./Anorexia.dat", col_types = "ifdd")

# (1) remove the subj variable
data <- subset(data, select = -subj)

# (2) re-coding
data$therapy <- gsub('b', 'behaviour', data$therapy)
data$therapy <- gsub('f', 'family', data$therapy)
data$therapy <- gsub('c', 'control', data$therapy)

# (3) rename
data <- data %>%
  rename(weight_before = before,
         weight_after = after)
# (4) adding a variable
data <- data %>%
  mutate(weight_gain = weight_after - weight_before)

#print
print(data)
# latex_table <- xtable(data, caption = "Summary Table")
# print(latex_table, include.rownames = FALSE)

# (b)
# (1) boxplot
boxplot <- ggplot(data, aes(x = therapy, y = weight_gain, fill = therapy)) + geom_boxplot() + labs(x = 'Therapy', y = 'Weight_gain')
ggsave(filename = './figures-and-tables/boxplot.png', plot = boxplot)

# (2) scatterplot
ggplot(data, aes(x = weight_before, y = weight_gain, color = therapy)) + 
  geom_point() +
  labs(x = 'Weight_before', y = 'Weight_gain')
ggsave('./figures-and-tables/scatterplot.png')

# (3) table
table <- data %>%
  group_by(therapy) %>%
  summarise(
    Mean_Weight_Gain = mean(weight_gain),
    Max_Weight_Gain = max(weight_gain),
    Fraction_Girls = mean(weight_gain > 0)
  )
print(table)
latex_table <- xtable(table, caption = "Summary Table")
print(latex_table, include.rownames = FALSE)

# weight_gain
max_weight_gain <- max(data$weight_gain)
girls_with_most_weight_gain <- data[data$weight_gain == max_weight_gain, ]


# (c) summary
model <- lm(weight_gain ~ therapy, data = data)
print(model)
latex_table <- xtable(model, caption = "Summary Table")
print(latex_table, include.rownames = FALSE)

# 
data$therapy <- factor(data$therapy, levels = c("control","behaviour","family"))
model_1 <- lm(weight_gain ~ therapy, data = data)
print(model_1)
latex_table <- xtable(model_1, caption = "Summary Table")
print(latex_table, include.rownames = FALSE)

# (d)
mean <- mean(data$weight_gain)
SST <- sum((data$weight_gain - mean)^2)
group_mean <- tapply(data$weight_gain, data$therapy, mean)
SSB <- sum((group_mean - mean)^2 * table(data$therapy))
SSW <- sum((data$weight_gain - ave(data$weight_gain, data$therapy))^2)
# verification
print(c(SSB, SSW, SST, SSB + SSW - SST))
print(SSB/SST)
