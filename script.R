library(openxlsx)
library(ggplot2)
library(corrplot)
database <-  read.xlsx("Data 1.xlsx", sheet = 1) 
str(database)
database$Country <- as.factor(database$Country)
num_data <- database
num_data$Country <- as.integer(database$Country)
num_data$Numn. <- NULL
corr.matrix <- cor(num_data)
corrplot.mixed(corr.matrix, tl.cex=0.75, number.cex=0.75)
UK <- subset(database, Country  == "UK")
Us <- subset(database, Country  == "US")
NL <- subset(database, Country  == "NL")
mean_value <- mean(UK$Duration.of.employment, na.rm = TRUE)
median_value <- median(UK$Duration.of.employment, na.rm = TRUE)
ggplot(UK, aes(x = Duration.of.employment)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Duration of Employment in UK",x = "Duration of Employment") +
  theme_minimal()
mean_value <- mean(Us$Duration.of.employment, na.rm = TRUE)
median_value <- median(Us$Duration.of.employment, na.rm = TRUE)
ggplot(Us, aes(x = Duration.of.employment)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Duration of Employment in US",x = "Duration of Employment") +
  theme_minimal()
mean_value <- mean(NL$Duration.of.employment, na.rm = TRUE)
median_value <- median(NL$Duration.of.employment, na.rm = TRUE)
ggplot(NL, aes(x = Duration.of.employment)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Duration of Employment in NL",x = "Duration of Employment") +
  theme_minimal()
mean_value <- mean(UK$`Work/Life.Balance`, na.rm = TRUE)
median_value <- median(UK$`Work/Life.Balance`, na.rm = TRUE)
ggplot(UK, aes(x = `Work/Life.Balance`)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Work/Life.Balance in UK",x = "Work/Life.Balance") +
  theme_minimal()
mean_value <- mean(Us$`Work/Life.Balance`, na.rm = TRUE)
median_value <- median(Us$`Work/Life.Balance`, na.rm = TRUE)
ggplot(UK, aes(x = `Work/Life.Balance`)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Work/Life.Balance in Us",x = "Work/Life.Balance") +
  theme_minimal()
mean_value <- mean(NL$`Work/Life.Balance`, na.rm = TRUE)
median_value <- median(NL$`Work/Life.Balance`, na.rm = TRUE)
ggplot(NL, aes(x = `Work/Life.Balance`)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Work/Life.Balance in NL",x = "Work/Life.Balance") +
  theme_minimal()
mean_value <- mean(UK$Culture.and.values, na.rm = TRUE)
median_value <- median(UK$Culture.and.values, na.rm = TRUE)
ggplot(UK, aes(x = Culture.and.values)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Culture and Values in UK",x = "Culture and Values") +
  theme_minimal()
mean_value <- mean(US$Culture.and.values, na.rm = TRUE)
median_value <- median(US$Culture.and.values, na.rm = TRUE)
ggplot(US, aes(x = Culture.and.values)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Culture and Values in US",x = "Culture and Values") +
  theme_minimal()
mean_value <- mean(NL$Culture.and.values, na.rm = TRUE)
median_value <- median(NL$Culture.and.values, na.rm = TRUE)
ggplot(NL, aes(x = Culture.and.values)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Culture and Values in NL",x = "Culture and Values") +
  theme_minimal()
mean_value <- mean(UK$Diversity.and.Inclusion, na.rm = TRUE)
median_value <- median(UK$Diversity.and.Inclusion, na.rm = TRUE)
ggplot(UK, aes(x = Diversity.and.Inclusion)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Diversity and Inclusion UK",x = "Diversity and Inclusion") +
  theme_minimal()
mean_value <- mean(Us$Diversity.and.Inclusion, na.rm = TRUE)
median_value <- median(Us$Diversity.and.Inclusion, na.rm = TRUE)
ggplot(Us, aes(x = Diversity.and.Inclusion)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Diversity and Inclusion US",x = "Diversity and Inclusion") +
  theme_minimal()
mean_value <- mean(NL$Diversity.and.Inclusion, na.rm = TRUE)
median_value <- median(NL$Diversity.and.Inclusion, na.rm = TRUE)
ggplot(NL, aes(x = Diversity.and.Inclusion)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Diversity and Inclusion NL",x = "Diversity and Inclusion") +
  theme_minimal()
mean_value <- mean(UK$Career.Opportunities, na.rm = TRUE)
median_value <- median(UK$Career.Opportunities, na.rm = TRUE)
ggplot(UK, aes(x = Career.Opportunities)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Career Opportunities in  UK",x = "Career Opportunities") +
  theme_minimal()
mean_value <- mean(Us$Career.Opportunities, na.rm = TRUE)
median_value <- median(Us$Career.Opportunities, na.rm = TRUE)
ggplot(Us, aes(x = Career.Opportunities)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Career Opportunities in  US",x = "Career Opportunities") +
  theme_minimal()
mean_value <- mean(NL$Career.Opportunities, na.rm = TRUE)
median_value <- median(NL$Career.Opportunities, na.rm = TRUE)
ggplot(NL, aes(x = Career.Opportunities)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Career Opportunities in  NL",x = "Career Opportunities") +
  theme_minimal()
mean_value <- mean(UK$Compensation.and.benefits, na.rm = TRUE)
median_value <- median(UK$Compensation.and.benefits, na.rm = TRUE)
ggplot(UK, aes(x = Compensation.and.benefits)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Compensationv and Benefits in  UK",x = "Compensation and Benefits") +
  theme_minimal()
mean_value <- mean(Us$Compensation.and.benefits, na.rm = TRUE)
median_value <- median(Us$Compensation.and.benefits, na.rm = TRUE)
ggplot(Us, aes(x = Compensation.and.benefits)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Compensationv and Benefits in  US",x = "Compensation and Benefits") +
  theme_minimal()
mean_value <- mean(NL$Compensation.and.benefits, na.rm = TRUE)
median_value <- median(NL$Compensation.and.benefits, na.rm = TRUE)
ggplot(NL, aes(x = Compensation.and.benefits)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Compensationv and Benefits in  NL",x = "Compensation and Benefits") +
  theme_minimal()
mean_value <- mean(UK$Senior.Management, na.rm = TRUE)
median_value <- median(UK$Senior.Management, na.rm = TRUE)
ggplot(UK, aes(x = Senior.Management)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Senior Management in  UK",x = "Senior.Management") +
  theme_minimal()
mean_value <- mean(Us$Senior.Management, na.rm = TRUE)
median_value <- median(Us$Senior.Management, na.rm = TRUE)
ggplot(Us, aes(x = Senior.Management)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Senior Management in  US",x = "Senior.Management") +
  theme_minimal()
mean_value <- mean(NL$Senior.Management, na.rm = TRUE)
median_value <- median(NL$Senior.Management, na.rm = TRUE)
ggplot(NL, aes(x = Senior.Management)) +
  geom_density(fill = "lightblue", alpha = 0.5)  + 
  geom_vline(xintercept = mean_value, linetype = "dashed", color = "red", size = 1) +  # Mean line
  geom_vline(xintercept = median_value, linetype = "dotted", color = "green", size = 1) +  # Median line
  labs(title = "Senior Management in  NL",x = "Senior.Management") +
  theme_minimal()