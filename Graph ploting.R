library(ggplot2)
library(dplyr)
library(RColorBrewer)

df <- read.csv("cleaned_data.csv")
library(plotly)

Age <- ggplot(df, aes(x = Age, fill = Obesity_Level)) +
  geom_histogram(binwidth = 5, alpha = 0.8, position = "dodge") +
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Obesity Level") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal()

ggplotly(Age, tooltip = c("Age","Obesity_Level", "count"))

#Gender vs each obesity type
Gender <- ggplot(df, aes(x = Obesity_Level, fill = Gender)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Gender vs Obesity Level")+
  scale_fill_brewer(palette = "RdYlBu")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

ggplotly(Gender,tooltip = c("Gender","Obesity_Level","count"))
table(df$Gender,df$Obesity_Level)


#Age distribution density plot
# Create a separate data frame for each obesity level
df_obesity_t1 <- subset(df, Obesity_Level == "Obesity_T1")
df_obesity_t2 <- subset(df, Obesity_Level == "Obesity_T2")
df_obesity_t3 <- subset(df, Obesity_Level == "Obesity_T3")

# Plot the age distribution for each obesity level
p <- ggplot() +
  geom_density(data = df_obesity_t1, aes(x = Age, color = "Obesity_T1"), fill = "blue", alpha = 0.2) +
  geom_density(data = df_obesity_t2, aes(x = Age, color = "Obesity_T2"), fill = "green", alpha = 0.2) +
  geom_density(data = df_obesity_t3, aes(x = Age, color = "Obesity_T3"), fill = "red", alpha = 0.2) +
  labs(title = "Age Distribution by Obesity Level", x = "Age", y = "Density", color = "Obesity Level") +
  scale_color_manual(values = c("Obesity_T1" = "blue", "Obesity_T2" = "green", "Obesity_T3" = "red"))

# Convert plot to interactive plot with hover data
ggplotly(p, tooltip = c("Obesity_Level", "Age"))

ggplot() +
  geom_histogram(data = df_obesity_t1, aes(x = Age, fill = "Obesity_T1"), alpha = 0.5, bins = 10) +
  geom_histogram(data = df_obesity_t2, aes(x = Age, fill = "Obesity_T2"), alpha = 0.5, bins = 10) +
  geom_histogram(data = df_obesity_t3, aes(x = Age, fill = "Obesity_T3"), alpha = 0.5, bins = 10) +
  labs(title = "Age Distribution by Obesity Level", x = "Age", y = "Count", fill = "Obesity Level") +
  scale_fill_manual(values = c("Obesity_T1" = "blue", "Obesity_T2" = "green", "Obesity_T3" = "red")) +
  theme(legend.position = "top") +
  geom_label(aes(label = ..count..), stat = "count", size = 3, position = "stack", show.legend = FALSE) +
  facet_wrap(~Obesity_Level, ncol = 1)
# Plot the age distribution for each obesity level with plotly
p <- plot_ly()

p <- p %>% add_trace(data = df_obesity_t1, x = ~Age, type = "histogram", name = "Obesity_T1",
                     marker = list(color = "blue", opacity = 0.5), 
                     text = ~paste("Age: ", Age, "<br>Obesity Level: ", "Obesity_T1"))

p <- p %>% add_trace(data = df_obesity_t2, x = ~Age, type = "histogram", name = "Obesity_T2",
                     marker = list(color = "green", opacity = 0.5), 
                     text = ~paste("Age: ", Age, "<br>Obesity Level: ", "Obesity_T2"))

p <- p %>% add_trace(data = df_obesity_t3, x = ~Age, type = "histogram", name = "Obesity_T3",
                     marker = list(color = "red", opacity = 0.5), 
                     text = ~paste("Age: ", Age, "<br>Obesity Level: ", "Obesity_T3"))

p <- p %>% layout(title = "Age Distribution by Obesity Level", xaxis = list(title = "Age"), yaxis = list(title = "Density"))

# Display the plot
p

#Age vs Obesity Type
age <- ggplot(df, aes(x = Obesity_Level, y = Age)) +
  geom_boxplot(outlier.shape = NA,position = position_dodge(0.75)) +
  stat_summary(fun = mean, geom = "point", aes(shape = "Mean"), size = 0.5, color = "black") +
  stat_summary(fun = median, geom = "line", aes(linetype = "Median"), size = 1.2, color = "black") +
  scale_linetype_manual(name = "Statistics", values = c("Median" = "solid","Mean"=4)) +
  labs(x = "Obesity Level", y = "Age", title = "Age vs Obesity Level") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))
ggplotly(age,tooltip = c("Age","Obesity_Level","count","mean"))


#Family overweight history vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Family_Overweight_History)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Family Overweight History vs Obesity Level")+
  scale_fill_brewer(palette = "Accent")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))


#High Caloric food consumption vs obesity type
ggplot(dataset, aes(x = Obesity_Level, fill = High_Caloric_Food_Consumption)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "High Caloric Food consumption vs Obesity Level")+
  scale_fill_brewer(palette = "BuGn")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))


#Vegetable consumption frequency vs obesity type
table(df$Obesity_Level,df$Vegetable_Consumption_Frequency)
vege <- ggplot(df, aes(x = Obesity_Level, fill = Vegetable_Consumption_Frequency)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Vegetable Consumption Frequency vs Obesity Level")+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

ggplotly(vege,tooltip = c("Obesity_Level","count","Vegetable_Consumption_Frequency"))


#Daily meal intake vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Daily_meal_intake)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Daily Meal Intake vs Obesity Level")+
  scale_fill_brewer(palette = "Pastel1")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))


#Food consumption between meals vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Food_Consumption_between_meals)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Food Consumption between meals vs Obesity Level")+
  scale_fill_brewer(palette = "Pastel2")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

#Smoking vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Smoking)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Smoking vs Obesity Level")+
  scale_fill_brewer(palette = "Reds")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))


#Daily water intake vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Daily_Water_Intake)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Daily Water Intake vs Obesity Level")+
  scale_fill_brewer(palette = "Purples")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

#Carnonated drink vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Carbonated_Drinks_Consumption)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Carbonated Drinks Consumption vs Obesity Level")+
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))


#Mode of Transportation vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Mode_of_Transportation)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Mode of Transportation vs Obesity Level")+
  scale_fill_brewer(palette = "Set3")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

#Alcohol vs obesity type
ggplot(df, aes(x = Obesity_Level, fill = Alcohol_Consumption_Frequency)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Obesity Level", y = "Count", title = "Alcohol Consumption Frequency vs Obesity Level")+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

#physical activity vs obesity type
ggplot(df, aes(x = Physical_Activity_Frequency, fill = Obesity_Level)) +
  geom_density(alpha = 0.5) +
  labs(x = "Physical Activity Frequency", y = "Density", title = "Physical Activity Frequency vs Obesity Type") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

#physical activity 2
ggplot(df, aes(x = Obesity_Level, y = Physical_Activity_Frequency)) +
  geom_boxplot(outlier.shape = NA,position = position_dodge(0.75)) +
  stat_summary(fun = mean, geom = "point", aes(shape = "Mean"), size = 0.5, color = "black") +
  stat_summary(fun = median, geom = "line", aes(linetype = "Median"), size = 1.2, color = "black") +
  scale_linetype_manual(name = "Statistics", values = c("Median" = "solid","Mean"=4)) +
  labs(x = "Obesity Level", y = "Age", title = "Physical Activity Frequency vs Obesity Level") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))


#physical activity 3
ggplot(df, aes(x = Obesity_Level, y = Physical_Activity_Frequency)) +
  geom_violin(scale = "count", trim = FALSE) +
  labs(x = "Obesity Level", y = "Physical Activity Frequency", title = "Physical Activity Frequency vs Obesity Type") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

#physcial activity 4
ggplot(df, aes(x = Obesity_Level, y = Physical_Activity_Frequency, color = Obesity_Level)) +
  geom_point(size = 3, position = position_jitterdodge()) +
  labs(x = "Obesity Level", y = "Physical Activity Frequency", title = "Physical Activity Frequency vs Obesity Type") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))

#Physical Activity frequency vs obesity type
ggplot(cleaned_data, aes(x = Physical_Activity_Frequency, fill = Obesity_Level)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(x = "Physical Activity Frequency", y = "Count") +
  facet_wrap(~Obesity_Level) +
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")) +
  theme_minimal()

#tech use duration vs obesity type
ggplot(df, aes(x = Technology_Use_Duration, fill = Obesity_Level)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(x = "Technology Use Frequency", y = "Count") +
  facet_wrap(~Obesity_Level) +
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")) +
  theme_minimal()

#tech use 2
ggplot(df, aes(x = Obesity_Level, y = Technology_Use_Duration)) +
  geom_boxplot(outlier.shape = NA,position = position_dodge(0.75)) +
  stat_summary(fun = mean, geom = "point", aes(shape = "Mean"), size = 0.5, color = "black") +
  stat_summary(fun = median, geom = "line", aes(linetype = "Median"), size = 1.2, color = "black") +
  scale_linetype_manual(name = "Statistics", values = c("Median" = "solid","Mean"=4)) +
  labs(x = "Obesity Level", y = "Technology Use Duration (hour)", title = "Technology use duration vs Obesity Level") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(legend.position = "right",
        legend.box.just = "right",
        legend.margin = margin(5, 5, 5, 5),
        legend.box.background = element_rect(colour = "black", fill = NA))



