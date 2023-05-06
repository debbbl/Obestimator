library(dplyr)
library(readr)

dataset <- read_csv("ObesityDataSet_raw_and_data_sinthetic.csv")
glimpse(dataset) 
summary(dataset)
dataset <- dataset %>%
  #modify column names
  rename(Family_Overweight_History = family_history_with_overweight,
         High_Caloric_Food_Consumption = FAVC,
         Vegetable_Consumption_Frequency = FCVC,
         Daily_meal_intake = NCP,
         Food_Consumption_between_meals = CAEC,
         Smoking = SMOKE,
         Daily_Water_Intake = CH2O,
         Carbonated_Drinks_Consumption = SCC,
         Physical_Activity_Frequency = FAF,
         Technology_Use_Duration = TUE,
         Alcohol_Consumption_Frequency = CALC,
         Mode_of_Transportation = MTRANS,
         Obesity_Level = NObeyesdad) %>%
  
  #modify data
  mutate(Age=floor(Age), 
         Vegetable_Consumption_Frequency = case_when(
           Vegetable_Consumption_Frequency >= 1 & Vegetable_Consumption_Frequency < 2 ~ "never",
           Vegetable_Consumption_Frequency >= 2 & Vegetable_Consumption_Frequency < 3 ~ "sometimes",
           Vegetable_Consumption_Frequency == 3 ~ "always",
         ),
         Daily_meal_intake=case_when(
           Daily_meal_intake >= 1 & Daily_meal_intake < 2 ~ "1",
           Daily_meal_intake >= 2 & Daily_meal_intake < 3 ~ "2",
           Daily_meal_intake == 3 ~ "3",
           Daily_meal_intake > 3 ~ "More than 3",
         ),
         Daily_Water_Intake = case_when(
           Daily_Water_Intake < 1 ~ "Less than a liter",
           Daily_Water_Intake >= 1 & Daily_Water_Intake <= 2 ~ "1 to 2 L",
           Daily_Water_Intake > 2 ~ "More than 2 L"
         ),
         Obesity_Level=case_when(
           Obesity_Level == "Insufficient_Weight" ~ "Underweight",
           Obesity_Level == "Normal_Weight" ~ "Normal_Weight",
           Obesity_Level == "Overweight_Level_I" ~ "Overweight",
           Obesity_Level == "Overweight_Level_II" ~ "Overweight",
           Obesity_Level == "Obesity_Type_I" ~ "Obesity_T1",
           Obesity_Level == "Obesity_Type_II" ~ "Obesity_T2",
           Obesity_Level == "Obesity_Type_III" ~ "Obesity_T3"
         ))
         
glimpse(dataset)
write.csv(dataset,file="cleaned_data1.csv",row.names = FALSE)  
 
