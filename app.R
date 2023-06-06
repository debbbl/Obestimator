library(dplyr)
library(e1071)
library(glmnet)
library(randomForest)
library(readr)
library(caret)
library(nnet)
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(gridExtra)
library(patchwork)
library(shinythemes)

#read the dataset
data <- read.csv("cleaned_data1.csv")
unique(data$Gender)
unique(data$Family_Overweight_History)
unique(data$High_Caloric_Food_Consumption)
unique(data$Vegetable_Consumption_Frequency)
unique(data$Daily_meal_intake)
unique(data$Food_Consumption_between_meals)
unique(data$Smoking)
unique(data$Daily_meal_intake)
unique(data$Carbonated_Drinks_Consumption)
unique(data$Alcohol_Consumption_Frequency)
unique(data$Mode_of_Transportation)
unique(data$Obesity_Level)

#Data pre-processing
# Convert character columns to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

#feature scaling
num_vars <- c("Age","Physical_Activity_Frequency","Technology_Use_Duration")
data[num_vars] <- lapply(data[num_vars], scale)

index = sample(2, nrow(data), replace=TRUE, prob = c(0.7,0.3)) 
#70% of data is assign to the training set and 30% is assign to the testing set#  

Training = data[index==1,]
Testing = data[index==2,]
Testing = select(Testing, -Height, -Weight, -Obesity_Level)

RFM = randomForest(Obesity_Level ~ Gender + Age + Family_Overweight_History + 
                     High_Caloric_Food_Consumption + Daily_meal_intake +
                     Vegetable_Consumption_Frequency + 
                     Food_Consumption_between_meals + Smoking + 
                     Daily_Water_Intake + Carbonated_Drinks_Consumption 
                   + Physical_Activity_Frequency + Technology_Use_Duration + 
                     Alcohol_Consumption_Frequency + Mode_of_Transportation, data = Training)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Obestimator"),
  sidebarLayout(
    sidebarPanel(
      # get user input
      selectInput("gender", "Gender:", choices = unique(data$Gender)),
      numericInput("age", "Age:", value = ""),
      selectInput("family_overweight_history","Family Overweight History:",choices = unique(data$Family_Overweight_History)),
      selectInput(inputId = "high_caloric_food_consumption", "High Caloric Food Consumption:", choices = unique(data$High_Caloric_Food_Consumption)),
      selectInput(inputId = "vegetable_consumption_frequency", "Vegetable Consumption Frequency:", choices = unique(data$Vegetable_Consumption_Frequency)),
      selectInput(inputId = "daily_meal_intake", "Daily meal intake:", choices = unique(data$Daily_meal_intake)),
      selectInput(inputId = "food_consumption_between_meals", "Food consumption between meals:", choices = unique(data$Food_Consumption_between_meals)),
      selectInput(inputId = "smoking", "Smoking:", choices = unique(data$Smoking)),
      selectInput(inputId = "daily_water_intake", "Daily Water Intake:", choices = unique(data$Daily_Water_Intake)),
      selectInput(inputId = "carbonated_drinks_consumption", "Carbonated Drink Consumption:", choices = unique(data$Carbonated_Drinks_Consumption)),
      numericInput(inputId = "physical_activity_frequency", "Physical Activity Frequency", value = ""),
      numericInput(inputId = "technology_use_duration", "Technology use duration:", value = ""),
      selectInput(inputId = "alcohol_consumption_frequency", "Alcohol Consumption Frequency:", choices = unique(data$Alcohol_Consumption_Frequency)),
      selectInput(inputId = "mode_of_transportation", "Mode of Transportation:", choices = unique(data$Mode_of_Transportation)),
      br(),
      actionButton("enter", label = "Predict")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  # tab1 - results and suggestions
                  tabPanel("Result and Suggestions", br(),
                           verbatimTextOutput("prediction_output")
                  ),
                  
                  # tab2 - obesity information
                  tabPanel("General Information of Obesity",
                           br(),
                           h3("General Information of Obesity"),
                           p("Obesity is a medical condition characterized by an excessive accumulation of body fat. It is a complex disorder influenced by various factors, including genetics, lifestyle, and environmental factors. Obesity is commonly measured using the body mass index (BMI), which is calculated by dividing a person's weight in kilograms by the square of their height in meters."),
                           p("Causes of Obesity:"),
                           tags$ul(
                             tags$li("Genetics and family history: Some individuals may have a genetic predisposition to obesity, making it more challenging for them to maintain a healthy weight."),
                             tags$li("Unhealthy diet high in calories and fat: Consuming an excess of calorie-dense foods, especially those high in fat and sugar, can contribute to weight gain."),
                             tags$li("Lack of physical activity: Leading a sedentary lifestyle with little to no physical activity can lead to weight gain and obesity."),
                             tags$li("Sedentary lifestyle: Spending excessive time sitting or engaging in activities that require minimal movement can contribute to weight gain."),
                             tags$li("Certain medical conditions and medications: Some medical conditions, such as hypothyroidism or polycystic ovary syndrome (PCOS), can increase the risk of obesity. Certain medications, such as antidepressants or corticosteroids, may also lead to weight gain."),
                             tags$li("Psychological factors and emotional eating: Emotional factors, such as stress, anxiety, or depression, can contribute to overeating or engaging in unhealthy eating habits."),
                             tags$li("Environmental factors, such as easy access to unhealthy foods: Living in an environment with easy access to high-calorie, low-nutrient foods can contribute to unhealthy eating habits and weight gain.")
                           ),
                           p("Prevention and Management of Obesity:"),
                           tags$ol(
                             tags$li("Adopting a balanced and nutritious diet with portion control: Consuming a variety of nutrient-dense foods in appropriate portions can help maintain a healthy weight."),
                             tags$li("Incorporating regular physical activity into daily routine: Engaging in regular exercise, such as brisk walking, cycling, or swimming, can help burn calories and improve overall fitness."),
                             tags$li("Limiting consumption of sugary and high-fat foods: Reducing the intake of foods and beverages high in added sugars and unhealthy fats can contribute to weight management."),
                             tags$li("Making healthy lifestyle choices, such as getting enough sleep and managing stress: A healthy lifestyle that includes adequate sleep and stress management can support weight management."),
                             tags$li("Seeking professional help, such as a registered dietitian or healthcare provider, for personalized guidance: Consulting with a healthcare professional can provide individualized advice and support for weight management."),
                             tags$li("Building a support system and engaging in community programs or support groups: Surrounding oneself with a supportive network and participating in community programs or support groups can provide motivation and accountability."),
                             tags$li("Regularly monitoring and managing weight and overall health: Keeping track of weight, body measurements, and health indicators can help identify changes and take appropriate actions for weight management.")
                           ),
                           p("It's important to consult with a healthcare professional for personalized advice and guidance on managing obesity.")
                  ),
                  
                  tabPanel("Raw Dataset",
                           br(),
                           h3("Raw Dataset"),
                           tableOutput(outputId = "raw_dataset")
                  ),
                  
                  # tab4 - graph
                  tabPanel("Graph", 
                           fluidRow(
                             plotOutput("plot1")
                           ),
                           fluidRow(
                             plotOutput("plot2")
                           ),
                           fluidRow(
                             plotOutput("plot3")
                           ),
                           fluidRow(
                             plotOutput("plot4")
                           ),
                           fluidRow(
                             plotOutput("plot5")
                           ),
                           fluidRow(
                             plotOutput("plot6")
                           ),
                           fluidRow(
                             plotOutput("plot7")
                           ),
                           fluidRow(
                             plotOutput("plot8")
                           ),
                           fluidRow(
                             plotOutput("plot9")
                           ),
                           fluidRow(
                             plotOutput("plot10")
                           ),
                           fluidRow(
                             plotOutput("plot11")
                           ),
                           fluidRow(
                             plotOutput("plot12")
                           ),
                           fluidRow(
                             plotOutput("plot13")
                           ),
                           fluidRow(
                             plotOutput("plot14")
                           )
                  ),
                  
                  tabPanel("BMI Calculator", br(),
                           numericInput(inputId = "height", "Height (cm):", value = ""),
                           numericInput(inputId = "weight", "Weight (kg):", value = ""),
                           br(),
                           h2("Your BMI is:"),
                           actionButton(inputId = "calculate_bmi", label = "Calculate BMI"),
                           verbatimTextOutput(outputId = "bmi_output"),
                           br(),
                           h4("What does your BMI mean?"),
                           p("Body Mass Index (BMI) is a measure of body fat based on an individual's height and weight. Here are the general BMI categories:"),
                           p("Underweight: BMI less than 18.5"),
                           p("Normal weight: BMI between 18.5 and 24.9"),
                           p("Overweight: BMI between 25 and 29.9"),
                           p("Obese (Class I): BMI between 30 and 34.9"),
                           p("Obese (Class II): BMI between 35 and 39.9"),
                           p("Obese (Class III): BMI 40 or higher")
                  )
                  
      )
    )
  )
)

df <- read_csv("cleaned_data1.csv")

server <- function(input, output) {
  # Perform prediction on user input
  observeEvent(input$enter, {
    # Suppress warnings within this block of code
    suppressWarnings({
      # Validate input values
      if (is.na(input$age) || !is.numeric(input$age)) {
        return(showNotification("Invalid input: Age must be a numeric value.", type = "warning"))
      }
      if (is.na(input$physical_activity_frequency) || !is.numeric(input$physical_activity_frequency)) {
        return(showNotification("Invalid input: Physical Activity Frequency must be a numeric value.", type = "warning"))
      }
      if (is.na(input$technology_use_duration) || !is.numeric(input$technology_use_duration)) {
        return(showNotification("Invalid input: Technology Use Duration must be a numeric value.", type = "warning"))
      }
      
      # Prepare the input data for prediction
      prediction_data <- data.frame(
        Gender = factor(input$gender, levels = unique(data$Gender)),
        Age = as.numeric(input$age),
        Family_Overweight_History = factor(input$family_overweight_history, levels = unique(data$Family_Overweight_History)),
        High_Caloric_Food_Consumption = factor(input$high_caloric_food_consumption, levels = unique(data$High_Caloric_Food_Consumption)),
        Vegetable_Consumption_Frequency = factor(input$vegetable_consumption_frequency, levels = unique(data$Vegetable_Consumption_Frequency)),
        Daily_meal_intake = factor(input$daily_meal_intake, levels = unique(data$Daily_meal_intake)),
        Food_Consumption_between_meals = factor(input$food_consumption_between_meals, levels = unique(data$Food_Consumption_between_meals)),
        Smoking = factor(input$smoking, levels = unique(data$Smoking)),
        Daily_Water_Intake = factor(input$daily_water_intake, levels = unique(data$Daily_Water_Intake)),
        Carbonated_Drinks_Consumption = factor(input$carbonated_drinks_consumption, levels = unique(data$Carbonated_Drinks_Consumption)),
        Alcohol_Consumption_Frequency = factor(input$alcohol_consumption_frequency, levels = unique(data$Alcohol_Consumption_Frequency)),
        Mode_of_Transportation = factor(input$mode_of_transportation, levels = unique(data$Mode_of_Transportation)),
        Physical_Activity_Frequency = as.numeric(input$physical_activity_frequency),
        Technology_Use_Duration = as.numeric(input$technology_use_duration),
        stringsAsFactors = FALSE
      )
      
      # Scale numerical variables
      numeric_vars <- c("Age", "Physical_Activity_Frequency", "Technology_Use_Duration")
      prediction_data[, numeric_vars] <- lapply(prediction_data[, numeric_vars], function(x) {
        (x - min(data[, numeric_vars], na.rm = TRUE)) / (max(data[, numeric_vars], na.rm = TRUE) - min(data[, numeric_vars], na.rm = TRUE))
      })
      
      # Perform prediction using the Random Forest model
      prediction <- predict(RFM, newdata = prediction_data)
      
      output$prediction_output <- renderText({
        if (is.na(prediction) || prediction == "Unknown") {
          return("Apologies, but the prediction could not be made due to missing data or an unknown obesity level. Please ensure all input fields are filled correctly.")
        }
        
        suggestion <- ""
        
        if (prediction == "Normal_Weight") {
          suggestion <- "You have a normal weight. Keep up the good work! Here are some suggestions to maintain a healthy lifestyle:\n\n"
          # Add suggestions for maintaining a healthy lifestyle for normal weight individuals
          suggestion <- paste(suggestion, "- Continue to eat a balanced diet that includes a variety of fruits, vegetables, whole grains, lean proteins, and healthy fats.\n", sep = "")
          suggestion <- paste(suggestion, "- Engage in regular physical activity to support overall health and well-being.\n", sep = "")
          suggestion <- paste(suggestion, "- Stay hydrated by drinking an adequate amount of water throughout the day.\n", sep = "")
          suggestion <- paste(suggestion, "- Practice portion control and mindful eating to maintain a healthy weight.\n", sep = "")
        } else if (prediction == "Overweight") {
          suggestion <- "You are overweight. It's important to make significant lifestyle changes to improve your health. Here are some suggestions:\n\n"
          # Add suggestions for overweight individuals
          suggestion <- paste(suggestion, "- Work with a healthcare professional or registered dietitian to develop a comprehensive weight management plan.\n", sep = "")
          suggestion <- paste(suggestion, "- Adopt a balanced and calorie-controlled eating plan that focuses on whole, unprocessed foods.\n", sep = "")
          suggestion <- paste(suggestion, "- Increase your physical activity level to promote weight loss. Aim for at least 150-300 minutes of moderate-intensity aerobic activity per week.\n", sep = "")
          suggestion <- paste(suggestion, "- Incorporate strength training exercises to build muscle mass and increase metabolism.\n", sep = "")
          suggestion <- paste(suggestion, "- Practice portion control and mindful eating. Pay attention to hunger and fullness cues.\n", sep = "")
          suggestion <- paste(suggestion, "- Seek support from healthcare professionals, registered dietitians, or weight management programs to help you achieve your goals.\n", sep = "")
          suggestion <- paste(suggestion, "- Stay motivated by setting realistic and achievable targets. Celebrate small victories along the way.\n", sep = "")
        } else if (prediction == "Obesity_T1") {
          suggestion <- paste(suggestion, "You have obesity level 1. It's important to adopt healthy lifestyle habits and make positive changes. Here are some suggestions:\n\n", sep = "")
          # Add suggestions for obesity level 1 individuals
          suggestion <- paste(suggestion, "- Focus on a balanced and calorie-controlled diet that includes whole, unprocessed foods.\n", sep = "")
          suggestion <- paste(suggestion, "- Increase your physical activity level to promote weight loss and improve cardiovascular health.\n", sep = "")
          suggestion <- paste(suggestion, "- Incorporate strength training exercises to build muscle mass and boost metabolism.\n", sep = "")
          suggestion <- paste(suggestion, "- Practice portion control and mindful eating. Be aware of emotional eating triggers.\n", sep = "")
          suggestion <- paste(suggestion, "- Seek support from healthcare professionals, registered dietitians, or weight management programs for personalized guidance.\n", sep = "")
          suggestion <- paste(suggestion, "- Stay motivated by setting realistic goals and tracking your progress.\n", sep = "")
        } else if (prediction == "Obesity_T2") {
          suggestion <- paste(suggestion, "You have obesity level 2. It's crucial to make significant changes to improve your health. Here are some suggestions:\n\n", sep = "")
          # Add suggestions for obesity level 2 individuals
          # Add suggestions for obesity level 2 individuals
          suggestion <- paste(suggestion, "- Work with healthcare professionals or registered dietitians to create a comprehensive weight management plan.\n", sep = "")
          suggestion <- paste(suggestion, "- Adopt a well-balanced, calorie-controlled diet that emphasizes whole, unprocessed foods and limits added sugars and saturated fats.\n", sep = "")
          suggestion <- paste(suggestion, "- Increase your physical activity level significantly to promote weight loss and improve overall fitness. Aim for at least 150-300 minutes of moderate-intensity aerobic activity per week.\n", sep = "")
          suggestion <- paste(suggestion, "- Incorporate strength training exercises into your routine to build lean muscle mass and boost metabolism.\n", sep = "")
          suggestion <- paste(suggestion, "- Practice portion control and mindful eating. Pay attention to hunger and fullness cues.\n", sep = "")
          suggestion <- paste(suggestion, "- Seek support from healthcare professionals, registered dietitians, or weight management programs to address underlying factors contributing to obesity and provide guidance and accountability.\n", sep = "")
          suggestion <- paste(suggestion, "- Stay motivated by setting realistic and achievable goals. Celebrate progress and milestones along the way.\n", sep = "")
        } else if (prediction == "Obesity_T3") {
          suggestion <- paste(suggestion, "You have obesity level 3. It's crucial to prioritize your health and make significant lifestyle changes. Here are some suggestions:\n\n", sep = "")
          # Add suggestions for obesity level 3 individuals
          suggestion <- paste(suggestion, "- Consult with healthcare professionals or registered dietitians for personalized guidance and support in managing your condition.\n", sep = "")
          suggestion <- paste(suggestion, "- Implement a well-structured, calorie-controlled diet plan under professional supervision.\n", sep = "")
          suggestion <- paste(suggestion, "- Incorporate regular physical activity that includes both aerobic exercises and strength training to support weight loss and improve overall fitness.\n", sep = "")
          suggestion <- paste(suggestion, "- Seek counseling or support groups to address emotional and psychological aspects related to obesity.\n", sep = "")
          suggestion <- paste(suggestion, "- Consider medical interventions or surgical options, if recommended by healthcare professionals, as a part of a comprehensive treatment plan.\n", sep = "")
          suggestion <- paste(suggestion, "- Engage in ongoing monitoring and follow-up with healthcare professionals to ensure progress and make necessary adjustments.\n", sep = "")
        }
        
        # Return the prediction and suggestions
        paste("Predicted Obesity Level:", prediction, "\n\n", suggestion)
      })
      
    })
  })
  
  observeEvent(input$calculate_bmi, {
    # Validate input values
    if (is.na(input$height) || !is.numeric(input$height) || input$height <= 0) {
      return(showNotification("Invalid input: Height must be a positive numeric value.", type = "warning"))
    }
    if (is.na(input$weight) || !is.numeric(input$weight) || input$weight <= 0) {
      return(showNotification("Invalid input: Weight must be a positive numeric value.", type = "warning"))
    }
    
    # Calculate BMI
    height <- input$height / 100  # Convert height from cm to meters
    weight <- input$weight
    bmi <- weight / (height * height)
    
    # Display BMI result
    output$bmi_output <- renderText({
      paste("BMI:", round(bmi,2))
    })
  })
  
  output$raw_dataset <- renderTable({
    # Load your raw dataset here
    # Replace "your_dataset.csv" with the actual file path or dataset name
    dataset <- read.csv("cleaned_data1.csv")
    
    # Return the dataset to be displayed
    dataset
  })
  
  output$plot1 <- renderPlot({
    ggplot(df, aes(x = Age, fill = Obesity_Level)) +
      geom_histogram(binwidth = 5, alpha = 0.8, position = "dodge") +
      labs(x = "Age", y = "Frequency", title = "Age Distribution by Obesity Level") +
      scale_fill_brewer(palette = "RdYlBu") +
      theme_minimal()
  })
  
  output$plot2 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Gender)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Gender vs Obesity Level")+
      scale_fill_brewer(palette = "RdYlBu")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot3 <- renderPlot({
    df_obesity_t1 <- subset(df, Obesity_Level == "Obesity_T1")
    df_obesity_t2 <- subset(df, Obesity_Level == "Obesity_T2")
    df_obesity_t3 <- subset(df, Obesity_Level == "Obesity_T3")
    
    ggplot() +
      geom_density(data = df_obesity_t1, aes(x = Age, color = "Obesity_T1"), fill = "blue", alpha = 0.2) +
      geom_density(data = df_obesity_t2, aes(x = Age, color = "Obesity_T2"), fill = "green", alpha = 0.2) +
      geom_density(data = df_obesity_t3, aes(x = Age, color = "Obesity_T3"), fill = "red", alpha = 0.2) +
      labs(title = "Age Distribution by Obesity Level", x = "Age", y = "Density", color = "Obesity Level") +
      scale_color_manual(values = c("Obesity_T1" = "blue", "Obesity_T2" = "green", "Obesity_T3" = "red"))+
      theme_minimal()
  })
  
  output$plot4 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Family_Overweight_History)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Family Overweight History vs Obesity Level")+
      scale_fill_brewer(palette = "Accent")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot5 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = High_Caloric_Food_Consumption)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "High Caloric Food consumption vs Obesity Level")+
      scale_fill_brewer(palette = "BuGn")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
    
  })
  
  output$plot6 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Vegetable_Consumption_Frequency)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Vegetable Consumption Frequency vs Obesity Level")+
      scale_fill_brewer(palette = "Spectral")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot7 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Daily_meal_intake)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Daily Meal Intake vs Obesity Level")+
      scale_fill_brewer(palette = "Pastel1")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot8 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Food_Consumption_between_meals)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Food Consumption between meals vs Obesity Level")+
      scale_fill_brewer(palette = "Pastel2")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot9 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Smoking)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Smoking vs Obesity Level")+
      scale_fill_brewer(palette = "Reds")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot10 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Daily_Water_Intake)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Daily Water Intake vs Obesity Level")+
      scale_fill_brewer(palette = "Purples")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot11 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Carbonated_Drinks_Consumption)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Carbonated Drinks Consumption vs Obesity Level")+
      scale_fill_brewer(palette = "Blues")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot12 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Mode_of_Transportation)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Mode of Transportation vs Obesity Level")+
      scale_fill_brewer(palette = "Set3")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot13 <- renderPlot({
    ggplot(df, aes(x = Obesity_Level, fill = Alcohol_Consumption_Frequency)) + 
      geom_bar(position = "dodge") + 
      labs(x = "Obesity Level", y = "Count", title = "Alcohol Consumption Frequency vs Obesity Level")+
      scale_fill_brewer(palette = "Spectral")+
      theme_minimal()+
      theme(legend.position = "right",
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(colour = "black", fill = NA))
  })
  
  output$plot14 <- renderPlot({
    ggplot(df, aes(x = Physical_Activity_Frequency, fill = Obesity_Level)) +
      geom_histogram(binwidth = 1, position = "dodge") +
      labs(x = "Physical Activity Frequency", y = "Count") +
      facet_wrap(~Obesity_Level) +
      scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")) +
      theme_minimal()
  })
  
  output$plot15 <- renderPlot({
    ggplot(df, aes(x = Technology_Use_Duration, fill = Obesity_Level)) +
      geom_histogram(binwidth = 1, position = "dodge") +
      labs(x = "Technology Use Frequency", y = "Count") +
      facet_wrap(~Obesity_Level) +
      scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")) +
      theme_minimal()
    
  })
}

# Run the Shiny app
shinyApp(ui, server)

