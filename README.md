# Obesity Level Estimation Web Application

This project presents an interactive Shiny web application designed to estimate obesity levels based on various personal and lifestyle factors. The project is built as part of the "Introduction to Data Science" course (WIE 2003) at University of Malaya.

## Project Overview

Obesity is a global health issue characterized by excessive body fat, which poses serious risks to health. This project aims to leverage data science techniques to estimate an individual's obesity level. This project allows users to input their data and receive predictions on their weight status, along with personalized recommendations for managing their weight.

## Features

- **User-Friendly Interface**: The web app allows users to easily input data and receive immediate feedback on their weight status.
- **Obesity Level Prediction**: Utilizes a Random Forest model to classify users into one of six categories: underweight, normal, overweight, obesity T1, obesity T2, or obesity T3.
- **Personalized Recommendations**: Based on the predicted obesity level, the app offers advice on diet, exercise, and other lifestyle changes.
- **General Information on Obesity**: Provides users with educational content on obesity, its causes, and prevention strategies.
- **BMI Calculator**: A simple tool to calculate and interpret BMI based on height and weight.
- **EDA Visualization**: Includes exploratory data analysis graphs that help users understand the data used to train the model.

## How to Access

The Shiny web application can be accessed here: [Obesity Level Estimation App](https://debbbl.shinyapps.io/IntroToDS/).

## Technical Details

### Data Preprocessing
- **Feature Selection**: Certain features like height and weight were excluded from model training to focus on lifestyle and demographic factors.
- **Data Transformation**: Categorical variables were converted to factors, and feature scaling was applied to numerical data.
- **Training and Testing**: The dataset was split into 70% training and 30% testing data to evaluate model performance.

### Model Selection
Three machine learning models were tested:
1. **Random Forest**: Achieved the highest accuracy of 87.07%.
2. **Support Vector Machine (SVM)**: Accuracy of 68.61%.
3. **Multinomial Logistic Regression**: Accuracy of 62.93%.

The Random Forest model was selected for deployment due to its superior accuracy and the ability to assess feature importance.

## Deployment
The application is deployed on ShinyApps.io and provides an interactive environment where users can predict their obesity levels and explore related data. 

## Insights and Conclusions
- The model highlights the importance of lifestyle factors in predicting obesity levels, including diet, physical activity, and family history.
- Personalized recommendations are provided to help users manage their weight based on their individual risk factors.
- This tool can be used by healthcare professionals and individuals alike to raise awareness and encourage proactive health management.
