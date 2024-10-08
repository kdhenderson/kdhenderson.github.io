---
layout: page
title: Attrition Data Analysis
description: Data Analysis of Employee Attrition and Income - A Case Study
img: assets/img/top3_income_jobLevel_box.png
importance: 2
category: recent
related_publications: false
---

<div id="header">

<h4 class="author">Analysis by Kristin Henderson</h4>
<h4 class="date">Spring/Summer 2024</h4>
<p><br></p>
</div>

### Introduction

To address the challenge of retaining talented employees, the leadership
team of Frito Lay have engaged DDS Analytics to conduct an in-depth
analysis aimed at uncovering the underlying factors behind employee
turnover within their organization. This study, leveraging data from 870
employees across 36 distinct variables, is designed to yield actionable
insights to inform decision-making processes. By employing a combination
of visualizations, statistical methodologies, and the creation of
predictive models, the objective is to provide strategic guidance and
practical tools to enhance future strategies and organizational
policies.

**Presentation**

View the video presentation of this analysis:
<https://youtu.be/aY4CYfuHOf4>.

**This imports necessary libraries.**

    library(RCurl)
    library(tidyverse)
    library(ggplot2)
    library(DataExplorer)
    library(Hmisc) #rcorr(), generates a correlation matrix
    library(corrplot) #to plot the correlation matrix
    library(gridExtra) #to arrange plots in a grid
    library(RColorBrewer)
    library(caret) #ConfusionMatrix()
    library(e1071) #naiveBayes()
    library(class) #knn
    library(combinat)
    library(patchwork)
    library(GGally)
    library(kableExtra) #kable() tables
    library(pander) #pander() tables
    library(dplyr) #loading this after Hmisc because of conflict in summarize? can use summarise instead
    library(olsrr) #variable selection
    library(car) #model evaluation

### Objective 1: Identify the top three factors that lead to attrition.

**I import the primary data set from the cloud as `cs2`. Then, I convert
the numerical variables that are Likert scale responses to factors,
creating a new data frame, `cs2_conv`.**

    # import data: CaseStudy2-data.csv from AWS S3 msdsds6306 bucket

    cs2 = read.table(textConnection(getURL(
      "https://s3.us-east-2.amazonaws.com/msdsds6306/CaseStudy2-data.csv"
    )), sep=",", header=TRUE, stringsAsFactors = TRUE)

    # save a copy of the data
    # write.csv(cs2, file = "data/CaseStudy2-data.csv", row.names = FALSE)

    # get a sense of the data
    str(cs2)

    'data.frame':   870 obs. of  36 variables:
     $ ID                      : int  1 2 3 4 5 6 7 8 9 10 ...
     $ Age                     : int  32 40 35 32 24 27 41 37 34 34 ...
     $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
     $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 3 2 3 2 2 3 3 3 2 ...
     $ DailyRate               : int  117 1308 200 801 567 294 1283 309 1333 653 ...
     $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 2 3 2 2 2 3 3 2 ...
     $ DistanceFromHome        : int  13 14 18 1 2 10 5 10 10 10 ...
     $ Education               : int  4 3 2 4 1 2 5 4 4 4 ...
     $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 4 2 3 6 2 4 2 2 6 ...
     $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ EmployeeNumber          : int  859 1128 1412 2016 1646 733 1448 1105 1055 1597 ...
     $ EnvironmentSatisfaction : int  2 3 3 3 1 4 2 4 3 4 ...
     $ Gender                  : Factor w/ 2 levels "Female","Male": 2 2 2 1 1 2 2 1 1 2 ...
     $ HourlyRate              : int  73 44 60 48 32 32 90 88 87 92 ...
     $ JobInvolvement          : int  3 2 3 3 3 3 4 2 3 2 ...
     $ JobLevel                : int  2 5 3 3 1 3 1 2 1 2 ...
     $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 6 5 8 7 5 7 8 9 1 ...
     $ JobSatisfaction         : int  4 3 4 4 4 1 3 4 3 3 ...
     $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 1 3 3 2 3 1 2 1 2 2 ...
     $ MonthlyIncome           : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
     $ MonthlyRate             : int  9250 17544 19944 24032 17218 4809 5561 24223 18410 15332 ...
     $ NumCompaniesWorked      : int  2 1 2 1 1 1 2 2 1 1 ...
     $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
     $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 2 2 2 1 ...
     $ PercentSalaryHike       : int  11 14 11 19 13 21 12 14 19 14 ...
     $ PerformanceRating       : int  3 3 3 3 3 4 3 3 3 3 ...
     $ RelationshipSatisfaction: int  3 1 3 3 3 3 1 3 4 2 ...
     $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
     $ StockOptionLevel        : int  1 0 0 2 0 2 0 3 1 1 ...
     $ TotalWorkingYears       : int  8 21 10 14 6 9 7 8 1 8 ...
     $ TrainingTimesLastYear   : int  3 2 2 3 2 4 5 5 2 3 ...
     $ WorkLifeBalance         : int  2 4 3 3 3 2 2 3 3 2 ...
     $ YearsAtCompany          : int  5 20 2 14 6 9 4 1 1 8 ...
     $ YearsInCurrentRole      : int  2 7 2 10 3 7 2 0 1 2 ...
     $ YearsSinceLastPromotion : int  0 4 2 5 1 1 0 0 0 7 ...
     $ YearsWithCurrManager    : int  3 9 2 7 3 7 3 0 0 7 ...

    df_summary = summary(cs2)
    df_summary

           ID             Age        Attrition           BusinessTravel   DailyRate                       Department  DistanceFromHome
     Min.   :  1.0   Min.   :18.00   No :730   Non-Travel       : 94    Min.   : 103.0   Human Resources       : 35   Min.   : 1.000  
     1st Qu.:218.2   1st Qu.:30.00   Yes:140   Travel_Frequently:158    1st Qu.: 472.5   Research & Development:562   1st Qu.: 2.000  
     Median :435.5   Median :35.00             Travel_Rarely    :618    Median : 817.5   Sales                 :273   Median : 7.000  
     Mean   :435.5   Mean   :36.83                                      Mean   : 815.2                                Mean   : 9.339  
     3rd Qu.:652.8   3rd Qu.:43.00                                      3rd Qu.:1165.8                                3rd Qu.:14.000  
     Max.   :870.0   Max.   :60.00                                      Max.   :1499.0                                Max.   :29.000  
                                                                                                                                      
       Education              EducationField EmployeeCount EmployeeNumber   EnvironmentSatisfaction    Gender      HourlyRate    
     Min.   :1.000   Human Resources : 15    Min.   :1     Min.   :   1.0   Min.   :1.000           Female:354   Min.   : 30.00  
     1st Qu.:2.000   Life Sciences   :358    1st Qu.:1     1st Qu.: 477.2   1st Qu.:2.000           Male  :516   1st Qu.: 48.00  
     Median :3.000   Marketing       :100    Median :1     Median :1039.0   Median :3.000                        Median : 66.00  
     Mean   :2.901   Medical         :270    Mean   :1     Mean   :1029.8   Mean   :2.701                        Mean   : 65.61  
     3rd Qu.:4.000   Other           : 52    3rd Qu.:1     3rd Qu.:1561.5   3rd Qu.:4.000                        3rd Qu.: 83.00  
     Max.   :5.000   Technical Degree: 75    Max.   :1     Max.   :2064.0   Max.   :4.000                        Max.   :100.00  
                                                                                                                                 
     JobInvolvement     JobLevel                          JobRole    JobSatisfaction  MaritalStatus MonthlyIncome    MonthlyRate   
     Min.   :1.000   Min.   :1.000   Sales Executive          :200   Min.   :1.000   Divorced:191   Min.   : 1081   Min.   : 2094  
     1st Qu.:2.000   1st Qu.:1.000   Research Scientist       :172   1st Qu.:2.000   Married :410   1st Qu.: 2840   1st Qu.: 8092  
     Median :3.000   Median :2.000   Laboratory Technician    :153   Median :3.000   Single  :269   Median : 4946   Median :14074  
     Mean   :2.723   Mean   :2.039   Manufacturing Director   : 87   Mean   :2.709                  Mean   : 6390   Mean   :14326  
     3rd Qu.:3.000   3rd Qu.:3.000   Healthcare Representative: 76   3rd Qu.:4.000                  3rd Qu.: 8182   3rd Qu.:20456  
     Max.   :4.000   Max.   :5.000   Sales Representative     : 53   Max.   :4.000                  Max.   :19999   Max.   :26997  
                                     (Other)                  :129                                                                 
     NumCompaniesWorked Over18  OverTime  PercentSalaryHike PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel
     Min.   :0.000      Y:870   No :618   Min.   :11.0      Min.   :3.000     Min.   :1.000            Min.   :80    Min.   :0.0000  
     1st Qu.:1.000              Yes:252   1st Qu.:12.0      1st Qu.:3.000     1st Qu.:2.000            1st Qu.:80    1st Qu.:0.0000  
     Median :2.000                        Median :14.0      Median :3.000     Median :3.000            Median :80    Median :1.0000  
     Mean   :2.728                        Mean   :15.2      Mean   :3.152     Mean   :2.707            Mean   :80    Mean   :0.7839  
     3rd Qu.:4.000                        3rd Qu.:18.0      3rd Qu.:3.000     3rd Qu.:4.000            3rd Qu.:80    3rd Qu.:1.0000  
     Max.   :9.000                        Max.   :25.0      Max.   :4.000     Max.   :4.000            Max.   :80    Max.   :3.0000  
                                                                                                                                     
     TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsAtCompany   YearsInCurrentRole YearsSinceLastPromotion
     Min.   : 0.00     Min.   :0.000         Min.   :1.000   Min.   : 0.000   Min.   : 0.000     Min.   : 0.000         
     1st Qu.: 6.00     1st Qu.:2.000         1st Qu.:2.000   1st Qu.: 3.000   1st Qu.: 2.000     1st Qu.: 0.000         
     Median :10.00     Median :3.000         Median :3.000   Median : 5.000   Median : 3.000     Median : 1.000         
     Mean   :11.05     Mean   :2.832         Mean   :2.782   Mean   : 6.962   Mean   : 4.205     Mean   : 2.169         
     3rd Qu.:15.00     3rd Qu.:3.000         3rd Qu.:3.000   3rd Qu.:10.000   3rd Qu.: 7.000     3rd Qu.: 3.000         
     Max.   :40.00     Max.   :6.000         Max.   :4.000   Max.   :40.000   Max.   :18.000     Max.   :15.000         
                                                                                                                        
     YearsWithCurrManager
     Min.   : 0.00       
     1st Qu.: 2.00       
     Median : 3.00       
     Mean   : 4.14       
     3rd Qu.: 7.00       
     Max.   :17.00       
                         

    # convert numerical ranking variables to factors
    numVars_to_fact = c("Education", "EnvironmentSatisfaction",
                        "JobInvolvement", "JobLevel", "JobSatisfaction",
                        "PerformanceRating", "RelationshipSatisfaction",
                        "StockOptionLevel", "WorkLifeBalance")
    cs2_conv = cs2 %>%
      mutate(across(all_of(numVars_to_fact), as.factor))
    # check the conversion
    str(cs2_conv)

    'data.frame':   870 obs. of  36 variables:
     $ ID                      : int  1 2 3 4 5 6 7 8 9 10 ...
     $ Age                     : int  32 40 35 32 24 27 41 37 34 34 ...
     $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
     $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 3 2 3 2 2 3 3 3 2 ...
     $ DailyRate               : int  117 1308 200 801 567 294 1283 309 1333 653 ...
     $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 2 3 2 2 2 3 3 2 ...
     $ DistanceFromHome        : int  13 14 18 1 2 10 5 10 10 10 ...
     $ Education               : Factor w/ 5 levels "1","2","3","4",..: 4 3 2 4 1 2 5 4 4 4 ...
     $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 4 2 3 6 2 4 2 2 6 ...
     $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ EmployeeNumber          : int  859 1128 1412 2016 1646 733 1448 1105 1055 1597 ...
     $ EnvironmentSatisfaction : Factor w/ 4 levels "1","2","3","4": 2 3 3 3 1 4 2 4 3 4 ...
     $ Gender                  : Factor w/ 2 levels "Female","Male": 2 2 2 1 1 2 2 1 1 2 ...
     $ HourlyRate              : int  73 44 60 48 32 32 90 88 87 92 ...
     $ JobInvolvement          : Factor w/ 4 levels "1","2","3","4": 3 2 3 3 3 3 4 2 3 2 ...
     $ JobLevel                : Factor w/ 5 levels "1","2","3","4",..: 2 5 3 3 1 3 1 2 1 2 ...
     $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 6 5 8 7 5 7 8 9 1 ...
     $ JobSatisfaction         : Factor w/ 4 levels "1","2","3","4": 4 3 4 4 4 1 3 4 3 3 ...
     $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 1 3 3 2 3 1 2 1 2 2 ...
     $ MonthlyIncome           : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
     $ MonthlyRate             : int  9250 17544 19944 24032 17218 4809 5561 24223 18410 15332 ...
     $ NumCompaniesWorked      : int  2 1 2 1 1 1 2 2 1 1 ...
     $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
     $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 2 2 2 1 ...
     $ PercentSalaryHike       : int  11 14 11 19 13 21 12 14 19 14 ...
     $ PerformanceRating       : Factor w/ 2 levels "3","4": 1 1 1 1 1 2 1 1 1 1 ...
     $ RelationshipSatisfaction: Factor w/ 4 levels "1","2","3","4": 3 1 3 3 3 3 1 3 4 2 ...
     $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
     $ StockOptionLevel        : Factor w/ 4 levels "0","1","2","3": 2 1 1 3 1 3 1 4 2 2 ...
     $ TotalWorkingYears       : int  8 21 10 14 6 9 7 8 1 8 ...
     $ TrainingTimesLastYear   : int  3 2 2 3 2 4 5 5 2 3 ...
     $ WorkLifeBalance         : Factor w/ 4 levels "1","2","3","4": 2 4 3 3 3 2 2 3 3 2 ...
     $ YearsAtCompany          : int  5 20 2 14 6 9 4 1 1 8 ...
     $ YearsInCurrentRole      : int  2 7 2 10 3 7 2 0 1 2 ...
     $ YearsSinceLastPromotion : int  0 4 2 5 1 1 0 0 0 7 ...
     $ YearsWithCurrManager    : int  3 9 2 7 3 7 3 0 0 7 ...

    # get the proportion of attrition
    summary(cs2_conv$Attrition)

     No Yes 
    730 140 

    NoAttProb = summary(cs2_conv$Attrition)["No"] / sum(summary(cs2_conv$Attrition))
    NoAttProb

           No 
    0.8390805 

    AttProb = summary(cs2_conv$Attrition)["Yes"] / sum(summary(cs2_conv$Attrition))
    AttProb

          Yes 
    0.1609195 

**For the exploratory data analysis (EDA), first I visualize the data. I
loop through and plot nearly all the numerical variables in histograms
and frequency polygons by density grouped by attrition category. I also
loop through each categorical variable, find the proportion of each
level within each attrition category and plot them as bar charts.**

    # plot the distributions of numerical variables with respect to attrition group

    # select all variables except ID, EmployeeCount (all=1), and StandardHours (all=80))
    cs2_conv_subset = cs2_conv %>% select(-ID, -EmployeeCount, -StandardHours)
    # check the modification
    # str(cs2_conv_subset)

    # filter the numerical variables
    numVars = sapply(cs2_conv_subset, is.numeric)
    cs2_numerical = cs2_conv_subset[, numVars] #this does not include integers that were converted to factors
    # check the result
    # str(cs2_numerical)

    # function to calculate binwidth (by Freedman-Diaconis rule, Bin Width=2*IQR/(cuberoot(n)))
    # and flexibly set a binwidth for each variable
    calculate_binwidth = function(data) {
      IQR = quantile(data, 0.75) - quantile(data, 0.25)
      n = length(data)
      bw = 2 * IQR / (n^(1/3))
      return(bw)
    }

    # iterate over numerical variables and create plots
    dist_plots = list()
    for (col in names(cs2_numerical)) {
      # calculate binwidth for current variable
      bw = calculate_binwidth(cs2_conv[[col]])
      
      # create a histogram with density not frequency for each variable
      hist_plot = cs2_conv %>%
        ggplot(aes(x = .data[[col]], y = after_stat(density), fill = Attrition)) +
        geom_histogram(binwidth = bw) +
        facet_wrap(~Attrition)
        labs(title = paste("Histogram of", col))

      # save the histogram plot
      # ggsave(filename = paste0("plots/EDA/", col, "_histogram.png"), plot = hist_plot, device = "png")
      
      # create a frequency polygon plot for each variable
      freqpoly_plot = cs2_conv %>%
        ggplot(aes(x = .data[[col]], y = after_stat(density), color = Attrition)) +
        geom_freqpoly(binwidth = bw) +
        labs(title = paste("Frequency Polygon of", col))
      
      # save the frequency polygon plot
      # ggsave(filename = paste0("plots/EDA/", col, "_freqpoly.png"), plot = freqpoly_plot, device = "png")

      dist_plots[[col]] = list(hist_plot, freqpoly_plot)
    }

    # display plots
    for (col in names(cs2_numerical)) {
      print(dist_plots[[col]][[1]]) #displays histogram
      print(dist_plots[[col]][[2]]) #displays frequency polygon
    }

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-1.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-2.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-3.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-4.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-5.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-6.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-7.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-8.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-9.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-10.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-11.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-12.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-13.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-14.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-15.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-16.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-17.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-18.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-19.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-20.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-21.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-22.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-23.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-24.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-25.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-26.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-27.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-28.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-29.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-30.png)

    # make bar charts to visualize categorical variables with respect to attrition group
    # filter categorical variables (factors)
    cs2_categorical = cs2_conv %>%
      select(where(is.factor) & -Over18) #Over18(all=Y)
    # check the result
    # str(cs2_categorical)

    # iterate over categorical variables and create bar plots
    bar_plots = list()
    for (col in names(cs2_categorical)) {
      if (col != "Attrition") { #check if the column is not "Attrition"
        # calculate proportions of each category within each level of Attrition
        prop_cat_data = cs2_categorical %>%
          group_by(Attrition = cs2_conv$Attrition, .data[[col]]) %>%
          dplyr::summarize(count = n(), .groups = "drop") %>% #this gets rid of the warning message about grouping
          group_by(Attrition) %>%
          mutate(proportion = count / sum(count))
        
        bar_plot = prop_cat_data %>%
          ggplot(aes(x = .data[[col]], y = proportion, fill = Attrition)) +
          geom_bar(position = "dodge", stat = "identity") +
          labs(title = paste("Bar Chart of", col)) + 
          coord_flip()
        
        # save the bar plot
        # ggsave(filename = paste0("plots/EDA/", col, "_barplot.png"), plot = bar_plot, device = "png")
        
        bar_plots[[col]] = bar_plot
      }
    }

    # display plots if they exist (there is no plot for attrition)
    for (col in names(cs2_categorical)) {
      if (!is.null(bar_plots[[col]]))
        print(bar_plots[[col]])
    }

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-31.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-32.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-33.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-34.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-35.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-36.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-37.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-38.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-39.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-40.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-41.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-42.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-43.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-44.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-45.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/univariatePlots-46.png)

Many of the numerical features have right-skewed distributions,
e.g. salary, and the variables that are related to years working.

With the plots above, I look for features that may have higher
attrition.

*Numerical* features with higher attrition may include the following.

-   age (under ~32)  
-   distance from home (above ~13)  
-   employee number (There are three peaks.)  
-   num companies worked (higher above ~5, a bit higher ~1)  
-   percent salary hike?  
-   total working years (higher below 8 and bump at ~40 for
    retirement?)  
-   training time?  
-   years at company (higher below ~2 and bump at ~40 for retirement?)  
-   years in current role (higher below ~2)  
-   years with current manager (higher below ~1)  
-   hourly rate? (seems flat depending on how you bin it)  
-   daily rate (low-mid, ~250-650)  
-   monthly rate? (more or less flat depending on how you bin it)  
-   monthly income (much higher below ~3000)

*Categorical* features with higher attrition may include the following.

-   business travel (higher with frequent)  
-   department (higher in sales)  
-   education (slightly higher with lower education, 1-3)  
-   education field (higher in tech degree and marketing)  
-   environmental satisfaction (higher in 1, lower in 2-4)  
-   gender (slightly higher in males)  
-   job involvement (higher attrition 1-2, lower 3-4)  
-   job level (much higher in 1, lower in 2-5)  
-   job role (higher in sales rep, technician and scientist)  
-   job satisfaction (higher in 1-3, lower in 4)  
-   marital status (much higher in single)  
-   over time (much higher with yes)  
-   relationship satisfaction (higher in 1, lower in 3)  
-   stock option level (higher with 0)  
-   work life balance (higher in 1)

Something odd, performance rating doesn’t seem to have much effect, but
attrition is higher in 4.

These factors could be correlated with age and higher skill: age,
education, environmental satisfaction(?), job involvement(?), job level,
job satisfaction(?), marital status, monthly income, overtime, stock
option level, total working years, years at company, years in current
role, years with current manager.

**The next step of my EDA is to check for missing values before
proceeding with further analysis.**

    # are there missing values
    missing_count = sum(is.na(cs2_conv) | cs2_conv == "")
    writeLines(sprintf("There are %d missing values in the dataset.", missing_count))

    There are 0 missing values in the dataset.

The data set appears to be complete.

**This function generates a report to facilitate EDA. It is good to know
about and look over for ideas, though perhaps not as useful as my
step-by-step analysis.**

    create_report(cs2_conv, y = "Attrition")

**For the numerical variables, I generate a matrix of correlation
coefficients and plot them. There is also a function to generate a table
of correlation and p-values, but only the first few rows are displayed
for the sake of space. I find the matrix to be sufficient.**


    # compute correlation matrix and p-values
    cor_mat = rcorr(as.matrix(cs2_numerical))

    # This function makes a table of each variable combination with the correlation coefficient and p-value
    # ++++++++++++++++++++++++++++
    # flattenCorrMatrix
    # ++++++++++++++++++++++++++++
    # cormat : matrix of the correlation coefficients
    # pmat : matrix of the correlation p-values
    flattenCorrMatrix = function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut]
        )
    }

    # call the function with extracted correlation and p-values
    corrTable = flattenCorrMatrix(cor_mat$r, cor_mat$P)
    kable(head(corrTable), caption = "correlation coefficients") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
correlation coefficients
</caption>
<thead>
<tr>
<th style="text-align:left;">
row
</th>
<th style="text-align:left;">
column
</th>
<th style="text-align:right;">
cor
</th>
<th style="text-align:right;">
p
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:left;">
DailyRate
</td>
<td style="text-align:right;">
0.0127367
</td>
<td style="text-align:right;">
0.7075463
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:left;">
DistanceFromHome
</td>
<td style="text-align:right;">
0.0066471
</td>
<td style="text-align:right;">
0.8447809
</td>
</tr>
<tr>
<td style="text-align:left;">
DailyRate
</td>
<td style="text-align:left;">
DistanceFromHome
</td>
<td style="text-align:right;">
0.0149445
</td>
<td style="text-align:right;">
0.6597985
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:left;">
EmployeeNumber
</td>
<td style="text-align:right;">
0.0104706
</td>
<td style="text-align:right;">
0.7577761
</td>
</tr>
<tr>
<td style="text-align:left;">
DailyRate
</td>
<td style="text-align:left;">
EmployeeNumber
</td>
<td style="text-align:right;">
-0.0297344
</td>
<td style="text-align:right;">
0.3810454
</td>
</tr>
<tr>
<td style="text-align:left;">
DistanceFromHome
</td>
<td style="text-align:left;">
EmployeeNumber
</td>
<td style="text-align:right;">
-0.0046415
</td>
<td style="text-align:right;">
0.8912620
</td>
</tr>
</tbody>
</table>


    # plot the correlations using corrplot
    corrplot(cor_mat$r, type = "upper", order = "hclust", insig = "blank")

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/correlationMatrix-1.png)

    # If I include the p.mat argument I get an error.
    # I think because pairs of the same variable create an NA.

From this matrix, there is evidence of correlation between some
numerical variables, including total working years with these below:

-   years at company  
-   years in current role  
-   years with current manager  
-   (years since last promotion)  
-   monthly income

**Next in the EDA, I look at each numerical variable for statistically
significant evidence that the mean value of that variable differs
between the populations of employees that stay or go. I perform t-tests
for each numerical feature with the null hypothesis that the means of
the attrition groups are equal.**

    # extract numerical column names from cs2_numerical
    numerical_columns = colnames(cs2_numerical)

    # initialize an empty list to store t-test results
    t_test_results = list()

    # initialize an empty dataframe to store summary statistics
    summary_table = data.frame(variable = character(), 
                                mean_Attrition_Yes = numeric(), 
                                mean_Attrition_No = numeric(),
                                p_value = numeric(),
                                stringsAsFactors = FALSE)

    # initialize empty lists to store variable names with p-value < 0.05 or < 0.001
    significant_variables = list() #p-value < 0.05
    super_sig_variables = list() # p-value < 0.001

    # loop through each numerical variable
    for (col in numerical_columns) {
      # get data for the current column
      data_col = cs2_conv[[col]]
        
      # run a t-test
      t_test_result = t.test(data_col ~ Attrition, data = cs2_conv)
          
      # store t-test result in the list
      t_test_results[[col]] = t_test_result
          
      # extract relevant information and add to summary table
      summary_table = summary_table %>%
        add_row(variable = col,
          mean_Attrition_Yes = mean(data_col[cs2_conv$Attrition == "Yes"], na.rm = TRUE),
          mean_Attrition_No = mean(data_col[cs2_conv$Attrition == "No"], na.rm = TRUE),
          p_value = t_test_result$p.value)
          
      # check if p-value is less than 0.05
      if (t_test_result$p.value < 0.05) {
      significant_variables[[col]] = t_test_result
      }
      # check if p-value is less than 0.001
      if (t_test_result$p.value < 0.001) {
      super_sig_variables[[col]] = t_test_result
      }
    }

    # print summary table
    kable(summary_table, caption = "t-test results") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
t-test results
</caption>
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:right;">
mean\_Attrition\_Yes
</th>
<th style="text-align:right;">
mean\_Attrition\_No
</th>
<th style="text-align:right;">
p\_value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
33.785714
</td>
<td style="text-align:right;">
37.412329
</td>
<td style="text-align:right;">
0.0000505
</td>
</tr>
<tr>
<td style="text-align:left;">
DailyRate
</td>
<td style="text-align:right;">
784.292857
</td>
<td style="text-align:right;">
821.160274
</td>
<td style="text-align:right;">
0.3188749
</td>
</tr>
<tr>
<td style="text-align:left;">
DistanceFromHome
</td>
<td style="text-align:right;">
10.957143
</td>
<td style="text-align:right;">
9.028767
</td>
<td style="text-align:right;">
0.0164052
</td>
</tr>
<tr>
<td style="text-align:left;">
EmployeeNumber
</td>
<td style="text-align:right;">
998.371429
</td>
<td style="text-align:right;">
1035.865753
</td>
<td style="text-align:right;">
0.4978776
</td>
</tr>
<tr>
<td style="text-align:left;">
HourlyRate
</td>
<td style="text-align:right;">
67.292857
</td>
<td style="text-align:right;">
65.291781
</td>
<td style="text-align:right;">
0.2744798
</td>
</tr>
<tr>
<td style="text-align:left;">
MonthlyIncome
</td>
<td style="text-align:right;">
4764.785714
</td>
<td style="text-align:right;">
6702.000000
</td>
<td style="text-align:right;">
0.0000002
</td>
</tr>
<tr>
<td style="text-align:left;">
MonthlyRate
</td>
<td style="text-align:right;">
13624.285714
</td>
<td style="text-align:right;">
14460.123288
</td>
<td style="text-align:right;">
0.1980950
</td>
</tr>
<tr>
<td style="text-align:left;">
NumCompaniesWorked
</td>
<td style="text-align:right;">
3.078571
</td>
<td style="text-align:right;">
2.660274
</td>
<td style="text-align:right;">
0.0978823
</td>
</tr>
<tr>
<td style="text-align:left;">
PercentSalaryHike
</td>
<td style="text-align:right;">
15.328571
</td>
<td style="text-align:right;">
15.175342
</td>
<td style="text-align:right;">
0.6692297
</td>
</tr>
<tr>
<td style="text-align:left;">
TotalWorkingYears
</td>
<td style="text-align:right;">
8.185714
</td>
<td style="text-align:right;">
11.602740
</td>
<td style="text-align:right;">
0.0000007
</td>
</tr>
<tr>
<td style="text-align:left;">
TrainingTimesLastYear
</td>
<td style="text-align:right;">
2.650000
</td>
<td style="text-align:right;">
2.867123
</td>
<td style="text-align:right;">
0.0594832
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsAtCompany
</td>
<td style="text-align:right;">
5.192857
</td>
<td style="text-align:right;">
7.301370
</td>
<td style="text-align:right;">
0.0002563
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsInCurrentRole
</td>
<td style="text-align:right;">
2.907143
</td>
<td style="text-align:right;">
4.453425
</td>
<td style="text-align:right;">
0.0000015
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsSinceLastPromotion
</td>
<td style="text-align:right;">
2.135714
</td>
<td style="text-align:right;">
2.175343
</td>
<td style="text-align:right;">
0.8983165
</td>
</tr>
<tr>
<td style="text-align:left;">
YearsWithCurrManager
</td>
<td style="text-align:right;">
2.942857
</td>
<td style="text-align:right;">
4.369863
</td>
<td style="text-align:right;">
0.0000051
</td>
</tr>
</tbody>
</table>

    # print list of variables with p-value < 0.05
    significant_variable_names = names(significant_variables)
    cat("\nVariables with p-value < 0.05:\n", paste(significant_variable_names, collapse = ", "))


    Variables with p-value < 0.05:
     Age, DistanceFromHome, MonthlyIncome, TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsWithCurrManager

    # print list of variables with p-value < 0.001
    super_sig_variable_names = names(super_sig_variables)
    cat("\nVariables with p-value < 0.001:\n", paste(super_sig_variable_names, collapse = ", "))


    Variables with p-value < 0.001:
     Age, MonthlyIncome, TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsWithCurrManager

    # print full t-test results for variables with p-value < 0.001
    cat("\nT-test results for variables with p-value < 0.001:\n")


    T-test results for variables with p-value < 0.001:

    for (col in super_sig_variable_names) {
      cat("Variable:", col, "\n")
      print(super_sig_variables[[col]])
      cat("\n")
    }

    Variable: Age 

        Welch Two Sample t-test

    data:  data_col by Attrition
    t = 4.1509, df = 184.91, p-value = 5.05e-05
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
     1.902905 5.350324
    sample estimates:
     mean in group No mean in group Yes 
             37.41233          33.78571 


    Variable: MonthlyIncome 

        Welch Two Sample t-test

    data:  data_col by Attrition
    t = 5.3249, df = 228.45, p-value = 2.412e-07
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
     1220.382 2654.047
    sample estimates:
     mean in group No mean in group Yes 
             6702.000          4764.786 


    Variable: TotalWorkingYears 

        Welch Two Sample t-test

    data:  data_col by Attrition
    t = 5.1364, df = 201.19, p-value = 6.596e-07
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
     2.105259 4.728792
    sample estimates:
     mean in group No mean in group Yes 
            11.602740          8.185714 


    Variable: YearsAtCompany 

        Welch Two Sample t-test

    data:  data_col by Attrition
    t = 3.7256, df = 191.55, p-value = 0.0002563
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
     0.9922099 3.2248155
    sample estimates:
     mean in group No mean in group Yes 
             7.301370          5.192857 


    Variable: YearsInCurrentRole 

        Welch Two Sample t-test

    data:  data_col by Attrition
    t = 4.9513, df = 208, p-value = 1.522e-06
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
     0.9306052 2.1619584
    sample estimates:
     mean in group No mean in group Yes 
             4.453425          2.907143 


    Variable: YearsWithCurrManager 

        Welch Two Sample t-test

    data:  data_col by Attrition
    t = 4.6826, df = 209.75, p-value = 5.084e-06
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
     0.8262439 2.0277679
    sample estimates:
     mean in group No mean in group Yes 
             4.369863          2.942857 

This provides additional evidence of numerical variables that likely
influence attrition to narrow the pool of important features in the data
set.

Variables with p-value &lt; 0.05: Age, DistanceFromHome, MonthlyIncome,
TotalWorkingYears, YearsAtCompany, YearsInCurrentRole,
YearsWithCurrManager  
(The last 5 are highly correlated as I saw above.)

Variables with p-value &lt; 0.001: Age, MonthlyIncome,
TotalWorkingYears, YearsAtCompany, YearsInCurrentRole,
YearsWithCurrManager

**To gather statistical evidence of categorical variables associated
with attrition, I perform chi-square tests for each categorical feature
with the null hypothesis that the proportion of attrition is the same
across all categories (feature levels).**

    # extract categorical column names from cs2_categorical, excluding Attrition
    categorical_columns = colnames(cs2_categorical)
    categorical_columns = categorical_columns[categorical_columns != "Attrition"]

    # initialize an empty list to store chi-square test results
    chisq_test_results = list()

    # initialize an empty dataframe to store summary statistics
    summary_table2 = data.frame(variable = character(), 
                                p_value = numeric(),
                                stringsAsFactors = FALSE)

    # initialize empty lists to store variable names with p-value < 0.05 or < 0.001
    significant_variables2 = list()
    super_sig_variables2 = list()

    # loop through each categorical variable
    for (col in categorical_columns) {
      # create a contingency table for the chi-square test
      contingency_table = table(cs2_conv[[col]], cs2_conv$Attrition)
      # print(contingency_table)
      
      # run a chi-square test
      chisq_test_result = chisq.test(contingency_table)
          
      # store chi-square test result in the list
      chisq_test_results[[col]] = chisq_test_result
          
      # extract relevant information and add to summary table
      summary_table2 = summary_table2 %>%
        add_row(variable = col,
          p_value = chisq_test_result$p.value)
          
      # check if p-value is less than 0.05
      if (chisq_test_result$p.value < 0.05) {
      significant_variables2[[col]] = chisq_test_result
      }
      # check if p-value is less than 0.001
      if (chisq_test_result$p.value < 0.001) {
      super_sig_variables2[[col]] = chisq_test_result
      }
    }

    # print summary table
    kable(summary_table2, caption = "chi-square test results") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
chi-square test results
</caption>
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:right;">
p\_value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
BusinessTravel
</td>
<td style="text-align:right;">
0.0499254
</td>
</tr>
<tr>
<td style="text-align:left;">
Department
</td>
<td style="text-align:right;">
0.0094238
</td>
</tr>
<tr>
<td style="text-align:left;">
Education
</td>
<td style="text-align:right;">
0.6242838
</td>
</tr>
<tr>
<td style="text-align:left;">
EducationField
</td>
<td style="text-align:right;">
0.2682198
</td>
</tr>
<tr>
<td style="text-align:left;">
EnvironmentSatisfaction
</td>
<td style="text-align:right;">
0.0105409
</td>
</tr>
<tr>
<td style="text-align:left;">
Gender
</td>
<td style="text-align:right;">
0.5151297
</td>
</tr>
<tr>
<td style="text-align:left;">
JobInvolvement
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
JobLevel
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
JobRole
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
JobSatisfaction
</td>
<td style="text-align:right;">
0.0111512
</td>
</tr>
<tr>
<td style="text-align:left;">
MaritalStatus
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
OverTime
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
PerformanceRating
</td>
<td style="text-align:right;">
0.7461706
</td>
</tr>
<tr>
<td style="text-align:left;">
RelationshipSatisfaction
</td>
<td style="text-align:right;">
0.3727117
</td>
</tr>
<tr>
<td style="text-align:left;">
StockOptionLevel
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
WorkLifeBalance
</td>
<td style="text-align:right;">
0.0024951
</td>
</tr>
</tbody>
</table>

    # print list of variables with p-value < 0.05
    significant_variable_names2 = names(significant_variables2)
    cat("\nVariables with p-value < 0.05:\n", paste(significant_variable_names2, collapse = ", "))


    Variables with p-value < 0.05:
     BusinessTravel, Department, EnvironmentSatisfaction, JobInvolvement, JobLevel, JobRole, JobSatisfaction, MaritalStatus, OverTime, StockOptionLevel, WorkLifeBalance

    # print list of variables with p-value < 0.001
    super_sig_variable_names2 = names(super_sig_variables2)
    cat("\nVariables with p-value < 0.001:\n", paste(super_sig_variable_names2, collapse = ", "))


    Variables with p-value < 0.001:
     JobInvolvement, JobLevel, JobRole, MaritalStatus, OverTime, StockOptionLevel

    # print full chi-square test results for variables with p-value < 0.001
    cat("\nChi-square test results for variables with p-value < 0.001:\n")


    Chi-square test results for variables with p-value < 0.001:

    for (col in super_sig_variable_names2) {
      cat("Variable:", col, "\n")
      print(super_sig_variables2[[col]])
      cat("\n")
    }

    Variable: JobInvolvement 

        Pearson's Chi-squared test

    data:  contingency_table
    X-squared = 41.465, df = 3, p-value = 5.211e-09


    Variable: JobLevel 

        Pearson's Chi-squared test

    data:  contingency_table
    X-squared = 41.533, df = 4, p-value = 2.085e-08


    Variable: JobRole 

        Pearson's Chi-squared test

    data:  contingency_table
    X-squared = 60.543, df = 8, p-value = 3.647e-10


    Variable: MaritalStatus 

        Pearson's Chi-squared test

    data:  contingency_table
    X-squared = 34.406, df = 2, p-value = 3.379e-08


    Variable: OverTime 

        Pearson's Chi-squared test with Yates' continuity correction

    data:  contingency_table
    X-squared = 62.762, df = 1, p-value = 2.333e-15


    Variable: StockOptionLevel 

        Pearson's Chi-squared test

    data:  contingency_table
    X-squared = 56.245, df = 3, p-value = 3.724e-12

Although, the assumptions of a chi-square test may be violated here, a
deeper knowledge of the test would be helpful, and use of alternative
tests might be more appropriate, this at least provides additional
evidence of categorical variables that likely influence attrition,
facilitating a narrower pool of variables to consider.

Variables with p-value &lt; 0.05: BusinessTravel, Department,
EnvironmentSatisfaction, JobInvolvement, JobLevel, JobRole,
JobSatisfaction, MaritalStatus, OverTime, StockOptionLevel,
WorkLifeBalance

Variables with p-value &lt; 0.001: JobInvolvement, JobLevel, JobRole,
MaritalStatus, OverTime, StockOptionLevel

**To look at co-variation among numerical variables that seem likely to
influence attrition, I make scatter plots of all combinations of those
with highly significant t-test results.**

    # look at co-variation among numerical variables

    # function to create scatter plots for each pair of variables
    make_scatterplot = function(var1, var2) {
      cs2_conv %>% 
        ggplot(aes(x = .data[[var1]], y = .data[[var2]], color = Attrition)) +
        geom_point(alpha = 0.5, position = "jitter") +
        geom_smooth() +
        labs(title = paste(var1, "vs", var2))
    }

    # loop through each variable
    for (i in 1:(length(super_sig_variable_names) - 1)) {
      var1 = super_sig_variable_names[i]
      
      # plot var1 with every other variable that comes after it in the list
      for (j in (i + 1):length(super_sig_variable_names)) {
        var2 = super_sig_variable_names[j]
        scatter = make_scatterplot(var1, var2)
        
        # display and save the individual plots
        print(scatter) # using print() so it's easier to find where I call the plot
        filename = paste0("plots/EDA/scatter_", var1, "_vs_", var2, ".png")
        # ggsave(filename, plot = scatter, device = "png")
      }
    }

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-1.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-2.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-3.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-4.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-5.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-6.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-7.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-8.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-9.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-10.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-11.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-12.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-13.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-14.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_num-15.png)

**To do the same for combinations of numerical and categorical
variables, I make boxplots of all combinations of those with highly
significant t-test and chi-square test results.**

    # look at co-variation between numerical and categorical variables

    # function to create boxplots for each pair of variables
    make_boxplot = function(catvar, numvar) {
      cs2_conv %>% group_by(Attrition) %>% 
        ggplot(aes(x = .data[[catvar]], y = .data[[numvar]], fill = Attrition)) +
        geom_boxplot() +
        coord_flip() +
        labs(title = paste(catvar, "vs", numvar))
    }

    # loop through each variable
    for (i in 1:(length(super_sig_variable_names2))) {
      catvar = super_sig_variable_names2[i]
      
      # plot catvar with every numvar
      for (j in 1:length(super_sig_variable_names)) {
        numvar = super_sig_variable_names[j]
        boxplt = make_boxplot(catvar, numvar)
        
        # display and save the individual plots
        # I tried to display them in a grid in the interest of space, but they are just too small.
        print(boxplt)
        filename = paste0("plots/EDA/boxplt_", catvar, "_vs_", numvar, ".png")
        # ggsave(filename, plot = boxplt, device = "png")
      }
    }

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-1.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-2.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-3.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-4.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-5.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-6.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-7.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-8.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-9.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-10.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-11.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-12.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-13.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-14.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-15.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-16.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-17.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-18.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-19.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-20.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-21.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-22.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-23.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-24.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-25.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-26.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-27.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-28.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-29.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-30.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-31.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-32.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-33.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-34.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-35.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_c_n-36.png)

**To do the same for combinations of categorical variables, I make bar
charts of all combinations of those with highly significant chi-square
test results.**

    # look at co-variation among categorical variables

    # It probably is easier to see relationships if I were to calculate proportions a different way to normalize the data.
    # Currently, proportions are calculated like this. For each attrition group, and each level of variable 1, the count of each level of variable 2 (the fill variable) is divided by the sum of the total count for all levels of that variable (within the attrition and variable 1 level).

    make_barchart = function(var1, var2) {
      cs2_conv %>% 
        group_by(Attrition, .data[[var1]], .data[[var2]]) %>%
        dplyr::summarize(n = n(), .groups = 'drop') %>%  #count occurrences within each group
        group_by(Attrition, .data[[var1]]) %>%  # group by Attrition and var1
        mutate(prop = n / sum(n)) %>% #calculate proportion within each group
        ggplot(aes(y = .data[[var1]], x = prop, fill = .data[[var2]])) +  #use prop as fill
        geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
        facet_wrap(~Attrition, labeller = labeller(Attrition = c("No" = "No Attrition", "Yes" = "Yes Attrition"))) +
        labs(title = paste(var1, "vs", var2),
             y = var1,
             x = paste0("Proportion of ", var2, " within Attrition and ", var1, " level"),
             fill = var2) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(palette = "Set1")
    }

    # loop through each variable
    for (i in 1:(length(super_sig_variable_names2) - 1)) {
      var1 = super_sig_variable_names2[i]
        
      for (j in (i + 1):length(super_sig_variable_names2)) {
        var2 = super_sig_variable_names2[j]
        barplt = make_barchart(var1, var2)
        
        # display and save the individual plots
        print(barplt)
        filename = paste0("plots/EDA/barplt_", var1, "_vs_", var2, ".png")
        # ggsave(filename, plot = barplt, device = "png")
      }
    }

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-1.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-2.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-3.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-4.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-5.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-6.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-7.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-8.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-9.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-10.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-11.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-12.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-13.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-14.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/covariation_cat-15.png)

**Observations from co-variation plots**

**Job level**

-   Trends:
    -   There is a positive trend between job level and several other
        variables: age, monthly income, total working years, years at
        company, years in current role and years with current manager.  
-   Differences:
    -   Job level 4: age and monthly income look like they may
        contribute to attrition specifically in this job level  
    -   Job level 5: years at company and years in current role may
        contribute to attrition in this job level. (This suggests
        perhaps retirement is a factor, although total working years
        doesn’t show the same variation.)

**Marital Status**

-   Trends:
    -   Singles that leave are lower in median age, have fewer total
        working years, years at company and years in current role than
        other groups.  
    -   Otherwise, age is similar in marital status.  
    -   All marital status levels have higher attrition with overtime,
        though this trend seems more pronounced in singles.  
-   Observations:
    -   Singles fall entirely within stock option level 0.

**Overtime and Stock Option**

-   Trends:
    -   Across age, monthly income, total working years, years at
        company, years in current role and years with current manager,
        trends between attrition categories appear similar in overtime
        and stock option levels.  
    -   Stock option 2 may have a different trend with median age and
        years in current role being higher among those that leave.  
-   Observations:
    -   There are interesting trends in overtime and stock options
        between job roles.

**Monthly income**

-   Trends:
    -   Monthly income is positively associated with total working years
        with little difference in attrition groups.  
    -   There a positive association between monthly income and age.  
-   Differences:
    -   There seems to be higher attrition in older employees with lower
        incomes.  
    -   Higher income earners tend to leave if they have been with the
        company &gt; 10 years or in their current role or with their
        current manager &gt; ~7 years.

Some additional ideas:

-   It might be a good idea to derive a variable, percent of working
    years at a company from total working years and years at company.
    This may offset the confound of age.

**As there is high correlation between total working years, years at
company, years in current role and years with current manager, I want to
see if deriving one variable from two of these, might help offset the
correlation with age and extract possibly more meaningful information
with the use of fewer variables.**

    cs2_conv2 = cs2_conv %>% 
      mutate(
        PercentWrkYrsAtCompany = ifelse(TotalWorkingYears == 0, 0,
                                        round(YearsAtCompany / TotalWorkingYears * 100, 2)),
        PercentYrs_wManager = ifelse(TotalWorkingYears == 0, 0,
                                     round(YearsWithCurrManager / TotalWorkingYears * 100, 2)),
        PercentYrs_inRole = ifelse(TotalWorkingYears == 0, 0,
                                   round(YearsInCurrentRole / TotalWorkingYears * 100, 2)),
        PercentYrs_inRoleAtComp = ifelse(YearsAtCompany == 0, 0,
                                   round(YearsInCurrentRole / YearsAtCompany * 100, 2))
      )
    # str(cs2_conv2)
    # summary(cs2_conv2)

    # plot these derived variables with other variables that seem relevant and alone to see the distributions
    cs2_conv2 %>% ggplot(aes(x = Age, y = PercentWrkYrsAtCompany, color = Attrition)) +
      geom_point(alpha = 0.5, position = "jitter") + geom_smooth()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-1.png)

    cs2_conv2 %>% ggplot(aes(x = MonthlyIncome, y = PercentWrkYrsAtCompany, color = Attrition)) +
      geom_point(alpha = 0.5, position = "jitter") + geom_smooth()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-2.png)

    cs2_conv2 %>% ggplot(aes(x = Age, y = PercentYrs_inRoleAtComp, color = Attrition)) +
      geom_point(alpha = 0.5, position = "jitter") + geom_smooth()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-3.png)

    cs2_conv2 %>% ggplot(aes(x = MonthlyIncome, y = PercentYrs_inRoleAtComp, color = Attrition)) +
      geom_point(alpha = 0.5, position = "jitter") + geom_smooth()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-4.png)

    cs2_conv2 %>% ggplot(aes(x = MaritalStatus, y = PercentWrkYrsAtCompany, fill = Attrition)) +
      geom_boxplot(position = "dodge")

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-5.png)

    cs2_conv2 %>% ggplot(aes(x = JobLevel, y = PercentWrkYrsAtCompany, fill = Attrition)) +
      geom_boxplot(position = "dodge")

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-6.png)

    cs2_conv2 %>% ggplot(aes(x = MaritalStatus, y = PercentYrs_inRoleAtComp, fill = Attrition)) +
      geom_boxplot(position = "dodge")

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-7.png)

    cs2_conv2 %>% ggplot(aes(x = JobLevel, y = PercentYrs_inRoleAtComp, fill = Attrition)) +
      geom_boxplot(position = "dodge")

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-8.png)

    cs2_conv2 %>% ggplot(aes(x = PercentWrkYrsAtCompany, after_stat(density), fill = Attrition)) +
      geom_histogram(binwidth = 10) + facet_wrap(~Attrition)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-9.png)

    cs2_conv2 %>% ggplot(aes(x = PercentYrs_inRoleAtComp, y = after_stat(density), fill = Attrition)) +
      geom_histogram(binwidth = 10) + facet_wrap(~Attrition)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/derivedVariables-10.png)

    # run t-tests to check for possible differences
    t.test(PercentWrkYrsAtCompany ~ Attrition, data = cs2_conv2)


        Welch Two Sample t-test

    data:  PercentWrkYrsAtCompany by Attrition
    t = 0.94099, df = 187.08, p-value = 0.3479
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
     -3.316668  9.366557
    sample estimates:
     mean in group No mean in group Yes 
             68.70130          65.67636 

    t.test(PercentYrs_wManager ~ Attrition, data = cs2_conv2)


        Welch Two Sample t-test

    data:  PercentYrs_wManager by Attrition
    t = 3.2429, df = 188.9, p-value = 0.001399
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
      3.638589 14.938990
    sample estimates:
     mean in group No mean in group Yes 
             41.57358          32.28479 

    t.test(PercentYrs_inRole ~ Attrition, data = cs2_conv2)


        Welch Two Sample t-test

    data:  PercentYrs_inRole by Attrition
    t = 3.058, df = 189.5, p-value = 0.00255
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
      3.079205 14.270408
    sample estimates:
     mean in group No mean in group Yes 
             42.37452          33.69971 

    t.test(PercentYrs_inRoleAtComp ~ Attrition, data = cs2_conv2)


        Welch Two Sample t-test

    data:  PercentYrs_inRoleAtComp by Attrition
    t = 3.55, df = 177.46, p-value = 0.0004933
    alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    95 percent confidence interval:
      5.468542 19.158856
    sample estimates:
     mean in group No mean in group Yes 
              59.0657           46.7520 

Based on the plots and t-test results, percent years in role at company
(derived from years in role divided by years at company), seems like the
most potentially useful of these derived variables.

**I further explore the usefulness of the percent years in role at
company by plotting combinations of percent years in role at company,
monthly income, age, and job level.**


    cs2_conv2 %>% ggplot(aes(x = PercentYrs_inRoleAtComp, y = MonthlyIncome, color = JobLevel)) +
      geom_point(alpha = 0.5, position = "jitter") + geom_smooth()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/unnamed-chunk-1-1.png)


    cs2_conv2 %>% ggplot(aes(x = PercentYrs_inRoleAtComp, y = Age, color = JobLevel)) +
      geom_point(alpha = 0.5, position = "jitter") + geom_smooth()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/unnamed-chunk-1-2.png)


    cs2_conv2 %>% ggplot(aes(x = MonthlyIncome, y = Age, color = JobLevel)) +
      geom_point(alpha = 0.5, position = "jitter") + geom_smooth()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/unnamed-chunk-1-3.png)

Monthly income is a good predictor of job level (or vice versa) no
matter the percent years in role at the company. Age is not as good
(though there is some predictability). There is a much wider
distribution of ages the lower the income, particularly under 5000.

Job level seems like a powerful predictor.

**So far, the visual and statistical evidence and my best guess for top
features leading to attrition are Job Level, Marital Status, Monthly
Income and Overtime. To confirm this and/or narrow my list to 3, I want
to use the list of 12 features that seem like they have the greatest
differences between attrition categories. I generate all the unique
combinations of 3 variables. Then using the loop in this code, I use
each combination to build a Naive Bayes model to predict attrition, and
rank them by how well each combination performed.**

    modelIdx = c(2,15,16,17,19,20,24,29,30,33,34,36) #indexes of predicted top features
    # these are the indexes for Age, JobInvolvement, JobLevel, JobRole, MaritalStatus, MonthlyIncome, OverTime, StockOptionLevel, TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsWithCurrManager

    # initialize an empty data frame to store results
    results = data.frame(Index1 = integer(), Index2 = integer(), Index3 = integer(), Sensitivity = numeric(), Specificity = numeric(), Accuracy = numeric(), stringsAsFactors = FALSE)

    # number of different splits
    iters = 50

    # 70-30 train/test split
    splitPerc = 0.7

    # generate all combinations of 3 indexes
    index_combinations = combinat::combn(modelIdx, 3)

    # loop through each combination of 3 indexes
    for (i in 1:ncol(index_combinations)) {
      index_set = index_combinations[, i]

      # create holders for the sensitivity, specificity, and accuracy for each iteration
      masterSens = numeric(iters)
      masterSpec = numeric(iters)
      masterAcc = numeric(iters)

      # iterate over the specified number of iterations
      for (j in 1:iters) {
        set.seed(j)

        # sample 70% for training
        trainIdx = sample(1:dim(cs2_conv2)[1], round(splitPerc * dim(cs2_conv2)[1]))
        # select the rows for training
        AttritionTrn = cs2_conv2[trainIdx, ]
        # select the remaining rows for testing
        AttritionTst = cs2_conv2[-trainIdx, ]

        # temporarily set the current indexes for modeling
        temp_modelIdx = index_set

        # train the model
        modelNB = naiveBayes(AttritionTrn[, temp_modelIdx], AttritionTrn$Attrition, laplace = 1)

        # predict using the model
        preds = predict(modelNB, AttritionTst[, temp_modelIdx])

        # make a confusion matrix
        CM = confusionMatrix(table(preds, AttritionTst$Attrition))

        # store sensitivity, specificity, and accuracy
        masterSens[j] = CM$byClass["Sensitivity"]
        masterSpec[j] = CM$byClass["Specificity"]
        masterAcc[j] = CM$overall["Accuracy"]
      }

      # calculate average sensitivity, specificity, and accuracy
      avg_sensitivity = mean(masterSens)
      avg_specificity = mean(masterSpec)
      avg_accuracy = mean(masterAcc)

      # store the results including the three indexes
      results = rbind(results, list(Index1 = index_set[1], Index2 = index_set[2], Index3 = index_set[3], Sensitivity = avg_sensitivity, Specificity = avg_specificity, Accuracy = avg_accuracy))
    }

    # output the results
    results = results %>% arrange(-Specificity)
    kable(head(results, 10), caption = "3 variable combos to predict attrition") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
3 variable combos to predict attrition
</caption>
<thead>
<tr>
<th style="text-align:right;">
Index1
</th>
<th style="text-align:right;">
Index2
</th>
<th style="text-align:right;">
Index3
</th>
<th style="text-align:right;">
Sensitivity
</th>
<th style="text-align:right;">
Specificity
</th>
<th style="text-align:right;">
Accuracy
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.9409578
</td>
<td style="text-align:right;">
0.3656826
</td>
<td style="text-align:right;">
0.8475096
</td>
</tr>
<tr>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.9571661
</td>
<td style="text-align:right;">
0.3050640
</td>
<td style="text-align:right;">
0.8511877
</td>
</tr>
<tr>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.9470747
</td>
<td style="text-align:right;">
0.3003832
</td>
<td style="text-align:right;">
0.8422222
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.9722697
</td>
<td style="text-align:right;">
0.2859146
</td>
<td style="text-align:right;">
0.8607663
</td>
</tr>
<tr>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.9593601
</td>
<td style="text-align:right;">
0.2833596
</td>
<td style="text-align:right;">
0.8495019
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.9585989
</td>
<td style="text-align:right;">
0.2794801
</td>
<td style="text-align:right;">
0.8483525
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0.9544960
</td>
<td style="text-align:right;">
0.2769441
</td>
<td style="text-align:right;">
0.8440613
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.9107722
</td>
<td style="text-align:right;">
0.2750041
</td>
<td style="text-align:right;">
0.8068966
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
0.9586844
</td>
<td style="text-align:right;">
0.2522911
</td>
<td style="text-align:right;">
0.8435249
</td>
</tr>
<tr>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
0.9626306
</td>
<td style="text-align:right;">
0.2439620
</td>
<td style="text-align:right;">
0.8453640
</td>
</tr>
</tbody>
</table>

The top three features that predict attrition are within the group that
I initially highlighted. They are Job Level, Monthly Income and
Overtime. Stock Option Level and Marital Status are also strong
predictors.

Obtaining marital status data may pose challenges due to potential
privacy considerations, unlike the other predictors which are likely
readily available through employment records.

**This provides an overview of the first step of my analysis process
focusing on visual evidence.**

I started the analysis by plotting each variable separated by attrition
group to compare their populations. Visually, the many features on this
list seemed to show differences in these populations, but the four
starred variables seemed particularly important.

*Important features predicted by visual evidence:*  
Age  
Business Travel  
Distance from home  
Department  
Education Field  
Environmental Satisfaction  
Job Involvement  
Job Level \*  
Job Role  
Job Satisfaction  
Marital Status \*  
Monthly Income \*  
Number Companies Worked For  
Overtime \*  
Relationship Satisfaction  
Stock Option Level  
Total Working Years  
Work Life Balance  
Years At Company  
Years In Current Role  
Years With Current Manager

**To illustrate an example of positive and negative visual evidence, I
plot attrition by job level and gender.**

    # plot positive predictors example: Job Level for presentation
    # also plot negative predictor: Gender

    # I revert to using my original converted dataset `cs2_conv`.

    # plot positive predictor example: job level and attrition
    # calculation of proportion of employees: employees in job level attrition yes / total employees in job level, to normalize each job level 
    jobLevel_bar = cs2_conv %>%
      group_by(JobLevel, Attrition) %>%
      dplyr::summarize(n = n()) %>%
      mutate(prop = n / sum(n)) %>%
      filter(Attrition == "Yes") %>% 
      ggplot(aes(y = JobLevel, x = prop, fill = JobLevel)) +
      geom_bar(stat = "identity") +
      labs(y = "Job Level", x = "Proportion of Attrition",
           fill = "Job Level",
           title = "Proportion of Attrition within Each Job Level",
           caption = "Normalized by Total Employees per Job Level") +
      theme_bw() +
      scale_fill_brewer(palette = "Set1")
    print(jobLevel_bar)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_1-1.png)

    # ggsave(jobLevel_bar, filename = "plots/top3_jobLevel_bar.png")

    # plot negative predictor example: gender and attrition
    genderBar = cs2_conv %>% 
      group_by(Gender, Attrition) %>%
      dplyr::summarize(n = n()) %>%
      mutate(prop = n / sum(n)) %>%
      filter(Attrition == "Yes") %>% 
      ggplot(aes(y = Gender, x = prop)) +
      geom_bar(stat = "identity", fill = "#377EB8") + #fill color = Attrition group blue
      labs(y = "Gender", x = "Proportion of Attrition",
           fill = "Gender",
           title = "Proportion of Attrition within Each Gender",
           caption = "Normalized by Total Employees per Gender") +
      theme_bw() +
      theme(text = element_text(size = 12))
    genderBar

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_1-2.png)

    # ggsave(genderBar, filename = "plots/top3negExample_genderBar.png")

Job level 1 has a much higher percent attrition than any other job
level. Over a quarter of the employees in job level 1 leave the company.
This is at least 13% higher than any other job level. Also of note, only
5% of employees in job level 4 leave.

On the other hand, gender doesn’t contribute to attrition. There is very
little difference in attrition rates between males and females (17% and
15%).

**This provides an overview of the other steps in my analysis process
focusing on statistical evidence and modeling.**  
I tested for statistically significant differences in the attrition
groups for each variable that was highlighted by visual evidence. For
the numerical values, I used a t-test to predict the likelihood that the
means of attrition groups are equal. For the categorical variables, I
used a chi-square test to predict the likelihood that proportion of
attrition is the same across categories of a variable.

*These variables, including the four starred previously, had significant
differences below the alpha 0.05 level.*  
Age  
Job Involvement  
Job Level \*  
Job Role  
Marital Status \*  
Monthly Income \*  
Overtime \*  
Stock Option Level  
Total Working Years  
Years At Company  
Years In Current Role  
Years With Current Manager

From this list, I identified a subset of three variables that achieved
the highest specificity when used in a predictive model for attrition.
In the context of the model, specificity refers to the percentage of
instances where attrition is correctly predicted.

The subset of three top factors that lead to attrition are Job Level,
Monthly Income and Overtime.

    # plot the other positive predictors for presentation: Monthly Income, Job Level and Overtime

    # plot monthly income and attrition with a boxplot and two histograms
    incomeBox = cs2_conv %>% 
      ggplot(aes(x = MonthlyIncome, y = Attrition, fill = Attrition)) +
      geom_boxplot() +
      ylab("Attrition") +
      theme_bw() +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      scale_fill_brewer(palette = "Set1")

    yAtt_hist = cs2_conv %>% filter(Attrition == "Yes") %>% 
      ggplot(aes(x = MonthlyIncome)) +
      geom_histogram(binwidth = 1000, fill = "#377EB8") +
      labs(title = "Distribution of Monthly Income by Employee Attrition",
           y = "Yes") +
      theme_bw() +
        theme(legend.position = "none",
              axis.title.x = element_blank())
      
    nAtt_hist = cs2_conv %>% filter(Attrition == "No") %>% 
      ggplot(aes(x = MonthlyIncome)) +
      geom_histogram(binwidth = 1000, fill = "#E41A1C") +
      labs(#title = "Employee Attrition and Monthly Income",
           y = "No") +
      theme_bw() + 
      theme(legend.position = "none")

    incomePlt = yAtt_hist / incomeBox / nAtt_hist
    print(incomePlt)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_2-1.png)

    # ggsave(incomePlt, filename = "plots/top3_incomeDist.png")


    # make co-variation plots split by attrition groups

    # plot positive predictor: job level and monthly income
    income_jobLevel_box = ggplot(cs2_conv, aes(x = JobLevel, y = MonthlyIncome, fill = Attrition)) +
      geom_boxplot() +
      labs(x = "Job Level", y = "Monthly Income", fill = "Attrition") +
      ggtitle("Monthly Income by Job Level and Attrition") +
      theme_bw() + 
      scale_fill_brewer(palette = "Set1")
    income_jobLevel_box

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_2-2.png)

    # ggsave(income_jobLevel_box, filename = "plots/top3_income_jobLevel_box.png")


    # plot positive predictor: job level and attrition, with NO overtime
    # employees NOT working overtime
    jobLevel_OTno_bar = cs2_conv %>%
      group_by(OverTime, JobLevel, Attrition) %>%
      dplyr::summarize(n = n()) %>%
      mutate(prop = n / sum(n)) %>%
      filter(Attrition == "Yes" & OverTime == "No") %>%
      ggplot(aes(y = JobLevel, x = prop, fill = JobLevel)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(y = "Job Level", x = "Proportion of Attrition",
           fill = "Job Level",
           subtitle = "Proportion of Attrition within Each Job Level",
           title = "Employees with No Overtime Hours",
           caption = "Normalized by Total Employees per Job Level") +
      xlim(0.00, 0.55) +
      theme_bw() +
      theme(text = element_text(size = 12)) +
      scale_fill_brewer(palette = "Set1")
    print(jobLevel_OTno_bar)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_2-3.png)

    # ggsave(jobLevel_OTno_bar, filename = "plots/top3_jobLevel_OTno_bar.png")

    # plot positive predictor: job level and attrition, with overtime
    # employees working at least an hour of overtime
    jobLevel_OTyes_bar = cs2_conv %>%
      group_by(OverTime, JobLevel, Attrition) %>%
      dplyr::summarize(n = n()) %>%
      mutate(prop = n / sum(n)) %>%
      filter(Attrition == "Yes" & OverTime == "Yes") %>%
      ggplot(aes(y = JobLevel, x = prop, fill = JobLevel)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(y = "Job Level", x = "Proportion of Attrition",
           fill = "Job Level",
           subtitle = "Proportion of Attrition within Each Job Level",
           title = "Employees Working Overtime",
           caption = "Normalized by Total Employees per Job Level") +
      xlim(0.00, 0.55) +
      theme_bw() +
      theme(text = element_text(size = 12)) +
      scale_fill_brewer(palette = "Set1")
    print(jobLevel_OTyes_bar)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_2-4.png)

    # ggsave(jobLevel_OTyes_bar, filename = "plots/top3_jobLevel_OTyes_bar.png")

    # find 25th, 50th and 75th percentile of monthly incomes for each attrition group and job level
    # is there a stay/go cut off for job level 4
    mid50_income = cs2_conv %>%
      group_by(Attrition, JobLevel) %>%
      dplyr::summarise(q25 = quantile(MonthlyIncome, 0.25),
                median = median(MonthlyIncome),
                q75 = quantile(MonthlyIncome, 0.75))
    kable(mid50_income, caption = "monthly income q25, median, q75 for each job level and attrition group") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
monthly income q25, median, q75 for each job level and attrition group
</caption>
<thead>
<tr>
<th style="text-align:left;">
Attrition
</th>
<th style="text-align:left;">
JobLevel
</th>
<th style="text-align:right;">
q25
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q75
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
2293.00
</td>
<td style="text-align:right;">
2695.0
</td>
<td style="text-align:right;">
3207.0
</td>
</tr>
<tr>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
4632.25
</td>
<td style="text-align:right;">
5365.5
</td>
<td style="text-align:right;">
6265.0
</td>
</tr>
<tr>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
8624.50
</td>
<td style="text-align:right;">
9985.0
</td>
<td style="text-align:right;">
10911.5
</td>
</tr>
<tr>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
13770.00
</td>
<td style="text-align:right;">
15992.0
</td>
<td style="text-align:right;">
16799.0
</td>
</tr>
<tr>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
18844.00
</td>
<td style="text-align:right;">
19189.0
</td>
<td style="text-align:right;">
19636.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
2152.50
</td>
<td style="text-align:right;">
2424.5
</td>
<td style="text-align:right;">
2860.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
4712.25
</td>
<td style="text-align:right;">
5325.0
</td>
<td style="text-align:right;">
6162.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
7978.00
</td>
<td style="text-align:right;">
9582.0
</td>
<td style="text-align:right;">
10325.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
12552.50
</td>
<td style="text-align:right;">
12936.0
</td>
<td style="text-align:right;">
13065.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
19364.75
</td>
<td style="text-align:right;">
19695.0
</td>
<td style="text-align:right;">
19848.5
</td>
</tr>
</tbody>
</table>

The plot of distributions of monthly income reveals a smaller income
range and median income among employees that leave (in blue). The
high-income outliers are likely due to factors like retirement.

The boxplot of job levels and income illustrates the positive
relationship between these variables. It reveals a large income gap
between employees that stay and those that leave in job level 4. Job
levels 1 and 3 have a small trend of attrition with lower median
incomes.

Attrition rates are low for all job levels in the graph of employees
that do not work overtime, under 15% for job level 1 and under 10% for
the others. However, the story changes for employees working at least an
hour of overtime. Over 50% of employees in job level 1 working overtime
choose to leave. This pattern holds true across all job levels except
for job level 4, with attrition rates more than doubling among overtime
workers.

    # extra plots for the appendix

    # plot positive predictor example: overtime and attrition
    # calculation of proportion of employees: employees in overtime group & attrition yes / total employees in overtime group, to normalize each overtime group 
    overtime_bar = cs2_conv %>%
      group_by(OverTime, Attrition) %>%
      dplyr::summarize(n = n()) %>%
      mutate(prop = n / sum(n)) %>%
      filter(Attrition == "Yes") %>% 
      ggplot(aes(y = OverTime, x = prop)) +
      geom_bar(stat = "identity", fill = "#377EB8") +
      labs(y = "Overtime", x = "Proportion of Attrition",
           fill = "Overtime",
           title = "Proportion of Attrition for Employees With and Without Overtime",
           caption = "Normalized by Total Employees per OT Group") +
      theme_bw() +
      scale_fill_brewer(palette = "Set1")
    print(overtime_bar)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_appendix-1.png)

    # ggsave(overtime_bar, filename = "plots/appendix/top3_overtime_bar.png")

    # plot positive predictor: monthly income and overtime
    income_OT_box = ggplot(cs2_conv, aes(x = OverTime, y = MonthlyIncome, fill = Attrition)) +
      geom_boxplot() +
      labs(x = "Overtime", y = "Monthly Income", fill = "Attrition") +
      ggtitle("Monthly Income by Overtime and Attrition") +
      theme_bw() +
      scale_fill_brewer(palette = "Set1")
    print(income_OT_box)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/top3plots_appendix-2.png)

    # ggsave(income_OT_box, filename = "plots/appendix/top3_income_OT_box.png")

The attrition rate for overtime workers is more than three times that of
employees that don’t work any overtime.

There is a larger income gap between attrition groups in employees
working overtime, indicating a possibility that lower income and
overtime make attrition more likely.

**Top three factors leading to attrition: summary of trends, takeaways,
and recommendations**  
**Income**  
There is a large income gap between employees that stay and those that
leave in job level 4. Those that leave have a median income just under
13,000, while those that stay have a median income of almost 16,000. Job
levels 1 and 3 have a small trend of attrition with lower median
incomes. Overall, the median income for employees that leave is smaller
than that of those that stay.

**Job Level**  
Over a quarter of the employees in job level 1 leave the company, a much
higher attrition rate than any other job level.

**Overtime**  
Over 50% of employees in job level 1 working overtime choose to leave.
This pattern holds true across all job levels except for job level 4,
with attrition rates more than doubling among overtime workers.

**Recommendations**  
Job level 4, if below 14,000, increase monthly pay. For all other job
levels, reduce overtime. Job level 1 increase pay and promote to job
level 2.

### Objective 2: Identify job role specific trends.

**For each numerical variable, I plot the job roles in side-by-side box
plots. Using bar plots, I plot the proportion of the levels of each
categorical variable grouped by job roles.**

    # reorder levels of JobRole from low income/level to high
    cs2_conv$JobRole = factor(cs2_conv$JobRole, levels = c("Sales Representative", "Laboratory Technician", "Human Resources", "Research Scientist", "Sales Executive", "Healthcare Representative", "Manufacturing Director", "Research Director", "Manager"))

    # rename numerical and categorical variables for nicer axis titles
    custom_labels = list(
      JobLevel = "Job Level",
      JobRole = "Job Role",
      MaritalStatus = "Marital Status",
      MonthlyIncome = "Monthly Income",
      OverTime = "Overtime",
      StockOptionLevel = "Stock Option Level",
      TotalWorkingYears = "Total Working Years",
      YearsAtCompany = "Years At Company",
      YearsInCurrentRole = "Years in Current Role",
      YearsWithCurrManager = "Years with Current Manager"
    )

    # a function to get the appropriate label from the list above
    get_label = function(var) {
      if (var %in% names(custom_labels)) {
        return(custom_labels[[var]])
      } else {
        return(var)
      }
    }

    # numerical_columns = colnames(cs2_numerical)
    for (numvar in numerical_columns) {
      boxplot = cs2_conv %>%  
        ggplot(aes(x = .data[[numvar]], y = JobRole, fill = JobRole)) +
        geom_boxplot() +
        labs(title = paste("Job Role vs", get_label(numvar)),
             x = get_label(numvar), y = "Job Role") +
        theme_bw() +
        theme(legend.position = "none",
              text = element_text(size = 12)) +
        scale_fill_brewer(palette = "Spectral")
      
      # display each plot
      print(boxplot)
      
      # create a unique filename for each plot
      filename = paste0("plots/JobRole/boxplt_JobRole_vs_", numvar, ".png")
      
      # save the plots
      # ggsave(filename, plot = boxplot, device = "png")
    }

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-1.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-2.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-3.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-4.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-5.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-6.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-7.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-8.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-9.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-10.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-11.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-12.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-13.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-14.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-15.png)

    # categorical_columns = colnames(cs2_categorical)
    cat_cols = categorical_columns[!categorical_columns %in% "JobRole"]

    for (catvar in cat_cols) {
      barplot = cs2_conv %>%
        group_by(JobRole, .data[[catvar]], .drop = FALSE) %>%
        dplyr::summarize(count = n(), .groups = 'drop') %>%
        group_by(JobRole) %>%
        mutate(prop = count / sum(count)) %>%
        ggplot(aes(y = JobRole, x = prop, fill = .data[[catvar]])) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Job Role vs", get_label(catvar)),
             y = "Job Role", x = "Proportion", fill = get_label(catvar)) +
        theme_bw() +
        theme(legend.position = "right",
              text = element_text(size = 12)) +
        scale_fill_brewer(palette = "Set1")
      
      #display each plot
      print(barplot)
      
      # create a unique filename for each plot
      filename = paste0("plots/JobRole/barplot_JobRole_vs_", catvar, ".png")
      
      # save the plots
      # ggsave(filename, plot = barplot, device = "png")
    }

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-16.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-17.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-18.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-19.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-20.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-21.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-22.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-23.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-24.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-25.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-26.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-27.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-28.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-29.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/jobRolePlots-30.png)

**Trends in Job Roles**

1.  **Attrition, Tenure and Role Duration:**

-   Sales reps, lab techs, research scientists, and HR roles leave the
    company sooner than other roles and have the fewest total working
    years and number of companies worked.  
-   Sales reps and lab techs have the shortest duration in role and with
    manager.  
-   Median duration in role and with manager for sales reps, lab techs,
    research scientists, and HR is around 2 years, which is the 25th
    percentile of other roles.  
-   Managers and research directors tend to stay the longest at the
    company.  
-   The highest duration in role and with manager is for managers,
    research directors, and manufacturing directors.

1.  **Income Distribution:**

-   Monthly income splits into three groups (follow-up to investigate
    significance):
    -   Low: Median between $2500-3000 (sales reps, research scientists,
        lab techs, HR)  
    -   Middle: Median between $6500-7000 (sales execs, manufacturing
        directors, healthcare reps)  
    -   High: Median around $16000-17000 (managers, research directors)

1.  **Job Level:**

-   Sales reps have the lowest job levels (1-2).  
-   Research scientists, lab techs, and HR roles range from 1-3.  
-   Sales execs, manufacturing directors and healthcare reps fall in the
    middle range, 2-4.  
-   The only roles with level 5 are managers and research directors,
    with ranges from 3-5.

1.  **Age:**

-   Age follows the same trend as income and job level grouping, though
    there is more overlap in low and middle tiers.  
-   There is a wide age spread for research scientists (less than 20 to
    nearly 60).

1.  **Training, Promotion and Salary Increase:**

-   Training times and percent salary hike are similar for all roles.  
-   Median years since promotion is around 1 for all roles except
    managers whose median is 3 years.

1.  **Monthly, Daily and Hourly Rates:**

-   The spread is nearly the same for all roles.  
-   Managers have the highest monthly and hourly rates, yet the lowest
    daily rates.  
-   HR has the lowest median monthly and hourly rates.  
-   Healthcare reps have the highest daily rates.  
-   This suggests these rates are not related to pay (or billing).

1.  **Employee Demographics:**

-   Employee numbers are generally spread evenly across all roles,
    though HR has higher employee numbers. I assume this is an employee
    company ID number and doubt there is much significance.  
-   Managers tend to live closer to work. Median distance for sales reps
    is intermediate. All other roles have a similar spread in distance
    from home and tend to live further from work.  
-   Most roles are somewhat dominated by men except for sales reps,
    manufacturing directors, and managers.

1.  **Job Involvement and Satisfaction:**

-   Job involvement, stock options, work-life balance, and travel seem
    distributed similarly.  
-   Sales reps unsurprisingly have more frequent travel, yet unlike all
    other job roles, none of them say they have poor work-life
    balance.  
-   Research scientists have the highest proportion of overtime,
    followed by sales reps. Managers have the lowest.  
-   HR personnel have very high relationship satisfaction (with
    manager).  
-   Manufacturing directors have very high environment satisfaction.
    Heathcare reps and lab techs also have positive environment
    satisfaction. However, research directors and managers have the
    highest proportion of low environmental satisfaction across all
    roles.

1.  **Marital Status:**

-   Most sales reps are single, yet divorce rates are lowest among sales
    reps.  
-   Marriage rates are highest among managers.  
-   Divorce rate trends are highest in HR personnel.

1.  **Education:**

-   Sales reps have the lowest education level. Nearly 25% did not
    attend college, and another nearly 25% did not graduate from
    college. None hold doctorate degrees.  
-   All roles range from over 5% with no college to a percentage that
    hold doctorate degrees, even research directors. Additionally,
    research directors have the highest proportion of doctorate
    degrees.  
-   Sales execs have the highest proportion with a master’s degree.

1.  **Job Roles & Departmental Breakdown:**

-   Job Roles align with education field and department.  
-   Only human resource roles and managers hold human resource
    degrees.  
-   Likewise, only sales roles and managers hold marketing degrees.  
-   All the roles have a high proportion of employees with life science
    degrees and also have employees with other types of degrees.  
-   Managers fall into all departments.  
-   HR roles are in the HR department.  
-   Sales reps and execs are in the Sales department.  
-   The rest are in the Research and Development department.

**As job roles seem to segregate into 3 groups by monthly income and
other factors like job level, years in current role, with manager and at
company, total working years and to a lesser extent age, I make a new
median income level variable and group them into low, middle and high
levels. Then I use a t-test to see if the difference in mean incomes
between the pairs of levels (low vs. mid and mid vs. high) are
significantly different. I also plot the derived variables Percent work
years at company and Percent year in role at company vs the job roles
and run t-tests to check for differences in these mean percents between
the grouped income levels.**

    # reorder levels of JobRole
    cs2_conv2$JobRole = factor(cs2_conv2$JobRole, levels = c("Sales Representative", "Laboratory Technician", "Human Resources", "Research Scientist", "Sales Executive", "Healthcare Representative", "Manufacturing Director", "Research Director", "Manager"))

    # group roles by median income level (low, med, high)
    # test difference in salary between low and mid and mid and high
    cs2_conv2 = cs2_conv2 %>% 
      mutate(incomeGroup = case_when(
          JobRole %in% c("Sales Representative", "Research Scientist", "Human Resources", "Laboratory Technician") ~ "Low",
          JobRole %in% c("Sales Executive", "Manufacturing Director", "Healthcare Representative") ~ "Medium",
          TRUE ~ "High"
        )
      ) %>%
      mutate(incomeGroup = factor(incomeGroup, levels = c("Low", "Medium", "High")))

    # check the conversion
    # head(cs2_conv2)

    # filter the data
    cs2_LowMid = cs2_conv2 %>% filter(incomeGroup != "High")
    cs2_MidHigh = cs2_conv2 %>% filter(incomeGroup != "Low")

    # perform t-tests
    t_test_LowMid = t.test(MonthlyIncome ~ incomeGroup, data = cs2_LowMid)
    t_test_MidHigh = t.test(MonthlyIncome ~ incomeGroup, data = cs2_MidHigh)

    # print the results
    t_test_LowMid


        Welch Two Sample t-test

    data:  MonthlyIncome by incomeGroup
    t = -27.646, df = 522.61, p-value < 2.2e-16
    alternative hypothesis: true difference in means between group Low and group Medium is not equal to 0
    95 percent confidence interval:
     -4268.293 -3701.937
    sample estimates:
       mean in group Low mean in group Medium 
                3167.491             7152.606 

    t_test_MidHigh


        Welch Two Sample t-test

    data:  MonthlyIncome by incomeGroup
    t = -31.529, df = 152.77, p-value < 2.2e-16
    alternative hypothesis: true difference in means between group Medium and group High is not equal to 0
    95 percent confidence interval:
     -9904.980 -8736.867
    sample estimates:
    mean in group Medium   mean in group High 
                7152.606            16473.529 

Unsurprisingly, there is overwhelming evidence that mean incomes between
income groups are significantly different.

*For follow-up*  
It would be interesting to follow-up by plotting the 3 income groups and
income or other features of interest split by attrition groups. Also, it
would be good to investigate trends in income, job level, and overtime
between the different attrition groups in different job roles.

### Objective 3: Build a model to predict attrition.

(with a minimum of 60% sensitivity and 60% specificity)

**I start with a Naive Bayes model to predict attrition. There are 33
possible explanatory features in the data set. I choose to start with
the 18 that earlier analyses suggest may be significantly different
between the attrition groups. The goal with this block of code is to
build an externally validated model with 100 different train/test splits
of those 18 features. Then using intuition from earlier analyses, I
remove one feature at a time, until I have maximized the specificity of
the model.**

    # Naive Bayes classifier
    # start with lots of explanatory variables and distill down manually one at a time using domain knowledge

    # str(cs2_conv2) #this is the dataframe with my derived variables too


    # list indexes of explanatory variables 

    # for reference:
    # modelIdx = c(2,4:9,11:19,21,22,25:27,29:40) #list of all possible features
    # modelIdx = c(16,19,20,24,29) #top 5 features

    # start here: list of indexes of 18 significant variables (alpha = 0.05) from t-tests and chi-square tests
    # modelIdx = c(2,4,6,7,12,15,16,17,18,19,20,24,29,30,32,33,34,36) #significant features: sens 85.6%, spec 60.7%

    # end here: list of indexes which produced highest specificity, derived manually
    modelIdx = c(12,15,16,17,19,20,24,29,30,32,33,34,36) #sensitivity: 84.4% specificity: 62.3%

    # other promising combinations:
    # modelIdx = c(2,12,15,16,17,19,20,24,29,30,32,33,34,36) #84.2, 61.4
    # modelIdx = c(12,15,16,17,20,24,29,30,32,33,34,36) #84.6, 61.3
    # modelIdx = c(15,16,17,20,24,29,30,32,33,34,36) #84, 60.5


    # list the explanatory variables used
    names(cs2_conv2[,modelIdx])

     [1] "EnvironmentSatisfaction" "JobInvolvement"          "JobLevel"                "JobRole"                
     [5] "MaritalStatus"           "MonthlyIncome"           "OverTime"                "StockOptionLevel"       
     [9] "TotalWorkingYears"       "WorkLifeBalance"         "YearsAtCompany"          "YearsInCurrentRole"     
    [13] "YearsWithCurrManager"   

    # how many explanatory variables are used
    length(modelIdx)

    [1] 13

    # 100 different splits
    iters = 100

    # make holders for the stats from each iteration
    masterAccu = matrix(nrow = iters, ncol = 2)
    masterPval = matrix(nrow = iters, ncol = 2)
    masterSens = matrix(nrow = iters, ncol = 2)
    masterSpec = matrix(nrow = iters, ncol = 2)

    # 70-30 train/test split
    splitPerc = .7

    for(i in 1:iters)
    {
      set.seed(i)
      
      # sample 70%
      trainIdx = sample(1:dim(cs2_conv2)[1], round(splitPerc*dim(cs2_conv2)[1]))
      # choose the rows that match those sampled numbers for training
      AttritionTrn = (cs2_conv2[trainIdx,])
      # and the others for testing
      AttritionTst = (cs2_conv2[-trainIdx,])
      
      # head(AttritionTrn)
      # head(AttritionTst)
      
      # give the model the training variables, training labels
      modelNB = naiveBayes(AttritionTrn[,modelIdx], AttritionTrn$Attrition, laplace = 1)
      
      # use the model and testing explanatory variables (modelIdx) to predict the testing labels
      preds = predict(modelNB, AttritionTst[,modelIdx])
      
      # make a confusion matrix comparing the predicted labels to the true labels
      # predicted labels = rows, true labels = cols
      CM = confusionMatrix(table(preds, AttritionTst$Attrition))
      
      masterAccu[i,] = c(i, CM$overall["Accuracy"])
      masterPval[i,] = c(i, CM$overall["AccuracyPValue"])
      masterSens[i,] = c(i, CM$byClass["Sensitivity"])
      masterSpec[i,] = c(i, CM$byClass["Specificity"])
    }

    # organize the output data
    colnames(masterAccu) = c("Seed", "Accuracy")
    colnames(masterPval) = c("Seed", "AccuracyPValue")
    colnames(masterSens) = c("Seed", "Sensitivity")
    colnames(masterSpec) = c("Seed", "Specificity")
    NB_results = merge(as.data.frame(masterAccu), as.data.frame(masterPval), by = "Seed", all = TRUE)
    NB_results = merge(NB_results, as.data.frame(masterSens), by = "Seed", all = TRUE)
    NB_results = merge(NB_results, as.data.frame(masterSpec), by = "Seed", all = TRUE)

    NB_stats = colMeans(NB_results[,2:5])
    NB_stats = data.frame(
      Metric = c("Accuracy", "AccuracyPValue", "Sensitivity", "Specificity"),
      Value = c(sprintf("%.1f%%", NB_stats[1]*100),
                sprintf("%.2e", NB_stats[2]),
                sprintf("%.1f%%", NB_stats[3]*100),
                sprintf("%.1f%%", NB_stats[4]*100)))

    # output the metrics of the naive bayes model
    cat("Naive Bayes \n")

    Naive Bayes 

    cat("Accuracy:", NB_stats[1,2], "\n")

    Accuracy: 80.9% 

    cat("Accuracy P-Value:", NB_stats[2,2], "\n")

    Accuracy P-Value: 7.83e-01 

    cat("Sensitivity:", NB_stats[3,2], "\n")

    Sensitivity: 84.4% 

    cat("Specificity:", NB_stats[4,2], "\n")

    Specificity: 62.3% 

I start with these 18 features: Age, BusinessTravel, Department,
DistanceFromHome, EnvironmentSatisfaction, JobInvolvement, JobLevel,
JobRole, JobSatisfaction, MaritalStatus, MonthlyIncome, OverTime,
StockOptionLevel, TotalWorkingYears, WorkLifeBalance, YearsAtCompany,
YearsInCurrentRole, YearsWithCurrManager.

Using the method described above, I am able to achieve the highest
specificity of 62.3% and a combined sensitivity and specificity of 146.7
with 13 features: EnvironmentSatisfaction, JobInvolvement, JobLevel,
JobRole, MaritalStatus, MonthlyIncome, OverTime, StockOptionLevel,
TotalWorkingYears, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole,
YearsWithCurrManager.

**Then I attempt to validate my predicted best features for the model
with help from chatGPT. I asked chatGPT to help me write a nested loop
that would find every combination of a defined number of features, rank
them by highest specificity and return the top ten combinations. For
each iteration of the loop, I asked it to increase the number of
features by one, breaking out of the loop when the specificity stopped
increasing. This is very computationally heavy, and there is a bug in
the code when outputting results related to breaking out of the loop.**

    # perhaps it would be more efficient to do the train test splits on the outer loop?
    # also I should place the progress reporter (Sys.time()) in a different place in the code

    # Function to calculate sensitivity, specificity for all combinations of a given number of variables
    calculate_combinations <- function(modelIdx, max_vars = 18, iters = 10, splitPerc = 0.7) {
      
      # Initialize an empty list to store results
      results_list <- list()
      
      # Initialize a variable to keep track of the maximum specificity reached
      max_specificity_so_far <- 0
      
      # Initialize a variable to store the maximum mean specificity from previous iterations
      max_mean_specificity_previous <- 0

      # Loop over different number of variables
      for (num_vars in seq(8, max_vars)) {
        cat("Number of variables:", num_vars, "\n")
        start_time <- Sys.time()
        
        # Generate all combinations of variables
        index_combinations <- combinat::combn(modelIdx, num_vars)
        
        # Initialize an empty list to store results for this iteration
        temp_results_list <- list()
        
        # Loop through each combination of variables
        for (i in 1:ncol(index_combinations)) {
          index_set <- index_combinations[, i]
          
          # Create holders for the sensitivity, specificity for each iteration
          masterSens <- numeric(iters)
          masterSpec <- numeric(iters)
          
          # Iterate over the specified number of iterations
          for (j in 1:iters) {
            set.seed(j)
            
            # Sample 70% for training
            trainIdx <- sample(1:dim(cs2_conv2)[1], round(splitPerc * dim(cs2_conv2)[1]))
            # Select the rows for training
            AttritionTrn <- cs2_conv2[trainIdx, ]
            # Select the remaining rows for testing
            AttritionTst <- cs2_conv2[-trainIdx, ]
            
            # Temporarily set the current indexes for modeling
            temp_modelIdx <- index_set
            
            # Train the model
            modelNB <- naiveBayes(AttritionTrn[, temp_modelIdx], AttritionTrn$Attrition, laplace = 1)
            
            # Predict using the model
            preds <- predict(modelNB, AttritionTst[, temp_modelIdx])
            
            # Confusion matrix
            CM <- confusionMatrix(table(preds, AttritionTst$Attrition))
            
            # Store sensitivity, specificity
            masterSens[j] <- CM$byClass["Sensitivity"]
            masterSpec[j] <- CM$byClass["Specificity"]
          }
          
          # Average sensitivity, specificity
          avg_sensitivity <- mean(masterSens)
          avg_specificity <- mean(masterSpec)
          
          # Store the results including the combination of variables
          temp_results_list[[i]] <- list(variables = index_set, Sensitivity = avg_sensitivity, Specificity = avg_specificity)
        }
        
        end_time <- Sys.time()
        time_taken <- end_time - start_time
        cat("Time taken:", time_taken, "\n")
        
        # Store the results for this iteration
        results_list[[num_vars]] <- temp_results_list
        
        # Get the maximum mean specificity for this number of variables
        max_mean_specificity_current <- max(sapply(temp_results_list, function(x) x$Specificity))
        
        # Check if the maximum mean specificity for the current number of variables is less than the previous maximum
        if (max_mean_specificity_current < max_mean_specificity_previous) {
          cat("Maximum specificity reached for", num_vars, "variables is less than previous maximum. Stopping further iterations.\n")
          break
        }
        
        # Update the maximum mean specificity from previous iterations
        max_mean_specificity_previous <- max_mean_specificity_current
      }
      
      return(results_list)
    }

    # Define the list of indexes to loop through
    modelIdx <- c(2,4,6,7,12,15,16,17,18,19,20,24,29,30,32,33,34,36) #significant features

    # Define the maximum number of variables
    max_vars <- 18

    # Call the function to calculate combinations
    results <- calculate_combinations(modelIdx, max_vars)

    # Determine the maximum expected number of combinations
    max_combinations <- max_vars * 10  # Assuming a maximum of 10 combinations per number of variables

    # Preallocate the output data frame with maximum expected number of rows
    output_df <- data.frame(Number_of_Variables = integer(max_combinations),
                            Variable_Indexes = character(max_combinations),
                            Mean_Sensitivity = numeric(max_combinations),
                            Mean_Specificity = numeric(max_combinations),
                            Sum_of_Sensitivity_and_Specificity = numeric(max_combinations),
                            stringsAsFactors = FALSE)

    # Loop over different number of variables
    for (num_vars in seq(8, max_vars)) {
      cat("\nTop 10 combinations with the highest specificity for", num_vars, "variables:\n")

      sorted_indices <- order(sapply(results[[num_vars]], function(x) x$Specificity), decreasing = TRUE)
      for (i in 1:min(10, length(sorted_indices))) {
        index <- sorted_indices[i]
        cat("Combination", i, ": Variables =", results[[num_vars]][[index]]$variables, ", Specificity =", results[[num_vars]][[index]]$Specificity, "\n")

        # Assign the results to the preallocated rows in the output data frame
        row_index <- (num_vars - 10) * 10 + i  # Calculate the row index
        output_df$Number_of_Variables[row_index] <- num_vars
        output_df$Variable_Indexes[row_index] <- paste(results[[num_vars]][[index]]$variables, collapse = ", ")
        output_df$Mean_Sensitivity[row_index] <- results[[num_vars]][[index]]$Sensitivity
        output_df$Mean_Specificity[row_index] <- results[[num_vars]][[index]]$Specificity
        output_df$Sum_of_Sensitivity_and_Specificity[row_index] <- results[[num_vars]][[index]]$Sensitivity + results[[num_vars]][[index]]$Specificity
      }
    }

    # Print the final output data frame
    print(output_df)

This is useful for a few reasons. It confirms that the 13 variables I
chose `modelIdx = c(12,15,16,17,19,20,24,29,30,32,33,34,36)` result in
the highest specificity (61.7%) for all combinations of 13 variables. It
also returns higher specificity, 62.6% with 14 variables, the 13 above,
plus `Department` (index 6) and 62.2% with 15 variables, the 13 above,
plus `BusinessTravel` and `DistanceFromHome` (indexes 4 and 7).

**I want to use several of the best combinations of explanatory
variables to try to tune the laplace smoothing parameter. I also want to
see if I can adjust and tune the thresholding, because the dataset is
unbalanced with only 16% “Yes” in the Attrition variable. I evaluate all
combinations of variables and tuning parameters by averaging external
and internal validation results.**

    # computationally expensive, not sure how to do it a better way

    # define parameters (the ones I really used)
    # modelIdx_list = list(
    #   c(2, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36),      # Combination 1
    #   c(12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36),         # Combination 2
    #   c(6, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36),      # Combination 3
    #   c(4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36),   # Combination 4
    #   c(4, 6, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36) # Combination 5
    # )
    # alphas = seq(0.1, 1, 0.1) #smoothing parameters to try
    # thresholds = seq(0.1, 0.6, 0.02) #classifier thresholds to try for "Yes" based on ~0.16 proportion yes to no
    # iters = 100  #train/test split iterations
    # splitPerc = 0.7  #percentage of data for training

    # define shortened parameters for demo purposes
    modelIdx_list = list(
      c(12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36),         # Combination 2 (my hand-picked variables)
      c(4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36)    # Combination 4 (the *best*)
    )
    alphas = seq(0.1, 0.9, 0.2) #smoothing parameters to try
    thresholds = seq(0.3, 0.5, 0.05) #classifier thresholds to try for "Yes" based on ~0.16 proportion yes to no
    iters = 10  #train/test split iterations
    splitPerc = 0.7  #percentage of data for training

    # initialize a list to store results
    results_list = list()

    # time the loop 
    loop_start = Sys.time()

    # loop through modelIdx combinations
    for (modelIdx in modelIdx_list) {
      
      # loop through the alphas
      for (alpha in alphas) {
        
        # loop through the thresholds
        for (threshold in thresholds) {
          
          # time the threshold loop
          thresh_start = Sys.time()
          
          # make holders for the stats from each iteration for this modelIdx, alpha, and threshold
          iteration_stats = matrix(nrow = iters, ncol = 5)
          colnames(iteration_stats) = c("Seed", "Threshold", "Alpha", "Sensitivity", "Specificity")
          
          # counter for row index in iteration_stats
          row_counter = 1
          
          # external cross-validation loop
          for (i in 1:iters) {

            set.seed(i)

            # generate the test train split
            trainIdx = sample(1:nrow(cs2_conv), round(splitPerc * nrow(cs2_conv)))
            AttritionTrn = cs2_conv[trainIdx, ]
            AttritionTst = cs2_conv[-trainIdx, ]

            # train Naive Bayes model with current alpha and feature (modelIdx) set
            modelNB = naiveBayes(AttritionTrn[, modelIdx], AttritionTrn$Attrition, laplace = alpha)

            # predict on testing set
            preds = predict(modelNB, AttritionTst[, modelIdx], type = "raw") # raw returns the probabilities

            attrition_probs = preds[, "Yes"]

            predictions = ifelse(attrition_probs >= threshold, "Yes", "No")

            # calculate confusion matrix
            CM_no = confusionMatrix(table(predictions, AttritionTst$Attrition))

            # get sensitivity and specificity for "No" class
            sens_no = CM_no[4]$byClass["Sensitivity"] # because reference (positive class) is set to no
            spec_no = CM_no[4]$byClass["Specificity"]

            # store results for that iteration
            iteration_stats[row_counter, ] = c(i, threshold, alpha, sens_no, spec_no)
            row_counter = row_counter + 1

          } # complete external cross-validation loop
          
          # initialize variables to store predictions and actual values to calculate confusion matrix after all LOO
          all_predictions = c()
          all_actual = c()
          
          # internal cross-validation loop (Leave-One-Out Cross-Validation)
          for (row in 1:nrow(cs2_conv)) {
            
            # generate the test train split
            AttritionTrn = cs2_conv[-row, ]
            AttritionTst = cs2_conv[row, ]
            
            # train Naive Bayes model with current alpha and feature (modelIdx) set
            modelNB = naiveBayes(AttritionTrn[, modelIdx], AttritionTrn$Attrition, laplace = alpha)
            
            # predict on testing set
            preds = predict(modelNB, AttritionTst[, modelIdx], type = "raw") # raw returns the probabilities
            
            attrition_probs = preds[,"Yes"]
            attrition_probs = attrition_probs[["Yes"]] # get just the probability from the named number

            predictions = ifelse(attrition_probs >= threshold, "Yes", "No")
            
            # accumulate predictions and actual values
            all_predictions = c(all_predictions, predictions)
            all_actual = c(all_actual, as.character(AttritionTst$Attrition)) # turn factors to characters to match all_actual
            
          } # complete internal cross-validation loop

          # calculate confusion matrix using all predictions and actual values
          CM_no = confusionMatrix(table(all_predictions, all_actual))
          
          # get sensitivity and specificity for "No" class
          sens_icv = CM_no[4]$byClass["Sensitivity"] # because reference (positive class) is set to no
          spec_icv = CM_no[4]$byClass["Specificity"]
          
          # calculate mean sensitivity and specificity over ECV and ICV for this combination of modelIdx, alpha, and threshold
          # not sure if this is the best way since there are many more test/train splits in ICV
          # also don't know if people ever really average ECV and ICV results
          avg_sens = mean(c(mean(iteration_stats[, "Sensitivity"]), sens_icv))
          avg_spec = mean(c(mean(iteration_stats[, "Specificity"]), spec_icv))
          
          # store results for this combination of modelIdx, alpha, and threshold
          results_list = c(results_list, list(
            data.frame(
              ModelIdx = toString(modelIdx),
              Alpha = alpha,
              Threshold = threshold,
              AvgSensitivity = avg_sens,
              AvgSpecificity = avg_spec,
              AvgSum = avg_sens + avg_spec
            )
          ))

            # time the threshold loop
            thresh_end = Sys.time()
            time_taken_thresh = difftime(thresh_end, thresh_start, units = "secs")
            cat("Threshold Loop Time:", time_taken_thresh, "secs \n")
            cat("ModelIdx:", toString(modelIdx), "- Alpha:", alpha, "- Threshold:", threshold, "\n")
          
        } # complete threshold loop

      } # complete alpha loop
      
    } # complete modelIdx loop

    Threshold Loop Time: 5.592388 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.3 
    Threshold Loop Time: 6.021531 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.35 
    Threshold Loop Time: 5.547989 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.4 
    Threshold Loop Time: 5.553841 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.45 
    Threshold Loop Time: 6.12177 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.5 
    Threshold Loop Time: 6.420382 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.3 
    Threshold Loop Time: 5.520584 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.35 
    Threshold Loop Time: 7.136181 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.4 
    Threshold Loop Time: 6.214353 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.45 
    Threshold Loop Time: 5.380049 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.5 
    Threshold Loop Time: 6.685035 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.3 
    Threshold Loop Time: 4.998083 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.35 
    Threshold Loop Time: 4.644609 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.4 
    Threshold Loop Time: 5.171444 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.45 
    Threshold Loop Time: 4.761993 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.5 
    Threshold Loop Time: 4.782534 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.3 
    Threshold Loop Time: 5.089848 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.35 
    Threshold Loop Time: 4.768193 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.4 
    Threshold Loop Time: 4.86963 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.45 
    Threshold Loop Time: 4.742829 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.5 
    Threshold Loop Time: 4.720387 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.3 
    Threshold Loop Time: 4.678431 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.35 
    Threshold Loop Time: 5.60258 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.4 
    Threshold Loop Time: 5.327845 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.45 
    Threshold Loop Time: 4.64058 secs 
    ModelIdx: 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.5 
    Threshold Loop Time: 6.054231 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.3 
    Threshold Loop Time: 5.122041 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.35 
    Threshold Loop Time: 5.147665 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.4 
    Threshold Loop Time: 5.249031 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.45 
    Threshold Loop Time: 5.486604 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.1 - Threshold: 0.5 
    Threshold Loop Time: 5.605057 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.3 
    Threshold Loop Time: 5.666585 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.35 
    Threshold Loop Time: 5.104779 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.4 
    Threshold Loop Time: 5.114549 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.45 
    Threshold Loop Time: 5.132987 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.3 - Threshold: 0.5 
    Threshold Loop Time: 5.240235 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.3 
    Threshold Loop Time: 5.867367 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.35 
    Threshold Loop Time: 5.544347 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.4 
    Threshold Loop Time: 4.961782 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.45 
    Threshold Loop Time: 5.413003 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.5 - Threshold: 0.5 
    Threshold Loop Time: 5.472714 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.3 
    Threshold Loop Time: 5.191352 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.35 
    Threshold Loop Time: 5.131339 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.4 
    Threshold Loop Time: 5.16242 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.45 
    Threshold Loop Time: 5.0915 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.7 - Threshold: 0.5 
    Threshold Loop Time: 5.148092 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.3 
    Threshold Loop Time: 5.51221 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.35 
    Threshold Loop Time: 5.823147 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.4 
    Threshold Loop Time: 5.233849 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.45 
    Threshold Loop Time: 5.136843 secs 
    ModelIdx: 4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36 - Alpha: 0.9 - Threshold: 0.5 

    # loop time
    loop_end = Sys.time()
    time_taken_loop = difftime(loop_end, loop_start, units = "mins")
    cat("Total Loop Time:", time_taken_loop, "mins \n")

    Total Loop Time: 4.477766 mins 

    # convert results_list to a data frame
    results_df = do.call(rbind, results_list)

    # output top 10 combinations for each ModelIdx
    top_10_combos = results_df %>%
      arrange(ModelIdx, desc(AvgSum), desc(AvgSpecificity), desc(AvgSensitivity)) %>%
      group_by(ModelIdx) %>%
      #top_n(10, wt = AvgSum)
      #make a shorter version for demo purpose
      top_n(5, wt = AvgSum)
    kable(top_10_combos, caption = "demo top 10 sensitivity + specificity tuned parameter combos") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
demo top 10 sensitivity + specificity tuned parameter combos
</caption>
<thead>
<tr>
<th style="text-align:left;">
ModelIdx
</th>
<th style="text-align:right;">
Alpha
</th>
<th style="text-align:right;">
Threshold
</th>
<th style="text-align:right;">
AvgSensitivity
</th>
<th style="text-align:right;">
AvgSpecificity
</th>
<th style="text-align:right;">
AvgSum
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.8390322
</td>
<td style="text-align:right;">
0.6387663
</td>
<td style="text-align:right;">
1.477798
</td>
</tr>
<tr>
<td style="text-align:left;">
12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.8415109
</td>
<td style="text-align:right;">
0.6349447
</td>
<td style="text-align:right;">
1.476455
</td>
</tr>
<tr>
<td style="text-align:left;">
12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.3
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.8403885
</td>
<td style="text-align:right;">
0.6349447
</td>
<td style="text-align:right;">
1.475333
</td>
</tr>
<tr>
<td style="text-align:left;">
12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.9
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.8417456
</td>
<td style="text-align:right;">
0.6335161
</td>
<td style="text-align:right;">
1.475262
</td>
</tr>
<tr>
<td style="text-align:left;">
12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.7
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.8401521
</td>
<td style="text-align:right;">
0.6349447
</td>
<td style="text-align:right;">
1.475097
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.7
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.8244917
</td>
<td style="text-align:right;">
0.6559893
</td>
<td style="text-align:right;">
1.480481
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.9
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.8242569
</td>
<td style="text-align:right;">
0.6559893
</td>
<td style="text-align:right;">
1.480246
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.5
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.8235836
</td>
<td style="text-align:right;">
0.6559893
</td>
<td style="text-align:right;">
1.479573
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.3
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.8228953
</td>
<td style="text-align:right;">
0.6559893
</td>
<td style="text-align:right;">
1.478884
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.1
</td>
<td style="text-align:right;">
0.45
</td>
<td style="text-align:right;">
0.8210799
</td>
<td style="text-align:right;">
0.6547697
</td>
<td style="text-align:right;">
1.475850
</td>
</tr>
</tbody>
</table>

Based on this parameter tuning, the highest sum of sensitivity and
specificity scores my Naive Bayes model achieves is 148.79 (Sensitivity
of 83.11% and Specificity of 65.68%). The optimization was achieved with
`ModelIdx = c(4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36)`,
which are the indexes of the columns in `cs2_conv`, `alpha = 0.7` and
`threshold (of "Yes" >=) = 0.46`. The variables are `BusinessTravel`,
`DistanceFromHome`, `EnvironmentSatisfaction`, `JobInvolvement`,
`JobLevel`, `JobRole`, `MaritalStatus`, `MonthlyIncome`, `OverTime`,
`StockOptionLevel`, `TotalWorkingYears`, `WorkLifeBalance`,
`YearsAtCompany`, `YearsInCurrentRole`, `YearsWithCurrManager`. Now, I
will train my model using these and the full data set and use that to
predict the labels on the competition set.

**I would like to compare a k-Nearest Neighbors (k-NN) model to the
Naive Bayes model. I want to see if I can use k-NN with both numerical
and categorical variables, or just numerical (maybe convert the
numerical factors back to numbers). Another option would be to fit
separate models to each category in the categorical variables. I may
want to experiment with the explanatory variables used, scale the
variables, try ECV + ICV, tune k, tune the threshold or over or
under-sample. I start by transforming the variables.**

    # starting with the original dataset `cs2` since the ordinal categorical variables are in an integer class
    # there are a few other variables that I could transform into ranked like business travel
    # not sure it is valid to do it for marital status or overtime

    # make a copy of the original dataset
    numerical_df = cs2

    # check the data
    # head(numerical_df$BusinessTravel)

    # reorder factor levels
    numerical_df$BusinessTravel = factor(numerical_df$BusinessTravel, levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"), labels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))

    # rename the levels to numbers
    levels(numerical_df$BusinessTravel) = c(1:3)

    # convert factor to integer
    numerical_df$BusinessTravel = as.integer(numerical_df$BusinessTravel)

    # check the conversion
    # head(numerical_df$BusinessTravel)

    # repeat for marital status and overtime :/
    # head(numerical_df$MaritalStatus)
    numerical_df$MaritalStatus = factor(numerical_df$MaritalStatus, levels = c("Single", "Married", "Divorced"), labels = c("Single", "Married", "Divorced"))
    levels(numerical_df$MaritalStatus) = c(1:3)
    numerical_df$MaritalStatus = as.integer(numerical_df$MaritalStatus)
    # head(numerical_df$MaritalStatus)

    # head(numerical_df$OverTime)
    numerical_df$OverTime = factor(numerical_df$OverTime, levels = c("Yes", "No"), labels = c("Yes", "No"))
    levels(numerical_df$OverTime) = c(1:2)
    numerical_df$OverTime = as.integer(numerical_df$OverTime)
    # head(numerical_df$OverTime)

    # scale the variables
    # str(numerical_df)
    # list of columns to exclude from scaling
    exclude_columns = c("ID", "EmployeeCount", "EmployeeNumber", "StandardHours")
    # scale the numeric variables and reassign values to same columns
    numerical_df = numerical_df %>%
      mutate_at(vars(-matches(paste(exclude_columns, collapse = "|"))), 
                .funs = function(x) if(is.numeric(x)) scale(x) else x)
    # str(numerical_df)

**This is a modified version of the NB variable optimization code block
from above. I start with the variables with evidence of signficant (p
&lt; 0.05) differences between the attrition groups. They are either
integers or I can reasonably order their levels and tranform them into
integers. If they can’t reasonably be ordered, for example department
and job role, I don’t include them. This loop finds all the unique
combinations of a specified number of these scaled, numeric variables.
It iterates through a number of train/test splits and finds the mean
sensitivity, specificity and sum of those for each combination for that
number of variables. It returns the top ten combinations for that number
of variables. Then each loop increases the number of variables used in
the model by one.**

    # this is a computationally heavy, lengthy process depending on iterations.

    modelIdx_num = c(2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36) # sig numeric vars (16)

    # Function to calculate sensitivity, specificity for all combinations of a given number of variables
    calculate_combinations <- function(modelIdx, max_vars = 15, iters = 10, splitPerc = 0.7) {
      # Initialize an empty list to store results
      results_list <- list()
      
      # Initialize a variable to keep track of the maximum specificity reached
      # max_specificity_so_far <- 0
      
      # Initialize a variable to store the maximum mean specificity from previous iterations
      # max_mean_specificity_previous <- 0

      # Loop over different number of variables
      for (num_vars in seq(8, max_vars)) {
        cat("Number of variables:", num_vars, "\n")
        start_time <- Sys.time()
        
        # Generate all combinations of variables
        index_combinations <- combinat::combn(modelIdx, num_vars)
        
        # Initialize an empty list to store results for this iteration
        temp_results_list <- list()
        
        # Loop through each combination of variables
        for (i in 1:ncol(index_combinations)) {
          index_set <- index_combinations[, i]
          
          # Create holders for the sensitivity, specificity for each iteration
          masterSens <- numeric(iters)
          masterSpec <- numeric(iters)
          
          # Iterate over the specified number of iterations
          for (j in 1:iters) {
            set.seed(j)
            
            # Sample 70% for training
            trainIdx <- sample(1:dim(numerical_df)[1], round(splitPerc * dim(numerical_df)[1]))
            # Select the rows for training
            AttritionTrn <- numerical_df[trainIdx, ]
            # Select the remaining rows for testing
            AttritionTst <- numerical_df[-trainIdx, ]
            
            # Temporarily set the current indexes for modeling
            temp_modelIdx <- index_set
            
            # Train the model
            classifications <- knn(AttritionTrn[, temp_modelIdx], AttritionTst[, temp_modelIdx], AttritionTrn$Attrition, prob = TRUE, k = 15)

            # Predict using the model
            # preds <- predict(modelNB, AttritionTst[, temp_modelIdx])

            # Confusion matrix
            CM <- confusionMatrix(table(classifications, AttritionTst$Attrition))
            
            # Store sensitivity, specificity
            masterSens[j] <- CM$byClass["Sensitivity"]
            masterSpec[j] <- CM$byClass["Specificity"]
          }
          
          # Average sensitivity, specificity
          avg_sensitivity <- mean(masterSens)
          avg_specificity <- mean(masterSpec)
          
          # Store the results including the combination of variables
          temp_results_list[[i]] <- list(variables = index_set, Sensitivity = avg_sensitivity, Specificity = avg_specificity)
        }
        
        end_time <- Sys.time()
        time_taken <- end_time - start_time
        cat("Time taken:", time_taken, "\n")
        
        # Store the results for this iteration
        results_list[[num_vars]] <- temp_results_list
        
        # Get the maximum mean specificity for this number of variables
        max_mean_specificity_current <- max(sapply(temp_results_list, function(x) x$Specificity))
        
        # Check if the maximum mean specificity for the current number of variables is less than the previous maximum
        # if (max_mean_specificity_current < max_mean_specificity_previous) {
        #   cat("Maximum specificity reached for", num_vars, "variables is less than previous maximum. Stopping further iterations.\n")
        #   break
        # }
        
        # Update the maximum mean specificity from previous iterations
        # max_mean_specificity_previous <- max_mean_specificity_current
      }
      
      return(results_list)
    }

    # Define the list of indexes to loop through
    modelIdx <- c(2,4,7,12,15,16,18,19,20,24,29,30,32,33,34,36) #significant features

    # Define the maximum number of variables
    max_vars <- 15

    # Call the function to calculate combinations
    results <- calculate_combinations(modelIdx, max_vars)

    # Determine the maximum expected number of combinations
    max_combinations <- (max_vars - 8) * 10  # Assuming a maximum of 10 combinations per number of variables

    # Preallocate the output data frame with maximum expected number of rows
    output_df <- data.frame(Number_of_Variables = integer(max_combinations),
                            Variable_Indexes = character(max_combinations),
                            Mean_Sensitivity = numeric(max_combinations),
                            Mean_Specificity = numeric(max_combinations),
                            Sum_of_Sensitivity_and_Specificity = numeric(max_combinations),
                            stringsAsFactors = FALSE)

    # Loop over different number of variables
    for (num_vars in seq(8, max_vars)) {
      cat("\nTop 10 combinations with the highest specificity for", num_vars, "variables:\n")

      sorted_indices <- order(sapply(results[[num_vars]], function(x) x$Specificity), decreasing = TRUE)
      for (i in 1:min(10, length(sorted_indices))) {
        index <- sorted_indices[i]
        # cat("Combination", i, ": Variables =", results[[num_vars]][[index]]$variables, ", Specificity =", results[[num_vars]][[index]]$Specificity, "\n")

        # Assign the results to the preallocated rows in the output data frame
        row_index <- (num_vars - 8) * 10 + i  # Calculate the row index
        output_df$Number_of_Variables[row_index] <- num_vars
        output_df$Variable_Indexes[row_index] <- paste(results[[num_vars]][[index]]$variables, collapse = ", ")
        output_df$Mean_Sensitivity[row_index] <- results[[num_vars]][[index]]$Sensitivity
        output_df$Mean_Specificity[row_index] <- results[[num_vars]][[index]]$Specificity
        output_df$Sum_of_Sensitivity_and_Specificity[row_index] <- results[[num_vars]][[index]]$Sensitivity + results[[num_vars]][[index]]$Specificity
      }
    }

    # Print the final output data frame
    print(output_df)

From a pool of these variables, `Age`, `BusinessTravel`,
`DistanceFromHome`, `EnvironmentSatisfaction`, `JobInvolvement`,
`JobLevel`, `JobRole`, `JobSatisfaction`, `MaritalStatus`,
`MonthlyIncome`, `OverTime`, `StockOptionLevel`, `TotalWorkingYears`,
`WorkLifeBalance`, `YearsAtCompany`, `YearsInCurrentRole`,
`YearsWithCurrManager`, I try to predict which combinations might yield
the highest sensitivity and specificity. Overall, it doesn’t look very
promising, perhaps because I didn’t adjust the threshold or tune the
parameter k. I choose generally the best performing set of increasing
numbers of variables to use in the next block of code, where I will tune
the parameters.

**Using several of the what seem likely to be the best combinations of
explanatory variables, I tune several parameters in the k-NN model. I
try to adjust and tune the thresholding, because the dataset is
unbalanced with only 16% “Yes” in the Attrition variable. I also tune
the number of neighbors, k. I evaluate all combinations of variables and
tuning parameters with both external and internal validation. There is
probably a better, clearer way to tune k.**

    # I comment out some parameters to make a demo version of this code, so that it doesn't take so long to run.

    # define parameters
    modelIdx_list = list(
      c(2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36), # sig numeric vars (16)
      # c(12,16,19,24,29,33,34,36), #8var
      # c(16,18,20,24,29,33,34,36), #8var
      # c(2,12,16,19,24,29,33,34,36), #9var
      # c(2,12,16,19,24,29,30,33,34,36), #10var
      # c(2,12,16,19,20,24,29,30,33,34,36), #11var
      # c(2,7,16,18,19,20,24,29,30,33,34,36), #12var
      # c(2,7,12,16,18,19,20,24,29,30,33,34,36), #13var
      c(2,7,12,15,16,18,19,20,24,29,30,33,34,36), #14var
      # c(2,7,12,15,16,18,19,20,24,29,30,32,33,34,36), #15var
      # c(12,15,16,19,20,24,29,30,32,33,34,36),     # NB Combination 2 (my hand-picked variables) minus job role
      c(4,7,12,15,16,19,20,24,29,30,32,33,34,36)    # NB Combination 4 (the *best*) minus job role
      # c(2, 7, 20, 22, 25, 30, 31, 33, 34, 35, 36) # all numeric variables
    )

    # actual set of parameters used
    # thresholds = seq(0.1, 0.6, 0.02) #classifier thresholds to try for "Yes" based on ~0.16 proportion yes to no
    # numKs = seq(1, 49, 2) #odd Ks to try
    # iters = 100  #train/test split iterations
    # splitPerc = 0.7  #percentage of data for training

    # demo set
    thresholds = seq(0.1, 0.5, 0.05) #classifier thresholds to try for "Yes" based on ~0.16 proportion yes to no
    numKs = seq(3, 49, 2) #odd Ks to try
    iters = 10  #train/test split iterations
    splitPerc = 0.7  #percentage of data for training

    # for debugging
    # modelIdx = c(2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36)
    # threshold = 0.5
    # k = 11
    # i = 1

    # initialize a list to store results
    results_list = list()

    # time the loop 
    loop_start = Sys.time()
    cat("Start time:", loop_start, "\n")

    Start time: 1724516800 

    # loop through modelIdx combinations
    for (modelIdx in modelIdx_list) {
      
      # loop through the thresholds
      for (threshold in thresholds) {
        
        # loop through the Ks
        for (k in numKs) {
          
          # time the K loop
          k_start = Sys.time()
          
          # make holders for the stats from each iteration for this modelIdx, threshold and K
          iteration_stats = matrix(nrow = iters, ncol = 5)
          colnames(iteration_stats) = c("Seed", "Threshold", "K", "Sensitivity", "Specificity")
          
          # counter for row index in iteration_stats
          row_counter = 1
          
          # external cross-validation loop
          for (i in 1:iters) {

            set.seed(i)

            # generate the test train split
            trainIdx = sample(1:nrow(numerical_df), round(splitPerc * nrow(numerical_df)))
            AttritionTrn = numerical_df[trainIdx, ]
            AttritionTst = numerical_df[-trainIdx, ]

            # train kNN model with current k and feature (modelIdx) set
            classifications = knn(AttritionTrn[, modelIdx], AttritionTst[, modelIdx], AttritionTrn$Attrition, prob = T, k = k)
            # create a table of predicted vs actual values for assessing classification accuracy
            #table(classifications, AttritionTst$Attrition)
       
            # display the classifications and their attributes; useful for debugging and understanding model output
            classifications
            attributes(classifications)
            
            # compute probabilities specifically for the "YES" class, adjusting based on predicted labels
            probs = ifelse(classifications == "YES", attributes(classifications)$prob, 1 - attributes(classifications)$prob)

            # apply the new threshold to reclassify observations
            NewClass = ifelse(probs >= threshold, "Yes", "No")

            # this didn't work if all the predictions were No because the Yes row was missing 
            # tabulate the new classifications against actual values
            # table(NewClass, AttritionTst$Attrition)
            # calculate the confusion matrix
            # don't need to relevel the confusion matrix because I am looking at sensitivity + specificity
            # CM_no = confusionMatrix(table(relevel(as.factor(NewClass), ref = "No"), AttritionTst$Attrition), mode = "everything")
            
            # create a table with rows for both "Yes" and "No" classes (in the event there are zero "Yes"s)
            table_predicted_actual = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("No", "Yes"), c("No", "Yes")))
            
            # populate the counts based on the predictions and actual classes
            table_predicted_actual["No", "No"] = sum(NewClass == "No" & AttritionTst$Attrition == "No")
            table_predicted_actual["No", "Yes"] = sum(NewClass == "No" & AttritionTst$Attrition == "Yes")
            table_predicted_actual["Yes", "No"] = sum(NewClass == "Yes" & AttritionTst$Attrition == "No")
            table_predicted_actual["Yes", "Yes"] = sum(NewClass == "Yes" & AttritionTst$Attrition == "Yes")

            # calculate the confusion matrix for this iteration
            CM_no = confusionMatrix(table_predicted_actual, mode = "everything")
            
            # get sensitivity and specificity for "No" class
            sens_no = CM_no[4]$byClass["Sensitivity"] # because reference (positive class) is set to no
            spec_no = CM_no[4]$byClass["Specificity"]

            # store results for that iteration
            iteration_stats[row_counter, ] = c(i, threshold, k, sens_no, spec_no)
            row_counter = row_counter + 1

          } # complete external cross-validation loop
          
          # initialize variables to store predictions and actual values to calculate confusion matrix after all LOO
          all_predictions = c()
          all_actual = c()
          
          # internal cross-validation (Leave-One-Out Cross-Validation)
          classifications = knn.cv(numerical_df[, modelIdx], numerical_df$Attrition,
                             prob = TRUE, k = k)
          # create a table of predicted vs actual values for assessing classification accuracy
          #table(classifications, AttritionTst$Attrition)
       
          # display the classifications and their attributes; useful for debugging and understanding model output
          classifications
          attributes(classifications)
            
          # compute probabilities specifically for the "YES" class, adjusting based on predicted labels
          probs = ifelse(classifications == "YES", attributes(classifications)$prob, 1 - attributes(classifications)$prob)

          # apply the new threshold to reclassify observations
          NewClass = ifelse(probs >= threshold, "Yes", "No")
          
          # tabulate the new classifications against actual values
          # table(NewClass, AttritionTst$Attrition)
          # calculate the confusion matrix
          # don't need to relevel the confusion matrix because I am looking about sensitivity + specificity
          # CM_no = confusionMatrix(table(relevel(as.factor(NewClass), ref = "No"), cs2_conv$Attrition), mode = "everything")
          
          # create a table with rows for both "Yes" and "No" classes (in the event there are zero "Yes"s)
          table_predicted_actual = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("No", "Yes"), c("No", "Yes")))
            
          # populate the counts based on the predictions and actual classes
          table_predicted_actual["No", "No"] = sum(NewClass == "No" & cs2_conv$Attrition == "No")
          table_predicted_actual["No", "Yes"] = sum(NewClass == "No" & cs2_conv$Attrition == "Yes")
          table_predicted_actual["Yes", "No"] = sum(NewClass == "Yes" & cs2_conv$Attrition == "No")
          table_predicted_actual["Yes", "Yes"] = sum(NewClass == "Yes" & cs2_conv$Attrition == "Yes")
            
          # calculate the confusion matrix for this iteration
          CM_no = confusionMatrix(table_predicted_actual, mode = "everything")
            
          # get sensitivity and specificity for "No" class
          sens_icv = CM_no[4]$byClass["Sensitivity"] # because reference (positive class) is set to no
          spec_icv = CM_no[4]$byClass["Specificity"]

          # calculate mean sensitivity and specificity over ECV and ICV for this combination of modelIdx, threshold and k
          # not sure if I should even average these??
          avg_sens = mean(c(mean(iteration_stats[, "Sensitivity"]), sens_icv))
          avg_spec = mean(c(mean(iteration_stats[, "Specificity"]), spec_icv))
          
          # store results for this combination of modelIdx, threshold and K
          results_list = c(results_list, list(
            data.frame(
              ModelIdx = toString(modelIdx),
              Threshold = threshold,
              K = k,
              AvgSensitivity = avg_sens,
              AvgSpecificity = avg_spec,
              AvgSum = avg_sens + avg_spec
            )
          ))

            # time the k loop
            k_end = Sys.time()
            time_taken_thresh = difftime(k_end, k_start, units = "secs")
            # cat("K Loop Time:", time_taken_thresh, "secs \n")
            # cat("ModelIdx:", toString(modelIdx), "- Threshold:", threshold, "- K:", k, "\n")
          
        } # complete k loop

      } # complete threshold loop
      
      # print progress report
      progress_time = Sys.time()
      progress = difftime(progress_time, loop_start, units = "mins")
      cat("Total Elapsed Time:", progress, "mins \n")
      
    } # complete modelIdx loop

    Total Elapsed Time: 0.3571209 mins 
    Total Elapsed Time: 0.7068673 mins 
    Total Elapsed Time: 1.113254 mins 

    # loop time
    # loop_end = Sys.time()
    # time_taken_loop = difftime(loop_end, loop_start, units = "mins")
    # cat("Total Loop Time:", time_taken_loop, "mins \n")

    # convert results_list to a data frame
    results_df = do.call(rbind, results_list)

    # plot the optimal k (though not particularly helpful or needed with the other tuning)
    # aggregate results by K
    results_agg = results_df %>%
      group_by(K) %>%
      dplyr::summarize(Mean_Sensitivity = mean(AvgSensitivity),
                Mean_Specificity = mean(AvgSpecificity))
    # plot the average sensitivity and specificity for each K
    ggplot(results_agg, aes(x = K)) +
      geom_line(aes(y = Mean_Sensitivity, color = "Mean Sensitivity")) +
      geom_line(aes(y = Mean_Specificity, color = "Mean Specificity")) +
      labs(title = "Mean sensitivity and specificity for different values of K",
           x = "K parameter", y = "Metric", color = "Metric") +
      scale_color_manual(values = c("Mean Sensitivity" = "darkgreen", "Mean Specificity" = "darkblue")) +
      theme_bw()

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/KNNtuning-1.png)

    # output top 10 combinations for each ModelIdx
    top_10_combos = results_df %>%
      arrange(ModelIdx, desc(AvgSum), desc(AvgSpecificity), desc(AvgSensitivity)) %>%
      group_by(ModelIdx) %>%
      top_n(10, wt = AvgSum)
    kable(top_10_combos, caption = "demo top 10 sensitivity + specificity tuned knn parameters") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
demo top 10 sensitivity + specificity tuned knn parameters
</caption>
<thead>
<tr>
<th style="text-align:left;">
ModelIdx
</th>
<th style="text-align:right;">
Threshold
</th>
<th style="text-align:right;">
K
</th>
<th style="text-align:right;">
AvgSensitivity
</th>
<th style="text-align:right;">
AvgSpecificity
</th>
<th style="text-align:right;">
AvgSum
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
0.8368230
</td>
<td style="text-align:right;">
0.6553327
</td>
<td style="text-align:right;">
1.492156
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.8195869
</td>
<td style="text-align:right;">
0.6709875
</td>
<td style="text-align:right;">
1.490574
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0.7915156
</td>
<td style="text-align:right;">
0.6956844
</td>
<td style="text-align:right;">
1.487200
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0.8248028
</td>
<td style="text-align:right;">
0.6613105
</td>
<td style="text-align:right;">
1.486113
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.8019084
</td>
<td style="text-align:right;">
0.6836708
</td>
<td style="text-align:right;">
1.485579
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.8388773
</td>
<td style="text-align:right;">
0.6412330
</td>
<td style="text-align:right;">
1.480110
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
0.8068725
</td>
<td style="text-align:right;">
0.6692766
</td>
<td style="text-align:right;">
1.476149
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.7747236
</td>
<td style="text-align:right;">
0.6965216
</td>
<td style="text-align:right;">
1.471245
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
0.8256930
</td>
<td style="text-align:right;">
0.6421069
</td>
<td style="text-align:right;">
1.467800
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 4, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.10
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.6672257
</td>
<td style="text-align:right;">
0.7987358
</td>
<td style="text-align:right;">
1.465962
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
0.8100142
</td>
<td style="text-align:right;">
0.6421706
</td>
<td style="text-align:right;">
1.452185
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.7889211
</td>
<td style="text-align:right;">
0.6613497
</td>
<td style="text-align:right;">
1.450271
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
0.7937247
</td>
<td style="text-align:right;">
0.6544004
</td>
<td style="text-align:right;">
1.448125
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.8173574
</td>
<td style="text-align:right;">
0.6294154
</td>
<td style="text-align:right;">
1.446773
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.8091314
</td>
<td style="text-align:right;">
0.6371005
</td>
<td style="text-align:right;">
1.446232
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0.8734955
</td>
<td style="text-align:right;">
0.5707366
</td>
<td style="text-align:right;">
1.444232
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.7705838
</td>
<td style="text-align:right;">
0.6735651
</td>
<td style="text-align:right;">
1.444149
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.8764539
</td>
<td style="text-align:right;">
0.5676696
</td>
<td style="text-align:right;">
1.444123
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0.8709846
</td>
<td style="text-align:right;">
0.5730479
</td>
<td style="text-align:right;">
1.444033
</td>
</tr>
<tr>
<td style="text-align:left;">
2, 7, 12, 15, 16, 18, 19, 20, 24, 29, 30, 33, 34, 36
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.8676089
</td>
<td style="text-align:right;">
0.5762523
</td>
<td style="text-align:right;">
1.443861
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.7583289
</td>
<td style="text-align:right;">
0.7413052
</td>
<td style="text-align:right;">
1.499634
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
49
</td>
<td style="text-align:right;">
0.8057511
</td>
<td style="text-align:right;">
0.6937896
</td>
<td style="text-align:right;">
1.499541
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0.7694411
</td>
<td style="text-align:right;">
0.7289421
</td>
<td style="text-align:right;">
1.498383
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.8191319
</td>
<td style="text-align:right;">
0.6781339
</td>
<td style="text-align:right;">
1.497266
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.7807184
</td>
<td style="text-align:right;">
0.7157678
</td>
<td style="text-align:right;">
1.496486
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
0.7962160
</td>
<td style="text-align:right;">
0.7000963
</td>
<td style="text-align:right;">
1.496312
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:right;">
0.7892095
</td>
<td style="text-align:right;">
0.7068075
</td>
<td style="text-align:right;">
1.496017
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
0.7746572
</td>
<td style="text-align:right;">
0.7148029
</td>
<td style="text-align:right;">
1.489460
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
0.8105285
</td>
<td style="text-align:right;">
0.6769495
</td>
<td style="text-align:right;">
1.487478
</td>
</tr>
<tr>
<td style="text-align:left;">
4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36
</td>
<td style="text-align:right;">
0.15
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
0.8100356
</td>
<td style="text-align:right;">
0.6700381
</td>
<td style="text-align:right;">
1.480074
</td>
</tr>
</tbody>
</table>

Based on this parameter tuning, the highest sum of sensitivity and
specificity scores my k-NN model achieves is 148.78 (Sensitivity of
75.16% and Specificity of 73.62%). The optimization was achieved with
`ModelIdx = c(4, 7, 12, 15, 16, 19, 20, 24, 29, 30, 32, 33, 34, 36)`,
which are the indexes of the columns in `cs2` (and `numerical_df`) and
`k = 41` and `threshold (of "Yes" >=) = 0.14`. This is the same list of
variables that achieve the best performance (148.79) in the Naive Bayes
model, minus `JobRole`. Sensitivity and specificity are more balanced in
this model, probably because the threshold here is quite low, 0.14
versus 0.46 in the Naive Bayes model. I think I will use the Naive Bayes
model for the final predictions because it has a tiny bit higher sum of
sensitivity and specificity (only 0.01), without using what seems like a
very low threshold 0.14, when the percentage of Attrition in the actual
data set is ~16%. I would prefer to use the simpler model. While the
Naive Bayes model does use one more variable, I think the work of
pre-processing variables for k-NN offsets the extra variable.

### Objective 4: Build a R Shiny app to visualize some of the relationships.

**This R Shiny app displays the distributions of monthly income by
attrition group in a boxplot and histograms. A user can select
individual or combinations of job roles and also a salary range. The app
will interactively update with the distributions for the user selections
and will display the percent of selected employees in each attrition
group.**

R Shiny Employee Attrition and Income App is here:
<https://kdhenderson.shinyapps.io/Employee_Attrition_and_Income/>.

### Objective 5: Predict the attrition on the competition set.

**I import the unlabeled competition data set from the cloud as
`competition`. I convert the numerical ranked variables to factors. So
that my column indexes are the same as those in the primary data set, I
add an `Attrition` column. I train a Naive Bayes model on the entire
labeled primary data set using the tuned alpha parameter. Then I adjust
the classifications with the tuned threshold.**

    # make predictions with Naive Bayes model

    # variables used: `BusinessTravel`, `DistanceFromHome`, `EnvironmentSatisfaction`, `JobInvolvement`, `JobLevel`, `JobRole`, `MaritalStatus`, `MonthlyIncome`, `OverTime`, `StockOptionLevel`, `TotalWorkingYears`, `WorkLifeBalance`, `YearsAtCompany`, `YearsInCurrentRole`, `YearsWithCurrManager`

    # parameters
    # train_dataframe = cs2_conv
    # adjust index numbers based on missing Attrition column #3 or add column to competition set
    ModelIdx = c(4, 7, 12, 15, 16, 17, 19, 20, 24, 29, 30, 32, 33, 34, 36)
    alpha = 0.7
    threshold = 0.46 # "Yes" >=

    # import data: CaseStudy2CompSet No Attrition.csv from AWS S3 msdsds6306 bucket

    url = "https://msdsds6306.s3.us-east-2.amazonaws.com/CaseStudy2CompSet+No+Attrition.csv"
    # encoded_url = URLencode(url) # need to encode the spaces as %20 (or do it manually)
    competition = read.table(textConnection(getURL(url)), sep = ",", header = TRUE, stringsAsFactors = TRUE)

    # save a copy of the competition unlabeled data
    # write.csv(competition, file = "data/CaseStudy2CompSet No Attrition.csv", row.names = FALSE)

    # get a sense of the data
    str(competition)

    'data.frame':   300 obs. of  35 variables:
     $ ID                      : int  1171 1172 1173 1174 1175 1176 1177 1178 1179 1180 ...
     $ Age                     : int  35 33 26 55 29 51 52 39 31 31 ...
     $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 3 3 3 3 2 1 3 3 2 ...
     $ DailyRate               : int  750 147 1330 1311 1246 1456 585 1387 1062 534 ...
     $ Department              : Factor w/ 3 levels "Human Resources",..: 2 1 2 2 3 2 3 2 2 2 ...
     $ DistanceFromHome        : int  28 2 21 2 19 1 29 10 24 20 ...
     $ Education               : int  3 3 3 3 3 4 4 5 3 3 ...
     $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 1 4 2 2 4 2 4 4 2 ...
     $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ EmployeeNumber          : int  1596 1207 1107 505 1497 145 2019 1618 1252 587 ...
     $ EnvironmentSatisfaction : int  2 2 1 3 3 1 1 2 3 1 ...
     $ Gender                  : Factor w/ 2 levels "Female","Male": 2 2 2 1 2 1 2 2 1 2 ...
     $ HourlyRate              : int  46 99 37 97 77 30 40 76 96 66 ...
     $ JobInvolvement          : int  4 3 3 3 2 2 3 3 2 3 ...
     $ JobLevel                : int  2 1 1 4 2 3 1 2 2 3 ...
     $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 3 2 3 4 8 1 9 5 1 1 ...
     $ JobSatisfaction         : int  3 3 3 4 3 1 4 1 1 3 ...
     $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 2 2 1 3 1 3 1 2 3 2 ...
     $ MonthlyIncome           : int  3407 3600 2377 16659 8620 7484 3482 5377 6812 9824 ...
     $ MonthlyRate             : int  25348 8429 19373 23258 23757 25796 19788 3835 17198 22908 ...
     $ NumCompaniesWorked      : int  1 1 1 2 1 3 2 2 1 3 ...
     $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
     $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 1 2 1 1 1 1 1 1 ...
     $ PercentSalaryHike       : int  17 13 20 13 14 20 15 13 19 12 ...
     $ PerformanceRating       : int  3 3 4 3 3 4 3 3 3 3 ...
     $ RelationshipSatisfaction: int  4 4 3 3 3 3 2 4 2 1 ...
     $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
     $ StockOptionLevel        : int  2 1 1 0 2 0 2 3 0 0 ...
     $ TotalWorkingYears       : int  10 5 1 30 10 23 16 10 10 12 ...
     $ TrainingTimesLastYear   : int  3 2 0 2 3 1 3 3 2 2 ...
     $ WorkLifeBalance         : int  2 3 2 3 3 2 2 3 3 3 ...
     $ YearsAtCompany          : int  10 5 1 5 10 13 9 7 10 1 ...
     $ YearsInCurrentRole      : int  9 4 1 4 7 12 8 7 9 0 ...
     $ YearsSinceLastPromotion : int  6 1 0 1 0 12 0 7 1 0 ...
     $ YearsWithCurrManager    : int  8 4 0 2 4 8 0 7 8 0 ...

    summary(competition)

           ID            Age                  BusinessTravel   DailyRate                       Department  DistanceFromHome
     Min.   :1171   Min.   :19.00   Non-Travel       : 32    Min.   : 102.0   Human Resources       : 11   Min.   : 1.00   
     1st Qu.:1246   1st Qu.:31.00   Travel_Frequently: 57    1st Qu.: 448.0   Research & Development:209   1st Qu.: 2.00   
     Median :1320   Median :36.00   Travel_Rarely    :211    Median : 775.0   Sales                 : 80   Median : 7.00   
     Mean   :1320   Mean   :37.86                            Mean   : 784.8                                Mean   : 9.26   
     3rd Qu.:1395   3rd Qu.:44.00                            3rd Qu.:1117.0                                3rd Qu.:14.00   
     Max.   :1470   Max.   :60.00                            Max.   :1490.0                                Max.   :29.00   
                                                                                                                           
       Education              EducationField EmployeeCount EmployeeNumber   EnvironmentSatisfaction    Gender      HourlyRate    
     Min.   :1.000   Human Resources :  7    Min.   :1     Min.   :   2.0   Min.   :1.000           Female:105   Min.   : 30.00  
     1st Qu.:2.000   Life Sciences   :130    1st Qu.:1     1st Qu.: 508.8   1st Qu.:2.000           Male  :195   1st Qu.: 50.00  
     Median :3.000   Marketing       : 27    Median :1     Median : 994.5   Median :3.000                        Median : 66.00  
     Mean   :2.973   Medical         : 94    Mean   :1     Mean   :1020.9   Mean   :2.733                        Mean   : 66.07  
     3rd Qu.:4.000   Other           : 12    3rd Qu.:1     3rd Qu.:1542.5   3rd Qu.:4.000                        3rd Qu.: 83.00  
     Max.   :5.000   Technical Degree: 30    Max.   :1     Max.   :2065.0   Max.   :4.000                        Max.   :100.00  
                                                                                                                                 
     JobInvolvement     JobLevel                        JobRole   JobSatisfaction  MaritalStatus MonthlyIncome    MonthlyRate   
     Min.   :1.000   Min.   :1.0   Research Scientist       :61   Min.   :1.000   Divorced: 65   Min.   : 1232   Min.   : 2097  
     1st Qu.:2.000   1st Qu.:1.0   Sales Executive          :57   1st Qu.:2.000   Married :128   1st Qu.: 3034   1st Qu.: 8420  
     Median :3.000   Median :2.0   Laboratory Technician    :55   Median :3.000   Single  :107   Median : 5208   Median :15091  
     Mean   :2.743   Mean   :2.2   Manufacturing Director   :31   Mean   :2.767                  Mean   : 7103   Mean   :14499  
     3rd Qu.:3.000   3rd Qu.:3.0   Manager                  :30   3rd Qu.:4.000                  3rd Qu.: 9750   3rd Qu.:20330  
     Max.   :4.000   Max.   :5.0   Healthcare Representative:29   Max.   :4.000                  Max.   :19973   Max.   :26914  
                                   (Other)                  :37                                                                 
     NumCompaniesWorked Over18  OverTime  PercentSalaryHike PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel
     Min.   :0.000      Y:300   No :212   Min.   :11.00     Min.   :3.000     Min.   :1.000            Min.   :80    Min.   :0.0000  
     1st Qu.:1.000              Yes: 88   1st Qu.:12.00     1st Qu.:3.000     1st Qu.:2.000            1st Qu.:80    1st Qu.:0.0000  
     Median :2.000                        Median :14.00     Median :3.000     Median :3.000            Median :80    Median :1.0000  
     Mean   :2.547                        Mean   :15.17     Mean   :3.153     Mean   :2.803            Mean   :80    Mean   :0.7833  
     3rd Qu.:4.000                        3rd Qu.:18.00     3rd Qu.:3.000     3rd Qu.:4.000            3rd Qu.:80    3rd Qu.:1.0000  
     Max.   :9.000                        Max.   :25.00     Max.   :4.000     Max.   :4.000            Max.   :80    Max.   :3.0000  
                                                                                                                                     
     TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsAtCompany   YearsInCurrentRole YearsSinceLastPromotion
     Min.   : 0.00     Min.   :0.000         Min.   :1.000   Min.   : 0.000   Min.   : 0.00      Min.   : 0.00          
     1st Qu.: 6.00     1st Qu.:2.000         1st Qu.:2.000   1st Qu.: 3.000   1st Qu.: 2.00      1st Qu.: 0.00          
     Median :10.00     Median :2.000         Median :3.000   Median : 5.000   Median : 3.00      Median : 1.00          
     Mean   :12.44     Mean   :2.683         Mean   :2.747   Mean   : 7.527   Mean   : 4.33      Mean   : 2.29          
     3rd Qu.:18.00     3rd Qu.:3.000         3rd Qu.:3.000   3rd Qu.:10.000   3rd Qu.: 7.00      3rd Qu.: 3.00          
     Max.   :38.00     Max.   :6.000         Max.   :4.000   Max.   :37.000   Max.   :18.00      Max.   :15.00          
                                                                                                                        
     YearsWithCurrManager
     Min.   : 0.00       
     1st Qu.: 2.00       
     Median : 3.00       
     Mean   : 4.38       
     3rd Qu.: 7.00       
     Max.   :17.00       
                         

    # convert numerical ranked variables to factors
    numVars_to_fact = c("Education", "EnvironmentSatisfaction",
                        "JobInvolvement", "JobLevel", "JobSatisfaction",
                        "PerformanceRating", "RelationshipSatisfaction",
                        "StockOptionLevel", "WorkLifeBalance")
    competition = competition %>%
      mutate(across(all_of(numVars_to_fact), as.factor))
    # check the conversion
    # str(competition)

    # add the Attrition column between Age and BusinessTravel
    competition = competition %>%
      mutate(Attrition = NA, .after = "Age")
    # str(competition)

    # train the Naive Bayes model
    fullsetNBmodel = naiveBayes(cs2_conv[, modelIdx], cs2_conv$Attrition, laplace = alpha)

    # predict the labels on the competition set
    preds = predict(fullsetNBmodel, competition[, modelIdx], type = "raw") # raw returns the probabilities

    # get the Yes probabilities to adjust threshold
    attrition_probs = preds[, "Yes"]

    # classify with new threshold
    predictions = ifelse(attrition_probs >= threshold, "Yes", "No")

    # add the labels to the Attrition column
    competition = competition %>% mutate(Attrition = factor(predictions))
    # str(competition)

    # get the proportion of attrition for curiousity
    summary(competition$Attrition)

     No Yes 
    242  58 

    NoAttProb = summary(competition$Attrition)["No"] / sum(summary(competition$Attrition))
    NoAttProb

           No 
    0.8066667 

    AttProb = summary(competition$Attrition)["Yes"] / sum(summary(competition$Attrition))
    AttProb

          Yes 
    0.1933333 

    # make dataframe with just competition set ordered IDs and labels
    competitionLabels = competition %>% select(ID, Attrition)
    str(competitionLabels)

    'data.frame':   300 obs. of  2 variables:
     $ ID       : int  1171 1172 1173 1174 1175 1176 1177 1178 1179 1180 ...
     $ Attrition: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...

    # save to file
    # write.csv(competitionLabels, file = "data/Case2PredictionsHenderson Attrition.csv", row.names = FALSE)

### Objective 6: Create a linear regression model to predict monthly salaries (first version).

(with &lt; $3000 RMSE for training and validation set)

**To create a new dataframe `cs2_regres`, I transform the original data
set by reordering and converting factor variables to integers. Based on
the EDA scatter plots, I also derive some quadratic variables.**

    # starting with the original dataset `cs2` since the ordinal categorical variables are in an integer class
    # there are a few other variables that I could transform into ranked variables, like business travel
    # not sure it is valid to do it for marital status or overtime

    # make a copy of the original dataset
    numerical_df = cs2

    # check the data
    head(numerical_df$BusinessTravel)

    [1] Travel_Rarely     Travel_Rarely     Travel_Frequently Travel_Rarely     Travel_Frequently Travel_Frequently
    Levels: Non-Travel Travel_Frequently Travel_Rarely

    # reorder factor levels
    numerical_df$BusinessTravel = factor(numerical_df$BusinessTravel, levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"), labels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))

    # rename the levels to numbers
    levels(numerical_df$BusinessTravel) = c(1:3)

    # convert factor to integer
    numerical_df$BusinessTravel = as.integer(numerical_df$BusinessTravel)

    # check the conversion
    head(numerical_df$BusinessTravel)

    [1] 2 2 3 2 3 3

    # repeat for marital status and overtime and attrition :/
    head(numerical_df$MaritalStatus)

    [1] Divorced Single   Single   Married  Single   Divorced
    Levels: Divorced Married Single

    numerical_df$MaritalStatus = factor(numerical_df$MaritalStatus, levels = c("Single", "Married", "Divorced"), labels = c("Single", "Married", "Divorced"))
    levels(numerical_df$MaritalStatus) = c(1:3)
    numerical_df$MaritalStatus = as.integer(numerical_df$MaritalStatus)
    head(numerical_df$MaritalStatus)

    [1] 3 1 1 2 1 3

    head(numerical_df$OverTime)

    [1] No  No  No  No  Yes No 
    Levels: No Yes

    numerical_df$OverTime = factor(numerical_df$OverTime, levels = c("Yes", "No"), labels = c("Yes", "No"))
    levels(numerical_df$OverTime) = c(1:2)
    numerical_df$OverTime = as.integer(numerical_df$OverTime)
    head(numerical_df$OverTime)

    [1] 2 2 2 2 1 2

    head(numerical_df$Attrition)

    [1] No No No No No No
    Levels: No Yes

    numerical_df$Attrition = factor(numerical_df$Attrition, levels = c("Yes", "No"), labels = c("Yes", "No"))
    levels(numerical_df$Attrition) = c(1:2)
    numerical_df$Attrition = as.integer(numerical_df$Attrition)
    head(numerical_df$Attrition)

    [1] 2 2 2 2 2 2

    # make a new DF, transform x variable (squaring term) and add it as a new column
    cs2_regres = numerical_df %>%
      mutate(
        YearsInCurrentRole2 = YearsInCurrentRole^2,
        YearsWithCurrManager2 = YearsWithCurrManager^2)

    #indexes from cs2_regres dataframe
    ModelIdx = c(2,3,4,5,7,8,12,14,15,16,18,19,21,22,24,25,26,27,29,30,31,32,33,34,35,36,37,38)

**To find the variables that produce a model with the lowest RMSE, I
start with an empty model formula. In a loop, I add each of the
variables in the `cs2_regres` dataframe independently to build a
regression model. I evaluate each model with leave-one-out cross
validation. I store each result in a dataframe, sorted by lowest RMSE.
Using these values as a guide, I manually choose the variable which
performs best. I add it as an explanatory variable to the model and
repeat the process with the remaining variables until I can no longer
achieve a lower RMSE with additional variables.**

    # unused indexes from cs2_regres dataframe 
    ModelIdx = c(2,3,4,5,8,14,18,19,21,22,24,25,26,27,29,31,32,33,34,35,37)
      
    # initialize an empty dataframe to store results
    results = data.frame(Index = numeric(length(ModelIdx)), Variable = character(length(ModelIdx)), RMSE = numeric(length(ModelIdx)))

    # get number of rows in data frame
    n = nrow(cs2_regres)

    # loop through each explanatory variable separately
    for (i in seq_along(ModelIdx)) {
      
      # create formula with the current explanatory variable 
      model_formula = as.formula(paste("MonthlyIncome ~ JobLevel + TotalWorkingYears + YearsWithCurrManager + YearsWithCurrManager2 + DistanceFromHome + JobInvolvement + EnvironmentSatisfaction + ", names(cs2_regres)[ModelIdx[i]]))
      # indexes of variables used in model c(16,30,36,7,15,38,12)

      # initialize vector to store squared error from each leave-one-out loop
      cv_error_model = numeric(n)
      
      # loop through setting each row as test set for internal n-fold CV
      for (j in 1:n) {
        # leave out the j-th observation
        test_row = cs2_regres[j, ]
        
        # use the remaining data for training       
        train_data = cs2_regres[-j, ]
        
        # fit the model with train_data
        model = lm(model_formula, data = train_data)
        
        # predict on the left-out observation
        pred = predict(model, newdata = test_row)
        
        # calculate the squared error
        cv_error_model[j] = (test_row$MonthlyIncome - pred)^2
      }
      
      # calculate the root mean squared error (RMSE) for the current model
      RMSE = sqrt(mean(cv_error_model))
      
      # store the variable index, name, and RMSE in the results dataframe
      results[i, "Index"] = ModelIdx[i]
      results[i, "Variable"] = names(cs2_regres)[ModelIdx[i]]
      results[i, "RMSE"] = RMSE
    }

    # sort results by RMSE in ascending order
    results = results[order(results$RMSE), ]

    # print the sorted table
    kable(results, caption = "demo of resulting RSME for each 1 added variable") %>% kable_styling(full_width = FALSE, position = "left")

<table class="table" style="width: auto !important; ">
<caption>
demo of resulting RSME for each 1 added variable
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Index
</th>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
RMSE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
12
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
PercentSalaryHike
</td>
<td style="text-align:right;">
1376.463
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:left;">
MonthlyRate
</td>
<td style="text-align:right;">
1376.495
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
1376.499
</td>
</tr>
<tr>
<td style="text-align:left;">
16
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
TrainingTimesLastYear
</td>
<td style="text-align:right;">
1376.594
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
PerformanceRating
</td>
<td style="text-align:right;">
1376.663
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
HourlyRate
</td>
<td style="text-align:right;">
1376.674
</td>
</tr>
<tr>
<td style="text-align:left;">
17
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:left;">
WorkLifeBalance
</td>
<td style="text-align:right;">
1376.718
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
BusinessTravel
</td>
<td style="text-align:right;">
1376.755
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Attrition
</td>
<td style="text-align:right;">
1376.770
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
NumCompaniesWorked
</td>
<td style="text-align:right;">
1376.825
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
MaritalStatus
</td>
<td style="text-align:right;">
1376.835
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
StockOptionLevel
</td>
<td style="text-align:right;">
1376.843
</td>
</tr>
<tr>
<td style="text-align:left;">
11
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
OverTime
</td>
<td style="text-align:right;">
1376.953
</td>
</tr>
<tr>
<td style="text-align:left;">
14
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
RelationshipSatisfaction
</td>
<td style="text-align:right;">
1376.964
</td>
</tr>
<tr>
<td style="text-align:left;">
7
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
JobSatisfaction
</td>
<td style="text-align:right;">
1376.974
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Education
</td>
<td style="text-align:right;">
1377.005
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
DailyRate
</td>
<td style="text-align:right;">
1377.028
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
YearsSinceLastPromotion
</td>
<td style="text-align:right;">
1377.295
</td>
</tr>
<tr>
<td style="text-align:left;">
19
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
YearsInCurrentRole
</td>
<td style="text-align:right;">
1377.296
</td>
</tr>
<tr>
<td style="text-align:left;">
21
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
YearsInCurrentRole2
</td>
<td style="text-align:right;">
1377.509
</td>
</tr>
<tr>
<td style="text-align:left;">
18
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
YearsAtCompany
</td>
<td style="text-align:right;">
1377.707
</td>
</tr>
</tbody>
</table>

Measured with internal n-fold cross validation, I achieve the lowest
RMSE of 1375.36 with the following explanatory variables `JobLevel`,
`TotalWorkingYears`, `YearsWithCurrManager`, `YearsWithCurrManager2`,
`DistanceFromHome`, `JobInvolvement`, `EnvironmentSatisfaction`.

**To measure separate RMSEs for a training and a validation set, I use
external cross validation for a linear regression model of
`MonthlyIncome` using the variables above.**

    # model formula from variable indexes (16,30,36,7,15,38,12)
    model_formula = as.formula("MonthlyIncome ~ JobLevel + TotalWorkingYears + YearsWithCurrManager + YearsWithCurrManager2 + DistanceFromHome + JobInvolvement + EnvironmentSatisfaction")

    # number of train/test splits
    iters = 100

    # split percentage
    split_perc = 0.7

    # initialize vector to store squared error from each train/test split loop for training and validation sets
    cv_error_train = numeric(iters)
    cv_error_test = numeric(iters)
      
    # external cross-validation loop
    for (i in 1:iters) {
      
      set.seed(i)

      # generate the test train split
      trainIdx = sample(1:nrow(cs2_regres), round(splitPerc * nrow(cs2_regres)))
      IncomeTrn = cs2_regres[trainIdx, ]
      IncomeTst = cs2_regres[-trainIdx, ]
        
      # fit the model with train_data
      model = lm(model_formula, data = IncomeTrn)
        
      # predict on the training set (same data used to make model)
      predTrn = predict(model, newdata = IncomeTrn)
      
      # calculate the mean squared error of training pred
      cv_error_train[i] = mean((IncomeTrn$MonthlyIncome - predTrn)^2)
      
      # predict on the training set (same data used to make model)
      predTst = predict(model, newdata = IncomeTst)
        
      # calculate the mean squared error of training pred
      cv_error_test[i] = mean((IncomeTst$MonthlyIncome - predTst)^2)
    }
      
    # calculate the root mean squared error (RMSE) for the training set
    RMSE_trn = sqrt(mean(cv_error_train)) # root of avg of error from each iteration 
      
    # calculate the root mean squared error (RMSE) for the validation set
    RMSE_tst = sqrt(mean(cv_error_test))
      
    # print the results
    cat("\nRMSE of training set: ", RMSE_trn, "\n")


    RMSE of training set:  1357.371 

    cat("\nRMSE of validation set: ", RMSE_tst, "\n")


    RMSE of validation set:  1381.737 

For my linear regression model, the RMSE of my training set and
validation sets are 1357.37 and 1381.74, respectively. To achieve this,
I used these variables, `JobLevel`, `TotalWorkingYears`,
`YearsWithCurrManager`, `YearsWithCurrManager2`, `DistanceFromHome`,
`JobInvolvement`, `EnvironmentSatisfaction`. This list includes
`YearsWithCurrManager2`, which is a variable I derived from squaring
`YearsWithCurrManager`.

**I now explore step-wise variable selection.**

    # Stepwise Selection
    # data = cs2_regres

    library(boot)

    # Check for variables with only one level
    vars_one_level <- sapply(cs2_regres, function(x) length(unique(x)) == 1)

    # List variables with only one level
    names(cs2_regres)[vars_one_level]

    # Remove variables with only one level
    cs2_regres_clean <- cs2_regres[, !vars_one_level]

    library(boot)

    # Define a function to perform stepwise selection and return the selected model
    perform_stepwise_selection <- function(data) {
      stepwise_model <- step(lm(MonthlyIncome ~ ., data = data))
      return(stepwise_model)
    }

    # Define a function to calculate RMSE
    calculate_rmse <- function(actual, predicted) {
      return(sqrt(mean((actual - predicted)^2)))
    }

    # Define the number of iterations for cross-validation
    num_iterations <- 100

    # Initialize a data frame to store RMSEs and variable combinations
    results <- data.frame(RMSE = numeric(num_iterations),
                          Variables = character(num_iterations))

    # Perform cross-validation with multiple iterations
    for (i in 1:num_iterations) {
      # Split data into training and validation sets (adjust as per your data)
      set.seed(i)  # Set seed for reproducibility
      train_indices <- sample(nrow(cs2_regres_clean), 0.7 * nrow(cs2_regres_clean))
      train_data <- cs2_regres_clean[train_indices, ]
      validation_data <- cs2_regres_clean[-train_indices, ]
      
      # Perform stepwise selection on the training data
      selected_model <- perform_stepwise_selection(train_data)
      
      # Make predictions on the validation set
      predicted_values <- predict(selected_model, newdata = validation_data)
      
      # Calculate RMSE for this iteration
      rmse <- calculate_rmse(validation_data$MonthlyIncome, predicted_values)
      
      # Store RMSE and variable combination
      results[i, "RMSE"] <- rmse
      results[i, "Variables"] <- paste(names(coefficients(selected_model)), collapse = ", ")
    }

    # Sort results by RMSE
    results <- results[order(results$RMSE), ]

    # Output top five RMSEs and their variable combinations
    top_five <- head(results, 5)
    print(top_five)

**I import the unlabeled competition data set from the cloud as
`competition`. I create two new variables by squaring existing
variables. So that my column indexes are the same as those in the
primary data set, I add an `MonthlyIncome` column. I train a Linear
Regression model using the optimized variables and all the observations
in the labeled primary data set.**

    # make predictions with linear regression model

    # variables used: `JobLevel`, `TotalWorkingYears`, `YearsWithCurrManager`, `YearsWithCurrManager2`, `DistanceFromHome`, `JobInvolvement`, `EnvironmentSatisfaction`

    # parameters
    # train_dataframe = cs2_regres
    # adjust index numbers based on missing MonthlyIncome column or add column to competition set
    # regresIdx = c(16,30,36,7,15,38,12)


    # import data: CaseStudy2CompSet No Salary.csv from AWS S3 msdsds6306 bucket

    # first column header is `ï..ID` instead of `ID` which I guess is a UTF-8 encoding issue 
    # url = "https://msdsds6306.s3.us-east-2.amazonaws.com/CaseStudy2CompSet+No+Salary.csv"
    # competition = read.table(textConnection(getURL(url)), sep = ",", header = TRUE, stringsAsFactors = TRUE)

    # so I use read.csv and specify encoding = "UTF-8" and it imports correctly.
    url = "https://msdsds6306.s3.us-east-2.amazonaws.com/CaseStudy2CompSet+No+Salary.csv"
    competition = read.csv(url, header = TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")

    # save a copy of the competition unlabeled data
    # write.csv(competition, file = "data/CaseStudy2CompSet No Salary.csv", row.names = FALSE)

    # get a sense of the data
    # str(competition)
    # summary(competition)

    # create squared variables, both to keep the indexes consistent
    competition = competition %>%
      mutate(
        YearsInCurrentRole2 = YearsInCurrentRole^2,
        YearsWithCurrManager2 = YearsWithCurrManager^2)
    # check the conversion
    # str(competition)

    # add the MonthlyIncome column between MaritalStatus and MonthlyRate 
    competition = competition %>%
      mutate(MonthlyIncome = NA, .after = "MaritalStatus")
    # str(competition)

    # train the linear regression model on the full `cs2_regres` data set
    fullset_LM_model = lm(model_formula, data = cs2_regres)

    # predict the labels on the competition set
    preds = predict(fullset_LM_model, competition)

    # add the labels to the MonthlyIncome column
    competition = competition %>% mutate(MonthlyIncome = as.integer(preds))
    # str(competition)

    # for curiousity get the summary of MonthlyIncome from both datasets
    summary(cs2_regres$MonthlyIncome)

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       1081    2840    4946    6390    8182   19999 

    summary(competition$MonthlyIncome)

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       1576    2340    6008    6242    7056   19117 

    # make dataframe with just competition set ordered IDs and labels
    competitionLabels = competition %>% select(ID, MonthlyIncome)
    # str(competitionLabels)

    # save to file
    # write.csv(competitionLabels, file = "data/Case2PredictionsHenderson Salary.csv", row.names = FALSE)

#### **Update the model using improved variable selection methods**

### Objective 6: Create a linear regression model to predict monthly salaries (revised model).

(with &lt; $3000 RMSE for training and validation set)

**To create a new dataframe `cs2_regres2`. I reorder some factor levels
here (though it probably doesn’t matter).**

    # I decide to treat integer and non-integer categorical variables as categorical starting with the original dataset `cs2` since the ordinal categorical variables are in an integer class.

    # make a copy of the original dataset after converting numerical ranking variables to factors
    cs2_regres2 = cs2_conv

    # check the data
    head(cs2_regres2$BusinessTravel)

    [1] Travel_Rarely     Travel_Rarely     Travel_Frequently Travel_Rarely     Travel_Frequently Travel_Frequently
    Levels: Non-Travel Travel_Frequently Travel_Rarely

    # reorder factor levels
    cs2_regres2$BusinessTravel = factor(cs2_regres2$BusinessTravel, levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"), labels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))

    # check the conversion
    head(cs2_regres2$BusinessTravel)

    [1] Travel_Rarely     Travel_Rarely     Travel_Frequently Travel_Rarely     Travel_Frequently Travel_Frequently
    Levels: Non-Travel Travel_Rarely Travel_Frequently

    # repeat for marital status
    head(cs2_regres2$MaritalStatus)

    [1] Divorced Single   Single   Married  Single   Divorced
    Levels: Divorced Married Single

    cs2_regres2$MaritalStatus = factor(cs2_regres2$MaritalStatus, levels = c("Single", "Married", "Divorced"), labels = c("Single", "Married", "Divorced"))
    head(cs2_regres2$MaritalStatus)

    [1] Divorced Single   Single   Married  Single   Divorced
    Levels: Single Married Divorced

    # remove Over18 - factor with 1 level
    cs2_regres2 = cs2_regres2 %>% select(-Over18)
    str(cs2_regres2)

    'data.frame':   870 obs. of  35 variables:
     $ ID                      : int  1 2 3 4 5 6 7 8 9 10 ...
     $ Age                     : int  32 40 35 32 24 27 41 37 34 34 ...
     $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
     $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Rarely",..: 2 2 3 2 3 3 2 2 2 3 ...
     $ DailyRate               : int  117 1308 200 801 567 294 1283 309 1333 653 ...
     $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 2 3 2 2 2 3 3 2 ...
     $ DistanceFromHome        : int  13 14 18 1 2 10 5 10 10 10 ...
     $ Education               : Factor w/ 5 levels "1","2","3","4",..: 4 3 2 4 1 2 5 4 4 4 ...
     $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 4 2 3 6 2 4 2 2 6 ...
     $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ EmployeeNumber          : int  859 1128 1412 2016 1646 733 1448 1105 1055 1597 ...
     $ EnvironmentSatisfaction : Factor w/ 4 levels "1","2","3","4": 2 3 3 3 1 4 2 4 3 4 ...
     $ Gender                  : Factor w/ 2 levels "Female","Male": 2 2 2 1 1 2 2 1 1 2 ...
     $ HourlyRate              : int  73 44 60 48 32 32 90 88 87 92 ...
     $ JobInvolvement          : Factor w/ 4 levels "1","2","3","4": 3 2 3 3 3 3 4 2 3 2 ...
     $ JobLevel                : Factor w/ 5 levels "1","2","3","4",..: 2 5 3 3 1 3 1 2 1 2 ...
     $ JobRole                 : Factor w/ 9 levels "Sales Representative",..: 5 8 7 5 4 7 4 5 1 6 ...
     $ JobSatisfaction         : Factor w/ 4 levels "1","2","3","4": 4 3 4 4 4 1 3 4 3 3 ...
     $ MaritalStatus           : Factor w/ 3 levels "Single","Married",..: 3 1 1 2 1 3 2 3 2 2 ...
     $ MonthlyIncome           : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
     $ MonthlyRate             : int  9250 17544 19944 24032 17218 4809 5561 24223 18410 15332 ...
     $ NumCompaniesWorked      : int  2 1 2 1 1 1 2 2 1 1 ...
     $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 2 2 2 1 ...
     $ PercentSalaryHike       : int  11 14 11 19 13 21 12 14 19 14 ...
     $ PerformanceRating       : Factor w/ 2 levels "3","4": 1 1 1 1 1 2 1 1 1 1 ...
     $ RelationshipSatisfaction: Factor w/ 4 levels "1","2","3","4": 3 1 3 3 3 3 1 3 4 2 ...
     $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
     $ StockOptionLevel        : Factor w/ 4 levels "0","1","2","3": 2 1 1 3 1 3 1 4 2 2 ...
     $ TotalWorkingYears       : int  8 21 10 14 6 9 7 8 1 8 ...
     $ TrainingTimesLastYear   : int  3 2 2 3 2 4 5 5 2 3 ...
     $ WorkLifeBalance         : Factor w/ 4 levels "1","2","3","4": 2 4 3 3 3 2 2 3 3 2 ...
     $ YearsAtCompany          : int  5 20 2 14 6 9 4 1 1 8 ...
     $ YearsInCurrentRole      : int  2 7 2 10 3 7 2 0 1 2 ...
     $ YearsSinceLastPromotion : int  0 4 2 5 1 1 0 0 0 7 ...
     $ YearsWithCurrManager    : int  3 9 2 7 3 7 3 0 0 7 ...

**To find the variables that produce a model with the best prediction
and lowest RMSE, I create and compare models using forward, backward and
stepwise variable selection. I evaluate each candidate model with cross
validation and the following metrics, adjusted R-squared, AIC, BIC,
RMSE\_Train (80/20), CVPRESS, and RMSE\_Test.**

    # library(car)
    # library(kableExtra)
    #library(olsrr)

    # Fit the model will all variables
    fit = lm(MonthlyIncome ~ . - EmployeeCount - StandardHours, data = cs2_regres2) # removed aliased variables

    # Calculate VIF
    vif_values = vif(fit)
    print(vif_values)

                                    GVIF Df GVIF^(1/(2*Df))
    ID                          1.096327  1        1.047057
    Age                         2.042160  1        1.429042
    Attrition                   1.533611  1        1.238391
    BusinessTravel              1.162671  2        1.038399
    DailyRate                   1.069008  1        1.033928
    Department                120.636700  2        3.314132
    DistanceFromHome            1.109161  1        1.053167
    Education                   1.467884  4        1.049147
    EducationField              3.375656  5        1.129369
    EmployeeNumber              1.086228  1        1.042223
    EnvironmentSatisfaction     1.236561  3        1.036023
    Gender                      1.063360  1        1.031194
    HourlyRate                  1.068540  1        1.033702
    JobInvolvement              1.279705  3        1.041961
    JobLevel                   23.519418  4        1.483981
    JobRole                  1360.714690  8        1.569858
    JobSatisfaction             1.255779  3        1.038689
    MaritalStatus               3.173388  2        1.334691
    MonthlyRate                 1.083265  1        1.040800
    NumCompaniesWorked          1.439581  1        1.199825
    OverTime                    1.169916  1        1.081626
    PercentSalaryHike           2.698229  1        1.642629
    PerformanceRating           2.672490  1        1.634775
    RelationshipSatisfaction    1.234630  3        1.035753
    StockOptionLevel            3.469022  3        1.230367
    TotalWorkingYears           5.782735  1        2.404732
    TrainingTimesLastYear       1.083486  1        1.040906
    WorkLifeBalance             1.233537  3        1.035600
    YearsAtCompany              5.449373  1        2.334389
    YearsInCurrentRole          3.136986  1        1.771154
    YearsSinceLastPromotion     1.945934  1        1.394968
    YearsWithCurrManager        2.905212  1        1.704468

    # Variable selection with significance level
    fore1 = ols_step_forward_p(fit, p_val = 0.2, details = FALSE)
    back1 = ols_step_backward_p(fit, p_val = 0.2, details = FALSE) # bigger p-remove, more variables in the model; smaller, fewer
    step1 = ols_step_both_p(fit, p_enter = 0.2, p_remove = 0.2, details = FALSE) # stepwise selection
    fore1


                                     Stepwise Summary                                  
    ---------------------------------------------------------------------------------
    Step    Variable                AIC          SBC       SBIC      R2       Adj. R2 
    ---------------------------------------------------------------------------------
     0      Base Model           17145.913    17155.450      NA    0.00000    0.00000 
     1      JobLevel             14903.153    14931.764      NA    0.92476    0.92441 
     2      JobRole              14549.801    14616.560      NA    0.95079    0.95010 
     3      TotalWorkingYears    14516.912    14588.439      NA    0.95272    0.95200 
     4      BusinessTravel       14510.833    14591.897      NA    0.95327    0.95245 
     5      DailyRate            14508.366    14594.199      NA    0.95351    0.95263 
     6      ID                   14507.882    14598.483      NA    0.95364    0.95271 
     7      Education            14508.119    14617.794      NA    0.95405    0.95291 
     8      Department           14508.347    14627.559      NA    0.95425    0.95301 
    ---------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  982.850 
    R-Squared                 0.954       MSE                993397.211 
    Adj. R-Squared            0.953       Coef. Var              15.597 
    Pred R-Squared            0.952       AIC                 14508.347 
    MAE                     750.488       SBC                 14627.559 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17529210346.763         23    762139580.294    767.205    0.0000 
    Residual        840414040.433        846       993397.211                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     2406.444       480.268                  5.011    0.000     1463.787     3349.101 
                           JobLevel2     1718.401       139.357        0.179    12.331    0.000     1444.875     1991.926 
                           JobLevel3     4969.685       186.184        0.388    26.692    0.000     4604.248     5335.122 
                           JobLevel4     8345.512       282.314        0.460    29.561    0.000     7791.394     8899.629 
                           JobLevel5    10956.672       332.389        0.481    32.963    0.000    10304.268    11609.075 
        JobRoleLaboratory Technician     -553.843       347.398       -0.046    -1.594    0.111    -1235.707      128.020 
              JobRoleHuman Resources     -160.035       488.393       -0.006    -0.328    0.743    -1118.639      798.568 
           JobRoleResearch Scientist     -355.408       346.876       -0.031    -1.025    0.306    -1036.246      325.430 
              JobRoleSales Executive     1223.232       199.731        0.112     6.124    0.000      831.205     1615.258 
    JobRoleHealthcare Representative      654.111       377.637        0.040     1.732    0.084      -87.105     1395.326 
       JobRoleManufacturing Director      766.332       375.657        0.050     2.040    0.042       29.003     1503.662 
            JobRoleResearch Director     4193.228       407.197        0.214    10.298    0.000     3393.993     4992.462 
                      JobRoleManager     4284.007       336.139        0.219    12.745    0.000     3624.242     4943.772 
                   TotalWorkingYears       44.199         7.766        0.072     5.692    0.000       28.956       59.441 
         BusinessTravelTravel_Rarely      344.502       111.437        0.034     3.091    0.002      125.776      563.229 
     BusinessTravelTravel_Frequently      205.236       131.572        0.017     1.560    0.119      -53.010      463.482 
                           DailyRate        0.179         0.085        0.016     2.100    0.036        0.012        0.345 
                                  ID       -0.228         0.136       -0.012    -1.668    0.096       -0.495        0.040 
                          Education2      -37.724       126.274       -0.003    -0.299    0.765     -285.570      210.123 
                          Education3      -89.765       116.405       -0.009    -0.771    0.441     -318.242      138.713 
                          Education4       58.401       122.739        0.006     0.476    0.634     -182.507      299.309 
                          Education5     -457.613       226.221       -0.017    -2.023    0.043     -901.634      -13.593 
    DepartmentResearch & Development      248.790       417.223        0.026     0.596    0.551     -570.123     1067.703 
                     DepartmentSales     -337.864       426.663       -0.034    -0.792    0.429    -1175.306      499.578 
    ----------------------------------------------------------------------------------------------------------------------

    back1


                                         Stepwise Summary                                     
    ----------------------------------------------------------------------------------------
    Step    Variable                       AIC          SBC       SBIC      R2       Adj. R2 
    ----------------------------------------------------------------------------------------
     0      Full Model                  14567.970    14882.690      NA    0.95541    0.95187 
     1      HourlyRate                  14565.970    14875.922      NA    0.95541    0.95193 
     2      Attrition                   14564.013    14869.196      NA    0.95541    0.95198 
     3      OverTime                    14562.084    14862.499      NA    0.95541    0.95204 
     4      Age                         14560.178    14855.824      NA    0.95540    0.95209 
     5      EnvironmentSatisfaction     14554.492    14835.833      NA    0.95538    0.95225 
     6      EducationField              14545.361    14802.860      NA    0.95534    0.95250 
     7      YearsAtCompany              14543.740    14796.470      NA    0.95532    0.95254 
     8      MaritalStatus               14540.603    14783.796      NA    0.95528    0.95260 
     9      StockOptionLevel            14536.170    14765.057      NA    0.95520    0.95269 
     10     JobInvolvement              14531.512    14746.094      NA    0.95513    0.95279 
     11     TrainingTimesLastYear       14530.114    14739.928      NA    0.95510    0.95281 
     12     JobSatisfaction             14525.876    14721.384      NA    0.95500    0.95289 
     13     NumCompaniesWorked          14524.487    14715.227      NA    0.95497    0.95291 
     14     RelationshipSatisfaction    14520.524    14696.958      NA    0.95487    0.95297 
     15     YearsInCurrentRole          14519.184    14690.850      NA    0.95483    0.95299 
     16     WorkLifeBalance             14515.161    14672.521      NA    0.95473    0.95306 
     17     YearsWithCurrManager        14513.771    14666.363      NA    0.95470    0.95308 
     18     MonthlyRate                 14512.431    14660.255      NA    0.95466    0.95310 
     19     YearsSinceLastPromotion     14511.385    14654.440      NA    0.95461    0.95310 
     20     DistanceFromHome            14510.453    14648.739      NA    0.95456    0.95310 
     21     EmployeeNumber              14509.712    14643.230      NA    0.95449    0.95309 
     22     Gender                      14509.157    14637.906      NA    0.95442    0.95307 
     23     PercentSalaryHike           14508.841    14632.822      NA    0.95433    0.95303 
     24     PerformanceRating           14508.347    14627.559      NA    0.95425    0.95301 
    ----------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  982.850 
    R-Squared                 0.954       MSE                993397.211 
    Adj. R-Squared            0.953       Coef. Var              15.597 
    Pred R-Squared            0.952       AIC                 14508.347 
    MAE                     750.488       SBC                 14627.559 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17529210346.763         23    762139580.294    767.205    0.0000 
    Residual        840414040.433        846       993397.211                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     2406.444       480.268                  5.011    0.000     1463.787     3349.101 
                                  ID       -0.228         0.136       -0.012    -1.668    0.096       -0.495        0.040 
         BusinessTravelTravel_Rarely      344.502       111.437        0.034     3.091    0.002      125.776      563.229 
     BusinessTravelTravel_Frequently      205.236       131.572        0.017     1.560    0.119      -53.010      463.482 
                           DailyRate        0.179         0.085        0.016     2.100    0.036        0.012        0.345 
    DepartmentResearch & Development      248.790       417.223        0.026     0.596    0.551     -570.123     1067.703 
                     DepartmentSales     -337.864       426.663       -0.034    -0.792    0.429    -1175.306      499.578 
                          Education2      -37.724       126.274       -0.003    -0.299    0.765     -285.570      210.123 
                          Education3      -89.765       116.405       -0.009    -0.771    0.441     -318.242      138.713 
                          Education4       58.401       122.739        0.006     0.476    0.634     -182.507      299.309 
                          Education5     -457.613       226.221       -0.017    -2.023    0.043     -901.634      -13.593 
                           JobLevel2     1718.401       139.357        0.179    12.331    0.000     1444.875     1991.926 
                           JobLevel3     4969.685       186.184        0.388    26.692    0.000     4604.248     5335.122 
                           JobLevel4     8345.512       282.314        0.460    29.561    0.000     7791.394     8899.629 
                           JobLevel5    10956.672       332.389        0.481    32.963    0.000    10304.268    11609.075 
        JobRoleLaboratory Technician     -553.843       347.398       -0.046    -1.594    0.111    -1235.707      128.020 
              JobRoleHuman Resources     -160.035       488.393       -0.006    -0.328    0.743    -1118.639      798.568 
           JobRoleResearch Scientist     -355.408       346.876       -0.031    -1.025    0.306    -1036.246      325.430 
              JobRoleSales Executive     1223.232       199.731        0.112     6.124    0.000      831.205     1615.258 
    JobRoleHealthcare Representative      654.111       377.637        0.040     1.732    0.084      -87.105     1395.326 
       JobRoleManufacturing Director      766.332       375.657        0.050     2.040    0.042       29.003     1503.662 
            JobRoleResearch Director     4193.228       407.197        0.214    10.298    0.000     3393.993     4992.462 
                      JobRoleManager     4284.007       336.139        0.219    12.745    0.000     3624.242     4943.772 
                   TotalWorkingYears       44.199         7.766        0.072     5.692    0.000       28.956       59.441 
    ----------------------------------------------------------------------------------------------------------------------

    step1


                                       Stepwise Summary                                    
    -------------------------------------------------------------------------------------
    Step    Variable                    AIC          SBC       SBIC      R2       Adj. R2 
    -------------------------------------------------------------------------------------
     0      Base Model               17145.913    17155.450      NA    0.00000    0.00000 
     1      JobLevel (+)             14903.153    14931.764      NA    0.92476    0.92441 
     2      JobRole (+)              14549.801    14616.560      NA    0.95079    0.95010 
     3      TotalWorkingYears (+)    14516.912    14588.439      NA    0.95272    0.95200 
     4      BusinessTravel (+)       14510.833    14591.897      NA    0.95327    0.95245 
     5      DailyRate (+)            14508.366    14594.199      NA    0.95351    0.95263 
     6      ID (+)                   14507.882    14598.483      NA    0.95364    0.95271 
     7      Education (+)            14508.119    14617.794      NA    0.95405    0.95291 
     8      Department (+)           14508.347    14627.559      NA    0.95425    0.95301 
    -------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  982.850 
    R-Squared                 0.954       MSE                993397.211 
    Adj. R-Squared            0.953       Coef. Var              15.597 
    Pred R-Squared            0.952       AIC                 14508.347 
    MAE                     750.488       SBC                 14627.559 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17529210346.763         23    762139580.294    767.205    0.0000 
    Residual        840414040.433        846       993397.211                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     2406.444       480.268                  5.011    0.000     1463.787     3349.101 
                           JobLevel2     1718.401       139.357        0.179    12.331    0.000     1444.875     1991.926 
                           JobLevel3     4969.685       186.184        0.388    26.692    0.000     4604.248     5335.122 
                           JobLevel4     8345.512       282.314        0.460    29.561    0.000     7791.394     8899.629 
                           JobLevel5    10956.672       332.389        0.481    32.963    0.000    10304.268    11609.075 
        JobRoleLaboratory Technician     -553.843       347.398       -0.046    -1.594    0.111    -1235.707      128.020 
              JobRoleHuman Resources     -160.035       488.393       -0.006    -0.328    0.743    -1118.639      798.568 
           JobRoleResearch Scientist     -355.408       346.876       -0.031    -1.025    0.306    -1036.246      325.430 
              JobRoleSales Executive     1223.232       199.731        0.112     6.124    0.000      831.205     1615.258 
    JobRoleHealthcare Representative      654.111       377.637        0.040     1.732    0.084      -87.105     1395.326 
       JobRoleManufacturing Director      766.332       375.657        0.050     2.040    0.042       29.003     1503.662 
            JobRoleResearch Director     4193.228       407.197        0.214    10.298    0.000     3393.993     4992.462 
                      JobRoleManager     4284.007       336.139        0.219    12.745    0.000     3624.242     4943.772 
                   TotalWorkingYears       44.199         7.766        0.072     5.692    0.000       28.956       59.441 
         BusinessTravelTravel_Rarely      344.502       111.437        0.034     3.091    0.002      125.776      563.229 
     BusinessTravelTravel_Frequently      205.236       131.572        0.017     1.560    0.119      -53.010      463.482 
                           DailyRate        0.179         0.085        0.016     2.100    0.036        0.012        0.345 
                                  ID       -0.228         0.136       -0.012    -1.668    0.096       -0.495        0.040 
                          Education2      -37.724       126.274       -0.003    -0.299    0.765     -285.570      210.123 
                          Education3      -89.765       116.405       -0.009    -0.771    0.441     -318.242      138.713 
                          Education4       58.401       122.739        0.006     0.476    0.634     -182.507      299.309 
                          Education5     -457.613       226.221       -0.017    -2.023    0.043     -901.634      -13.593 
    DepartmentResearch & Development      248.790       417.223        0.026     0.596    0.551     -570.123     1067.703 
                     DepartmentSales     -337.864       426.663       -0.034    -0.792    0.429    -1175.306      499.578 
    ----------------------------------------------------------------------------------------------------------------------

    # forward: JobLevel, JobRole, TotalWorkingYears, BusinessTravel, DailyRate, ID, Education, Department
    # backward: ID, BusinessTravel, DailyRate, Department, Education, JobLevel, JobRole, TotalWorkingYears
    # stepwise: JobLevel, JobRole, TotalWorkingYears, BusinessTravel, DailyRate, ID, Education, Department

    # Variable selection with AIC
    fore2 = ols_step_forward_aic(fit, details = FALSE)
    back2 = ols_step_backward_aic(fit, details = FALSE)
    step2 = ols_step_both_aic(fit, details = FALSE)
    fore2


                                     Stepwise Summary                                  
    ---------------------------------------------------------------------------------
    Step    Variable                AIC          SBC       SBIC      R2       Adj. R2 
    ---------------------------------------------------------------------------------
     0      Base Model           17145.913    17155.450      NA    0.00000    0.00000 
     1      JobLevel             14903.153    14931.764      NA    0.92476    0.92441 
     2      JobRole              14549.801    14616.560      NA    0.95079    0.95010 
     3      TotalWorkingYears    14516.912    14588.439      NA    0.95272    0.95200 
     4      BusinessTravel       14510.833    14591.897      NA    0.95327    0.95245 
     5      DailyRate            14508.366    14594.199      NA    0.95351    0.95263 
     6      ID                   14507.882    14598.483      NA    0.95364    0.95271 
    ---------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  989.387 
    R-Squared                 0.954       MSE                999567.033 
    Adj. R-Squared            0.953       Coef. Var              15.645 
    Pred R-Squared            0.952       AIC                 14507.882 
    MAE                     757.865       SBC                 14598.483 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                          ANOVA                                       
    ---------------------------------------------------------------------------------
                           Sum of                                                    
                          Squares         DF       Mean Square       F          Sig. 
    ---------------------------------------------------------------------------------
    Regression    17517993274.884         17    1030470192.640    1030.917    0.0000 
    Residual        851631112.311        852        999567.033                       
    Total         18369624387.195        869                                         
    ---------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     2047.802       198.897                 10.296    0.000     1657.417     2438.188 
                           JobLevel2     1708.803       138.458        0.178    12.342    0.000     1437.044     1980.561 
                           JobLevel3     4941.408       186.075        0.386    26.556    0.000     4576.189     5306.627 
                           JobLevel4     8261.027       280.819        0.456    29.418    0.000     7709.849     8812.206 
                           JobLevel5    10937.256       330.773        0.480    33.066    0.000    10288.031    11586.481 
        JobRoleLaboratory Technician       20.887       162.666        0.002     0.128    0.898     -298.386      340.159 
              JobRoleHuman Resources      153.145       238.202        0.006     0.643    0.520     -314.386      620.677 
           JobRoleResearch Scientist      212.049       159.830        0.018     1.327    0.185     -101.658      525.757 
              JobRoleSales Executive     1226.701       199.750        0.112     6.141    0.000      834.641     1618.762 
    JobRoleHealthcare Representative     1245.795       220.122        0.077     5.660    0.000      813.751     1677.840 
       JobRoleManufacturing Director     1353.309       216.300        0.088     6.257    0.000      928.766     1777.853 
            JobRoleResearch Director     4747.815       267.694        0.243    17.736    0.000     4222.398     5273.233 
                      JobRoleManager     4618.324       290.233        0.236    15.912    0.000     4048.668     5187.981 
                   TotalWorkingYears       45.282         7.716        0.074     5.869    0.000       30.138       60.427 
         BusinessTravelTravel_Rarely      331.967       111.601        0.033     2.975    0.003      112.923      551.011 
     BusinessTravelTravel_Frequently      187.285       131.735        0.016     1.422    0.155      -71.278      445.848 
                           DailyRate        0.174         0.085        0.015     2.044    0.041        0.007        0.341 
                                  ID       -0.213         0.136       -0.012    -1.561    0.119       -0.481        0.055 
    ----------------------------------------------------------------------------------------------------------------------

    back2


                                           Stepwise Summary                                        
    ---------------------------------------------------------------------------------------------
    Step    Variable                       AIC          SBC         SBIC         R2       Adj. R2 
    ---------------------------------------------------------------------------------------------
     0      Full Model                  14567.970    14882.690           NA    0.95541    0.95187 
     1      EducationField              14558.870    14849.748    12038.647    0.95537    0.95211 
     2      EnvironmentSatisfaction     14553.211    14829.784    12036.595    0.95535    0.95227 
     3      JobInvolvement              14548.857    14811.124    12035.865    0.95526    0.95236 
     4      StockOptionLevel            14544.370    14792.332    12035.021    0.95519    0.95245 
     5      RelationshipSatisfaction    14540.121    14773.777    12034.431    0.95510    0.95253 
     6      JobSatisfaction             14536.064    14755.414    12034.050    0.95499    0.95259 
     7      WorkLifeBalance             14532.198    14737.244    12033.878    0.95488    0.95265 
     8      MaritalStatus               14528.858    14724.366    12032.310    0.95485    0.95273 
     9      HourlyRate                  14526.858    14717.598    12030.154    0.95485    0.95279 
     10     Age                         14524.932    14710.903    12028.076    0.95485    0.95284 
     11     Attrition                   14523.015    14704.218    12026.013    0.95484    0.95289 
     12     OverTime                    14521.114    14697.548    12023.970    0.95484    0.95294 
     13     NumCompaniesWorked          14519.461    14691.127    12022.181    0.95482    0.95298 
     14     YearsWithCurrManager        14517.865    14684.762    12020.454    0.95480    0.95301 
     15     TrainingTimesLastYear       14516.385    14678.514    12018.848    0.95477    0.95304 
     16     MonthlyRate                 14515.205    14672.565    12017.547    0.95473    0.95305 
     17     YearsInCurrentRole          14513.998    14666.590    12016.225    0.95469    0.95307 
     18     YearsAtCompany              14512.431    14660.255    12014.547    0.95466    0.95310 
     19     YearsSinceLastPromotion     14511.385    14654.440    12013.395    0.95461    0.95310 
     20     Education                   14510.250    14634.231    12018.065    0.95425    0.95296 
     21     DistanceFromHome            14509.513    14628.725    12017.239    0.95419    0.95294 
     22     EmployeeNumber              14508.953    14623.397    12016.596    0.95411    0.95292 
     23     Gender                      14508.463    14618.139    12016.028    0.95403    0.95289 
     24     Department                  14508.418    14608.557    12017.888    0.95382    0.95279 
     25     PercentSalaryHike           14508.337    14603.707    12017.741    0.95372    0.95274 
     26     PerformanceRating           14507.882    14598.483    12017.223    0.95364    0.95271 
    ---------------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  989.387 
    R-Squared                 0.954       MSE                999567.033 
    Adj. R-Squared            0.953       Coef. Var              15.645 
    Pred R-Squared            0.952       AIC                 14507.882 
    MAE                     757.865       SBC                 14598.483 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                          ANOVA                                       
    ---------------------------------------------------------------------------------
                           Sum of                                                    
                          Squares         DF       Mean Square       F          Sig. 
    ---------------------------------------------------------------------------------
    Regression    17517993274.884         17    1030470192.640    1030.917    0.0000 
    Residual        851631112.311        852        999567.033                       
    Total         18369624387.195        869                                         
    ---------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     2047.802       198.897                 10.296    0.000     1657.417     2438.188 
                                  ID       -0.213         0.136       -0.012    -1.561    0.119       -0.481        0.055 
         BusinessTravelTravel_Rarely      331.967       111.601        0.033     2.975    0.003      112.923      551.011 
     BusinessTravelTravel_Frequently      187.285       131.735        0.016     1.422    0.155      -71.278      445.848 
                           DailyRate        0.174         0.085        0.015     2.044    0.041        0.007        0.341 
                           JobLevel2     1708.803       138.458        0.178    12.342    0.000     1437.044     1980.561 
                           JobLevel3     4941.408       186.075        0.386    26.556    0.000     4576.189     5306.627 
                           JobLevel4     8261.027       280.819        0.456    29.418    0.000     7709.849     8812.206 
                           JobLevel5    10937.256       330.773        0.480    33.066    0.000    10288.031    11586.481 
        JobRoleLaboratory Technician       20.887       162.666        0.002     0.128    0.898     -298.386      340.159 
              JobRoleHuman Resources      153.145       238.202        0.006     0.643    0.520     -314.386      620.677 
           JobRoleResearch Scientist      212.049       159.830        0.018     1.327    0.185     -101.658      525.757 
              JobRoleSales Executive     1226.701       199.750        0.112     6.141    0.000      834.641     1618.762 
    JobRoleHealthcare Representative     1245.795       220.122        0.077     5.660    0.000      813.751     1677.840 
       JobRoleManufacturing Director     1353.309       216.300        0.088     6.257    0.000      928.766     1777.853 
            JobRoleResearch Director     4747.815       267.694        0.243    17.736    0.000     4222.398     5273.233 
                      JobRoleManager     4618.324       290.233        0.236    15.912    0.000     4048.668     5187.981 
                   TotalWorkingYears       45.282         7.716        0.074     5.869    0.000       30.138       60.427 
    ----------------------------------------------------------------------------------------------------------------------

    step2


                                       Stepwise Summary                                    
    -------------------------------------------------------------------------------------
    Step    Variable                    AIC          SBC       SBIC      R2       Adj. R2 
    -------------------------------------------------------------------------------------
     0      Base Model               17145.913    17155.450      NA    0.00000    0.00000 
     1      JobLevel (+)             14903.153    14931.764      NA    0.92476    0.92441 
     2      JobRole (+)              14549.801    14616.560      NA    0.95079    0.95010 
     3      TotalWorkingYears (+)    14516.912    14588.439      NA    0.95272    0.95200 
     4      BusinessTravel (+)       14510.833    14591.897      NA    0.95327    0.95245 
     5      DailyRate (+)            14508.366    14594.199      NA    0.95351    0.95263 
     6      ID (+)                   14507.882    14598.483      NA    0.95364    0.95271 
    -------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  989.387 
    R-Squared                 0.954       MSE                999567.033 
    Adj. R-Squared            0.953       Coef. Var              15.645 
    Pred R-Squared            0.952       AIC                 14507.882 
    MAE                     757.865       SBC                 14598.483 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                          ANOVA                                       
    ---------------------------------------------------------------------------------
                           Sum of                                                    
                          Squares         DF       Mean Square       F          Sig. 
    ---------------------------------------------------------------------------------
    Regression    17517993274.884         17    1030470192.640    1030.917    0.0000 
    Residual        851631112.311        852        999567.033                       
    Total         18369624387.195        869                                         
    ---------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     2047.802       198.897                 10.296    0.000     1657.417     2438.188 
                           JobLevel2     1708.803       138.458        0.178    12.342    0.000     1437.044     1980.561 
                           JobLevel3     4941.408       186.075        0.386    26.556    0.000     4576.189     5306.627 
                           JobLevel4     8261.027       280.819        0.456    29.418    0.000     7709.849     8812.206 
                           JobLevel5    10937.256       330.773        0.480    33.066    0.000    10288.031    11586.481 
        JobRoleLaboratory Technician       20.887       162.666        0.002     0.128    0.898     -298.386      340.159 
              JobRoleHuman Resources      153.145       238.202        0.006     0.643    0.520     -314.386      620.677 
           JobRoleResearch Scientist      212.049       159.830        0.018     1.327    0.185     -101.658      525.757 
              JobRoleSales Executive     1226.701       199.750        0.112     6.141    0.000      834.641     1618.762 
    JobRoleHealthcare Representative     1245.795       220.122        0.077     5.660    0.000      813.751     1677.840 
       JobRoleManufacturing Director     1353.309       216.300        0.088     6.257    0.000      928.766     1777.853 
            JobRoleResearch Director     4747.815       267.694        0.243    17.736    0.000     4222.398     5273.233 
                      JobRoleManager     4618.324       290.233        0.236    15.912    0.000     4048.668     5187.981 
                   TotalWorkingYears       45.282         7.716        0.074     5.869    0.000       30.138       60.427 
         BusinessTravelTravel_Rarely      331.967       111.601        0.033     2.975    0.003      112.923      551.011 
     BusinessTravelTravel_Frequently      187.285       131.735        0.016     1.422    0.155      -71.278      445.848 
                           DailyRate        0.174         0.085        0.015     2.044    0.041        0.007        0.341 
                                  ID       -0.213         0.136       -0.012    -1.561    0.119       -0.481        0.055 
    ----------------------------------------------------------------------------------------------------------------------

    # forward: JobLevel, JobRole, TotalWorkingYears, BusinessTravel, DailyRate, ID
    # backward: ID + BusinessTravel + DailyRate + JobLevel + JobRole + TotalWorkingYears
    # stepwise: JobLevel, JobRole, TotalWorkingYears, BusinessTravel, DailyRate, ID


    # Create 2-way interactions with the most important variables from the variable selection above and run variable selection including them
    cs2_regresSmall = cs2_regres2 %>% 
      select(MonthlyIncome, ID, BusinessTravel, DailyRate, Department, Education, JobLevel, JobRole, TotalWorkingYears)
    str(cs2_regresSmall)

    'data.frame':   870 obs. of  9 variables:
     $ MonthlyIncome    : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
     $ ID               : int  1 2 3 4 5 6 7 8 9 10 ...
     $ BusinessTravel   : Factor w/ 3 levels "Non-Travel","Travel_Rarely",..: 2 2 3 2 3 3 2 2 2 3 ...
     $ DailyRate        : int  117 1308 200 801 567 294 1283 309 1333 653 ...
     $ Department       : Factor w/ 3 levels "Human Resources",..: 3 2 2 3 2 2 2 3 3 2 ...
     $ Education        : Factor w/ 5 levels "1","2","3","4",..: 4 3 2 4 1 2 5 4 4 4 ...
     $ JobLevel         : Factor w/ 5 levels "1","2","3","4",..: 2 5 3 3 1 3 1 2 1 2 ...
     $ JobRole          : Factor w/ 9 levels "Sales Representative",..: 5 8 7 5 4 7 4 5 1 6 ...
     $ TotalWorkingYears: int  8 21 10 14 6 9 7 8 1 8 ...

    fit2 = lm(MonthlyIncome ~ .^2, data = cs2_regresSmall)

    fore3 = ols_step_forward_p(fit2, p_val = 0.2, details = FALSE)
    back3 = ols_step_backward_p(fit2, p_val = 0.2, details = FALSE)
    # step3 = ols_step_both_p(fit2, p_enter = 0.2, p_remove = 0.2, details = FALSE) # this gets an error
    fore3


                                               Stepwise Summary                                            
    -----------------------------------------------------------------------------------------------------
    Step    Variable                               AIC          SBC         SBIC         R2       Adj. R2 
    -----------------------------------------------------------------------------------------------------
     0      Base Model                          17145.913    17155.450    14673.060    0.00000    0.00000 
     1      JobLevel                            14903.153    14931.764    12423.882    0.92476    0.92441 
     2      JobRole                             14549.801    14616.560    12056.020    0.95079    0.95010 
     3      TotalWorkingYears                   14516.912    14588.439    12021.959    0.95272    0.95200 
     4      BusinessTravel                      14510.833    14591.897    12012.640    0.95327    0.95245 
     5      DailyRate                           14508.366    14594.199    12008.918    0.95351    0.95263 
     6      ID:TotalWorkingYears                14506.847    14597.449    12006.143    0.95369    0.95277 
     7      Department                          14506.644    14606.782    12002.695    0.95392    0.95289 
     8      Education                           14506.843    14626.055    11995.688    0.95433    0.95309 
     9      JobLevel:TotalWorkingYears          14502.096    14640.382    11983.797    0.95499    0.95355 
     10     JobRole:TotalWorkingYears           14481.506    14657.940    11948.342    0.95685    0.95504 
     11     ID                                  14483.504    14664.707    11949.117    0.95685    0.95498 
     12     BusinessTravel:TotalWorkingYears    14482.435    14673.175    11944.889    0.95710    0.95514 
     13     ID:DailyRate                        14482.652    14678.161    11943.911    0.95719    0.95517 
    -----------------------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.978       RMSE                  950.795 
    R-Squared                 0.957       MSE                947577.972 
    Adj. R-Squared            0.955       Coef. Var              15.233 
    Pred R-Squared            0.953       AIC                 14482.652 
    MAE                     719.332       SBC                 14678.161 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17583134670.495         39    450849606.936    475.792    0.0000 
    Residual        786489716.700        830       947577.972                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                              Parameter Estimates                                                            
    ----------------------------------------------------------------------------------------------------------------------------------------
                                                 model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------------------------
                                           (Intercept)     2096.372       550.478                  3.808    0.000     1015.879     3176.866 
                                             JobLevel2     3454.874       375.618        0.361     9.198    0.000     2717.602     4192.146 
                                             JobLevel3     5910.328       426.433        0.461    13.860    0.000     5073.315     6747.341 
                                             JobLevel4    11466.881       803.739        0.632    14.267    0.000     9889.281    13044.482 
                                             JobLevel5    13660.280      1129.838        0.600    12.090    0.000    11442.604    15877.956 
                          JobRoleLaboratory Technician     -311.497       390.806       -0.026    -0.797    0.426    -1078.581      455.586 
                                JobRoleHuman Resources       32.530       638.927        0.001     0.051    0.959    -1221.574     1286.633 
                             JobRoleResearch Scientist     -436.485       388.177       -0.038    -1.124    0.261    -1198.409      325.440 
                                JobRoleSales Executive     -582.624       437.934       -0.053    -1.330    0.184    -1442.212      276.965 
                      JobRoleHealthcare Representative     -943.548       570.212       -0.058    -1.655    0.098    -2062.775      175.680 
                         JobRoleManufacturing Director    -1146.193       559.029       -0.075    -2.050    0.041    -2243.470      -48.916 
                              JobRoleResearch Director     2399.402       759.128        0.123     3.161    0.002      909.366     3889.438 
                                        JobRoleManager     2377.394       858.594        0.122     2.769    0.006      692.123     4062.665 
                                     TotalWorkingYears       86.402        33.537        0.141     2.576    0.010       20.574      152.230 
                           BusinessTravelTravel_Rarely      540.376       189.316        0.053     2.854    0.004      168.781      911.970 
                       BusinessTravelTravel_Frequently      579.880       222.998        0.049     2.600    0.009      142.174     1017.586 
                                             DailyRate       -0.005         0.173        0.000    -0.026    0.979       -0.344        0.335 
                      DepartmentResearch & Development      290.272       438.189        0.030     0.662    0.508     -569.816     1150.361 
                                       DepartmentSales     -295.333       446.250       -0.030    -0.662    0.508    -1171.245      580.578 
                                            Education2       32.039       125.270        0.003     0.256    0.798     -213.844      277.921 
                                            Education3      -32.169       115.093       -0.003    -0.280    0.780     -258.077      193.739 
                                            Education4       82.590       121.267        0.008     0.681    0.496     -155.435      320.616 
                                            Education5     -478.152       224.810       -0.018    -2.127    0.034     -919.416      -36.889 
                                                    ID       -0.343         0.362       -0.019    -0.948    0.343       -1.053        0.367 
                                  TotalWorkingYears:ID       -0.021         0.018       -0.021    -1.182    0.238       -0.056        0.014 
                           JobLevel2:TotalWorkingYears     -134.950        30.503       -0.162    -4.424    0.000     -194.823      -75.077 
                           JobLevel3:TotalWorkingYears      -75.976        31.922       -0.094    -2.380    0.018     -138.634      -13.318 
                           JobLevel4:TotalWorkingYears     -190.931        42.477       -0.275    -4.495    0.000     -274.306     -107.557 
                           JobLevel5:TotalWorkingYears     -173.090        53.061       -0.203    -3.262    0.001     -277.240      -68.940 
        JobRoleLaboratory Technician:TotalWorkingYears      -33.148        33.922       -0.027    -0.977    0.329      -99.731       33.435 
              JobRoleHuman Resources:TotalWorkingYears      -33.149        67.192       -0.009    -0.493    0.622     -165.035       98.738 
           JobRoleResearch Scientist:TotalWorkingYears       12.455        33.532        0.010     0.371    0.710      -53.362       78.272 
              JobRoleSales Executive:TotalWorkingYears      138.103        42.634        0.160     3.239    0.001       54.420      221.786 
    JobRoleHealthcare Representative:TotalWorkingYears      119.088        44.019        0.115     2.705    0.007       32.686      205.490 
       JobRoleManufacturing Director:TotalWorkingYears      144.953        44.067        0.134     3.289    0.001       58.458      231.449 
            JobRoleResearch Director:TotalWorkingYears      130.684        50.106        0.149     2.608    0.009       32.335      229.034 
                      JobRoleManager:TotalWorkingYears      136.550        51.948        0.178     2.629    0.009       34.586      238.515 
         TotalWorkingYears:BusinessTravelTravel_Rarely      -23.891        15.581       -0.042    -1.533    0.126      -54.475        6.692 
     TotalWorkingYears:BusinessTravelTravel_Frequently      -39.701        17.497       -0.047    -2.269    0.024      -74.044       -5.359 
                                          DailyRate:ID        0.000         0.000        0.027     1.305    0.192        0.000        0.001 
    ----------------------------------------------------------------------------------------------------------------------------------------

    back3


                                             Stepwise Summary                                          
    -------------------------------------------------------------------------------------------------
    Step    Variable                           AIC          SBC         SBIC         R2       Adj. R2 
    -------------------------------------------------------------------------------------------------
     0      Full Model                      14731.980    15857.344    11825.552    0.96358    0.95345 
     1      Department:TotalWorkingYears    14728.015    15843.843    11824.666    0.96358    0.95352 
     2      BusinessTravel:JobRole          14699.356    15738.888    11826.964    0.96344    0.95435 
     3      BusinessTravel:DailyRate        14695.645    15725.640    11826.326    0.96342    0.95446 
     4      ID:Education                    14688.416    15699.336    11826.154    0.96339    0.95468 
     5      DailyRate:TotalWorkingYears     14686.893    15693.045    11825.700    0.96337    0.95472 
     6      DailyRate:Department            14683.908    15680.523    11825.768    0.96333    0.95480 
     7      ID:Department                   14680.761    15667.839    11825.680    0.96329    0.95488 
     8      Department:JobLevel             14665.621    15614.551    11825.602    0.96326    0.95503 
     9      Department:Education            14653.709    15564.491    11828.657    0.96308    0.95519 
     10     ID:BusinessTravel               14651.246    15552.491    11829.244    0.96302    0.95524 
     11     ID:JobRole                      14643.335    15506.432    11836.209    0.96267    0.95532 
     12     DailyRate:JobRole               14634.444    15459.393    11842.237    0.96237    0.95544 
     13     Education:JobRole               14605.207    15277.565    11875.269    0.96083    0.95551 
     14     DailyRate:JobLevel              14601.174    15254.458    11878.290    0.96065    0.95554 
     15     BusinessTravel:Department       14597.511    15231.720    11881.682    0.96046    0.95555 
     16     Education:JobLevel              14582.447    15140.361    11897.425    0.95968    0.95559 
     17     DailyRate:Education             14577.808    15116.647    11899.891    0.95952    0.95564 
     18     Department                      14577.808    15116.647    11899.891    0.95952    0.95564 
     19     Department:JobRole              14544.171    14997.177    11901.383    0.95941    0.95563 
     20     JobRole:TotalWorkingYears       14538.485    14953.344    11910.696    0.95893    0.95555 
     21     ID:TotalWorkingYears            14537.830    14947.921    11911.203    0.95887    0.95554 
     22     Education:TotalWorkingYears     14535.726    14926.742    11916.196    0.95859    0.95546 
    -------------------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.979       RMSE                  935.119 
    R-Squared                 0.959       MSE                941546.719 
    Adj. R-Squared            0.955       Coef. Var              15.185 
    Pred R-Squared             -Inf       AIC                 14535.726 
    MAE                     700.747       SBC                 14926.742 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17608854637.839         61    288669748.161    306.591    0.0000 
    Residual        760769749.356        808       941546.719                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                              Parameter Estimates                                                           
    ---------------------------------------------------------------------------------------------------------------------------------------
                                                model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ---------------------------------------------------------------------------------------------------------------------------------------
                                          (Intercept)     1931.368       436.692                  4.423    0.000     1074.183     2788.552 
                                                   ID       -0.169         0.352       -0.009    -0.481    0.631       -0.859        0.521 
                          BusinessTravelTravel_Rarely      274.115       400.945        0.027     0.684    0.494     -512.901     1061.130 
                      BusinessTravelTravel_Frequently      663.288       461.583        0.056     1.437    0.151     -242.756     1569.332 
                                            DailyRate       -0.034         0.175       -0.003    -0.193    0.847       -0.378        0.310 
                                           Education2      213.046       407.188        0.019     0.523    0.601     -586.225     1012.317 
                                           Education3     -454.374       387.918       -0.048    -1.171    0.242    -1215.821      307.072 
                                           Education4      407.244       412.011        0.040     0.988    0.323     -401.494     1215.981 
                                           Education5     -278.193       801.607       -0.010    -0.347    0.729    -1851.669     1295.284 
                                            JobLevel2     1710.172       683.493        0.178     2.502    0.013      368.540     3051.804 
                                            JobLevel3     4372.304       975.583        0.341     4.482    0.000     2457.328     6287.279 
                                            JobLevel4    10004.610      1361.182        0.552     7.350    0.000     7332.740    12676.480 
                                            JobLevel5    10451.257      1774.960        0.459     5.888    0.000     6967.180    13935.333 
                         JobRoleLaboratory Technician      174.118       169.872        0.014     1.025    0.306     -159.324      507.560 
                               JobRoleHuman Resources       74.997       251.126        0.003     0.299    0.765     -417.940      567.934 
                            JobRoleResearch Scientist      194.985       167.254        0.017     1.166    0.244     -133.318      523.288 
                               JobRoleSales Executive      822.192       778.622        0.075     1.056    0.291     -706.169     2350.554 
                     JobRoleHealthcare Representative      218.821       806.431        0.013     0.271    0.786    -1364.126     1801.767 
                        JobRoleManufacturing Director     1129.165       580.726        0.074     1.944    0.052      -10.746     2269.075 
                             JobRoleResearch Director     3896.783       796.474        0.199     4.893    0.000     2333.381     5460.186 
                                       JobRoleManager     3947.259       718.521        0.202     5.494    0.000     2536.870     5357.648 
                                    TotalWorkingYears      101.416        29.196        0.166     3.474    0.001       44.108      158.725 
                                         ID:DailyRate        0.000         0.000        0.027     1.276    0.202        0.000        0.001 
                                         ID:JobLevel2       -0.775         0.318       -0.043    -2.435    0.015       -1.399       -0.150 
                                         ID:JobLevel3       -0.467         0.407       -0.020    -1.147    0.252       -1.267        0.332 
                                         ID:JobLevel4       -0.174         0.586       -0.005    -0.296    0.767       -1.325        0.978 
                                         ID:JobLevel5       -0.293         0.680       -0.006    -0.431    0.667       -1.629        1.043 
               BusinessTravelTravel_Rarely:Education2      -75.294       433.711       -0.006    -0.174    0.862     -926.627      776.039 
           BusinessTravelTravel_Frequently:Education2     -682.001       498.347       -0.028    -1.369    0.172    -1660.208      296.207 
               BusinessTravelTravel_Rarely:Education3      507.250       410.350        0.049     1.236    0.217     -298.228     1312.727 
           BusinessTravelTravel_Frequently:Education3      268.878       465.528        0.015     0.578    0.564     -644.908     1182.665 
               BusinessTravelTravel_Rarely:Education4     -313.654       435.213       -0.027    -0.721    0.471    -1167.935      540.626 
           BusinessTravelTravel_Frequently:Education4     -462.340       499.286       -0.020    -0.926    0.355    -1442.390      517.710 
               BusinessTravelTravel_Rarely:Education5       -2.820       849.168        0.000    -0.003    0.997    -1669.655     1664.015 
           BusinessTravelTravel_Frequently:Education5     -661.275       931.682       -0.013    -0.710    0.478    -2490.078     1167.528 
                BusinessTravelTravel_Rarely:JobLevel2      814.678       325.847        0.076     2.500    0.013      175.072     1454.283 
            BusinessTravelTravel_Frequently:JobLevel2      670.612       370.711        0.036     1.809    0.071      -57.058     1398.281 
                BusinessTravelTravel_Rarely:JobLevel3      528.060       428.411        0.035     1.233    0.218     -312.870     1368.991 
            BusinessTravelTravel_Frequently:JobLevel3      967.560       496.072        0.036     1.950    0.051       -6.182     1941.301 
                BusinessTravelTravel_Rarely:JobLevel4      451.852       906.659        0.021     0.498    0.618    -1327.832     2231.537 
            BusinessTravelTravel_Frequently:JobLevel4      903.738       977.555        0.024     0.924    0.356    -1015.109     2822.585 
                BusinessTravelTravel_Rarely:JobLevel5     2219.553      1284.285        0.092     1.728    0.084     -301.376     4740.482 
            BusinessTravelTravel_Frequently:JobLevel5     1716.209      1425.501        0.022     1.204    0.229    -1081.912     4514.330 
        BusinessTravelTravel_Rarely:TotalWorkingYears      -50.722        27.893       -0.089    -1.818    0.069     -105.474        4.029 
    BusinessTravelTravel_Frequently:TotalWorkingYears      -77.352        31.161       -0.091    -2.482    0.013     -138.517      -16.187 
               JobLevel2:JobRoleLaboratory Technician     -198.715       615.421       -0.008    -0.323    0.747    -1406.727     1009.297 
               JobLevel3:JobRoleLaboratory Technician    -4066.656      1057.198       -0.052    -3.847    0.000    -6141.834    -1991.478 
               JobLevel4:JobRoleLaboratory Technician           NA       945.287        0.000     0.359    0.720           NA           NA 
               JobLevel5:JobRoleLaboratory Technician           NA      1117.711        0.000     0.321    0.748           NA           NA 
                     JobLevel2:JobRoleHuman Resources      339.550       615.763        0.003     0.426    0.670    -1515.958     2195.057 
                     JobLevel3:JobRoleHuman Resources      359.095      1321.708       -0.004    -0.290    0.772    -1834.864     2553.054 
                     JobLevel4:JobRoleHuman Resources           NA       543.627        0.000     0.475    0.635           NA           NA 
                     JobLevel5:JobRoleHuman Resources           NA       628.512        0.000    -0.321    0.748           NA           NA 
                  JobLevel2:JobRoleResearch Scientist      262.454       594.335        0.039     1.640    0.101     -946.230     1471.137 
                  JobLevel3:JobRoleResearch Scientist     -382.783       669.702        0.002     0.354    0.724    -2977.169     2211.604 
                  JobLevel4:JobRoleResearch Scientist           NA       649.737        0.000    -0.596    0.551           NA           NA 
                  JobLevel5:JobRoleResearch Scientist           NA       604.821        0.000     0.392    0.695           NA           NA 
                     JobLevel2:JobRoleSales Executive      258.330       474.939        0.022     0.585    0.559     -808.758     1325.418 
                     JobLevel3:JobRoleSales Executive     -201.852        20.084       -0.001    -1.391    0.165    -1435.561     1031.857 
                     JobLevel4:JobRoleSales Executive           NA        21.342        0.001     2.920    0.004           NA           NA 
                     JobLevel5:JobRoleSales Executive           NA        27.605        0.000    -2.265    0.024           NA           NA 
           JobLevel2:JobRoleHealthcare Representative      974.696        36.601       -0.002    -0.857    0.392     -191.926     2141.318 
           JobLevel3:JobRoleHealthcare Representative      236.949            NA        0.000        NA       NA    -1077.611     1551.510 
           JobLevel4:JobRoleHealthcare Representative           NA            NA        0.005        NA       NA           NA           NA 
           JobLevel5:JobRoleHealthcare Representative           NA            NA        0.000        NA       NA           NA           NA 
              JobLevel2:JobRoleManufacturing Director           NA            NA        0.000        NA       NA           NA           NA 
              JobLevel3:JobRoleManufacturing Director     -387.127            NA        0.008        NA       NA    -1662.499      888.244 
              JobLevel4:JobRoleManufacturing Director           NA            NA       -0.009        NA       NA           NA           NA 
              JobLevel5:JobRoleManufacturing Director           NA            NA        0.000        NA       NA           NA           NA 
                   JobLevel2:JobRoleResearch Director           NA            NA        0.000        NA       NA           NA           NA 
                   JobLevel3:JobRoleResearch Director      237.018            NA        0.057        NA       NA     -950.188     1424.224 
                   JobLevel4:JobRoleResearch Director      277.747            NA        0.124        NA       NA     -654.512     1210.007 
                   JobLevel5:JobRoleResearch Director           NA            NA        0.283        NA       NA           NA           NA 
                             JobLevel2:JobRoleManager           NA            NA        0.000        NA       NA           NA           NA 
                             JobLevel3:JobRoleManager           NA            NA        0.003        NA       NA           NA           NA 
                             JobLevel4:JobRoleManager           NA            NA        0.003        NA       NA           NA           NA 
                             JobLevel5:JobRoleManager           NA            NA        0.007        NA       NA           NA           NA 
                          JobLevel2:TotalWorkingYears      -27.935            NA        0.990        NA       NA      -67.358       11.488 
                          JobLevel3:TotalWorkingYears       62.326            NA        0.271        NA       NA       20.434      104.218 
                          JobLevel4:TotalWorkingYears      -62.527            NA        1.627        NA       NA     -116.713       -8.341 
                          JobLevel5:TotalWorkingYears      -31.349            NA        4.575        NA       NA     -103.193       40.494 
    ---------------------------------------------------------------------------------------------------------------------------------------

    # With Interaction Terms:
    # Forward: JobLevel + JobRole + TotalWorkingYears + BusinessTravel + DailyRate + ID:TotalWorkingYears + Department + Education + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears + ID + BusinessTravel:TotalWorkingYears + ID:DailyRate 

    # Backward: ID + BusinessTravel + DailyRate + Education + JobLevel + JobRole + TotalWorkingYears + ID:DailyRate + ID:JobLevel + BusinessTravel:Education + BusinessTravel:JobLevel + BusinessTravel:TotalWorkingYears + JobLevel:JobRole + JobLevel:TotalWorkingYears 

    # Stepwise: MonthlyIncome ~ JobLevel + JobRole + TotalWorkingYears + BusinessTravel + DailyRate + ID:TotalWorkingYears + Department + Education + JobLevel:JobRole + Education:TotalWorkingYears


    # Candidate models from the variable selection methods above
    model8 = lm(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Department + Education + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall)

    model7a = lm(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Department + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall)

    # model7b = lm(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Education + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall)

    model6 = lm(MonthlyIncome ~ ID + BusinessTravel + DailyRate + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall)

    model8intFull = lm(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Department + Education + JobLevel + JobRole + TotalWorkingYears + ID:TotalWorkingYears + ID:DailyRate + ID:JobLevel + BusinessTravel:Education + BusinessTravel:JobLevel + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:JobRole + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall)

    model8int1 = lm(MonthlyIncome ~ ID + BusinessTravel + JobLevel + JobRole + TotalWorkingYears + DailyRate + Education + ID:DailyRate + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall) 

    model8int2 = lm(MonthlyIncome ~ ID + BusinessTravel + JobLevel + JobRole + TotalWorkingYears + ID:DailyRate + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall)

    # Using a restricted list of interaction variables, rerun variable selection with a smaller p-value or AIC
    fore4 = ols_step_forward_p(model8intFull, p_val = 0.15, details = FALSE)
    back4 = ols_step_backward_p(model8intFull, p_val = 0.15, details = FALSE)
    step4 = ols_step_both_p(model8intFull, p_enter = 0.15, p_remove = 0.15, details = FALSE)
    fore4


                                               Stepwise Summary                                            
    -----------------------------------------------------------------------------------------------------
    Step    Variable                               AIC          SBC         SBIC         R2       Adj. R2 
    -----------------------------------------------------------------------------------------------------
     0      Base Model                          17145.913    17155.450    14673.364    0.00000    0.00000 
     1      JobLevel                            14903.153    14931.764    12427.846    0.92476    0.92441 
     2      JobRole                             14549.801    14616.560    12062.925    0.95079    0.95010 
     3      TotalWorkingYears                   14516.912    14588.439    12031.194    0.95272    0.95200 
     4      BusinessTravel                      14510.833    14591.897    12024.166    0.95327    0.95245 
     5      DailyRate                           14508.366    14594.199    12022.724    0.95351    0.95263 
     6      ID:TotalWorkingYears                14506.847    14597.449    12022.239    0.95369    0.95277 
     7      Department                          14506.644    14606.782    12021.111    0.95392    0.95289 
     8      Education                           14506.843    14626.055    12016.514    0.95433    0.95309 
     9      JobLevel:TotalWorkingYears          14502.096    14640.382    12007.181    0.95499    0.95355 
     10     JobRole:TotalWorkingYears           14481.506    14657.940    11974.949    0.95685    0.95504 
     11     ID                                  14483.504    14664.707    11978.139    0.95685    0.95498 
     12     BusinessTravel:TotalWorkingYears    14482.435    14673.175    11976.481    0.95710    0.95514 
    -----------------------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.978       RMSE                  951.770 
    R-Squared                 0.957       MSE                948379.019 
    Adj. R-Squared            0.955       Coef. Var              15.240 
    Pred R-Squared            0.953       AIC                 14482.435 
    MAE                     720.268       SBC                 14673.175 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17581521422.397         38    462671616.379    487.855    0.0000 
    Residual        788102964.798        831       948379.019                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                              Parameter Estimates                                                            
    ----------------------------------------------------------------------------------------------------------------------------------------
                                                 model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------------------------
                                           (Intercept)     1963.375       541.188                  3.628    0.000      901.119     3025.631 
                                             JobLevel2     3436.166       375.502        0.359     9.151    0.000     2699.122     4173.211 
                                             JobLevel3     5904.527       426.590        0.461    13.841    0.000     5067.207     6741.847 
                                             JobLevel4    11448.283       803.952        0.631    14.240    0.000     9870.267    13026.299 
                                             JobLevel5    13621.607      1129.927        0.598    12.055    0.000    11403.761    15839.454 
                          JobRoleLaboratory Technician     -311.602       390.971       -0.026    -0.797    0.426    -1079.008      455.804 
                                JobRoleHuman Resources        7.248       638.903        0.000     0.011    0.991    -1246.806     1261.302 
                             JobRoleResearch Scientist     -423.518       388.214       -0.037    -1.091    0.276    -1185.513      338.477 
                                JobRoleSales Executive     -553.482       437.549       -0.051    -1.265    0.206    -1412.314      305.349 
                      JobRoleHealthcare Representative     -906.720       569.754       -0.056    -1.591    0.112    -2025.046      211.606 
                         JobRoleManufacturing Director    -1122.848       558.979       -0.073    -2.009    0.045    -2220.025      -25.672 
                              JobRoleResearch Director     2398.402       759.448        0.123     3.158    0.002      907.740     3889.064 
                                        JobRoleManager     2342.011       858.528        0.120     2.728    0.007      656.872     4027.151 
                                     TotalWorkingYears       86.378        33.551        0.141     2.575    0.010       20.523      152.234 
                           BusinessTravelTravel_Rarely      536.438       189.372        0.053     2.833    0.005      164.735      908.142 
                       BusinessTravelTravel_Frequently      576.349       223.076        0.048     2.584    0.010      138.491     1014.207 
                                             DailyRate        0.193         0.084        0.017     2.281    0.023        0.027        0.358 
                      DepartmentResearch & Development      259.067       437.721        0.027     0.592    0.554     -600.101     1118.235 
                                       DepartmentSales     -315.321       446.176       -0.032    -0.707    0.480    -1191.085      560.443 
                                            Education2       27.026       125.264        0.002     0.216    0.829     -218.844      272.897 
                                            Education3      -38.177       115.050       -0.004    -0.332    0.740     -263.999      187.645 
                                            Education4       82.580       121.318        0.008     0.681    0.496     -155.546      320.706 
                                            Education5     -481.239       224.893       -0.018    -2.140    0.033     -922.664      -39.814 
                                                    ID        0.015         0.236        0.001     0.064    0.949       -0.448        0.478 
                                  TotalWorkingYears:ID       -0.021         0.018       -0.021    -1.201    0.230       -0.056        0.014 
                           JobLevel2:TotalWorkingYears     -134.880        30.516       -0.162    -4.420    0.000     -194.778      -74.982 
                           JobLevel3:TotalWorkingYears      -76.451        31.934       -0.095    -2.394    0.017     -139.131      -13.771 
                           JobLevel4:TotalWorkingYears     -190.927        42.495       -0.275    -4.493    0.000     -274.337     -107.517 
                           JobLevel5:TotalWorkingYears     -172.386        53.081       -0.202    -3.248    0.001     -276.575      -68.198 
        JobRoleLaboratory Technician:TotalWorkingYears      -32.645        33.934       -0.026    -0.962    0.336      -99.252       33.962 
              JobRoleHuman Resources:TotalWorkingYears      -31.992        67.215       -0.008    -0.476    0.634     -163.922       99.938 
           JobRoleResearch Scientist:TotalWorkingYears       11.407        33.536        0.009     0.340    0.734      -54.419       77.233 
              JobRoleSales Executive:TotalWorkingYears      135.935        42.620        0.158     3.189    0.001       52.280      219.590 
    JobRoleHealthcare Representative:TotalWorkingYears      117.977        44.030        0.114     2.680    0.008       31.555      204.399 
       JobRoleManufacturing Director:TotalWorkingYears      144.176        44.081        0.133     3.271    0.001       57.652      230.700 
            JobRoleResearch Director:TotalWorkingYears      131.060        50.126        0.149     2.615    0.009       32.670      229.449 
                      JobRoleManager:TotalWorkingYears      138.065        51.957        0.180     2.657    0.008       36.084      240.047 
         TotalWorkingYears:BusinessTravelTravel_Rarely      -22.856        15.568       -0.040    -1.468    0.142      -53.413        7.701 
     TotalWorkingYears:BusinessTravelTravel_Frequently      -38.305        17.471       -0.045    -2.192    0.029      -72.598       -4.012 
    ----------------------------------------------------------------------------------------------------------------------------------------

    back4


                                             Stepwise Summary                                         
    ------------------------------------------------------------------------------------------------
    Step    Variable                          AIC          SBC         SBIC         R2       Adj. R2 
    ------------------------------------------------------------------------------------------------
     0      Full Model                     14545.808    15008.351    11948.518    0.95952    0.95564 
     1      Department                     14544.171    14997.177    11947.325    0.95941    0.95563 
     2      JobRole:TotalWorkingYears      14538.485    14953.344    11953.622    0.95893    0.95555 
     3      ID:TotalWorkingYears           14537.830    14947.921    11951.538    0.95887    0.95554 
     4      Education:TotalWorkingYears    14535.726    14926.742    11953.776    0.95859    0.95546 
     5      ID:JobLevel                    14534.299    14906.242    11956.706    0.95827    0.95534 
     6      ID:DailyRate                   14533.764    14900.938    11954.806    0.95820    0.95532 
    ------------------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.979       RMSE                  939.449 
    R-Squared                 0.958       MSE                944441.563 
    Adj. R-Squared            0.955       Coef. Var              15.208 
    Pred R-Squared             -Inf       AIC                 14533.764 
    MAE                     706.002       SBC                 14900.938 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17601793396.304         56    314317739.220    332.808    0.0000 
    Residual        767830990.892        813       944441.563                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                              Parameter Estimates                                                           
    ---------------------------------------------------------------------------------------------------------------------------------------
                                                model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ---------------------------------------------------------------------------------------------------------------------------------------
                                          (Intercept)     1981.230       404.054                  4.903    0.000     1188.119     2774.342 
                                                   ID       -0.195         0.136       -0.011    -1.432    0.152       -0.463        0.072 
                          BusinessTravelTravel_Rarely      245.749       400.066        0.024     0.614    0.539     -539.536     1031.033 
                      BusinessTravelTravel_Frequently      609.476       459.392        0.051     1.327    0.185     -292.259     1511.211 
                                            DailyRate        0.169         0.085        0.015     1.997    0.046        0.003        0.336 
                                           Education2      185.582       406.205        0.016     0.457    0.648     -611.753      982.916 
                                           Education3     -478.757       385.165       -0.050    -1.243    0.214    -1234.791      277.278 
                                           Education4      371.525       410.084        0.036     0.906    0.365     -433.424     1176.473 
                                           Education5     -319.505       800.673       -0.012    -0.399    0.690    -1891.135     1252.126 
                                            JobLevel2     1423.051       675.164        0.149     2.108    0.035       97.781     2748.321 
                                            JobLevel3     4215.794       944.986        0.329     4.461    0.000     2360.894     6070.695 
                                            JobLevel4    10017.276      1336.176        0.552     7.497    0.000     7394.514    12640.038 
                                            JobLevel5    10298.692      1769.042        0.452     5.822    0.000     6826.263    13771.121 
                         JobRoleLaboratory Technician      169.838       170.014        0.014     0.999    0.318     -163.881      503.556 
                               JobRoleHuman Resources      112.469       251.010        0.004     0.448    0.654     -380.235      605.174 
                            JobRoleResearch Scientist      182.884       167.389        0.016     1.093    0.275     -145.681      511.450 
                               JobRoleSales Executive      741.559       778.979        0.068     0.952    0.341     -787.487     2270.605 
                     JobRoleHealthcare Representative      204.453       804.853        0.013     0.254    0.800    -1375.381     1784.287 
                        JobRoleManufacturing Director     1069.513       580.959        0.070     1.841    0.066      -70.843     2209.868 
                             JobRoleResearch Director     3812.860       792.844        0.195     4.809    0.000     2256.597     5369.123 
                                       JobRoleManager     3883.580       719.068        0.199     5.401    0.000     2472.132     5295.028 
                                    TotalWorkingYears       98.748        29.203        0.161     3.381    0.001       41.427      156.069 
               BusinessTravelTravel_Rarely:Education2      -70.855       433.197       -0.005    -0.164    0.870     -921.172      779.461 
           BusinessTravelTravel_Frequently:Education2     -609.467       496.132       -0.025    -1.228    0.220    -1583.317      364.384 
               BusinessTravelTravel_Rarely:Education3      509.591       408.245        0.049     1.248    0.212     -291.747     1310.929 
           BusinessTravelTravel_Frequently:Education3      284.918       462.830        0.016     0.616    0.538     -623.564     1193.399 
               BusinessTravelTravel_Rarely:Education4     -282.088       433.841       -0.025    -0.650    0.516    -1133.669      569.493 
           BusinessTravelTravel_Frequently:Education4     -384.481       495.626       -0.017    -0.776    0.438    -1357.339      588.378 
               BusinessTravelTravel_Rarely:Education5       13.732       848.268        0.000     0.016    0.987    -1651.321     1678.785 
           BusinessTravelTravel_Frequently:Education5     -549.789       927.689       -0.011    -0.593    0.554    -2370.737     1271.158 
                BusinessTravelTravel_Rarely:JobLevel2      818.867       326.016        0.077     2.512    0.012      178.935     1458.799 
            BusinessTravelTravel_Frequently:JobLevel2      677.880       371.024        0.037     1.827    0.068      -50.399     1406.158 
                BusinessTravelTravel_Rarely:JobLevel3      506.615       428.760        0.034     1.182    0.238     -334.993     1348.223 
            BusinessTravelTravel_Frequently:JobLevel3      943.197       496.668        0.035     1.899    0.058      -31.705     1918.100 
                BusinessTravelTravel_Rarely:JobLevel4      391.607       906.680        0.018     0.432    0.666    -1388.103     2171.317 
            BusinessTravelTravel_Frequently:JobLevel4      875.223       978.446        0.023     0.895    0.371    -1045.355     2795.800 
                BusinessTravelTravel_Rarely:JobLevel5     2207.097      1284.825        0.092     1.718    0.086     -314.869     4729.062 
            BusinessTravelTravel_Frequently:JobLevel5     1768.993      1422.982        0.023     1.243    0.214    -1024.159     4562.144 
        BusinessTravelTravel_Rarely:TotalWorkingYears      -47.958        27.900       -0.084    -1.719    0.086     -102.723        6.806 
    BusinessTravelTravel_Frequently:TotalWorkingYears      -75.124        31.186       -0.089    -2.409    0.016     -136.338      -13.910 
               JobLevel2:JobRoleLaboratory Technician     -314.561       614.068       -0.013    -0.512    0.609    -1519.907      890.785 
               JobLevel3:JobRoleLaboratory Technician    -4078.369      1054.039       -0.052    -3.869    0.000    -6147.327    -2009.410 
               JobLevel4:JobRoleLaboratory Technician           NA       943.066        0.000     0.173    0.863           NA           NA 
               JobLevel5:JobRoleLaboratory Technician           NA      1118.486        0.000     0.214    0.831           NA           NA 
                     JobLevel2:JobRoleHuman Resources      163.140       616.277        0.002     0.351    0.725    -1687.992     2014.271 
                     JobLevel3:JobRoleHuman Resources      239.102      1320.172       -0.004    -0.286    0.775    -1956.358     2434.563 
                     JobLevel4:JobRoleHuman Resources           NA       543.412        0.000     0.494    0.622           NA           NA 
                     JobLevel5:JobRoleHuman Resources           NA       620.366        0.000    -0.234    0.815           NA           NA 
                  JobLevel2:JobRoleResearch Scientist      216.593       591.741        0.037     1.582    0.114     -993.088     1426.275 
                  JobLevel3:JobRoleResearch Scientist     -378.175       666.125        0.002     0.332    0.740    -2969.523     2213.173 
                  JobLevel4:JobRoleResearch Scientist           NA       645.096        0.000    -0.532    0.595           NA           NA 
                  JobLevel5:JobRoleResearch Scientist           NA       594.881        0.000     0.482    0.630           NA           NA 
                     JobLevel2:JobRoleSales Executive      268.175       466.080        0.025     0.681    0.496     -798.481     1334.831 
                     JobLevel3:JobRoleSales Executive     -145.052        20.079       -0.001    -1.354    0.176    -1362.759     1072.655 
                     JobLevel4:JobRoleSales Executive           NA        21.327        0.001     2.897    0.004           NA           NA 
                     JobLevel5:JobRoleSales Executive           NA        27.634        0.000    -2.254    0.024           NA           NA 
           JobLevel2:JobRoleHealthcare Representative      936.244        36.428       -0.001    -0.758    0.449     -225.277     2097.764 
           JobLevel3:JobRoleHealthcare Representative      220.929            NA        0.000        NA       NA    -1086.598     1528.456 
           JobLevel4:JobRoleHealthcare Representative           NA            NA        0.004        NA       NA           NA           NA 
           JobLevel5:JobRoleHealthcare Representative           NA            NA        0.000        NA       NA           NA           NA 
              JobLevel2:JobRoleManufacturing Director           NA            NA        0.000        NA       NA           NA           NA 
              JobLevel3:JobRoleManufacturing Director     -343.474            NA        0.007        NA       NA    -1609.724      922.776 
              JobLevel4:JobRoleManufacturing Director           NA            NA       -0.009        NA       NA           NA           NA 
              JobLevel5:JobRoleManufacturing Director           NA            NA        0.000        NA       NA           NA           NA 
                   JobLevel2:JobRoleResearch Director           NA            NA        0.000        NA       NA           NA           NA 
                   JobLevel3:JobRoleResearch Director      286.575            NA        0.048        NA       NA     -881.109     1454.259 
                   JobLevel4:JobRoleResearch Director      317.407            NA        0.119        NA       NA     -597.455     1232.268 
                   JobLevel5:JobRoleResearch Director           NA            NA        0.284        NA       NA           NA           NA 
                             JobLevel2:JobRoleManager           NA            NA        0.000        NA       NA           NA           NA 
                             JobLevel3:JobRoleManager           NA            NA        0.003        NA       NA           NA           NA 
                             JobLevel4:JobRoleManager           NA            NA        0.004        NA       NA           NA           NA 
                             JobLevel5:JobRoleManager           NA            NA        0.006        NA       NA           NA           NA 
                          JobLevel2:TotalWorkingYears      -27.189            NA        0.893        NA       NA      -66.602       12.225 
                          JobLevel3:TotalWorkingYears       61.789            NA        0.253        NA       NA       19.927      103.651 
                          JobLevel4:TotalWorkingYears      -62.300            NA        1.541        NA       NA     -116.543       -8.057 
                          JobLevel5:TotalWorkingYears      -27.618            NA        4.476        NA       NA      -99.122       43.886 
    ---------------------------------------------------------------------------------------------------------------------------------------

    step4


                                                 Stepwise Summary                                              
    ---------------------------------------------------------------------------------------------------------
    Step    Variable                                   AIC          SBC         SBIC         R2       Adj. R2 
    ---------------------------------------------------------------------------------------------------------
     0      Base Model                              17145.913    17155.450    14673.364    0.00000    0.00000 
     1      JobLevel (+)                            14903.153    14931.764    12427.846    0.92476    0.92441 
     2      JobRole (+)                             14549.801    14616.560    12062.925    0.95079    0.95010 
     3      TotalWorkingYears (+)                   14516.912    14588.439    12031.194    0.95272    0.95200 
     4      BusinessTravel (+)                      14510.833    14591.897    12024.166    0.95327    0.95245 
     5      DailyRate (+)                           14508.366    14594.199    12022.724    0.95351    0.95263 
     6      ID:TotalWorkingYears (+)                14506.847    14597.449    12022.239    0.95369    0.95277 
     7      Department (+)                          14506.644    14606.782    12021.111    0.95392    0.95289 
     8      Education (+)                           14506.843    14626.055    12016.514    0.95433    0.95309 
     9      JobLevel:TotalWorkingYears (+)          14502.096    14640.382    12007.181    0.95499    0.95355 
     10     ID:TotalWorkingYears (-)                14504.934    14638.452    12008.825    0.95474    0.95335 
     11     JobRole:TotalWorkingYears (+)           14484.047    14655.713    11976.153    0.95662    0.95485 
     12     Department (-)                          14483.286    14645.415    11976.124    0.95646    0.95479 
     13     ID (+)                                  14482.513    14649.410    11976.603    0.95660    0.95488 
     14     BusinessTravel:TotalWorkingYears (+)    14481.292    14657.726    11974.743    0.95686    0.95505 
    ---------------------------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.978       RMSE                  954.430 
    R-Squared                 0.957       MSE                950257.644 
    Adj. R-Squared            0.955       Coef. Var              15.255 
    Pred R-Squared            0.953       AIC                 14481.292 
    MAE                     724.659       SBC                 14657.726 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17577109512.207         35    502203128.920    528.492    0.0000 
    Residual        792514874.989        834       950257.644                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                              Parameter Estimates                                                            
    ----------------------------------------------------------------------------------------------------------------------------------------
                                                 model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------------------------
                                           (Intercept)     1747.552       279.575                  6.251    0.000     1198.799     2296.305 
                                             JobLevel2     3436.066       375.871        0.359     9.142    0.000     2698.302     4173.829 
                                             JobLevel3     5912.556       426.949        0.462    13.848    0.000     5074.536     6750.576 
                                             JobLevel4    11566.564       800.352        0.638    14.452    0.000     9995.623    13137.504 
                                             JobLevel5    13574.124      1103.538        0.596    12.301    0.000    11408.085    15740.162 
                          JobRoleLaboratory Technician      268.233       242.730        0.022     1.105    0.269     -208.200      744.666 
                                JobRoleHuman Resources      349.642       455.570        0.013     0.767    0.443     -544.557     1243.841 
                             JobRoleResearch Scientist      144.010       238.987        0.012     0.603    0.547     -325.077      613.096 
                                JobRoleSales Executive     -548.140       437.967       -0.050    -1.252    0.211    -1407.786      311.506 
                      JobRoleHealthcare Representative     -323.847       480.434       -0.020    -0.674    0.500    -1266.849      619.154 
                         JobRoleManufacturing Director     -552.344       469.202       -0.036    -1.177    0.239    -1473.299      368.611 
                              JobRoleResearch Director     2959.810       700.478        0.151     4.225    0.000     1584.904     4334.716 
                                        JobRoleManager     2802.084       817.122        0.143     3.429    0.001     1198.226     4405.941 
                                     TotalWorkingYears       77.841        32.375        0.127     2.404    0.016       14.295      141.387 
                           BusinessTravelTravel_Rarely      544.211       188.953        0.054     2.880    0.004      173.332      915.091 
                       BusinessTravelTravel_Frequently      576.252       223.237        0.048     2.581    0.010      138.080     1014.424 
                                             DailyRate        0.189         0.084        0.017     2.243    0.025        0.024        0.355 
                                            Education2       21.138       125.146        0.002     0.169    0.866     -224.499      266.775 
                                            Education3      -38.940       115.014       -0.004    -0.339    0.735     -264.691      186.811 
                                            Education4       77.448       121.140        0.008     0.639    0.523     -160.327      315.224 
                                            Education5     -495.972       224.972       -0.018    -2.205    0.028     -937.550      -54.395 
                                                    ID       -0.212         0.135       -0.012    -1.575    0.116       -0.476        0.052 
                           JobLevel2:TotalWorkingYears     -134.894        30.545       -0.162    -4.416    0.000     -194.849      -74.939 
                           JobLevel3:TotalWorkingYears      -77.279        31.960       -0.096    -2.418    0.016     -140.011      -14.547 
                           JobLevel4:TotalWorkingYears     -195.431        42.402       -0.282    -4.609    0.000     -278.659     -112.203 
                           JobLevel5:TotalWorkingYears     -169.228        51.897       -0.199    -3.261    0.001     -271.093      -67.364 
        JobRoleLaboratory Technician:TotalWorkingYears      -32.588        33.966       -0.026    -0.959    0.338      -99.257       34.081 
              JobRoleHuman Resources:TotalWorkingYears      -34.418        67.254       -0.009    -0.512    0.609     -166.424       97.589 
           JobRoleResearch Scientist:TotalWorkingYears       12.675        33.559        0.010     0.378    0.706      -53.196       78.546 
              JobRoleSales Executive:TotalWorkingYears      136.024        42.661        0.158     3.188    0.001       52.288      219.759 
    JobRoleHealthcare Representative:TotalWorkingYears      117.551        44.065        0.113     2.668    0.008       31.060      204.042 
       JobRoleManufacturing Director:TotalWorkingYears      145.199        44.121        0.134     3.291    0.001       58.599      231.800 
            JobRoleResearch Director:TotalWorkingYears      131.617        50.135        0.150     2.625    0.009       33.211      230.024 
                      JobRoleManager:TotalWorkingYears      132.610        51.754        0.173     2.562    0.011       31.027      234.192 
         TotalWorkingYears:BusinessTravelTravel_Rarely      -24.342        15.432       -0.043    -1.577    0.115      -54.632        5.948 
     TotalWorkingYears:BusinessTravelTravel_Frequently      -39.107        17.480       -0.046    -2.237    0.026      -73.418       -4.797 
    ----------------------------------------------------------------------------------------------------------------------------------------

    # fore5 = ols_step_forward_aic(model8intFull, details = FALSE) # this errors
    back5 = ols_step_backward_aic(model8intFull, details = FALSE)
    # step5 = ols_step_both_aic(model8intFull, details = FALSE) # this errors
    back5


                                           Stepwise Summary                                        
    ---------------------------------------------------------------------------------------------
    Step    Variable                       AIC          SBC         SBIC         R2       Adj. R2 
    ---------------------------------------------------------------------------------------------
     0      Full Model                  14545.808    15008.351    11948.518    0.95952    0.95564 
     1      JobLevel:JobRole            14500.508    14810.460    11962.612    0.95864    0.95541 
     2      BusinessTravel:Education    14494.359    14766.163    11968.545    0.95817    0.95535 
     3      BusinessTravel:JobLevel     14488.144    14721.800    11974.495    0.95770    0.95528 
     4      Education                   14482.974    14697.556    11948.613    0.95756    0.95535 
     5      ID:JobLevel                 14480.949    14676.457    11952.924    0.95727    0.95526 
     6      DailyRate                   14478.950    14669.690    11989.703    0.95727    0.95531 
     7      ID:TotalWorkingYears        14478.467    14664.438    11989.986    0.95719    0.95529 
     8      Department                  14477.928    14654.362    11988.943    0.95702    0.95522 
    ---------------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.978       RMSE                  952.586 
    R-Squared                 0.957       MSE                946590.233 
    Adj. R-Squared            0.955       Coef. Var              15.225 
    Pred R-Squared            0.953       AIC                 14477.928 
    MAE                     721.605       SBC                 14654.362 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17580168132.874         35    502290518.082    530.631    0.0000 
    Residual        789456254.321        834       946590.233                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                              Parameter Estimates                                                            
    ----------------------------------------------------------------------------------------------------------------------------------------
                                                 model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------------------------
                                           (Intercept)     1911.484       255.632                  7.477    0.000     1409.727     2413.242 
                                                    ID       -0.565         0.186       -0.031    -3.044    0.002       -0.929       -0.201 
                           BusinessTravelTravel_Rarely      538.693       188.969        0.053     2.851    0.004      167.782      909.604 
                       BusinessTravelTravel_Frequently      531.837       223.765        0.045     2.377    0.018       92.627      971.046 
                                             JobLevel2     3461.417       374.861        0.361     9.234    0.000     2725.635     4197.199 
                                             JobLevel3     5929.361       425.636        0.463    13.931    0.000     5093.917     6764.805 
                                             JobLevel4    11449.241       794.590        0.631    14.409    0.000     9889.610    13008.873 
                                             JobLevel5    13685.639      1107.375        0.601    12.359    0.000    11512.069    15859.208 
                          JobRoleLaboratory Technician      281.712       242.050        0.023     1.164    0.245     -193.388      756.812 
                                JobRoleHuman Resources      298.204       453.625        0.011     0.657    0.511     -592.176     1188.584 
                             JobRoleResearch Scientist      126.430       238.005        0.011     0.531    0.595     -340.730      593.590 
                                JobRoleSales Executive     -570.172       437.603       -0.052    -1.303    0.193    -1429.104      288.761 
                      JobRoleHealthcare Representative     -369.207       482.406       -0.023    -0.765    0.444    -1316.080      577.667 
                         JobRoleManufacturing Director     -572.028       468.678       -0.037    -1.221    0.223    -1491.956      347.900 
                              JobRoleResearch Director     2925.000       699.332        0.150     4.183    0.000     1552.344     4297.657 
                                        JobRoleManager     2883.003       817.734        0.147     3.526    0.000     1277.945     4488.061 
                                     TotalWorkingYears       81.752        32.962        0.134     2.480    0.013       17.055      146.450 
                                          ID:DailyRate        0.000         0.000        0.027     2.649    0.008        0.000        0.001 
         BusinessTravelTravel_Rarely:TotalWorkingYears      -24.157        15.501       -0.042    -1.558    0.120      -54.582        6.269 
     BusinessTravelTravel_Frequently:TotalWorkingYears      -35.185        17.660       -0.042    -1.992    0.047      -69.848       -0.521 
                          TotalWorkingYears:Education2       -0.945        10.751       -0.001    -0.088    0.930      -22.047       20.157 
                          TotalWorkingYears:Education3       -8.047         9.337       -0.013    -0.862    0.389      -26.374       10.280 
                          TotalWorkingYears:Education4       -2.523         9.517       -0.004    -0.265    0.791      -21.202       16.156 
                          TotalWorkingYears:Education5      -41.826        15.284       -0.025    -2.737    0.006      -71.825      -11.827 
                           JobLevel2:TotalWorkingYears     -135.855        30.530       -0.164    -4.450    0.000     -195.779      -75.931 
                           JobLevel3:TotalWorkingYears      -78.703        31.887       -0.097    -2.468    0.014     -141.291      -16.114 
                           JobLevel4:TotalWorkingYears     -190.720        42.216       -0.275    -4.518    0.000     -273.581     -107.859 
                           JobLevel5:TotalWorkingYears     -174.547        51.893       -0.205    -3.364    0.001     -276.404      -72.691 
        JobRoleLaboratory Technician:TotalWorkingYears      -33.854        33.873       -0.027    -0.999    0.318     -100.341       32.633 
              JobRoleHuman Resources:TotalWorkingYears      -29.605        66.990       -0.008    -0.442    0.659     -161.093      101.883 
           JobRoleResearch Scientist:TotalWorkingYears       15.313        33.535        0.013     0.457    0.648      -50.510       81.135 
              JobRoleSales Executive:TotalWorkingYears      138.168        42.698        0.160     3.236    0.001       54.360      221.977 
    JobRoleHealthcare Representative:TotalWorkingYears      120.265        44.265        0.116     2.717    0.007       33.380      207.149 
       JobRoleManufacturing Director:TotalWorkingYears      146.706        44.164        0.136     3.322    0.001       60.020      233.391 
            JobRoleResearch Director:TotalWorkingYears      135.828        50.135        0.155     2.709    0.007       37.423      234.233 
                      JobRoleManager:TotalWorkingYears      130.320        51.791        0.170     2.516    0.012       28.664      231.976 
    ----------------------------------------------------------------------------------------------------------------------------------------

    # Evaluate models above with internal cross-validation
    # define training control
    train_control = trainControl(method="LOOCV")

    # train the model 
    model8CV = train(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Department + Education + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall, trControl = train_control, method = "lm")

    model7aCV = train(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Department + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall, trControl = train_control, method = "lm")

    # model7bCV = train(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Education + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall, trControl = train_control, method = "lm")

    model6CV = train(MonthlyIncome ~ ID + BusinessTravel + DailyRate + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall, trControl = train_control, method = "lm")

    model8intFullCV = train(MonthlyIncome ~ ID + BusinessTravel + DailyRate + Department + Education + JobLevel + JobRole + TotalWorkingYears + ID:TotalWorkingYears + ID:DailyRate + ID:JobLevel + BusinessTravel:Education + BusinessTravel:JobLevel + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:JobRole + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall, trControl = train_control, method = "lm")

    model8int1CV = train(MonthlyIncome ~ ID + BusinessTravel + JobLevel + JobRole + TotalWorkingYears + DailyRate + Education + ID:DailyRate + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall, trControl = train_control, method = "lm")

    model8int2CV = train(MonthlyIncome ~ ID + BusinessTravel + JobLevel + JobRole + TotalWorkingYears + ID:DailyRate + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall, trControl = train_control, method = "lm")

    model8CV

    Linear Regression 

    870 samples
      8 predictor

    No pre-processing
    Resampling: Leave-One-Out Cross-Validation 
    Summary of sample sizes: 869, 869, 869, 869, 869, 869, ... 
    Resampling results:

      RMSE      Rsquared  MAE     
      1007.978  0.951882  770.7709

    Tuning parameter 'intercept' was held constant at a value of TRUE

    model7aCV

    Linear Regression 

    870 samples
      7 predictor

    No pre-processing
    Resampling: Leave-One-Out Cross-Validation 
    Summary of sample sizes: 869, 869, 869, 869, 869, 869, ... 
    Resampling results:

      RMSE      Rsquared   MAE     
      1007.927  0.9518864  771.4089

    Tuning parameter 'intercept' was held constant at a value of TRUE

    # model7bCV
    model6CV

    Linear Regression 

    870 samples
      6 predictor

    No pre-processing
    Resampling: Leave-One-Out Cross-Validation 
    Summary of sample sizes: 869, 869, 869, 869, 869, 869, ... 
    Resampling results:

      RMSE      Rsquared  MAE     
      1009.009  0.951783  773.4415

    Tuning parameter 'intercept' was held constant at a value of TRUE

    model8intFullCV

    Linear Regression 

    870 samples
      8 predictor

    No pre-processing
    Resampling: Leave-One-Out Cross-Validation 
    Summary of sample sizes: 869, 869, 869, 869, 869, 869, ... 
    Resampling results:

      RMSE      Rsquared   MAE     
      1012.851  0.9514514  763.7219

    Tuning parameter 'intercept' was held constant at a value of TRUE

    model8int1CV

    Linear Regression 

    870 samples
      7 predictor

    No pre-processing
    Resampling: Leave-One-Out Cross-Validation 
    Summary of sample sizes: 869, 869, 869, 869, 869, 869, ... 
    Resampling results:

      RMSE      Rsquared   MAE     
      994.6385  0.9531522  755.3346

    Tuning parameter 'intercept' was held constant at a value of TRUE

    model8int2CV

    Linear Regression 

    870 samples
      7 predictor

    No pre-processing
    Resampling: Leave-One-Out Cross-Validation 
    Summary of sample sizes: 869, 869, 869, 869, 869, 869, ... 
    Resampling results:

      RMSE     Rsquared   MAE     
      991.292  0.9534655  752.3138

    Tuning parameter 'intercept' was held constant at a value of TRUE

    # Examine residuals
    plot(model8)

![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/regression_variableSelection_V2-1.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/regression_variableSelection_V2-2.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/regression_variableSelection_V2-3.png)![](Analysis_of_Employee_Attrition_and_Income_V2_files/figure-markdown_strict/regression_variableSelection_V2-4.png)

    # Add squared and logged terms
    str(cs2_regresSmall)

    'data.frame':   870 obs. of  9 variables:
     $ MonthlyIncome    : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
     $ ID               : int  1 2 3 4 5 6 7 8 9 10 ...
     $ BusinessTravel   : Factor w/ 3 levels "Non-Travel","Travel_Rarely",..: 2 2 3 2 3 3 2 2 2 3 ...
     $ DailyRate        : int  117 1308 200 801 567 294 1283 309 1333 653 ...
     $ Department       : Factor w/ 3 levels "Human Resources",..: 3 2 2 3 2 2 2 3 3 2 ...
     $ Education        : Factor w/ 5 levels "1","2","3","4",..: 4 3 2 4 1 2 5 4 4 4 ...
     $ JobLevel         : Factor w/ 5 levels "1","2","3","4",..: 2 5 3 3 1 3 1 2 1 2 ...
     $ JobRole          : Factor w/ 9 levels "Sales Representative",..: 5 8 7 5 4 7 4 5 1 6 ...
     $ TotalWorkingYears: int  8 21 10 14 6 9 7 8 1 8 ...

    cs2_regresSmall2 = cs2_regresSmall %>% 
      mutate(logMonthlyIncome = log(MonthlyIncome),
             MonthlyIncome2 = MonthlyIncome^2,
             logID = log(ID),
             ID2 = ID^2,
             logDailyRate = log(DailyRate),
             DailyRate2 = DailyRate^2,
             logTotalWorkingYears = log(TotalWorkingYears),
             TotalWorkingYears2 = TotalWorkingYears^2)
    str(cs2_regresSmall2)

    'data.frame':   870 obs. of  17 variables:
     $ MonthlyIncome       : int  4403 19626 9362 10422 3760 8793 2127 6694 2220 5063 ...
     $ ID                  : int  1 2 3 4 5 6 7 8 9 10 ...
     $ BusinessTravel      : Factor w/ 3 levels "Non-Travel","Travel_Rarely",..: 2 2 3 2 3 3 2 2 2 3 ...
     $ DailyRate           : int  117 1308 200 801 567 294 1283 309 1333 653 ...
     $ Department          : Factor w/ 3 levels "Human Resources",..: 3 2 2 3 2 2 2 3 3 2 ...
     $ Education           : Factor w/ 5 levels "1","2","3","4",..: 4 3 2 4 1 2 5 4 4 4 ...
     $ JobLevel            : Factor w/ 5 levels "1","2","3","4",..: 2 5 3 3 1 3 1 2 1 2 ...
     $ JobRole             : Factor w/ 9 levels "Sales Representative",..: 5 8 7 5 4 7 4 5 1 6 ...
     $ TotalWorkingYears   : int  8 21 10 14 6 9 7 8 1 8 ...
     $ logMonthlyIncome    : num  8.39 9.88 9.14 9.25 8.23 ...
     $ MonthlyIncome2      : num  1.94e+07 3.85e+08 8.76e+07 1.09e+08 1.41e+07 ...
     $ logID               : num  0 0.693 1.099 1.386 1.609 ...
     $ ID2                 : num  1 4 9 16 25 36 49 64 81 100 ...
     $ logDailyRate        : num  4.76 7.18 5.3 6.69 6.34 ...
     $ DailyRate2          : num  13689 1710864 40000 641601 321489 ...
     $ logTotalWorkingYears: num  2.08 3.04 2.3 2.64 1.79 ...
     $ TotalWorkingYears2  : num  64 441 100 196 36 81 49 64 1 64 ...

    sum(is.na(cs2_regresSmall2))

    [1] 0

    # Identify numeric columns
    numeric_columns <- sapply(cs2_regresSmall2, is.numeric)

    # Check for NaNs in numeric columns
    nan_check <- sapply(cs2_regresSmall2[, numeric_columns], function(x) sum(is.nan(x)))
    print(nan_check)

           MonthlyIncome                   ID            DailyRate    TotalWorkingYears     logMonthlyIncome       MonthlyIncome2 
                       0                    0                    0                    0                    0                    0 
                   logID                  ID2         logDailyRate           DailyRate2 logTotalWorkingYears   TotalWorkingYears2 
                       0                    0                    0                    0                    0                    0 

    # Check for Infs in numeric columns
    inf_check <- sapply(cs2_regresSmall2[, numeric_columns], function(x) sum(is.infinite(x)))
    print(inf_check)

           MonthlyIncome                   ID            DailyRate    TotalWorkingYears     logMonthlyIncome       MonthlyIncome2 
                       0                    0                    0                    0                    0                    0 
                   logID                  ID2         logDailyRate           DailyRate2 logTotalWorkingYears   TotalWorkingYears2 
                       0                    0                    0                    0                    7                    0 

    # Replace Infs with a large finite number
    cs2_regresSmall2$logTotalWorkingYears[is.infinite(cs2_regresSmall2$logTotalWorkingYears)] <- max(cs2_regresSmall2$logTotalWorkingYears[!is.infinite(cs2_regresSmall2$logTotalWorkingYears)], na.rm = TRUE)

    fit_trans1 = lm(MonthlyIncome ~ . -logMonthlyIncome -MonthlyIncome2, data = cs2_regresSmall2)
    fit_trans2 = lm(logMonthlyIncome ~ . -MonthlyIncome -MonthlyIncome2, data = cs2_regresSmall2)
    fit_trans3 = lm(MonthlyIncome2 ~ . -MonthlyIncome -logMonthlyIncome, data = cs2_regresSmall2)

    fore6 = ols_step_forward_p(fit_trans1, p_val = 0.2, details = FALSE)
    back6 = ols_step_backward_p(fit_trans1, p_val = 0.2, details = FALSE)
    step6 = ols_step_both_p(fit_trans1, p_enter = 0.2, p_remove = 0.2, details = FALSE)
    fore6


                                      Stepwise Summary                                  
    ----------------------------------------------------------------------------------
    Step    Variable                 AIC          SBC       SBIC      R2       Adj. R2 
    ----------------------------------------------------------------------------------
     0      Base Model            17145.913    17155.450      NA    0.00000    0.00000 
     1      JobLevel              14903.153    14931.764      NA    0.92476    0.92441 
     2      JobRole               14549.801    14616.560      NA    0.95079    0.95010 
     3      TotalWorkingYears     14516.912    14588.439      NA    0.95272    0.95200 
     4      TotalWorkingYears2    14506.128    14582.424      NA    0.95341    0.95265 
     5      logID                 14502.757    14583.821      NA    0.95370    0.95289 
     6      logDailyRate          14499.937    14585.770      NA    0.95395    0.95309 
     7      BusinessTravel        14495.172    14590.542      NA    0.95442    0.95345 
     8      Education             14494.697    14609.141      NA    0.95486    0.95369 
     9      ID2                   14494.857    14614.069      NA    0.95495    0.95373 
    ----------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  975.259 
    R-Squared                 0.955       MSE                978112.725 
    Adj. R-Squared            0.954       Coef. Var              15.477 
    Pred R-Squared            0.953       AIC                 14494.857 
    MAE                     745.991       SBC                 14614.069 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17542141021.679         23    762701783.551    779.769    0.0000 
    Residual        827483365.517        846       978112.725                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     1887.174       465.101                  4.058    0.000      974.288     2800.061 
                           JobLevel2     1617.409       142.062        0.169    11.385    0.000     1338.574     1896.244 
                           JobLevel3     4852.402       187.827        0.379    25.834    0.000     4483.740     5221.064 
                           JobLevel4     8460.641       281.981        0.467    30.004    0.000     7907.177     9014.105 
                           JobLevel5    11078.569       331.103        0.487    33.460    0.000    10428.689    11728.448 
        JobRoleLaboratory Technician      -30.756       162.669       -0.003    -0.189    0.850     -350.038      288.525 
              JobRoleHuman Resources      102.713       236.725        0.004     0.434    0.664     -361.925      567.351 
           JobRoleResearch Scientist      169.855       160.948        0.015     1.055    0.292     -146.050      485.760 
              JobRoleSales Executive     1171.799       198.885        0.107     5.892    0.000      781.433     1562.165 
    JobRoleHealthcare Representative     1203.639       218.250        0.074     5.515    0.000      775.264     1632.014 
       JobRoleManufacturing Director     1292.773       215.144        0.084     6.009    0.000      870.494     1715.053 
            JobRoleResearch Director     4719.410       267.441        0.241    17.647    0.000     4194.484     5244.335 
                      JobRoleManager     4526.010       288.720        0.231    15.676    0.000     3959.318     5092.701 
                   TotalWorkingYears       94.105        17.377        0.154     5.416    0.000       59.999      128.212 
                  TotalWorkingYears2       -1.699         0.531       -0.090    -3.199    0.001       -2.741       -0.656 
                               logID     -133.840        52.583       -0.029    -2.545    0.011     -237.049      -30.631 
                        logDailyRate      124.977        52.668        0.018     2.373    0.018       21.601      228.353 
         BusinessTravelTravel_Rarely      307.896       110.985        0.030     2.774    0.006       90.057      525.735 
     BusinessTravelTravel_Frequently      181.586       130.747        0.015     1.389    0.165      -75.041      438.212 
                          Education2      -71.147       125.691       -0.006    -0.566    0.572     -317.850      175.557 
                          Education3      -96.332       115.631       -0.010    -0.833    0.405     -323.290      130.626 
                          Education4       39.610       122.008        0.004     0.325    0.746     -199.865      279.085 
                          Education5     -511.951       225.089       -0.019    -2.274    0.023     -953.750      -70.153 
                                 ID2        0.000         0.000        0.015     1.339    0.181        0.000        0.001 
    ----------------------------------------------------------------------------------------------------------------------

    back6


                                       Stepwise Summary                                   
    ------------------------------------------------------------------------------------
    Step    Variable                   AIC          SBC       SBIC      R2       Adj. R2 
    ------------------------------------------------------------------------------------
     0      Full Model              14502.314    14650.137      NA    0.95519    0.95364 
     1      logTotalWorkingYears    14500.315    14643.370      NA    0.95519    0.95370 
     2      ID                      14498.321    14636.608      NA    0.95519    0.95375 
     3      DailyRate2              14496.773    14630.291      NA    0.95516    0.95378 
     4      DailyRate               14495.744    14624.493      NA    0.95511    0.95379 
     5      Department              14494.857    14614.069      NA    0.95495    0.95373 
    ------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  975.259 
    R-Squared                 0.955       MSE                978112.725 
    Adj. R-Squared            0.954       Coef. Var              15.477 
    Pred R-Squared            0.953       AIC                 14494.857 
    MAE                     745.991       SBC                 14614.069 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17542141021.679         23    762701783.551    779.769    0.0000 
    Residual        827483365.517        846       978112.725                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     1887.174       465.101                  4.058    0.000      974.288     2800.061 
         BusinessTravelTravel_Rarely      307.896       110.985        0.030     2.774    0.006       90.057      525.735 
     BusinessTravelTravel_Frequently      181.586       130.747        0.015     1.389    0.165      -75.041      438.212 
                          Education2      -71.147       125.691       -0.006    -0.566    0.572     -317.850      175.557 
                          Education3      -96.332       115.631       -0.010    -0.833    0.405     -323.290      130.626 
                          Education4       39.610       122.008        0.004     0.325    0.746     -199.865      279.085 
                          Education5     -511.951       225.089       -0.019    -2.274    0.023     -953.750      -70.153 
                           JobLevel2     1617.409       142.062        0.169    11.385    0.000     1338.574     1896.244 
                           JobLevel3     4852.402       187.827        0.379    25.834    0.000     4483.740     5221.064 
                           JobLevel4     8460.641       281.981        0.467    30.004    0.000     7907.177     9014.105 
                           JobLevel5    11078.569       331.103        0.487    33.460    0.000    10428.689    11728.448 
        JobRoleLaboratory Technician      -30.756       162.669       -0.003    -0.189    0.850     -350.038      288.525 
              JobRoleHuman Resources      102.713       236.725        0.004     0.434    0.664     -361.925      567.351 
           JobRoleResearch Scientist      169.855       160.948        0.015     1.055    0.292     -146.050      485.760 
              JobRoleSales Executive     1171.799       198.885        0.107     5.892    0.000      781.433     1562.165 
    JobRoleHealthcare Representative     1203.639       218.250        0.074     5.515    0.000      775.264     1632.014 
       JobRoleManufacturing Director     1292.773       215.144        0.084     6.009    0.000      870.494     1715.053 
            JobRoleResearch Director     4719.410       267.441        0.241    17.647    0.000     4194.484     5244.335 
                      JobRoleManager     4526.010       288.720        0.231    15.676    0.000     3959.318     5092.701 
                   TotalWorkingYears       94.105        17.377        0.154     5.416    0.000       59.999      128.212 
                               logID     -133.840        52.583       -0.029    -2.545    0.011     -237.049      -30.631 
                                 ID2        0.000         0.000        0.015     1.339    0.181        0.000        0.001 
                        logDailyRate      124.977        52.668        0.018     2.373    0.018       21.601      228.353 
                  TotalWorkingYears2       -1.699         0.531       -0.090    -3.199    0.001       -2.741       -0.656 
    ----------------------------------------------------------------------------------------------------------------------

    step6


                                        Stepwise Summary                                    
    --------------------------------------------------------------------------------------
    Step    Variable                     AIC          SBC       SBIC      R2       Adj. R2 
    --------------------------------------------------------------------------------------
     0      Base Model                17145.913    17155.450      NA    0.00000    0.00000 
     1      JobLevel (+)              14903.153    14931.764      NA    0.92476    0.92441 
     2      JobRole (+)               14549.801    14616.560      NA    0.95079    0.95010 
     3      TotalWorkingYears (+)     14516.912    14588.439      NA    0.95272    0.95200 
     4      TotalWorkingYears2 (+)    14506.128    14582.424      NA    0.95341    0.95265 
     5      logID (+)                 14502.757    14583.821      NA    0.95370    0.95289 
     6      logDailyRate (+)          14499.937    14585.770      NA    0.95395    0.95309 
     7      BusinessTravel (+)        14495.172    14590.542      NA    0.95442    0.95345 
     8      Education (+)             14494.697    14609.141      NA    0.95486    0.95369 
     9      ID2 (+)                   14494.857    14614.069      NA    0.95495    0.95373 
    --------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                               Model Summary                             
    --------------------------------------------------------------------
    R                         0.977       RMSE                  975.259 
    R-Squared                 0.955       MSE                978112.725 
    Adj. R-Squared            0.954       Coef. Var              15.477 
    Pred R-Squared            0.953       AIC                 14494.857 
    MAE                     745.991       SBC                 14614.069 
    --------------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                         ANOVA                                      
    -------------------------------------------------------------------------------
                           Sum of                                                  
                          Squares         DF      Mean Square       F         Sig. 
    -------------------------------------------------------------------------------
    Regression    17542141021.679         23    762701783.551    779.769    0.0000 
    Residual        827483365.517        846       978112.725                      
    Total         18369624387.195        869                                       
    -------------------------------------------------------------------------------

                                                     Parameter Estimates                                                   
    ----------------------------------------------------------------------------------------------------------------------
                               model         Beta    Std. Error    Std. Beta      t        Sig         lower        upper 
    ----------------------------------------------------------------------------------------------------------------------
                         (Intercept)     1887.174       465.101                  4.058    0.000      974.288     2800.061 
                           JobLevel2     1617.409       142.062        0.169    11.385    0.000     1338.574     1896.244 
                           JobLevel3     4852.402       187.827        0.379    25.834    0.000     4483.740     5221.064 
                           JobLevel4     8460.641       281.981        0.467    30.004    0.000     7907.177     9014.105 
                           JobLevel5    11078.569       331.103        0.487    33.460    0.000    10428.689    11728.448 
        JobRoleLaboratory Technician      -30.756       162.669       -0.003    -0.189    0.850     -350.038      288.525 
              JobRoleHuman Resources      102.713       236.725        0.004     0.434    0.664     -361.925      567.351 
           JobRoleResearch Scientist      169.855       160.948        0.015     1.055    0.292     -146.050      485.760 
              JobRoleSales Executive     1171.799       198.885        0.107     5.892    0.000      781.433     1562.165 
    JobRoleHealthcare Representative     1203.639       218.250        0.074     5.515    0.000      775.264     1632.014 
       JobRoleManufacturing Director     1292.773       215.144        0.084     6.009    0.000      870.494     1715.053 
            JobRoleResearch Director     4719.410       267.441        0.241    17.647    0.000     4194.484     5244.335 
                      JobRoleManager     4526.010       288.720        0.231    15.676    0.000     3959.318     5092.701 
                   TotalWorkingYears       94.105        17.377        0.154     5.416    0.000       59.999      128.212 
                  TotalWorkingYears2       -1.699         0.531       -0.090    -3.199    0.001       -2.741       -0.656 
                               logID     -133.840        52.583       -0.029    -2.545    0.011     -237.049      -30.631 
                        logDailyRate      124.977        52.668        0.018     2.373    0.018       21.601      228.353 
         BusinessTravelTravel_Rarely      307.896       110.985        0.030     2.774    0.006       90.057      525.735 
     BusinessTravelTravel_Frequently      181.586       130.747        0.015     1.389    0.165      -75.041      438.212 
                          Education2      -71.147       125.691       -0.006    -0.566    0.572     -317.850      175.557 
                          Education3      -96.332       115.631       -0.010    -0.833    0.405     -323.290      130.626 
                          Education4       39.610       122.008        0.004     0.325    0.746     -199.865      279.085 
                          Education5     -511.951       225.089       -0.019    -2.274    0.023     -953.750      -70.153 
                                 ID2        0.000         0.000        0.015     1.339    0.181        0.000        0.001 
    ----------------------------------------------------------------------------------------------------------------------

    fore7 = ols_step_forward_p(fit_trans2, p_val = 0.2, details = FALSE)
    back7 = ols_step_backward_p(fit_trans2, p_val = 0.2, details = FALSE)
    step7 = ols_step_both_p(fit_trans2, p_enter = 0.2, p_remove = 0.2, details = FALSE)
    fore7


                                      Stepwise Summary                                  
    ----------------------------------------------------------------------------------
    Step    Variable                  AIC         SBC       SBIC      R2       Adj. R2 
    ----------------------------------------------------------------------------------
     0      Base Model              1752.980    1762.517      NA    0.00000    0.00000 
     1      JobLevel                 -47.889     -19.278      NA    0.87496    0.87439 
     2      logTotalWorkingYears     -79.546     -46.166      NA    0.87971    0.87901 
     3      JobRole                 -190.641    -119.114      NA    0.89606    0.89448 
     4      BusinessTravel          -199.676    -118.612      NA    0.89760    0.89581 
     5      logDailyRate            -202.689    -116.856      NA    0.89819    0.89628 
     6      TotalWorkingYears       -203.815    -113.214      NA    0.89856    0.89653 
     7      TotalWorkingYears2      -221.167    -125.797      NA    0.90079    0.89869 
     8      Education               -220.928    -106.484      NA    0.90167    0.89912 
    ----------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                             Model Summary                           
    ----------------------------------------------------------------
    R                       0.950       RMSE                  0.207 
    R-Squared               0.902       MSE                   0.044 
    Adj. R-Squared          0.899       Coef. Var             2.461 
    Pred R-Squared          0.896       AIC                -220.928 
    MAE                     0.154       SBC                -106.484 
    ----------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                    ANOVA                                 
    ---------------------------------------------------------------------
                   Sum of                                                
                  Squares         DF    Mean Square       F         Sig. 
    ---------------------------------------------------------------------
    Regression    342.898         22         15.586    353.042    0.0000 
    Residual       37.394        847          0.044                      
    Total         380.291        869                                     
    ---------------------------------------------------------------------

                                                Parameter Estimates                                              
    ------------------------------------------------------------------------------------------------------------
                               model      Beta    Std. Error    Std. Beta      t        Sig      lower    upper 
    ------------------------------------------------------------------------------------------------------------
                         (Intercept)     7.508         0.085                 88.064    0.000     7.341    7.675 
                           JobLevel2     0.451         0.030        0.327    14.950    0.000     0.392    0.510 
                           JobLevel3     0.902         0.040        0.489    22.631    0.000     0.824    0.980 
                           JobLevel4     1.193         0.060        0.457    19.897    0.000     1.075    1.310 
                           JobLevel5     1.315         0.070        0.401    18.717    0.000     1.177    1.453 
                logTotalWorkingYears     0.015         0.020        0.018     0.723    0.470    -0.025    0.054 
        JobRoleLaboratory Technician     0.018         0.035        0.010     0.509    0.611    -0.050    0.085 
              JobRoleHuman Resources     0.049         0.050        0.013     0.983    0.326    -0.049    0.148 
           JobRoleResearch Scientist     0.068         0.034        0.041     1.990    0.047     0.001    0.135 
              JobRoleSales Executive     0.263         0.042        0.167     6.222    0.000     0.180    0.346 
    JobRoleHealthcare Representative     0.272         0.046        0.116     5.859    0.000     0.181    0.363 
       JobRoleManufacturing Director     0.280         0.046        0.127     6.126    0.000     0.190    0.370 
            JobRoleResearch Director     0.558         0.057        0.198     9.818    0.000     0.446    0.669 
                      JobRoleManager     0.533         0.061        0.189     8.685    0.000     0.412    0.653 
         BusinessTravelTravel_Rarely     0.072         0.024        0.049     3.034    0.002     0.025    0.118 
     BusinessTravelTravel_Frequently     0.049         0.028        0.029     1.775    0.076    -0.005    0.104 
                        logDailyRate     0.022         0.011        0.021     1.947    0.052     0.000    0.044 
                   TotalWorkingYears     0.025         0.005        0.284     4.657    0.000     0.014    0.036 
                  TotalWorkingYears2    -0.001         0.000       -0.210    -4.379    0.000    -0.001    0.000 
                          Education2    -0.006         0.027       -0.004    -0.223    0.824    -0.058    0.046 
                          Education3    -0.026         0.025       -0.019    -1.072    0.284    -0.075    0.022 
                          Education4     0.016         0.026        0.011     0.601    0.548    -0.035    0.066 
                          Education5    -0.069         0.048       -0.018    -1.453    0.147    -0.163    0.024 
    ------------------------------------------------------------------------------------------------------------

    back7


                                      Stepwise Summary                                  
    ----------------------------------------------------------------------------------
    Step    Variable                  AIC         SBC       SBIC      R2       Adj. R2 
    ----------------------------------------------------------------------------------
     0      Full Model              -211.392     -63.569      NA    0.90217    0.89880 
     1      ID2                     -213.386     -70.331      NA    0.90217    0.89892 
     2      Department              -216.911     -83.393      NA    0.90212    0.89910 
     3      logTotalWorkingYears    -218.365     -89.616      NA    0.90206    0.89916 
     4      DailyRate2              -219.778     -95.798      NA    0.90199    0.89921 
     5      DailyRate               -220.496    -101.284      NA    0.90185    0.89918 
     6      ID                      -221.191    -106.747      NA    0.90170    0.89915 
     7      logID                   -222.391    -112.716      NA    0.90161    0.89917 
    ----------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                             Model Summary                           
    ----------------------------------------------------------------
    R                       0.950       RMSE                  0.207 
    R-Squared               0.902       MSE                   0.044 
    Adj. R-Squared          0.899       Coef. Var             2.460 
    Pred R-Squared          0.897       AIC                -222.391 
    MAE                     0.154       SBC                -112.716 
    ----------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                    ANOVA                                 
    ---------------------------------------------------------------------
                   Sum of                                                
                  Squares         DF    Mean Square       F         Sig. 
    ---------------------------------------------------------------------
    Regression    342.875         21         16.327    370.037    0.0000 
    Residual       37.417        848          0.044                      
    Total         380.291        869                                     
    ---------------------------------------------------------------------

                                                Parameter Estimates                                              
    ------------------------------------------------------------------------------------------------------------
                               model      Beta    Std. Error    Std. Beta      t        Sig      lower    upper 
    ------------------------------------------------------------------------------------------------------------
                         (Intercept)     7.518         0.084                 89.378    0.000     7.353    7.683 
         BusinessTravelTravel_Rarely     0.071         0.024        0.048     2.992    0.003     0.024    0.117 
     BusinessTravelTravel_Frequently     0.049         0.028        0.028     1.748    0.081    -0.006    0.103 
                          Education2    -0.006         0.027       -0.004    -0.217    0.828    -0.058    0.047 
                          Education3    -0.026         0.025       -0.019    -1.062    0.288    -0.074    0.022 
                          Education4     0.016         0.026        0.011     0.614    0.539    -0.035    0.067 
                          Education5    -0.069         0.048       -0.018    -1.444    0.149    -0.163    0.025 
                           JobLevel2     0.451         0.030        0.327    14.965    0.000     0.392    0.511 
                           JobLevel3     0.901         0.040        0.489    22.628    0.000     0.823    0.980 
                           JobLevel4     1.191         0.060        0.457    19.890    0.000     1.074    1.309 
                           JobLevel5     1.314         0.070        0.401    18.710    0.000     1.176    1.451 
        JobRoleLaboratory Technician     0.018         0.035        0.011     0.528    0.597    -0.050    0.086 
              JobRoleHuman Resources     0.049         0.050        0.013     0.986    0.324    -0.049    0.148 
           JobRoleResearch Scientist     0.068         0.034        0.041     1.999    0.046     0.001    0.135 
              JobRoleSales Executive     0.264         0.042        0.168     6.254    0.000     0.181    0.347 
    JobRoleHealthcare Representative     0.273         0.046        0.116     5.883    0.000     0.182    0.364 
       JobRoleManufacturing Director     0.281         0.046        0.128     6.157    0.000     0.191    0.371 
            JobRoleResearch Director     0.558         0.057        0.198     9.833    0.000     0.447    0.670 
                      JobRoleManager     0.533         0.061        0.190     8.697    0.000     0.413    0.654 
                   TotalWorkingYears     0.028         0.004        0.316     7.543    0.000     0.021    0.035 
                        logDailyRate     0.022         0.011        0.021     1.940    0.053     0.000    0.044 
                  TotalWorkingYears2    -0.001         0.000       -0.227    -5.501    0.000    -0.001    0.000 
    ------------------------------------------------------------------------------------------------------------

    step7


                                        Stepwise Summary                                    
    --------------------------------------------------------------------------------------
    Step    Variable                      AIC         SBC       SBIC      R2       Adj. R2 
    --------------------------------------------------------------------------------------
     0      Base Model                  1752.980    1762.517      NA    0.00000    0.00000 
     1      JobLevel (+)                 -47.889     -19.278      NA    0.87496    0.87439 
     2      logTotalWorkingYears (+)     -79.546     -46.166      NA    0.87971    0.87901 
     3      JobRole (+)                 -190.641    -119.114      NA    0.89606    0.89448 
     4      BusinessTravel (+)          -199.676    -118.612      NA    0.89760    0.89581 
     5      logDailyRate (+)            -202.689    -116.856      NA    0.89819    0.89628 
     6      TotalWorkingYears (+)       -203.815    -113.214      NA    0.89856    0.89653 
     7      TotalWorkingYears2 (+)      -221.167    -125.797      NA    0.90079    0.89869 
     8      logTotalWorkingYears (-)    -222.639    -132.037      NA    0.90073    0.89875 
     9      Education (+)               -222.391    -112.716      NA    0.90161    0.89917 
    --------------------------------------------------------------------------------------

    Final Model Output 
    ------------------

                             Model Summary                           
    ----------------------------------------------------------------
    R                       0.950       RMSE                  0.207 
    R-Squared               0.902       MSE                   0.044 
    Adj. R-Squared          0.899       Coef. Var             2.460 
    Pred R-Squared          0.897       AIC                -222.391 
    MAE                     0.154       SBC                -112.716 
    ----------------------------------------------------------------
     RMSE: Root Mean Square Error 
     MSE: Mean Square Error 
     MAE: Mean Absolute Error 
     AIC: Akaike Information Criteria 
     SBC: Schwarz Bayesian Criteria 

                                    ANOVA                                 
    ---------------------------------------------------------------------
                   Sum of                                                
                  Squares         DF    Mean Square       F         Sig. 
    ---------------------------------------------------------------------
    Regression    342.875         21         16.327    370.037    0.0000 
    Residual       37.417        848          0.044                      
    Total         380.291        869                                     
    ---------------------------------------------------------------------

                                                Parameter Estimates                                              
    ------------------------------------------------------------------------------------------------------------
                               model      Beta    Std. Error    Std. Beta      t        Sig      lower    upper 
    ------------------------------------------------------------------------------------------------------------
                         (Intercept)     7.518         0.084                 89.378    0.000     7.353    7.683 
                           JobLevel2     0.451         0.030        0.327    14.965    0.000     0.392    0.511 
                           JobLevel3     0.901         0.040        0.489    22.628    0.000     0.823    0.980 
                           JobLevel4     1.191         0.060        0.457    19.890    0.000     1.074    1.309 
                           JobLevel5     1.314         0.070        0.401    18.710    0.000     1.176    1.451 
        JobRoleLaboratory Technician     0.018         0.035        0.011     0.528    0.597    -0.050    0.086 
              JobRoleHuman Resources     0.049         0.050        0.013     0.986    0.324    -0.049    0.148 
           JobRoleResearch Scientist     0.068         0.034        0.041     1.999    0.046     0.001    0.135 
              JobRoleSales Executive     0.264         0.042        0.168     6.254    0.000     0.181    0.347 
    JobRoleHealthcare Representative     0.273         0.046        0.116     5.883    0.000     0.182    0.364 
       JobRoleManufacturing Director     0.281         0.046        0.128     6.157    0.000     0.191    0.371 
            JobRoleResearch Director     0.558         0.057        0.198     9.833    0.000     0.447    0.670 
                      JobRoleManager     0.533         0.061        0.190     8.697    0.000     0.413    0.654 
         BusinessTravelTravel_Rarely     0.071         0.024        0.048     2.992    0.003     0.024    0.117 
     BusinessTravelTravel_Frequently     0.049         0.028        0.028     1.748    0.081    -0.006    0.103 
                        logDailyRate     0.022         0.011        0.021     1.940    0.053     0.000    0.044 
                   TotalWorkingYears     0.028         0.004        0.316     7.543    0.000     0.021    0.035 
                  TotalWorkingYears2    -0.001         0.000       -0.227    -5.501    0.000    -0.001    0.000 
                          Education2    -0.006         0.027       -0.004    -0.217    0.828    -0.058    0.047 
                          Education3    -0.026         0.025       -0.019    -1.062    0.288    -0.074    0.022 
                          Education4     0.016         0.026        0.011     0.614    0.539    -0.035    0.067 
                          Education5    -0.069         0.048       -0.018    -1.444    0.149    -0.163    0.025 
    ------------------------------------------------------------------------------------------------------------

    # Candidate models with transformed variables
    modelTrans1 = lm(MonthlyIncome ~ BusinessTravel + Education + JobLevel + JobRole + TotalWorkingYears + logID + logDailyRate + TotalWorkingYears2, data = cs2_regresSmall2) 

    modelTrans2 = lm(logMonthlyIncome ~ JobLevel + JobRole + BusinessTravel + logDailyRate + TotalWorkingYears + TotalWorkingYears2 + Education, data = cs2_regresSmall2)

    modelTrans1CV = train(MonthlyIncome ~ BusinessTravel + Education + JobLevel + JobRole + TotalWorkingYears + logID + logDailyRate + TotalWorkingYears2, data = cs2_regresSmall2, trControl = train_control, method = "lm")
    modelTrans1CV

    Linear Regression 

    870 samples
      8 predictor

    No pre-processing
    Resampling: Leave-One-Out Cross-Validation 
    Summary of sample sizes: 869, 869, 869, 869, 869, 869, ... 
    Resampling results:

      RMSE      Rsquared   MAE   
      1001.033  0.9525427  766.95

    Tuning parameter 'intercept' was held constant at a value of TRUE

    # All candidate models
    summary(model8)


    Call:
    lm(formula = MonthlyIncome ~ ID + BusinessTravel + DailyRate + 
        Department + Education + JobLevel + JobRole + TotalWorkingYears, 
        data = cs2_regresSmall)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -3175.4  -617.2   -56.0   588.6  4195.1 

    Coefficients:
                                       Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                       2.406e+03  4.803e+02   5.011 6.61e-07 ***
    ID                               -2.276e-01  1.365e-01  -1.668  0.09569 .  
    BusinessTravelTravel_Rarely       3.445e+02  1.114e+02   3.091  0.00206 ** 
    BusinessTravelTravel_Frequently   2.052e+02  1.316e+02   1.560  0.11916    
    DailyRate                         1.785e-01  8.502e-02   2.100  0.03604 *  
    DepartmentResearch & Development  2.488e+02  4.172e+02   0.596  0.55113    
    DepartmentSales                  -3.379e+02  4.267e+02  -0.792  0.42866    
    Education2                       -3.772e+01  1.263e+02  -0.299  0.76521    
    Education3                       -8.976e+01  1.164e+02  -0.771  0.44084    
    Education4                        5.840e+01  1.227e+02   0.476  0.63433    
    Education5                       -4.576e+02  2.262e+02  -2.023  0.04340 *  
    JobLevel2                         1.718e+03  1.394e+02  12.331  < 2e-16 ***
    JobLevel3                         4.970e+03  1.862e+02  26.692  < 2e-16 ***
    JobLevel4                         8.346e+03  2.823e+02  29.561  < 2e-16 ***
    JobLevel5                         1.096e+04  3.324e+02  32.963  < 2e-16 ***
    JobRoleLaboratory Technician     -5.538e+02  3.474e+02  -1.594  0.11125    
    JobRoleHuman Resources           -1.600e+02  4.884e+02  -0.328  0.74324    
    JobRoleResearch Scientist        -3.554e+02  3.469e+02  -1.025  0.30585    
    JobRoleSales Executive            1.223e+03  1.997e+02   6.124 1.39e-09 ***
    JobRoleHealthcare Representative  6.541e+02  3.776e+02   1.732  0.08362 .  
    JobRoleManufacturing Director     7.663e+02  3.757e+02   2.040  0.04166 *  
    JobRoleResearch Director          4.193e+03  4.072e+02  10.298  < 2e-16 ***
    JobRoleManager                    4.284e+03  3.361e+02  12.745  < 2e-16 ***
    TotalWorkingYears                 4.420e+01  7.766e+00   5.692 1.74e-08 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 996.7 on 846 degrees of freedom
    Multiple R-squared:  0.9542,    Adjusted R-squared:  0.953 
    F-statistic: 767.2 on 23 and 846 DF,  p-value: < 2.2e-16

    summary(model7a)


    Call:
    lm(formula = MonthlyIncome ~ ID + BusinessTravel + DailyRate + 
        Department + JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -3213.3  -608.7   -65.9   615.7  4188.0 

    Coefficients:
                                       Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                       2.401e+03  4.717e+02   5.089 4.42e-07 ***
    ID                               -2.248e-01  1.365e-01  -1.647  0.09985 .  
    BusinessTravelTravel_Rarely       3.397e+02  1.116e+02   3.044  0.00241 ** 
    BusinessTravelTravel_Frequently   1.872e+02  1.316e+02   1.423  0.15524    
    DailyRate                         1.764e-01  8.512e-02   2.072  0.03855 *  
    DepartmentResearch & Development  2.335e+02  4.179e+02   0.559  0.57638    
    DepartmentSales                  -3.572e+02  4.272e+02  -0.836  0.40337    
    JobLevel2                         1.706e+03  1.384e+02  12.329  < 2e-16 ***
    JobLevel3                         4.934e+03  1.860e+02  26.532  < 2e-16 ***
    JobLevel4                         8.255e+03  2.807e+02  29.407  < 2e-16 ***
    JobLevel5                         1.091e+04  3.325e+02  32.812  < 2e-16 ***
    JobRoleLaboratory Technician     -5.718e+02  3.480e+02  -1.643  0.10078    
    JobRoleHuman Resources           -2.049e+02  4.889e+02  -0.419  0.67531    
    JobRoleResearch Scientist        -3.816e+02  3.472e+02  -1.099  0.27198    
    JobRoleSales Executive            1.225e+03  1.996e+02   6.141 1.26e-09 ***
    JobRoleHealthcare Representative  6.528e+02  3.784e+02   1.725  0.08483 .  
    JobRoleManufacturing Director     7.603e+02  3.761e+02   2.021  0.04357 *  
    JobRoleResearch Director          4.158e+03  4.073e+02  10.209  < 2e-16 ***
    JobRoleManager                    4.296e+03  3.366e+02  12.762  < 2e-16 ***
    TotalWorkingYears                 4.595e+01  7.726e+00   5.948 3.97e-09 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 998.8 on 850 degrees of freedom
    Multiple R-squared:  0.9538,    Adjusted R-squared:  0.9528 
    F-statistic: 924.5 on 19 and 850 DF,  p-value: < 2.2e-16

    summary(model6)


    Call:
    lm(formula = MonthlyIncome ~ ID + BusinessTravel + DailyRate + 
        JobLevel + JobRole + TotalWorkingYears, data = cs2_regresSmall)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -3212.2  -618.9   -74.1   620.3  4184.9 

    Coefficients:
                                       Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                       2.048e+03  1.989e+02  10.296  < 2e-16 ***
    ID                               -2.130e-01  1.364e-01  -1.561  0.11888    
    BusinessTravelTravel_Rarely       3.320e+02  1.116e+02   2.975  0.00302 ** 
    BusinessTravelTravel_Frequently   1.873e+02  1.317e+02   1.422  0.15549    
    DailyRate                         1.741e-01  8.519e-02   2.044  0.04129 *  
    JobLevel2                         1.709e+03  1.385e+02  12.342  < 2e-16 ***
    JobLevel3                         4.941e+03  1.861e+02  26.556  < 2e-16 ***
    JobLevel4                         8.261e+03  2.808e+02  29.418  < 2e-16 ***
    JobLevel5                         1.094e+04  3.308e+02  33.066  < 2e-16 ***
    JobRoleLaboratory Technician      2.089e+01  1.627e+02   0.128  0.89786    
    JobRoleHuman Resources            1.531e+02  2.382e+02   0.643  0.52045    
    JobRoleResearch Scientist         2.120e+02  1.598e+02   1.327  0.18496    
    JobRoleSales Executive            1.227e+03  1.998e+02   6.141 1.26e-09 ***
    JobRoleHealthcare Representative  1.246e+03  2.201e+02   5.660 2.07e-08 ***
    JobRoleManufacturing Director     1.353e+03  2.163e+02   6.257 6.22e-10 ***
    JobRoleResearch Director          4.748e+03  2.677e+02  17.736  < 2e-16 ***
    JobRoleManager                    4.618e+03  2.902e+02  15.912  < 2e-16 ***
    TotalWorkingYears                 4.528e+01  7.716e+00   5.869 6.29e-09 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 999.8 on 852 degrees of freedom
    Multiple R-squared:  0.9536,    Adjusted R-squared:  0.9527 
    F-statistic:  1031 on 17 and 852 DF,  p-value: < 2.2e-16

    summary(model8intFull)


    Call:
    lm(formula = MonthlyIncome ~ ID + BusinessTravel + DailyRate + 
        Department + Education + JobLevel + JobRole + TotalWorkingYears + 
        ID:TotalWorkingYears + ID:DailyRate + ID:JobLevel + BusinessTravel:Education + 
        BusinessTravel:JobLevel + BusinessTravel:TotalWorkingYears + 
        Education:TotalWorkingYears + JobLevel:JobRole + JobLevel:TotalWorkingYears + 
        JobRole:TotalWorkingYears, data = cs2_regresSmall)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -3101.8  -545.3   -69.4   500.6  4137.5 

    Coefficients: (19 not defined because of singularities)
                                                         Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                                         2.139e+03  6.634e+02   3.224 0.001316 ** 
    ID                                                  3.161e-02  3.937e-01   0.080 0.936023    
    BusinessTravelTravel_Rarely                         2.830e+02  4.033e+02   0.702 0.483032    
    BusinessTravelTravel_Frequently                     6.602e+02  4.626e+02   1.427 0.153902    
    DailyRate                                          -2.970e-02  1.772e-01  -0.168 0.866936    
    DepartmentResearch & Development                    2.705e+01  4.763e+02   0.057 0.954719    
    DepartmentSales                                    -4.347e+02  4.774e+02  -0.911 0.362756    
    Education2                                          1.827e+02  4.454e+02   0.410 0.681845    
    Education3                                         -2.417e+02  4.243e+02  -0.570 0.568960    
    Education4                                          5.969e+02  4.519e+02   1.321 0.186942    
    Education5                                          3.903e+02  8.784e+02   0.444 0.656962    
    JobLevel2                                           2.564e+03  8.275e+02   3.098 0.002016 ** 
    JobLevel3                                           5.099e+03  1.424e+03   3.582 0.000362 ***
    JobLevel4                                           1.038e+04  1.709e+03   6.072 1.95e-09 ***
    JobLevel5                                           1.118e+04  2.216e+03   5.045 5.63e-07 ***
    JobRoleLaboratory Technician                       -3.130e+02  4.142e+02  -0.756 0.450115    
    JobRoleHuman Resources                             -1.182e+02  6.736e+02  -0.175 0.860749    
    JobRoleResearch Scientist                          -2.104e+02  4.131e+02  -0.509 0.610668    
    JobRoleSales Executive                             -2.093e+02  1.110e+03  -0.189 0.850435    
    JobRoleHealthcare Representative                   -1.073e+03  1.242e+03  -0.864 0.388034    
    JobRoleManufacturing Director                      -5.185e+02  9.032e+02  -0.574 0.566036    
    JobRoleResearch Director                            2.363e+03  1.556e+03   1.519 0.129239    
    JobRoleManager                                      2.717e+03  1.761e+03   1.543 0.123135    
    TotalWorkingYears                                   1.367e+02  4.680e+01   2.921 0.003583 ** 
    ID:TotalWorkingYears                               -3.850e-02  3.150e-02  -1.222 0.221989    
    ID:DailyRate                                        4.695e-04  3.459e-04   1.358 0.174975    
    ID:JobLevel2                                       -5.882e-01  3.487e-01  -1.687 0.092013 .  
    ID:JobLevel3                                       -1.657e-01  4.873e-01  -0.340 0.733866    
    ID:JobLevel4                                        3.925e-01  8.956e-01   0.438 0.661324    
    ID:JobLevel5                                        5.722e-01  9.704e-01   0.590 0.555583    
    BusinessTravelTravel_Rarely:Education2             -7.849e+01  4.371e+02  -0.180 0.857533    
    BusinessTravelTravel_Frequently:Education2         -6.715e+02  4.997e+02  -1.344 0.179362    
    BusinessTravelTravel_Rarely:Education3              4.364e+02  4.122e+02   1.059 0.290129    
    BusinessTravelTravel_Frequently:Education3          1.936e+02  4.670e+02   0.415 0.678536    
    BusinessTravelTravel_Rarely:Education4             -2.965e+02  4.383e+02  -0.676 0.498968    
    BusinessTravelTravel_Frequently:Education4         -4.691e+02  5.008e+02  -0.937 0.349224    
    BusinessTravelTravel_Rarely:Education5             -3.475e+01  8.552e+02  -0.041 0.967597    
    BusinessTravelTravel_Frequently:Education5         -5.343e+02  9.689e+02  -0.551 0.581488    
    BusinessTravelTravel_Rarely:JobLevel2               7.949e+02  3.295e+02   2.412 0.016077 *  
    BusinessTravelTravel_Frequently:JobLevel2           6.952e+02  3.752e+02   1.853 0.064255 .  
    BusinessTravelTravel_Rarely:JobLevel3               5.681e+02  4.325e+02   1.314 0.189339    
    BusinessTravelTravel_Frequently:JobLevel3           9.998e+02  5.055e+02   1.978 0.048299 *  
    BusinessTravelTravel_Rarely:JobLevel4               5.333e+02  9.416e+02   0.566 0.571294    
    BusinessTravelTravel_Frequently:JobLevel4           1.101e+03  1.016e+03   1.084 0.278712    
    BusinessTravelTravel_Rarely:JobLevel5               2.195e+03  1.331e+03   1.649 0.099548 .  
    BusinessTravelTravel_Frequently:JobLevel5           1.649e+03  1.462e+03   1.128 0.259662    
    BusinessTravelTravel_Rarely:TotalWorkingYears      -5.009e+01  2.854e+01  -1.755 0.079682 .  
    BusinessTravelTravel_Frequently:TotalWorkingYears  -7.560e+01  3.177e+01  -2.379 0.017577 *  
    Education2:TotalWorkingYears                        5.725e+00  1.897e+01   0.302 0.762899    
    Education3:TotalWorkingYears                       -1.330e+01  1.563e+01  -0.851 0.395157    
    Education4:TotalWorkingYears                       -1.798e+01  1.646e+01  -1.092 0.275036    
    Education5:TotalWorkingYears                       -5.308e+01  3.402e+01  -1.560 0.119110    
    JobLevel2:JobRoleLaboratory Technician             -1.898e+02  6.717e+02  -0.283 0.777597    
    JobLevel3:JobRoleLaboratory Technician             -2.694e+03  1.487e+03  -1.812 0.070315 .  
    JobLevel4:JobRoleLaboratory Technician                     NA         NA      NA       NA    
    JobLevel5:JobRoleLaboratory Technician                     NA         NA      NA       NA    
    JobLevel2:JobRoleHuman Resources                    4.278e+02  1.026e+03   0.417 0.676821    
    JobLevel3:JobRoleHuman Resources                    5.298e+02  1.441e+03   0.368 0.713208    
    JobLevel4:JobRoleHuman Resources                           NA         NA      NA       NA    
    JobLevel5:JobRoleHuman Resources                           NA         NA      NA       NA    
    JobLevel2:JobRoleResearch Scientist                 4.862e+02  6.737e+02   0.722 0.470727    
    JobLevel3:JobRoleResearch Scientist                 1.429e+03  1.667e+03   0.857 0.391698    
    JobLevel4:JobRoleResearch Scientist                        NA         NA      NA       NA    
    JobLevel5:JobRoleResearch Scientist                        NA         NA      NA       NA    
    JobLevel2:JobRoleSales Executive                    1.678e+02  7.054e+02   0.238 0.812043    
    JobLevel3:JobRoleSales Executive                   -1.434e+02  9.882e+02  -0.145 0.884658    
    JobLevel4:JobRoleSales Executive                           NA         NA      NA       NA    
    JobLevel5:JobRoleSales Executive                           NA         NA      NA       NA    
    JobLevel2:JobRoleHealthcare Representative          7.217e+02  7.786e+02   0.927 0.354259    
    JobLevel3:JobRoleHealthcare Representative          1.955e+02  1.071e+03   0.183 0.855132    
    JobLevel4:JobRoleHealthcare Representative                 NA         NA      NA       NA    
    JobLevel5:JobRoleHealthcare Representative                 NA         NA      NA       NA    
    JobLevel2:JobRoleManufacturing Director                    NA         NA      NA       NA    
    JobLevel3:JobRoleManufacturing Director            -2.668e+02  1.013e+03  -0.263 0.792260    
    JobLevel4:JobRoleManufacturing Director                    NA         NA      NA       NA    
    JobLevel5:JobRoleManufacturing Director                    NA         NA      NA       NA    
    JobLevel2:JobRoleResearch Director                         NA         NA      NA       NA    
    JobLevel3:JobRoleResearch Director                  2.951e+02  8.921e+02   0.331 0.740922    
    JobLevel4:JobRoleResearch Director                  3.010e+02  4.850e+02   0.621 0.534996    
    JobLevel5:JobRoleResearch Director                         NA         NA      NA       NA    
    JobLevel2:JobRoleManager                                   NA         NA      NA       NA    
    JobLevel3:JobRoleManager                                   NA         NA      NA       NA    
    JobLevel4:JobRoleManager                                   NA         NA      NA       NA    
    JobLevel5:JobRoleManager                                   NA         NA      NA       NA    
    JobLevel2:TotalWorkingYears                        -1.124e+02  3.648e+01  -3.080 0.002138 ** 
    JobLevel3:TotalWorkingYears                        -3.676e+01  4.334e+01  -0.848 0.396601    
    JobLevel4:TotalWorkingYears                        -1.546e+02  5.625e+01  -2.748 0.006128 ** 
    JobLevel5:TotalWorkingYears                        -1.386e+02  7.068e+01  -1.962 0.050169 .  
    JobRoleLaboratory Technician:TotalWorkingYears     -4.057e+00  3.815e+01  -0.106 0.915332    
    JobRoleHuman Resources:TotalWorkingYears           -5.320e+01  7.699e+01  -0.691 0.489781    
    JobRoleResearch Scientist:TotalWorkingYears        -1.741e+01  3.756e+01  -0.464 0.643044    
    JobRoleSales Executive:TotalWorkingYears            9.611e+01  5.100e+01   1.885 0.059857 .  
    JobRoleHealthcare Representative:TotalWorkingYears  8.930e+01  5.393e+01   1.656 0.098181 .  
    JobRoleManufacturing Director:TotalWorkingYears     1.026e+02  5.388e+01   1.905 0.057202 .  
    JobRoleResearch Director:TotalWorkingYears          9.983e+01  6.689e+01   1.493 0.135949    
    JobRoleManager:TotalWorkingYears                    9.369e+01  7.569e+01   1.238 0.216191    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 968.3 on 793 degrees of freedom
    Multiple R-squared:  0.9595,    Adjusted R-squared:  0.9556 
    F-statistic: 247.3 on 76 and 793 DF,  p-value: < 2.2e-16

    summary(model8int1)


    Call:
    lm(formula = MonthlyIncome ~ ID + BusinessTravel + JobLevel + 
        JobRole + TotalWorkingYears + DailyRate + Education + ID:DailyRate + 
        BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + 
        JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2981.2  -590.4   -81.3   541.8  4225.4 

    Coefficients:
                                                         Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                                         1.833e+03  3.151e+02   5.818 8.52e-09 ***
    ID                                                 -5.671e-01  3.030e-01  -1.871 0.061634 .  
    BusinessTravelTravel_Rarely                         5.268e+02  1.894e+02   2.781 0.005543 ** 
    BusinessTravelTravel_Frequently                     5.260e+02  2.252e+02   2.336 0.019744 *  
    JobLevel2                                           3.436e+03  3.782e+02   9.085  < 2e-16 ***
    JobLevel3                                           5.898e+03  4.276e+02  13.792  < 2e-16 ***
    JobLevel4                                           1.144e+04  8.086e+02  14.152  < 2e-16 ***
    JobLevel5                                           1.356e+04  1.116e+03  12.148  < 2e-16 ***
    JobRoleLaboratory Technician                        2.624e+02  2.432e+02   1.079 0.280914    
    JobRoleHuman Resources                              2.915e+02  4.581e+02   0.636 0.524744    
    JobRoleResearch Scientist                           9.341e+01  2.407e+02   0.388 0.698104    
    JobRoleSales Executive                             -6.153e+02  4.406e+02  -1.397 0.162896    
    JobRoleHealthcare Representative                   -4.064e+02  4.841e+02  -0.840 0.401412    
    JobRoleManufacturing Director                      -5.997e+02  4.712e+02  -1.273 0.203496    
    JobRoleResearch Director                            2.880e+03  7.016e+02   4.104 4.46e-05 ***
    JobRoleManager                                      2.754e+03  8.249e+02   3.339 0.000878 ***
    TotalWorkingYears                                   8.404e+01  3.382e+01   2.485 0.013149 *  
    DailyRate                                          -7.276e-03  1.732e-01  -0.042 0.966497    
    Education2                                          6.307e+01  2.122e+02   0.297 0.766395    
    Education3                                          1.140e+02  1.861e+02   0.613 0.540097    
    Education4                                          2.955e+02  2.040e+02   1.448 0.147869    
    Education5                                          1.648e+02  4.596e+02   0.359 0.719968    
    ID:DailyRate                                        4.527e-04  3.377e-04   1.341 0.180370    
    BusinessTravelTravel_Rarely:TotalWorkingYears      -2.251e+01  1.557e+01  -1.446 0.148664    
    BusinessTravelTravel_Frequently:TotalWorkingYears  -3.394e+01  1.787e+01  -1.899 0.057888 .  
    TotalWorkingYears:Education2                       -3.694e+00  1.839e+01  -0.201 0.840811    
    TotalWorkingYears:Education3                       -1.509e+01  1.509e+01  -1.000 0.317738    
    TotalWorkingYears:Education4                       -2.067e+01  1.590e+01  -1.299 0.194151    
    TotalWorkingYears:Education5                       -5.160e+01  3.057e+01  -1.688 0.091837 .  
    JobLevel2:TotalWorkingYears                        -1.339e+02  3.078e+01  -4.352 1.52e-05 ***
    JobLevel3:TotalWorkingYears                        -7.634e+01  3.205e+01  -2.382 0.017444 *  
    JobLevel4:TotalWorkingYears                        -1.903e+02  4.267e+01  -4.459 9.35e-06 ***
    JobLevel5:TotalWorkingYears                        -1.692e+02  5.216e+01  -3.243 0.001228 ** 
    JobRoleLaboratory Technician:TotalWorkingYears     -3.229e+01  3.397e+01  -0.950 0.342215    
    JobRoleHuman Resources:TotalWorkingYears           -2.865e+01  6.753e+01  -0.424 0.671455    
    JobRoleResearch Scientist:TotalWorkingYears         1.825e+01  3.368e+01   0.542 0.588022    
    JobRoleSales Executive:TotalWorkingYears            1.420e+02  4.289e+01   3.311 0.000970 ***
    JobRoleHealthcare Representative:TotalWorkingYears  1.239e+02  4.446e+01   2.786 0.005453 ** 
    JobRoleManufacturing Director:TotalWorkingYears     1.503e+02  4.434e+01   3.390 0.000733 ***
    JobRoleResearch Director:TotalWorkingYears          1.403e+02  5.032e+01   2.788 0.005417 ** 
    JobRoleManager:TotalWorkingYears                    1.384e+02  5.218e+01   2.653 0.008128 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 974.3 on 829 degrees of freedom
    Multiple R-squared:  0.9572,    Adjusted R-squared:  0.9551 
    F-statistic:   463 on 40 and 829 DF,  p-value: < 2.2e-16

    summary(model8int2)


    Call:
    lm(formula = MonthlyIncome ~ ID + BusinessTravel + JobLevel + 
        JobRole + TotalWorkingYears + ID:DailyRate + BusinessTravel:TotalWorkingYears + 
        Education:TotalWorkingYears + JobLevel:TotalWorkingYears + 
        JobRole:TotalWorkingYears, data = cs2_regresSmall)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2948.3  -588.1   -75.3   553.5  4209.4 

    Coefficients:
                                                         Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                                         1.911e+03  2.556e+02   7.477 1.92e-13 ***
    ID                                                 -5.649e-01  1.855e-01  -3.044 0.002405 ** 
    BusinessTravelTravel_Rarely                         5.387e+02  1.890e+02   2.851 0.004470 ** 
    BusinessTravelTravel_Frequently                     5.318e+02  2.238e+02   2.377 0.017690 *  
    JobLevel2                                           3.461e+03  3.749e+02   9.234  < 2e-16 ***
    JobLevel3                                           5.929e+03  4.256e+02  13.931  < 2e-16 ***
    JobLevel4                                           1.145e+04  7.946e+02  14.409  < 2e-16 ***
    JobLevel5                                           1.369e+04  1.107e+03  12.359  < 2e-16 ***
    JobRoleLaboratory Technician                        2.817e+02  2.421e+02   1.164 0.244815    
    JobRoleHuman Resources                              2.982e+02  4.536e+02   0.657 0.511118    
    JobRoleResearch Scientist                           1.264e+02  2.380e+02   0.531 0.595417    
    JobRoleSales Executive                             -5.702e+02  4.376e+02  -1.303 0.192954    
    JobRoleHealthcare Representative                   -3.692e+02  4.824e+02  -0.765 0.444283    
    JobRoleManufacturing Director                      -5.720e+02  4.687e+02  -1.221 0.222616    
    JobRoleResearch Director                            2.925e+03  6.993e+02   4.183 3.19e-05 ***
    JobRoleManager                                      2.883e+03  8.177e+02   3.526 0.000446 ***
    TotalWorkingYears                                   8.175e+01  3.296e+01   2.480 0.013327 *  
    ID:DailyRate                                        4.340e-04  1.638e-04   2.649 0.008231 ** 
    BusinessTravelTravel_Rarely:TotalWorkingYears      -2.416e+01  1.550e+01  -1.558 0.119520    
    BusinessTravelTravel_Frequently:TotalWorkingYears  -3.518e+01  1.766e+01  -1.992 0.046661 *  
    TotalWorkingYears:Education2                       -9.450e-01  1.075e+01  -0.088 0.929977    
    TotalWorkingYears:Education3                       -8.047e+00  9.337e+00  -0.862 0.389043    
    TotalWorkingYears:Education4                       -2.523e+00  9.517e+00  -0.265 0.790996    
    TotalWorkingYears:Education5                       -4.183e+01  1.528e+01  -2.737 0.006339 ** 
    JobLevel2:TotalWorkingYears                        -1.359e+02  3.053e+01  -4.450 9.75e-06 ***
    JobLevel3:TotalWorkingYears                        -7.870e+01  3.189e+01  -2.468 0.013780 *  
    JobLevel4:TotalWorkingYears                        -1.907e+02  4.222e+01  -4.518 7.15e-06 ***
    JobLevel5:TotalWorkingYears                        -1.745e+02  5.189e+01  -3.364 0.000804 ***
    JobRoleLaboratory Technician:TotalWorkingYears     -3.385e+01  3.387e+01  -0.999 0.317881    
    JobRoleHuman Resources:TotalWorkingYears           -2.960e+01  6.699e+01  -0.442 0.658655    
    JobRoleResearch Scientist:TotalWorkingYears         1.531e+01  3.353e+01   0.457 0.648062    
    JobRoleSales Executive:TotalWorkingYears            1.382e+02  4.270e+01   3.236 0.001260 ** 
    JobRoleHealthcare Representative:TotalWorkingYears  1.203e+02  4.427e+01   2.717 0.006726 ** 
    JobRoleManufacturing Director:TotalWorkingYears     1.467e+02  4.416e+01   3.322 0.000933 ***
    JobRoleResearch Director:TotalWorkingYears          1.358e+02  5.013e+01   2.709 0.006881 ** 
    JobRoleManager:TotalWorkingYears                    1.303e+02  5.179e+01   2.516 0.012048 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 972.9 on 834 degrees of freedom
    Multiple R-squared:  0.957, Adjusted R-squared:  0.9552 
    F-statistic: 530.6 on 35 and 834 DF,  p-value: < 2.2e-16

    summary(modelTrans1)


    Call:
    lm(formula = MonthlyIncome ~ BusinessTravel + Education + JobLevel + 
        JobRole + TotalWorkingYears + logID + logDailyRate + TotalWorkingYears2, 
        data = cs2_regresSmall2)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -3051.1  -627.8   -51.2   598.7  4295.6 

    Coefficients:
                                       Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                       1704.4179   444.8155   3.832 0.000137 ***
    BusinessTravelTravel_Rarely        303.7757   110.9945   2.737 0.006333 ** 
    BusinessTravelTravel_Frequently    176.7867   130.7591   1.352 0.176735    
    Education2                         -71.0251   125.7500  -0.565 0.572351    
    Education3                         -99.2908   115.6643  -0.858 0.390892    
    Education4                          35.0545   122.0180   0.287 0.773961    
    Education5                        -516.8402   225.1648  -2.295 0.021955 *  
    JobLevel2                         1618.7382   142.1249  11.390  < 2e-16 ***
    JobLevel3                         4864.7427   187.6885  25.919  < 2e-16 ***
    JobLevel4                         8461.0962   282.1123  29.992  < 2e-16 ***
    JobLevel5                        11095.2055   331.0240  33.518  < 2e-16 ***
    JobRoleLaboratory Technician       -24.8392   162.6847  -0.153 0.878684    
    JobRoleHuman Resources             120.1665   236.4764   0.508 0.611477    
    JobRoleResearch Scientist          164.2496   160.9690   1.020 0.307840    
    JobRoleSales Executive            1176.0951   198.9519   5.911 4.91e-09 ***
    JobRoleHealthcare Representative  1209.6435   218.3057   5.541 4.02e-08 ***
    JobRoleManufacturing Director     1299.1118   215.1928   6.037 2.35e-09 ***
    JobRoleResearch Director          4712.7444   267.5194  17.616  < 2e-16 ***
    JobRoleManager                    4527.4683   288.8527  15.674  < 2e-16 ***
    TotalWorkingYears                   93.0054    17.3653   5.356 1.10e-07 ***
    logID                              -80.7336    34.5241  -2.338 0.019595 *  
    logDailyRate                       119.2873    52.5208   2.271 0.023383 *  
    TotalWorkingYears2                  -1.6759     0.5311  -3.156 0.001658 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 989.5 on 847 degrees of freedom
    Multiple R-squared:  0.9549,    Adjusted R-squared:  0.9537 
    F-statistic: 814.4 on 22 and 847 DF,  p-value: < 2.2e-16

    summary(modelTrans2)


    Call:
    lm(formula = logMonthlyIncome ~ JobLevel + JobRole + BusinessTravel + 
        logDailyRate + TotalWorkingYears + TotalWorkingYears2 + Education, 
        data = cs2_regresSmall2)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -0.8594 -0.1120 -0.0001  0.1084  0.5952 

    Coefficients:
                                       Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                       7.5179743  0.0841147  89.378  < 2e-16 ***
    JobLevel2                         0.4513147  0.0301582  14.965  < 2e-16 ***
    JobLevel3                         0.9013450  0.0398339  22.628  < 2e-16 ***
    JobLevel4                         1.1911995  0.0598908  19.890  < 2e-16 ***
    JobLevel5                         1.3135032  0.0702037  18.710  < 2e-16 ***
    JobRoleLaboratory Technician      0.0182445  0.0345243   0.528  0.59732    
    JobRoleHuman Resources            0.0494673  0.0501587   0.986  0.32431    
    JobRoleResearch Scientist         0.0683055  0.0341728   1.999  0.04595 *  
    JobRoleSales Executive            0.2640869  0.0422254   6.254 6.33e-10 ***
    JobRoleHealthcare Representative  0.2726471  0.0463449   5.883 5.80e-09 ***
    JobRoleManufacturing Director     0.2810423  0.0456486   6.157 1.15e-09 ***
    JobRoleResearch Director          0.5582316  0.0567741   9.833  < 2e-16 ***
    JobRoleManager                    0.5333385  0.0613214   8.697  < 2e-16 ***
    BusinessTravelTravel_Rarely       0.0705120  0.0235635   2.992  0.00285 ** 
    BusinessTravelTravel_Frequently   0.0485274  0.0277578   1.748  0.08078 .  
    logDailyRate                      0.0216274  0.0111497   1.940  0.05274 .  
    TotalWorkingYears                 0.0278028  0.0036859   7.543 1.18e-13 ***
    TotalWorkingYears2               -0.0006200  0.0001127  -5.501 4.99e-08 ***
    Education2                       -0.0057960  0.0266956  -0.217  0.82817    
    Education3                       -0.0260772  0.0245503  -1.062  0.28845    
    Education4                        0.0159078  0.0258986   0.614  0.53922    
    Education5                       -0.0689578  0.0477590  -1.444  0.14914    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.2101 on 848 degrees of freedom
    Multiple R-squared:  0.9016,    Adjusted R-squared:  0.8992 
    F-statistic:   370 on 21 and 848 DF,  p-value: < 2.2e-16

    # # AIC of candidate models
    # AIC(model8)
    # AIC(model7a)
    # AIC(model6)
    # AIC(model8intFull)
    # AIC(model8int1)
    # AIC(model8int2)
    # AIC(modelTrans1)
    # AIC(modelTrans2)


    # Evaluation of candidate models and comparison with Version1 model
    # Load necessary packages
    # library(car)
    # library(caret)

    # List of models
    old_model = lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + YearsWithCurrManager + YearsWithCurrManager^2 + DistanceFromHome + JobInvolvement + EnvironmentSatisfaction, data = cs2_regres2)

    # List of models and corresponding datasets
    models <- list(model8, model7a, model6, model8intFull, model8int1, model8int2, modelTrans1, modelTrans2, old_model)
    model_names <- c("model8", "model7a", "model6", "model8intFull", "model8int1", "model8int2", "modelTrans1", "modelTrans2", "old_model")
    datasets <- c("cs2_regres2", "cs2_regres2", "cs2_regres2", "cs2_regres2", "cs2_regres2", "cs2_regres2", "cs2_regresSmall2", "cs2_regresSmall2", "cs2_regres2")

    # Initialize an empty data frame to store results
    results_df <- data.frame(Model = character(),
                             Adj_R_Squared = numeric(),
                             AIC = numeric(),
                             BIC = numeric(),
                             RMSE_Train = numeric(),
                             CVPRESS = numeric(),
                             RMSE_Test = numeric(),
                             stringsAsFactors = FALSE)

    # Function to calculate CVPRESS
    cvpress <- function(model, data) {
      residuals <- residuals(model)
      h <- hatvalues(model)
      cvpress_value <- sum((residuals / (1 - h))^2)
      return(cvpress_value)
    }

    # Function to evaluate a single model
    evaluate_single_model <- function(model, train_data, test_data, response_var) {
      if (!inherits(model, "lm")) {
        stop("The model provided is not a valid linear model.")
      }
      
      # Model summary
      summary_stats <- summary(model)
      adj_r_squared <- summary_stats$adj.r.squared
      
      # AIC and BIC
      aic_value <- AIC(model)
      bic_value <- BIC(model)
      
      # RMSE on training data
      rmse_train <- sqrt(mean(residuals(model)^2))
      
      # CVPRESS
      cvpress_value <- cvpress(model, train_data)
      
      # Prediction accuracy (RMSE) on test data
      predictions <- predict(model, newdata=test_data)
      rmse_test <- sqrt(mean((predictions - test_data[[response_var]])^2))
      
      # Results
      results <- list(
        Adj_R_Squared = adj_r_squared,
        AIC = aic_value,
        BIC = bic_value,
        RMSE_Train = rmse_train,
        CVPRESS = cvpress_value,
        RMSE_Test = rmse_test
      )
      
      return(results)
    }

    # Create training and testing datasets based on cs2_regres2
    set.seed(123)
    trainIndex_cs2_regres2 <- createDataPartition(cs2_regres2$MonthlyIncome, p=0.8, list=FALSE)
    trainData_cs2_regres2 <- cs2_regres2[trainIndex_cs2_regres2,]
    testData_cs2_regres2 <- cs2_regres2[-trainIndex_cs2_regres2,]

    # Create training and testing datasets based on cs2_regresSmall2
    set.seed(123)
    trainIndex_cs2_regresSmall2 <- createDataPartition(cs2_regresSmall2$MonthlyIncome, p=0.8, list=FALSE)
    trainData_cs2_regresSmall2 <- cs2_regresSmall2[trainIndex_cs2_regresSmall2,]
    testData_cs2_regresSmall2 <- cs2_regresSmall2[-trainIndex_cs2_regresSmall2,]

    # Evaluate each model and store the results
    for (i in seq_along(models)) {
      model <- models[[i]]
      model_name <- model_names[i]
      dataset_name <- datasets[i]
      
      if (dataset_name == "cs2_regres2") {
        trainData <- trainData_cs2_regres2
        testData <- testData_cs2_regres2
      } else if (dataset_name == "cs2_regresSmall2") {
        trainData <- trainData_cs2_regresSmall2
        testData <- testData_cs2_regresSmall2
      }
      
      results <- evaluate_single_model(model, trainData, testData, "MonthlyIncome")
      
      # Add the results to the data frame
      results_df <- rbind(results_df, 
                          data.frame(Model = model_name,
                                     Adj_R_Squared = results$Adj_R_Squared,
                                     AIC = results$AIC,
                                     BIC = results$BIC,
                                     RMSE_Train = results$RMSE_Train,
                                     CVPRESS = results$CVPRESS,
                                     RMSE_Test = results$RMSE_Test,
                                     stringsAsFactors = FALSE))
    }

    # Print the results table
    print(results_df)

              Model Adj_R_Squared        AIC        BIC   RMSE_Train      CVPRESS RMSE_Test
    1        model8     0.9530060 14508.3465 14627.5588  982.8495052 8.839379e+08  1025.989
    2       model7a     0.9528093 14508.0845 14608.2228  987.2300657 8.838469e+08  1019.338
    3        model6     0.9527141 14507.8817 14598.4830  989.3868485 8.857464e+08  1023.496
    4 model8intFull     0.9556436 14507.8077 14879.7501  924.4757189          Inf  1004.716
    5    model8int1     0.9550913 14485.1980 14685.4747  951.0931906 8.606960e+08  1021.842
    6    model8int2     0.9552203 14477.9278 14654.3620  952.5864696 8.549141e+08  1016.073
    7   modelTrans1     0.9536858 14494.6970 14609.1409  976.2912116 8.717977e+08  1014.849
    8   modelTrans2     0.8991735  -222.3909  -112.7155    0.2073834 3.913144e+01  7769.196
    9     old_model     0.9258193 14895.7109 14967.2383 1242.1182815 1.390087e+09  1306.783

With this model:
Estimated Mean MonthlyIncome = *β*<sub>0</sub> + *β*<sub>1</sub>ID + *β*<sub>2</sub>BusinessTravel + *β*<sub>3</sub>JobLevel + *β*<sub>4</sub>JobRole + *β*<sub>5</sub>TotalWorkingYears + *β*<sub>6</sub>(ID×DailyRate) + *β*<sub>7</sub>(BusinessTravel×TotalWorkingYears) + *β*<sub>8</sub>(Education×TotalWorkingYears) + *β*<sub>9</sub>(JobLevel×TotalWorkingYears) + *β*<sub>10</sub>(JobRole×TotalWorkingYears)
I achieved an adjusted R-squared of 0.9552, AIC of 14478, BIC of 14654,
RMSE\_Train of 952.59, CVPRESS of 8.549141e+08, and RMSE\_Train of
1016.07. This is an improvement of the previous models metrics, adjusted
R-squared of 0.9258, AIC of 14896, BIC of 14967, RMSE\_Train of 1242.12,
CVPRESS of 1.390087e+09 and RMSE\_Test of 1306.78. The previous model
was:
Estimated Mean MonthlyIncome = *β*<sub>0</sub> + *β*<sub>1</sub>JobLevel + *β*<sub>2</sub>TotalWorkingYears + *β*<sub>3</sub>YearsWithCurrManager + *β*<sub>4</sub>(YearsWithCurrManager<sup>2</sup>) + *β*<sub>5</sub>DistanceFromHome + *β*<sub>6</sub>JobInvolvement + *β*<sub>7</sub>EnvironmentSatisfaction

*In the future, it would be worth conducting extra-sum-of-squares tests
on some variables included in these models as a further check on their
importance.*

**To measure separate RMSEs for a training and a validation set, I use
external cross validation for a linear regression model of
`MonthlyIncome` using the chosen model above.**

    # model8int2 = lm(MonthlyIncome ~ ID + BusinessTravel + JobLevel + JobRole + TotalWorkingYears + ID:DailyRate + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall)

    # another good performing model:
    # model8int1 = lm(MonthlyIncome ~ ID + BusinessTravel + JobLevel + JobRole + TotalWorkingYears + DailyRate + Education + ID:DailyRate + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears, data = cs2_regresSmall)

    model_formula = as.formula("MonthlyIncome ~ ID + BusinessTravel + JobLevel + JobRole + TotalWorkingYears + ID:DailyRate + BusinessTravel:TotalWorkingYears + Education:TotalWorkingYears + JobLevel:TotalWorkingYears + JobRole:TotalWorkingYears")

    # number of train/test splits
    iters = 100

    # split percentage
    splitPerc = 0.7

    # initialize vector to store squared error from each train/test split loop for training and validation sets
    cv_error_train = numeric(iters)
    cv_error_test = numeric(iters)
      
    # external cross-validation loop
    for (i in 1:iters) {
      
      set.seed(i)

      # generate the test train split
      trainIdx = sample(1:nrow(cs2_regresSmall), round(splitPerc * nrow(cs2_regresSmall)))
      IncomeTrn = cs2_regresSmall[trainIdx, ]
      IncomeTst = cs2_regresSmall[-trainIdx, ]
        
      # fit the model with train_data
      model = lm(model_formula, data = IncomeTrn)
        
      # predict on the training set (same data used to make model)
      predTrn = predict(model, newdata = IncomeTrn)
      
      # calculate the mean squared error of training pred
      cv_error_train[i] = mean((IncomeTrn$MonthlyIncome - predTrn)^2)
      
      # predict on the training set (same data used to make model)
      predTst = predict(model, newdata = IncomeTst)
        
      # calculate the mean squared error of training pred
      cv_error_test[i] = mean((IncomeTst$MonthlyIncome - predTst)^2)
    }
      
    # calculate the root mean squared error (RMSE) for the training set
    RMSE_trn = sqrt(mean(cv_error_train)) # root of avg of error from each iteration 
      
    # calculate the root mean squared error (RMSE) for the validation set
    RMSE_tst = sqrt(mean(cv_error_test))
      
    # print the results
    cat("\nRMSE of training set: ", RMSE_trn, "\n")


    RMSE of training set:  946.0464 

    cat("\nRMSE of validation set: ", RMSE_tst, "\n")


    RMSE of validation set:  998.5428 

For my linear regression model, the RMSE of my training set and
validation sets are 946.05 and 998.54, respectively. To achieve this, I
used these variables, `ID`, `BusinessTravel`, `JobLevel`, `JobRole`,
`TotalWorkingYears`, `ID:DailyRate`, `BusinessTravel:TotalWorkingYears`,
`Education:TotalWorkingYears`, `JobLevel:TotalWorkingYears`,
`JobRole:TotalWorkingYears`. This list includes some interaction terms.

**I import the unlabeled competition data set from the cloud as
`competition`. So that my column indexes are the same as those in the
primary data set, I add an `MonthlyIncome` column. I train a Linear
Regression model using the optimized variables and all the observations
in the labeled primary data set.**

    # make predictions with linear regression model

    # variables used: ID`, `BusinessTravel`, `JobLevel`, `JobRole`, `TotalWorkingYears`, `ID:DailyRate`, `BusinessTravel:TotalWorkingYears`, `Education:TotalWorkingYears`, `JobLevel:TotalWorkingYears`, `JobRole:TotalWorkingYears`


    # import data: CaseStudy2CompSet No Salary.csv from AWS S3 msdsds6306 bucket

    # first column header is `ï..ID` instead of `ID` which I guess is a UTF-8 encoding issue 
    # url = "https://msdsds6306.s3.us-east-2.amazonaws.com/CaseStudy2CompSet+No+Salary.csv"
    # competition = read.table(textConnection(getURL(url)), sep = ",", header = TRUE, stringsAsFactors = TRUE)

    # so I use read.csv and specify encoding = "UTF-8" and it imports correctly.
    url = "https://msdsds6306.s3.us-east-2.amazonaws.com/CaseStudy2CompSet+No+Salary.csv"
    competition = read.csv(url, header = TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")

    # save a copy of the competition unlabeled data
    # write.csv(competition, file = "data/CaseStudy2CompSet No Salary.csv", row.names = FALSE)

    # get a sense of the data
    # str(competition)
    # summary(competition)

    # add the MonthlyIncome column between MaritalStatus and MonthlyRate 
    competition = competition %>%
      mutate(MonthlyIncome = NA, .after = "MaritalStatus")
    str(competition)

    'data.frame':   300 obs. of  36 variables:
     $ ID                      : int  871 872 873 874 875 876 877 878 879 880 ...
     $ Age                     : int  43 33 55 36 27 39 33 21 30 51 ...
     $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 2 1 2 1 1 ...
     $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 2 3 3 1 3 3 1 2 2 3 ...
     $ DailyRate               : int  1422 461 267 1351 1302 895 750 251 1312 1405 ...
     $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 3 2 2 3 3 2 2 2 ...
     $ DistanceFromHome        : int  2 13 13 9 19 5 22 10 23 11 ...
     $ Education               : int  4 1 4 4 3 3 2 2 3 2 ...
     $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 2 3 2 5 6 3 2 2 6 ...
     $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ EmployeeNumber          : int  1849 995 1372 1949 1619 42 160 1279 159 1367 ...
     $ EnvironmentSatisfaction : int  1 2 1 1 4 4 3 1 1 4 ...
     $ Gender                  : Factor w/ 2 levels "Female","Male": 2 1 2 2 2 2 2 1 2 1 ...
     $ HourlyRate              : int  92 53 85 66 67 56 95 45 96 82 ...
     $ JobInvolvement          : int  3 3 4 4 2 3 3 2 1 2 ...
     $ JobLevel                : int  2 1 4 1 1 2 2 1 1 4 ...
     $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 7 8 3 3 9 8 3 7 5 ...
     $ JobSatisfaction         : int  4 4 3 2 1 4 2 3 3 2 ...
     $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 2 3 3 2 1 2 2 3 1 3 ...
     $ MonthlyIncome           : logi  NA NA NA NA NA NA ...
     $ MonthlyRate             : int  19246 17241 9277 9238 16290 3335 15480 25308 22310 24439 ...
     $ NumCompaniesWorked      : int  1 3 6 1 1 3 0 1 1 3 ...
     $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
     $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 1 1 1 1 1 ...
     $ PercentSalaryHike       : int  20 18 17 22 11 14 13 20 25 16 ...
     $ PerformanceRating       : int  4 3 3 4 3 3 3 4 4 3 ...
     $ RelationshipSatisfaction: int  3 1 3 2 1 3 1 3 3 2 ...
     $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
     $ StockOptionLevel        : int  1 0 0 0 2 1 1 0 3 0 ...
     $ TotalWorkingYears       : int  7 5 24 5 7 19 8 2 10 29 ...
     $ TrainingTimesLastYear   : int  5 4 2 3 3 6 2 2 2 1 ...
     $ WorkLifeBalance         : int  3 3 2 3 3 4 4 1 2 2 ...
     $ YearsAtCompany          : int  7 3 19 5 7 1 7 2 10 5 ...
     $ YearsInCurrentRole      : int  7 2 7 4 7 0 7 2 7 2 ...
     $ YearsSinceLastPromotion : int  7 0 3 0 0 0 0 2 0 0 ...
     $ YearsWithCurrManager    : int  7 2 8 2 7 0 7 2 9 3 ...

    # convert numerical ranking variables to factors
    numVars_to_fact = c("Education", "EnvironmentSatisfaction",
                        "JobInvolvement", "JobLevel", "JobSatisfaction",
                        "PerformanceRating", "RelationshipSatisfaction",
                        "StockOptionLevel", "WorkLifeBalance")
    competition = competition %>%
      mutate(across(all_of(numVars_to_fact), as.factor))
    # check the conversion
    str(competition)

    'data.frame':   300 obs. of  36 variables:
     $ ID                      : int  871 872 873 874 875 876 877 878 879 880 ...
     $ Age                     : int  43 33 55 36 27 39 33 21 30 51 ...
     $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 2 1 2 1 1 ...
     $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 2 3 3 1 3 3 1 2 2 3 ...
     $ DailyRate               : int  1422 461 267 1351 1302 895 750 251 1312 1405 ...
     $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 3 2 2 3 3 2 2 2 ...
     $ DistanceFromHome        : int  2 13 13 9 19 5 22 10 23 11 ...
     $ Education               : Factor w/ 5 levels "1","2","3","4",..: 4 1 4 4 3 3 2 2 3 2 ...
     $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 2 3 2 5 6 3 2 2 6 ...
     $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ EmployeeNumber          : int  1849 995 1372 1949 1619 42 160 1279 159 1367 ...
     $ EnvironmentSatisfaction : Factor w/ 4 levels "1","2","3","4": 1 2 1 1 4 4 3 1 1 4 ...
     $ Gender                  : Factor w/ 2 levels "Female","Male": 2 1 2 2 2 2 2 1 2 1 ...
     $ HourlyRate              : int  92 53 85 66 67 56 95 45 96 82 ...
     $ JobInvolvement          : Factor w/ 4 levels "1","2","3","4": 3 3 4 4 2 3 3 2 1 2 ...
     $ JobLevel                : Factor w/ 5 levels "1","2","3","4",..: 2 1 4 1 1 2 2 1 1 4 ...
     $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 7 8 3 3 9 8 3 7 5 ...
     $ JobSatisfaction         : Factor w/ 4 levels "1","2","3","4": 4 4 3 2 1 4 2 3 3 2 ...
     $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 2 3 3 2 1 2 2 3 1 3 ...
     $ MonthlyIncome           : logi  NA NA NA NA NA NA ...
     $ MonthlyRate             : int  19246 17241 9277 9238 16290 3335 15480 25308 22310 24439 ...
     $ NumCompaniesWorked      : int  1 3 6 1 1 3 0 1 1 3 ...
     $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
     $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 1 1 1 1 1 ...
     $ PercentSalaryHike       : int  20 18 17 22 11 14 13 20 25 16 ...
     $ PerformanceRating       : Factor w/ 2 levels "3","4": 2 1 1 2 1 1 1 2 2 1 ...
     $ RelationshipSatisfaction: Factor w/ 4 levels "1","2","3","4": 3 1 3 2 1 3 1 3 3 2 ...
     $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
     $ StockOptionLevel        : Factor w/ 4 levels "0","1","2","3": 2 1 1 1 3 2 2 1 4 1 ...
     $ TotalWorkingYears       : int  7 5 24 5 7 19 8 2 10 29 ...
     $ TrainingTimesLastYear   : int  5 4 2 3 3 6 2 2 2 1 ...
     $ WorkLifeBalance         : Factor w/ 4 levels "1","2","3","4": 3 3 2 3 3 4 4 1 2 2 ...
     $ YearsAtCompany          : int  7 3 19 5 7 1 7 2 10 5 ...
     $ YearsInCurrentRole      : int  7 2 7 4 7 0 7 2 7 2 ...
     $ YearsSinceLastPromotion : int  7 0 3 0 0 0 0 2 0 0 ...
     $ YearsWithCurrManager    : int  7 2 8 2 7 0 7 2 9 3 ...

    # train the linear regression model on the full `cs2_conv` data set
    fullset_LM_model = lm(model_formula, data = cs2_conv)
    summary(fullset_LM_model)


    Call:
    lm(formula = model_formula, data = cs2_conv)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2948.3  -588.1   -75.3   553.5  4209.4 

    Coefficients:
                                                         Estimate Std. Error t value Pr(>|t|)    
    (Intercept)                                         1.911e+03  2.556e+02   7.477 1.92e-13 ***
    ID                                                 -5.649e-01  1.855e-01  -3.044 0.002405 ** 
    BusinessTravelTravel_Frequently                     5.318e+02  2.238e+02   2.377 0.017690 *  
    BusinessTravelTravel_Rarely                         5.387e+02  1.890e+02   2.851 0.004470 ** 
    JobLevel2                                           3.461e+03  3.749e+02   9.234  < 2e-16 ***
    JobLevel3                                           5.929e+03  4.256e+02  13.931  < 2e-16 ***
    JobLevel4                                           1.145e+04  7.946e+02  14.409  < 2e-16 ***
    JobLevel5                                           1.369e+04  1.107e+03  12.359  < 2e-16 ***
    JobRoleLaboratory Technician                        2.817e+02  2.421e+02   1.164 0.244815    
    JobRoleHuman Resources                              2.982e+02  4.536e+02   0.657 0.511118    
    JobRoleResearch Scientist                           1.264e+02  2.380e+02   0.531 0.595417    
    JobRoleSales Executive                             -5.702e+02  4.376e+02  -1.303 0.192954    
    JobRoleHealthcare Representative                   -3.692e+02  4.824e+02  -0.765 0.444283    
    JobRoleManufacturing Director                      -5.720e+02  4.687e+02  -1.221 0.222616    
    JobRoleResearch Director                            2.925e+03  6.993e+02   4.183 3.19e-05 ***
    JobRoleManager                                      2.883e+03  8.177e+02   3.526 0.000446 ***
    TotalWorkingYears                                   8.175e+01  3.296e+01   2.480 0.013327 *  
    ID:DailyRate                                        4.340e-04  1.638e-04   2.649 0.008231 ** 
    BusinessTravelTravel_Frequently:TotalWorkingYears  -3.518e+01  1.766e+01  -1.992 0.046661 *  
    BusinessTravelTravel_Rarely:TotalWorkingYears      -2.416e+01  1.550e+01  -1.558 0.119520    
    TotalWorkingYears:Education2                       -9.450e-01  1.075e+01  -0.088 0.929977    
    TotalWorkingYears:Education3                       -8.047e+00  9.337e+00  -0.862 0.389043    
    TotalWorkingYears:Education4                       -2.523e+00  9.517e+00  -0.265 0.790996    
    TotalWorkingYears:Education5                       -4.183e+01  1.528e+01  -2.737 0.006339 ** 
    JobLevel2:TotalWorkingYears                        -1.359e+02  3.053e+01  -4.450 9.75e-06 ***
    JobLevel3:TotalWorkingYears                        -7.870e+01  3.189e+01  -2.468 0.013780 *  
    JobLevel4:TotalWorkingYears                        -1.907e+02  4.222e+01  -4.518 7.15e-06 ***
    JobLevel5:TotalWorkingYears                        -1.745e+02  5.189e+01  -3.364 0.000804 ***
    JobRoleLaboratory Technician:TotalWorkingYears     -3.385e+01  3.387e+01  -0.999 0.317881    
    JobRoleHuman Resources:TotalWorkingYears           -2.960e+01  6.699e+01  -0.442 0.658655    
    JobRoleResearch Scientist:TotalWorkingYears         1.531e+01  3.353e+01   0.457 0.648062    
    JobRoleSales Executive:TotalWorkingYears            1.382e+02  4.270e+01   3.236 0.001260 ** 
    JobRoleHealthcare Representative:TotalWorkingYears  1.203e+02  4.427e+01   2.717 0.006726 ** 
    JobRoleManufacturing Director:TotalWorkingYears     1.467e+02  4.416e+01   3.322 0.000933 ***
    JobRoleResearch Director:TotalWorkingYears          1.358e+02  5.013e+01   2.709 0.006881 ** 
    JobRoleManager:TotalWorkingYears                    1.303e+02  5.179e+01   2.516 0.012048 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 972.9 on 834 degrees of freedom
    Multiple R-squared:  0.957, Adjusted R-squared:  0.9552 
    F-statistic: 530.6 on 35 and 834 DF,  p-value: < 2.2e-16

    # predict the labels on the competition set
    preds = predict(fullset_LM_model, competition)

    # add the labels to the MonthlyIncome column
    competition = competition %>% mutate(MonthlyIncome = as.integer(preds))
    str(competition)

    'data.frame':   300 obs. of  36 variables:
     $ ID                      : int  871 872 873 874 875 876 877 878 879 880 ...
     $ Age                     : int  43 33 55 36 27 39 33 21 30 51 ...
     $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 2 1 2 1 1 ...
     $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 2 3 3 1 3 3 1 2 2 3 ...
     $ DailyRate               : int  1422 461 267 1351 1302 895 750 251 1312 1405 ...
     $ Department              : Factor w/ 3 levels "Human Resources",..: 3 2 3 2 2 3 3 2 2 2 ...
     $ DistanceFromHome        : int  2 13 13 9 19 5 22 10 23 11 ...
     $ Education               : Factor w/ 5 levels "1","2","3","4",..: 4 1 4 4 3 3 2 2 3 2 ...
     $ EducationField          : Factor w/ 6 levels "Human Resources",..: 2 2 3 2 5 6 3 2 2 6 ...
     $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
     $ EmployeeNumber          : int  1849 995 1372 1949 1619 42 160 1279 159 1367 ...
     $ EnvironmentSatisfaction : Factor w/ 4 levels "1","2","3","4": 1 2 1 1 4 4 3 1 1 4 ...
     $ Gender                  : Factor w/ 2 levels "Female","Male": 2 1 2 2 2 2 2 1 2 1 ...
     $ HourlyRate              : int  92 53 85 66 67 56 95 45 96 82 ...
     $ JobInvolvement          : Factor w/ 4 levels "1","2","3","4": 3 3 4 4 2 3 3 2 1 2 ...
     $ JobLevel                : Factor w/ 5 levels "1","2","3","4",..: 2 1 4 1 1 2 2 1 1 4 ...
     $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 8 7 8 3 3 9 8 3 7 5 ...
     $ JobSatisfaction         : Factor w/ 4 levels "1","2","3","4": 4 4 3 2 1 4 2 3 3 2 ...
     $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 2 3 3 2 1 2 2 3 1 3 ...
     $ MonthlyIncome           : int  5704 2623 12997 2438 2841 4117 5257 2348 3112 13733 ...
     $ MonthlyRate             : int  19246 17241 9277 9238 16290 3335 15480 25308 22310 24439 ...
     $ NumCompaniesWorked      : int  1 3 6 1 1 3 0 1 1 3 ...
     $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
     $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 1 1 1 1 1 ...
     $ PercentSalaryHike       : int  20 18 17 22 11 14 13 20 25 16 ...
     $ PerformanceRating       : Factor w/ 2 levels "3","4": 2 1 1 2 1 1 1 2 2 1 ...
     $ RelationshipSatisfaction: Factor w/ 4 levels "1","2","3","4": 3 1 3 2 1 3 1 3 3 2 ...
     $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
     $ StockOptionLevel        : Factor w/ 4 levels "0","1","2","3": 2 1 1 1 3 2 2 1 4 1 ...
     $ TotalWorkingYears       : int  7 5 24 5 7 19 8 2 10 29 ...
     $ TrainingTimesLastYear   : int  5 4 2 3 3 6 2 2 2 1 ...
     $ WorkLifeBalance         : Factor w/ 4 levels "1","2","3","4": 3 3 2 3 3 4 4 1 2 2 ...
     $ YearsAtCompany          : int  7 3 19 5 7 1 7 2 10 5 ...
     $ YearsInCurrentRole      : int  7 2 7 4 7 0 7 2 7 2 ...
     $ YearsSinceLastPromotion : int  7 0 3 0 0 0 0 2 0 0 ...
     $ YearsWithCurrManager    : int  7 2 8 2 7 0 7 2 9 3 ...

    # for curiousity get the summary of MonthlyIncome from both datasets
    summary(cs2_conv$MonthlyIncome)

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       1081    2840    4946    6390    8182   19999 

    summary(competition$MonthlyIncome)

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       1588    2719    5239    6067    6533   19449 

    # make dataframe with just competition set ordered IDs and labels
    competitionLabels = competition %>% dplyr::select(ID, MonthlyIncome)
    # str(competitionLabels)

    # save to file
    # write.csv(competitionLabels, file = "data/Case2PredictionsHenderson2 Salary.csv", row.names = FALSE)

### Conclusion

The goal of this analysis is to identify factors that lead to and help
predict employee attrition for the CEO and CFO of Frito Lay. This
analysis includes 36 unique features from 870 employees. Using a
combination of visualizations and statistical techniques, I identify
monthly income, job level and over time work as top factors leading to
employee attrition. I also document trends across different job roles
and provide a tool for interactive visualization of trends in income and
attrition rates among these roles. I constructed a model to forecast
employee attrition by identifying variables and parameters that yielded
the lowest error rates. I also developed an optimized linear regression
model for forecasting monthly salaries. With these models, I offer
predictions for 300 employees whose attrition outcomes are uncertain and
a separate cohort whose salaries are undetermined. Importantly, these
data reveal targeted steps that may be taken to mitigate attrition.
Specifically, increase monthly income for lower income earners in job
level 4, reduce over time work across job levels 1, 2, 3 and 5,
prioritize salary hikes and career advancement for personnel in job
level 1, where attrition rates are most pronounced. In providing
actionable intelligence alongside practical tools, my aim is to empower
Frito Lay in cultivating a work environment that promotes the retention
and development of high-value talent.

### Appendix

**R and package versions used**

    devtools::session_info()

    ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.3.2 (2023-10-31)
     os       macOS Monterey 12.7.5
     system   x86_64, darwin20
     ui       RStudio
     language (EN)
     collate  en_US.UTF-8
     ctype    en_US.UTF-8
     tz       America/New_York
     date     2024-08-24
     rstudio  2023.12.1+402 Ocean Storm (desktop)
     pandoc   3.1.1 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/ (via rmarkdown)

    ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
     package      * version    date (UTC) lib source
     abind          1.4-5      2016-07-21 [1] CRAN (R 4.3.0)
     backports      1.4.1      2021-12-13 [1] CRAN (R 4.3.0)
     base64enc      0.1-3      2015-07-28 [1] CRAN (R 4.3.0)
     bitops         1.0-7      2021-04-24 [1] CRAN (R 4.3.0)
     cachem         1.1.0      2024-05-16 [1] CRAN (R 4.3.3)
     car          * 3.1-2      2023-03-30 [1] CRAN (R 4.3.0)
     carData      * 3.0-5      2022-01-06 [1] CRAN (R 4.3.0)
     caret        * 6.0-94     2023-03-21 [1] CRAN (R 4.3.0)
     checkmate      2.3.1      2023-12-04 [1] CRAN (R 4.3.0)
     class        * 7.3-22     2023-05-03 [1] CRAN (R 4.3.2)
     cli            3.6.2      2023-12-11 [1] CRAN (R 4.3.0)
     cluster        2.1.4      2022-08-22 [1] CRAN (R 4.3.2)
     codetools      0.2-19     2023-02-01 [1] CRAN (R 4.3.2)
     colorspace     2.1-0      2023-01-23 [1] CRAN (R 4.3.0)
     combinat     * 0.0-8      2012-10-29 [1] CRAN (R 4.3.0)
     corrplot     * 0.92       2021-11-18 [1] CRAN (R 4.3.0)
     data.table     1.14.10    2023-12-08 [1] CRAN (R 4.3.0)
     DataExplorer * 0.8.3      2024-01-24 [1] CRAN (R 4.3.2)
     devtools       2.4.5      2022-10-11 [1] CRAN (R 4.3.0)
     digest         0.6.33     2023-07-07 [1] CRAN (R 4.3.0)
     dplyr        * 1.1.4      2023-11-17 [1] CRAN (R 4.3.0)
     e1071        * 1.7-14     2023-12-06 [1] CRAN (R 4.3.0)
     ellipsis       0.3.2      2021-04-29 [1] CRAN (R 4.3.0)
     evaluate       0.23       2023-11-01 [1] CRAN (R 4.3.0)
     fansi          1.0.6      2023-12-08 [1] CRAN (R 4.3.0)
     farver         2.1.1      2022-07-06 [1] CRAN (R 4.3.0)
     fastmap        1.2.0      2024-05-15 [1] CRAN (R 4.3.3)
     forcats      * 1.0.0      2023-01-29 [1] CRAN (R 4.3.0)
     foreach        1.5.2      2022-02-02 [1] CRAN (R 4.3.0)
     foreign        0.8-85     2023-09-09 [1] CRAN (R 4.3.2)
     Formula        1.2-5      2023-02-24 [1] CRAN (R 4.3.0)
     fs             1.6.3      2023-07-20 [1] CRAN (R 4.3.0)
     future         1.33.1     2023-12-22 [1] CRAN (R 4.3.0)
     future.apply   1.11.1     2023-12-21 [1] CRAN (R 4.3.0)
     generics       0.1.3      2022-07-05 [1] CRAN (R 4.3.0)
     GGally       * 2.2.1      2024-02-14 [1] CRAN (R 4.3.2)
     ggplot2      * 3.5.1      2024-04-23 [1] CRAN (R 4.3.2)
     ggstats        0.5.1      2023-11-21 [1] CRAN (R 4.3.0)
     globals        0.16.2     2022-11-21 [1] CRAN (R 4.3.0)
     glue           1.7.0      2024-01-09 [1] CRAN (R 4.3.0)
     goftest        1.2-3      2021-10-07 [1] CRAN (R 4.3.0)
     gower          1.0.1      2022-12-22 [1] CRAN (R 4.3.0)
     gridExtra    * 2.3        2017-09-09 [1] CRAN (R 4.3.0)
     gtable         0.3.4      2023-08-21 [1] CRAN (R 4.3.0)
     hardhat        1.3.1      2024-02-02 [1] CRAN (R 4.3.2)
     highr          0.10       2022-12-22 [1] CRAN (R 4.3.0)
     Hmisc        * 5.1-2      2024-03-11 [1] CRAN (R 4.3.2)
     hms            1.1.3      2023-03-21 [1] CRAN (R 4.3.0)
     htmlTable      2.4.2      2023-10-29 [1] CRAN (R 4.3.0)
     htmltools      0.5.7      2023-11-03 [1] CRAN (R 4.3.0)
     htmlwidgets    1.6.4      2023-12-06 [1] CRAN (R 4.3.0)
     httpuv         1.6.13     2023-12-06 [1] CRAN (R 4.3.0)
     igraph         1.6.0      2023-12-11 [1] CRAN (R 4.3.0)
     ipred          0.9-14     2023-03-09 [1] CRAN (R 4.3.0)
     iterators      1.0.14     2022-02-05 [1] CRAN (R 4.3.0)
     kableExtra   * 1.4.0      2024-01-24 [1] CRAN (R 4.3.2)
     knitr          1.45       2023-10-30 [1] CRAN (R 4.3.0)
     labeling       0.4.3      2023-08-29 [1] CRAN (R 4.3.0)
     later          1.3.2      2023-12-06 [1] CRAN (R 4.3.0)
     lattice      * 0.21-9     2023-10-01 [1] CRAN (R 4.3.2)
     lava           1.7.3      2023-11-04 [1] CRAN (R 4.3.0)
     lifecycle      1.0.4      2023-11-07 [1] CRAN (R 4.3.0)
     listenv        0.9.0      2022-12-16 [1] CRAN (R 4.3.0)
     lubridate    * 1.9.3      2023-09-27 [1] CRAN (R 4.3.0)
     magrittr       2.0.3      2022-03-30 [1] CRAN (R 4.3.0)
     MASS           7.3-60     2023-05-04 [1] CRAN (R 4.3.2)
     Matrix         1.6-1.1    2023-09-18 [1] CRAN (R 4.3.2)
     memoise        2.0.1      2021-11-26 [1] CRAN (R 4.3.0)
     mgcv           1.9-0      2023-07-11 [1] CRAN (R 4.3.2)
     mime           0.12       2021-09-28 [1] CRAN (R 4.3.0)
     miniUI         0.1.1.1    2018-05-18 [1] CRAN (R 4.3.0)
     ModelMetrics   1.2.2.2    2020-03-17 [1] CRAN (R 4.3.0)
     munsell        0.5.0      2018-06-12 [1] CRAN (R 4.3.0)
     networkD3      0.4        2017-03-18 [1] CRAN (R 4.3.0)
     nlme           3.1-163    2023-08-09 [1] CRAN (R 4.3.2)
     nnet           7.3-19     2023-05-03 [1] CRAN (R 4.3.2)
     nortest        1.0-4      2015-07-30 [1] CRAN (R 4.3.0)
     olsrr        * 0.6.0      2024-02-12 [1] CRAN (R 4.3.2)
     pander       * 0.6.5      2022-03-18 [1] CRAN (R 4.3.0)
     parallelly     1.36.0     2023-05-26 [1] CRAN (R 4.3.0)
     patchwork    * 1.2.0      2024-01-08 [1] CRAN (R 4.3.0)
     pillar         1.9.0      2023-03-22 [1] CRAN (R 4.3.0)
     pkgbuild       1.4.3      2023-12-10 [1] CRAN (R 4.3.0)
     pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.3.0)
     pkgload        1.3.3      2023-09-22 [1] CRAN (R 4.3.0)
     plyr           1.8.9      2023-10-02 [1] CRAN (R 4.3.0)
     pROC           1.18.5     2023-11-01 [1] CRAN (R 4.3.0)
     prodlim        2023.08.28 2023-08-28 [1] CRAN (R 4.3.0)
     profvis        0.3.8      2023-05-02 [1] CRAN (R 4.3.0)
     promises       1.2.1      2023-08-10 [1] CRAN (R 4.3.0)
     proxy          0.4-27     2022-06-09 [1] CRAN (R 4.3.0)
     purrr        * 1.0.2      2023-08-10 [1] CRAN (R 4.3.0)
     R6             2.5.1      2021-08-19 [1] CRAN (R 4.3.0)
     RColorBrewer * 1.1-3      2022-04-03 [1] CRAN (R 4.3.0)
     Rcpp           1.0.12     2024-01-09 [1] CRAN (R 4.3.0)
     RCurl        * 1.98-1.14  2024-01-09 [1] CRAN (R 4.3.0)
     readr        * 2.1.4      2023-02-10 [1] CRAN (R 4.3.0)
     recipes        1.0.9      2023-12-13 [1] CRAN (R 4.3.0)
     remotes        2.4.2.1    2023-07-18 [1] CRAN (R 4.3.0)
     reshape2       1.4.4      2020-04-09 [1] CRAN (R 4.3.0)
     rlang          1.1.3      2024-01-10 [1] CRAN (R 4.3.0)
     rmarkdown      2.25       2023-09-18 [1] CRAN (R 4.3.0)
     rpart          4.1.21     2023-10-09 [1] CRAN (R 4.3.2)
     rsconnect      1.2.1      2024-01-31 [1] CRAN (R 4.3.2)
     rstudioapi     0.15.0     2023-07-07 [1] CRAN (R 4.3.0)
     scales         1.3.0      2023-11-28 [1] CRAN (R 4.3.0)
     sessioninfo    1.2.2      2021-12-06 [1] CRAN (R 4.3.0)
     shiny          1.9.1      2024-08-01 [1] CRAN (R 4.3.3)
     stringi        1.8.3      2023-12-11 [1] CRAN (R 4.3.0)
     stringr      * 1.5.1      2023-11-14 [1] CRAN (R 4.3.0)
     survival       3.5-7      2023-08-14 [1] CRAN (R 4.3.2)
     svglite        2.1.3      2023-12-08 [1] CRAN (R 4.3.0)
     systemfonts    1.0.5      2023-10-09 [1] CRAN (R 4.3.0)
     tibble       * 3.2.1      2023-03-20 [1] CRAN (R 4.3.0)
     tidyr        * 1.3.0      2023-01-24 [1] CRAN (R 4.3.0)
     tidyselect     1.2.0      2022-10-10 [1] CRAN (R 4.3.0)
     tidyverse    * 2.0.0      2023-02-22 [1] CRAN (R 4.3.0)
     timechange     0.2.0      2023-01-11 [1] CRAN (R 4.3.0)
     timeDate       4032.109   2023-12-14 [1] CRAN (R 4.3.0)
     tzdb           0.4.0      2023-05-12 [1] CRAN (R 4.3.0)
     urlchecker     1.0.1      2021-11-30 [1] CRAN (R 4.3.0)
     usethis        2.2.2      2023-07-06 [1] CRAN (R 4.3.0)
     utf8           1.2.4      2023-10-22 [1] CRAN (R 4.3.0)
     vctrs          0.6.5      2023-12-01 [1] CRAN (R 4.3.0)
     viridisLite    0.4.2      2023-05-02 [1] CRAN (R 4.3.0)
     withr          2.5.2      2023-10-30 [1] CRAN (R 4.3.0)
     xfun           0.41       2023-11-01 [1] CRAN (R 4.3.0)
     xml2           1.3.6      2023-12-04 [1] CRAN (R 4.3.0)
     xtable         1.8-4      2019-04-21 [1] CRAN (R 4.3.0)
     yaml           2.3.8      2023-12-11 [1] CRAN (R 4.3.0)

     [1] /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library

    ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

    # rmarkdown::render("Analysis_of_Employee_Attrition_and_Income_V2.Rmd", "md_document")
