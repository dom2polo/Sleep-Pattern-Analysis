---
title: 'STAT 353 Project 2: Exploring Sleep Patterns in College Students'
author: "Dominic Apolo"
date: "2023-11-07"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


## add library here
library(ggplot2)
library(dplyr)
library(lessR)

survey = Read("https://www.lock5stat.com/datasets3e/SleepStudy.csv", quiet=TRUE)

```

## Background

Our goal in this project is to study the sleep patterns of college students. We are given a dataset that compiles information from a study of 253 college students' sleep patterns and related factors. It includes data on variables such as gender, class year, sleep preferences (early riser or night owl), academic metrics (GPA, classes missed), mental health scores (depression, anxiety, stress), happiness, alcohol use, bedtime, rise time, and sleep duration on both weekdays and weekends. Additionally, it records whether students had all-nighters. This dataset offers a comprehensive overview of college students' sleep, lifestyle choices, and mental well-being, making it a valuable resource for research and analysis in the context of college life.

## The Task

### Research Question

The questions has been provided as follow:

(1) Is there a significant difference in the average GPA between male and    female college students?

(2) Is there a significant difference in the average number of early classes   between the first two class years and other class years?

(3) Do students who identify as "larks" have significantly better cognitive   skills (cognition z-score) compared to "owls"?

(4) Is there a significant difference in the average number of classes        missed in a semester between students who had at least one early class     (EarlyClass=1) and those who didn't (EarlyClass=0)?

(5) Is there a significant difference in the average happiness level between   students with at least moderate depression and normal depression status?

(6) Is there a significant difference in average sleep quality scores         between students who reported having at least one all-nighter              (AllNighter=1) and those who didn't (AllNighter=0)?

(7) Do students who abstain from alcohol use have significantly better        stress scores than those who report heavy alcohol use?

(8) Is there a significant difference in the average number of drinks per     week between students of different genders?

(9) Is there a significant difference in the average weekday bedtime between   students with high and low stress (Stress=High vs. Stress=Normal)?

(10) Is there a significant difference in the average hours of sleep on       weekends between first two year students and other students?

## Data Preparation

```{r, echo = TRUE}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")
head(SleepStudy)
```

## Data Analysis

The questions has been provided as follow:

### 1. Is there a significant difference in the average GPA between male and female college students?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

# Perform the 2-sample z-test for comparing proportions
prop_result <- Prop_test(data = survey, variable = Gender, success = "1", by = GPA)

print(prop_result) 
  

# Bar diagram to visualize the data 
  SleepStudy <- SleepStudy %>%
  mutate(Gender = ifelse(Gender == 1, "Male", "Female"))
  
  ggplot(SleepStudy, aes(x = Gender, y = GPA, fill = Gender)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(title = "Average GPA by Gender",
       x = "Gender",
       y = "Average GPA") +
  theme_minimal() +
  scale_fill_manual(values = c("Gender", "Male" = "skyblue", "Female" = "lightpink"), name = "Gender")


# Change density plot fill colors by groups
ggplot(SleepStudy, aes(x=GPA, color = Gender, fill = Gender)) +
 geom_histogram(aes(y=after_stat(density)), alpha=0.5, 
                position="identity")+
 geom_density(alpha=.2) 
  

```

The chi-square test for Gender by GPA, with a chi-square statistic of 67.602 and 61 degrees of freedom, resulted in a p-value of 0.262. This test is evaluating the hypothesis of equal population proportions between different GPA levels for the variable Gender. The alternative hypothesis is two-sided. However, with a p-value of 0.262, there is no strong evidence to suggest a significant difference in proportions among the categories.

There is a bit of a difference between the two genders in regards to the GPA. Female students tend to have a higher GPA as shown in the bar diagram above. If we view the density diagram, we can see that higher density of female students have higher GPA while the male aren't that far behind but it is only slighter less.

### 2. Is there a significant difference in the average number of early classes between the first two class years and other class years?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")


ggplot(SleepStudy, aes(x = ClassYear, y = NumEarlyClass)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
  labs(title = "Average Number of Early Class for First two class Year and other Class",
       x = "ClassYear",
       y = "Number of Early Class") +
  theme_minimal()

SleepStudy <- SleepStudy %>%
mutate(ClassYear = ifelse(ClassYear %in% c("1", "2"), "first_second_year", "other_year"))

# Perform the 2-sample z-test for comparing proportions
test_result <- Prop_test(data = SleepStudy, variable = ClassYear, success = "first_second_year", by = NumEarlyClass)

print(test_result)

  # Create a plot with two different fill colors and a legend
ggplot(SleepStudy, aes(x = ClassYear, y = NumEarlyClass, fill = ClassYear)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(title = "Average Number of early class for first two class year and other year", 
  x = "Use of Alcohol", 
  y = "Measure of amount of stress") + 
    theme_minimal() +
    scale_fill_manual(values = c("ClassYear", "first_second_year" = "skyblue", "other_year" = "lightpink"), name = "ClassYear")


```

The chi-square test result you provided indicates that there is a significant difference in population proportions across the categories being compared. In this case, you have six categories, and the chi-square statistic is 33.849 with 5 degrees of freedom. The p-value is very small (p-value = 0.000002552), which suggests strong evidence against the null hypothesis of equal population proportions. Therefore, you would reject the null hypothesis and conclude that there is a significant difference in proportions among the categories.

### 3. Do students who identify as "larks" have significantly better cognitive skills (cognition z-score) compared to "owls"?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

# Subset the data to exclude rows where LarkOwl is "Neither"
subset_data <- survey[survey$LarkOwl != "Neither", ]

test_result <- t.test(CognitionZscore~LarkOwl, data=subset_data, alternative="two.sided")

print(test_result)


ggplot(SleepStudy %>% filter(LarkOwl != "Neither"), aes(x = LarkOwl, y = CognitionZscore)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
  labs(title = "Cognitive Skills of Larks vs. Owls",
       x = "LarksOwl",
       y = "Cognitive Skills (cognition z-score)") +
  theme_minimal()



```

The Welch Two Sample t-test for CognitionZscore by LarkOwl yielded a t-value of 0.80571 with degrees of freedom (df) approximately equal to 75.331. The p-value associated with this test is 0.4229. The alternative hypothesis is that the true difference in means between the "Lark" and "Owl" groups is not equal to 0. The 95 percent confidence interval for the difference in means is approximately (-0.1894, 0.4466). The sample mean for the "Lark" group is 0.0902, and for the "Owl" group, it is -0.0384. Based on the p-value, there is no significant evidence to reject the null hypothesis, suggesting that the means of the "Lark" and "Owl" groups are not significantly different.

### 4. Is there a significant difference in the average number of classes missed in a semester between students who had at least one early class (EarlyClass=1) and those who didn't (EarlyClass=0)?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

OneEarlyClass <- data.frame(SleepStudy %>% filter(grepl(1, SleepStudy$EarlyClass)))
ZeroEarlyClass <- data.frame(SleepStudy %>% filter(grepl(0, SleepStudy$EarlyClass)))

t_test_result = t.test(OneEarlyClass$ClassesMissed, ZeroEarlyClass$ClassesMissed)

print(t_test_result)


prop_result <- Prop_test(data = survey, variable = EarlyClass, success = "1", by = ClassesMissed) 

print(prop_result)

SleepStudy <- SleepStudy %>%
  mutate(EarlyClass = ifelse(EarlyClass == 1, "OneEarlyClass", "ZeroEarlyClass"))


ggplot(SleepStudy, aes(x = EarlyClass, y = ClassesMissed, fill = EarlyClass)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(title = "Average number of classes missed w/ Students with early Class",
    x = "Early Class", 
    y = "Classes Missed") + 
    theme_minimal() + 
    scale_fill_manual(values = c("EarlyClass", "OneEarlyClass" = "skyblue", "ZeroEarlyClass" = "lightpink"), name = "EarlyClass")

# Change density plot fill colors by groups
ggplot(SleepStudy, aes(x=ClassesMissed, color = EarlyClass, fill = EarlyClass)) +
 geom_histogram(aes(y=..density..), alpha=0.5, 
                position="identity")+
 geom_density(alpha=.2) 



```

The confidence interval (-1.5412830, 0.2233558) includes 0, supporting the conclusion that there may not be a substantial difference in the means. The means of the two groups are provided as sample estimates, with the group without early classes having a slightly higher mean (2.647059) compared to the group with early classes (1.988095).

The chi-square statistic is 23.753 with 14 degrees of freedom, resulting in a p-value of 0.049. The null hypothesis, which assumes equal population proportions, is tested against the alternative hypothesis that there is a difference in proportions. The p-value of 0.049 is less than the conventional significance level of 0.05, suggesting evidence to reject the null hypothesis. Therefore, there is an indication that the distribution of "EarlyClass" varies significantly across different levels of "ClassesMissed."

### 5. Is there a significant difference in the average happiness level between students with at least moderate depression and normal depression status?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

dnorm <- data.frame( SleepStudy %>% filter(grepl('normal',SleepStudy$DepressionStatus)))

dmod <- data.frame( SleepStudy %>% filter(grepl('moderate|severe',SleepStudy$DepressionStatus)))

# Perform a t-test to compare the average happiness levels in the two groups

  t_test_result <- t.test(dmod$Happiness, dnorm$Happiness)
  # Print the t-test result
  print(t_test_result)
  
    SleepStudy <- SleepStudy %>%
 mutate(DepressionStatus = ifelse(DepressionStatus %in% c("moderate", "severe"), "At Least Moderate", "Normal"))
    
    # Create a plot with two different fill colors and a legend
ggplot(SleepStudy, aes(x = DepressionStatus, y = Happiness, fill = DepressionStatus)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(title = "Average happiness level between students with Moderate and normal depression", 
  x = "Depression Status", 
  y = "Happiness Level") + 
    theme_minimal() +
scale_fill_manual(values = c("At Least Moderate" = "lightpink", "Normal" = "skyblue"), name = "Depression Status Label")

```

The t-test result indicates that there is a significant difference in happiness levels between the "dmod" and "dnorm" groups. The negative t-statistic and the small p-value suggest that the difference is statistically significant, and the confidence interval further supports this finding.

The confidence interval shows a range of values (-5.818614 to -2.119748) within which the true difference in means is likely to fall. Since this interval does not include 0, it reinforces the idea that there is a significant difference in happiness levels between the two groups.

The p-value is very small (8.616e-05 or 0.00008616), indicating strong evidence against the null hypothesis. This means that there is a significant difference in the happiness levels between the two groups. Students who have a depression status of normal have a much higher level of happiness.

### 6. Is there a significant difference in average sleep quality scores between students who reported having at least one all-nighter (AllNighter=1) and those who didn't (AllNighter=0)?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

# Perform the 2-sample z-test for comparing proportions
chi_result <- Prop_test(data = survey, variable = AllNighter, success = "1", by = PoorSleepQuality) 

print(chi_result)

# Display a Bar Diagram for visualization
SleepStudy <- SleepStudy %>%
mutate(AllNighter = ifelse(AllNighter == 1, "All Nighter", "No All Nighter"))
  
  # Create a plot with two different fill colors and a legend
ggplot(SleepStudy, aes(x = AllNighter, y = PoorSleepQuality, fill = AllNighter)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(title = "Stress Score by All-Nighter", 
  x = "All Nighter", 
  y = "Poor Sleep Quality") + 
    theme_minimal() +
scale_fill_manual(values = c("All Nighter" = "lightpink", "No All Nighter" = "skyblue"), name = "All Nighter Label")
  

```

The chi-square statistic is 14.573 with 15 degrees of freedom, resulting in a p-value of 0.4826. The null hypothesis, which assumes equal population proportions, is tested against the alternative hypothesis that there is a difference in proportions. The p-value of 0.4826 is larger than the conventional significance level of 0.05, suggesting a lack of evidence to reject the null hypothesis. Therefore, there is no strong indication that the distribution of "AllNighter" varies significantly across different levels of "PoorSleepQuality."  However, by looking at the diagram above, we can see that students who does not spend an all nighter have a much better sleep quality in average.

### 7. Do students who abstain from alcohol use have significantly better stress scores than those who report heavy alcohol use?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

# Subset the data for abstainers and heavy drinkers
abstainers_data <- SleepStudy %>% filter(AlcoholUse == "Abstain")
heavy_drinkers_data <- SleepStudy %>% filter(AlcoholUse == "Heavy")

# Perform a t-test2
t_test_result <- t.test(abstainers_data$StressScore, heavy_drinkers_data$StressScore)

# Print the t-test results
print(t_test_result)

SleepStudy <- SleepStudy %>% filter(AlcoholUse %in% c("Heavy", "Abstain"))

  SleepStudy <- SleepStudy %>%
  mutate(AlcoholUse = ifelse(AlcoholUse == "Abstain", "AbstainUse", "HeavyUse"))
  
  # Create a plot with two different fill colors and a legend
ggplot(SleepStudy, aes(x = AlcoholUse, y = StressScore, fill = AlcoholUse)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(title = "Stress Score by Alcohol Use", 
  x = "Use of Alcohol", 
  y = "Measure of amount of stress") + 
    theme_minimal() +
    scale_fill_manual(values = c("AlcoholUse", "AbstainUse" = "skyblue", "HeavyUse" = "lightpink"), name = "Alcohol Use Label")

```

The confidence interval provides a range of values within which you can be 95% confident that the true difference in means lies. In this case, the confidence interval ranges from -6.261170 to 3.327346. This interval includes 0, suggesting that there is a possibility that the true difference in means could be zero.

It can be shown through the plot diagram that students with heavy use of Alcohol have a much significantly higher stress score than students who abstain from alcohol. 

### 8. Is there a significant difference in the average number of drinks per week between students of different genders?

```{r}

SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

prop_result <- Prop_test(data = survey, variable = Gender, success = "1", by = Drinks)

print(prop_result) 

SleepStudy <- SleepStudy %>%
  mutate(Gender = ifelse(Gender == 1, "Male", "Female"))

ggplot(SleepStudy, aes(x = Gender, y = Drinks, fill = Gender)) +
 stat_summary(fun = "mean", geom = "bar") +
  labs(title = "Average Number of Drinks / Week by Gender",
       x = "Gender",
       y = "Average Num. of Drinks/Week") +
  theme_minimal() +
  scale_fill_manual(values = c("Gender", "Male" = "skyblue", "Female" = "lightpink"), name = "Gender")

  
```

The chi-square statistic is 70.646 with 17 degrees of freedom, resulting in a very low p-value of 0.00000001669. This small p-value leads to rejecting the null hypothesis, suggesting that there is a significant association between gender and the reported number of drinks. The alternative hypothesis indicates a two-sided test, emphasizing that the proportions differ across gender categories in terms of reported drinks. As also shown in the bar diagram, we can see that male students spend on average more number of drinks per week than female students.

### 9. Is there a significant difference in the average weekday bedtime between students with high and low stress (Stress=High vs. Stress=Normal)?

```{r}

High_stress <- data.frame(SleepStudy %>% filter(grepl('high', SleepStudy$Stress)))

Normal_stress <- data.frame(SleepStudy %>% filter(grepl('normal', SleepStudy$Stress)))

  t_test_result <- t.test(High_stress$WeekdayBed,Normal_stress$WeekdayBed)
  print(t_test_result)

# Perform the 2-sample t-test for comparing means
mean_result <- ttest(WeekdayBed~Stress, data=survey, alternative="two_sided")

print(mean_result)

# Bar Diagram
SleepStudy <- SleepStudy %>%
  mutate(Stress = ifelse(Stress == "high", "high", "normal"))

ggplot(SleepStudy, aes(x = Stress, y = WeekdayBed, fill = Stress)) +
 stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  labs(title = "Average Weekday Bedtime by Stress",
       x = "Stress",
       y = "Average Weekday Bedtime") +
  theme_minimal() +
  scale_fill_manual(values = c("Stress", "high" = "skyblue", "normal" = "lightpink"), name = "Stress")

```

The t-test for the mean difference has a t-value of 1.089 with 251 degrees of freedom and a p-value of 0.277. The 95% confidence interval for the mean difference is -0.138 to 0.479.

There is no strong evidence of a significant difference in weekday bedtime between students with normal and high stress levels. The analysis considers various statistical aspects, assumptions, and provides descriptive and inferential results for a comprehensive understanding of the comparison.

### 10. Is there a significant difference in the average hours of sleep on weekends between first two year students and other students?

```{r}
SleepStudy = read.csv("https://www.lock5stat.com/datasets3e/SleepStudy.csv")

first_second_year <- data.frame(SleepStudy %>% filter(grepl("^[1-2]", SleepStudy$ClassYear)))
third_fourth_year <- data.frame(SleepStudy %>% filter(grepl("^[3-4]", SleepStudy$ClassYear)))

t_test_result <- t.test(first_second_year$AverageSleep, third_fourth_year$AverageSleep)

print(t_test_result)

ggplot(SleepStudy, aes(x = ClassYear, y = AverageSleep)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
  labs(title = "Average Sleep on Weekends for First two class Year and other Class",
       x = "ClassYear",
       y = "Average Sleep on Weekends") +
  theme_minimal()


SleepStudy <- SleepStudy %>%
mutate(ClassYear = ifelse(ClassYear %in% c("1", "2"), "first_second_year", "other_year"))

# Create a plot with two different fill colors and a legend
ggplot(SleepStudy, aes(x = ClassYear, y = AverageSleep, fill = ClassYear)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(title = "Average Number of early class for first two class year and other year", 
  x = "Use of Alcohol", 
  y = "Measure of amount of stress") + 
    theme_minimal() +
    scale_fill_manual(values = c("ClassYear", "first_second_year" = "skyblue", "other_year" = "lightpink"), name = "ClassYear")



```

The p-value is 0.5492, which is not very small. A higher p-value suggests weaker evidence against the null hypothesis, indicating that there may not be a significant difference between the two groups.

The confidence interval ranges from -0.1660165 to 0.3113800. This interval includes 0, suggesting that there is a possibility that the true difference in means could be zero.

The t-statistic is relatively small, the p-value is not very small, and the 95% confidence interval includes zero, all of which indicate that the observed difference in means may not be statistically significant.

## Summary

The analysis of various factors related to college students' sleep patterns reveals several insights. While gender differences in GPA were not statistically significant, there is a slight variation favoring female students. Early classes significantly impact class attendance, indicating the importance of scheduling considerations. Sleep preferences, as classified by "larks" or "owls," did not show a strong correlation with cognitive skills. Mental health, particularly depression status, significantly influences happiness levels, emphasizing the interplay between mental well-being and emotional states. Sleep quality, as influenced by all-nighters, showed no statistically significant impact, but students without all-nighters tended to have better sleep quality. Heavy alcohol use correlated with higher stress scores, highlighting the potential impact of lifestyle choices on stress levels.

In the study, there wasn't a big difference in grades between males and female students, but having early morning classes made students miss more classes. The way people prefer to sleep didn't seem to affect how smart they are. If someone was moderately depressed, they tended to be less happy, and students who didn't drink alcohol had lower stress. The study gives us a peek into how sleep, mental health, and lifestyle choices connect for college students and affect how they feel overall.

## Reference

-   The data link: <https://www.lock5stat.com/datapage3e.html>
-   ChatGPT: <https://openai.com/>
-   STAT 353 PPT Notes: <https://rpubs.com/scsustat/Stat353PPT> 
