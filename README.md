# China Housing Market – Exploratory Data Analysis & Price Drivers (R)

## 📌 Overview
This project analyzes housing prices in Beijing, China, using cross-sectional data to understand the impact of house characteristics, structure, and location on pricing.  

- **Data Source:** Kaggle  
- **Year:** 2018  
- **Sample Size:** 221  
- **Main Variables:** Total price (1000 ¥), Price per square meter (¥/m²), Area (㎡), Number of rooms, Building type, Construction year, Renovation condition, Building structure, District  

## 🥅 Objective
- Examine how housing type, structure, and location affect house prices in Beijing.  
- Explore relationships between other house features and pricing patterns.  

## 🛠️ Methods
- Data cleaning and missing value imputation  
- Outlier detection and removal  
- Statistical summary and descriptive analysis  
- ANOVA, correlation analysis, and hypothesis testing  

## 🔍 Key Findings
- **Location is the strongest factor** affecting housing prices:  
  - High-priced houses are concentrated in **Dongcheng District**.  
  - Low-priced houses are found in **Shijingshan, Haidian, and Changping Districts**.  
  - Mid-priced houses are located mainly in **Mentougou and Shijingshan Districts**.  
- **Interior features influence prices:**  
  - Number of living rooms shows a positive correlation with price.  
  - Surprisingly, total area does not correlate strongly with price overall, but stratifying the data reveals some positive correlation within price tiers.  
- **Building type has minimal impact:**  
  - No significant price differences between Tower, Plate, or mixed building types.  

**Conclusion:**  
Location dominates the housing market pricing in Beijing, followed by interior features such as the number of rooms. Building type appears less relevant.

## 🛠️ Tools
- **Language:** R  
- **Packages:** tidyverse, ggplot2, dplyr  

## ▶️ How to Run
1. Open the `analysis.R` script in RStudio.  
2. Run all sections sequentially to reproduce data cleaning, analysis, and visualizations.  
3. Output plots are saved in the `/plots` folder.  

