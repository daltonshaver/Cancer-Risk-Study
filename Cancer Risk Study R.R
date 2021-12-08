library(ggplot2)
library(ggthemes)
library(tidyverse)
data = STAT_3010_Cancer_Risk_Dataset

#Descriptive Statistics for quantitative variables Age, Calories, and Fat

############## USE PYTHON FOR DESCRIPTIVE STATISTICS .describe

##Created range variables
range_Age = max(data$Age) - min(data$Age)
range_Calories = max(data$Calories) - min(data$Calories)
range_Fat = max(data$Fat) - min(data$Fat)

##Placed all needed statistics in one clean variable for Age, Calories, and Fat
Age_stat = c(summary(data$Age), sd(data$Age), range_Age, IQR(data$Age))
Calories_stat = c(summary(data$Calories), sd(data$Calories), range_Calories, IQR(data$Calories))
Fat_stat = c(summary(data$Fat), sd(data$Fat), range_Fat, IQR(data$Fat))

##Renamed column headers to include Standard Deviation, Range, and IQR
names(Age_stat) = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD", "Range", "IQR")
names(Calories_stat) = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD", "Range", "IQR")
names(Fat_stat) = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "SD", "Range", "IQR")

# 1. Complete Descriptive Statistics of each quantitative variable
Age_stat
Calories_stat
Fat_stat

# 2. Histograms for Age, Calories, and Fat
### AGE
ggplot(data, aes(x=Age))+
  geom_histogram(binwidth = 8, col="black", fill="mediumpurple")+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="black", size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        plot.title=element_text(size=15, hjust=0.5))+
  labs(title="Figure 1: Histogram of Age", x="Age (years)", y="Frequency")

### CALORIES
ggplot(data, aes(x=Calories))+
  geom_histogram(binwidth=450, col="black", fill="salmon")+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="black", size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        plot.title=element_text(size=15, hjust=0.5))+
  labs(title="Figure 3: Histogram of Calories Consumed Per Day", x="Calories", y="Frequency")

### FAT
ggplot(data, aes(x=Fat))+
  geom_histogram(binwidth = 20, col="black", fill="skyblue3")+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="black", size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        plot.title=element_text(size=15, hjust=0.5))+
  labs(title="Figure 5: Histogram of Fat Consumed Per Day", x="Fat (grams)", y="Frequency")

# 2. Boxplots for Age, Calories, and Fat
### AGE
ggplot(data, aes(x=Age))+
  geom_boxplot(col="black", fill="mediumpurple")+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="white"),
        axis.title.x=element_text(size=12),
        plot.title=element_text(size=15, hjust=0.5))+
  labs(title="Figure 2: Boxplot of Age", x="Age (years)")

### CALORIES
ggplot(data, aes(x=Calories))+
  geom_boxplot(col="black", fill="salmon", outlier.size=3)+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="white"),
        axis.title.x=element_text(size=12),
        plot.title=element_text(size=15, hjust=0.5))+
  labs(title="Figure 4: Boxplot of Calories Consumed Per Day", x="Calories")

### FAT
ggplot(data, aes(x=Fat))+
  geom_boxplot(col="black", fill="skyblue3", outlier.size=3)+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="white"),
        axis.title.x=element_text(size=12),
        plot.title=element_text(size=15, hjust=0.5))+
  labs(title="Figure 6: Boxplot of Fat Consumed Per Day", x="Fat (grams)")

# 3. Statistics for 'agecat'
### Categorized Age
data$agecat[data$Age<40] = "Millennial"
data$agecat[data$Age>=40 & data$Age<48] = "Late Gen X"
data$agecat[data$Age>=48 & data$Age<62] = "Early Gen X"
data$agecat[data$Age>=62] = "Baby Boomer"

### Table 1: Frequency Table for 'agecat'
table_agecat = table(data$agecat)
df_freq = as.data.frame(table(ord_agecat))
df_freq$Relative = round(prop.table(table(ord_agecat))*100, 1)
names(df_freq) = c("Age Category", "Frequency", "Percent")
total_row = c(sum(df_freq$Frequency), round(sum(df_freq$Percent), 0))
df_freq = rbind(df_freq, total_row)
df_freq

### Pie Chart for variable 'agecat'
df_agecat = as.data.frame(table_agecat)
names(df_agecat) = c("Age_Category", "Frequency")

ggplot(df_agecat, aes(x="", y=Frequency, fill=factor(Age_Category)))+
  geom_bar(width=1, stat="identity")+
  theme_void()+
  geom_text(aes(x=1.68, label=paste(round(Frequency/ sum(Frequency) * 100, 1), "%")),
            position = position_stack(vjust=0.5), size=5)+
  theme(plot.title=element_text(hjust=0.5),
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        text = element_text(size=12))+
  labs(fill="Age Category", x=NULL, y=NULL,
       title="Figure 7: Pie Chart of Age Category")+
  scale_fill_manual(values=c("plum2", "plum3", "orchid3", "orchid4"),
                    labels=c("Millennial", "Late Gen X", "Early Gen X", "Baby Boomer"))+
  coord_polar("y")


### Ordered Bar Chart for variable 'agecat'
ord_agecat = ordered(data$agecat, c("Millennial", "Late Gen X", "Early Gen X", "Baby Boomer"))

ggplot(data, aes(x=ord_agecat))+
  geom_bar(fill=c("plum2", "plum3", "orchid3", "orchid4"))+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="black", size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        plot.title=element_text(size=15, hjust=0.5))+
  labs(title="Figure 8: Bar Chart of Age Categories")+
  xlab("Age Categories")+
  ylab("Count")

# 4. Contingency Tables for 'Gender' by 'SmokeStat'
GenSmoke_table = table(data$Gender, data$SmokeStat) #Counts table
round(prop.table(GenSmoke_table),4) #Percent of Total table
round(prop.table(GenSmoke_table, 1),4) #Percent of Row table
round(prop.table(GenSmoke_table, 2),4) #Percent of column table

# 5. 100% Stacked Bar Chart of 'Gender' by 'SmokeStat'
prop_GenSmoke = round(prop.table(GenSmoke_table,2)*100,2)
df_GenSmoke = as.data.frame(prop_GenSmoke)
names(df_GenSmoke) = c("Gender", "Smoke_Status", "Percentage")

ggplot(df_GenSmoke, aes(x=Smoke_Status, y=Percentage, fill=Gender, label=Percentage,))+
  geom_bar(stat = "identity", show.legend = TRUE)+
  geom_text(size=5, position=position_stack(vjust=0.5))+
  theme_clean()+
  theme(legend.position="right",
        axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="black", size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        plot.title=element_text(size=15, hjust=0.5, color="black"))+
  scale_fill_manual(values = c("skyblue3", "plum3"), 
                    labels = c("Male", "Female"))+
  scale_x_discrete("Smoke Status", labels=c("Never", "Former", "Current Smoker"))+
  ggtitle("Figure 9: 100% Stacked Bar Chart of Smoke Status by Gender")

######################################################################################### HOW TO ADD % SIGN?

# 6.Inference
set.seed(22577)

### Sample of 80 observations from main data set
sample = data[sample(1:315,80,replace=F),]

### Function to compute Confidence Intervals
CI = function(x, alpha = .05, dec = 3){ 
  n = sum(!is.na(x)) 
  conf_level = (1-alpha)*100
  me = qt(1-alpha/2, n-1)*sd(x, na.rm=T)/sqrt(n) 
  mean = round(mean(x, na.rn = T), digits = dec)
  lower = round(mean(x, na.rn = T) - me, digits = dec)
  upper = round(mean(x, na.rn = T) + me, digits = dec)
  limits = data.frame(cbind(variable = deparse(substitute(x)), n, 
                             c.level = conf_level, mean, me = round(me, digits = dec), 
                             lower, upper))
  print(limits)
  rm(n, conf_level, lower, upper, mean)}

### 95% Confidence Interval for the Variable 'Calories'
CI(sample$Calories, 0.05, 2)




# 7. Five applications showing relationship between 'Age' and 'Calories'
### Explanatory (x axis): Age, Response (y axis): Calories
### Hypothesis: Late Gen X consumes more Calories than other Generations

# Tool 1: Scatter plot of Variables 'Age' and 'Calories'
ggplot(data, aes(x=Age, y=Calories))+ 
  geom_point()+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="black", size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        plot.title=element_text(size=15, hjust=0.5))+
  geom_smooth(method=lm, se=F, linetype="solid", color="tomato1")+ 
  ggtitle("Figure 10: Scatterplot of Calories by Age")

# Tool 2: Side-by-Side Boxplots
ggplot(data, aes(x=Calories, y=agecat, fill=Calories))+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        plot.title=element_text(size=15, hjust=0.5))+
  geom_boxplot(fill=c("orchid4", "orchid3", "plum3", "plum2"))+
  ggtitle("Figure 11: Side-by-Side Boxplots of Calories by Age Category")+
  ylab("Age Category")

# Tool 3: 100% Stacked Bar Chart
### Creation of Categorical Calories variable
data$calcat[data$Calories<1200] = "Low"
data$calcat[data$Calories>=1200 & data$Age<2000] = "Average"
data$calcat[data$Calories>=2000 & data$Age<3000] = "Above Average"
data$calcat[data$Calories>=3000] = "High"

ord_calcat = ordered(data$calcat, c("Low", "Average", "Above Average", "High"))

### Counts table
AgeCalories_table = table(data$agecat, data$calcat)
### Proportion table
prop_AgeCalories = round(prop.table(AgeCalories_table,2)*100,2)
### Data frame
df_AgeCalories = as.data.frame(prop_AgeCalories)
names(df_AgeCalories) = c("Age_Category", "Calories_Consumption", "Percentage")
### 100% Stacked Bar Chart
ggplot(df_AgeCalories, aes(x=factor(Calories_Consumption, level=c("Low", "Average", "Above Average", "High")), y=Percentage, fill=Age_Category, label=Percentage,))+
  geom_bar(stat = "identity", show.legend = TRUE)+
  geom_text(size=4, position=position_stack(vjust=0.5))+
  theme_clean()+
  theme(axis.text.x=element_text(color="black", size=12),
        axis.text.y=element_text(color="black", size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        plot.title=element_text(size=15, hjust=0.5))+
  theme(legend.position = "right")+
  scale_fill_manual(values = c("plum2", "plum3", "orchid3", "orchid4"), 
                    labels = c("Millennial", "Late Gen X", "Early Gen X", "Baby Boomer"))+
  labs(fill="Age Category", x="Calories Category", y="Percentage",
       title="Figure 12: 100% Stacked Bar Chart of Calories by Age")



# Tool 4: Stratified Analysis
strat_analysis = aggregate(x=data$Calories, by=list(data$agecat), FUN=summary)
names(strat_analysis) = c("Age Category", "Calories")
strat_analysis

# Tool 5: Contingency Table
### Use AgeCalories_table from earlier
AgeCalories_table
col_order = c("Low", "Average", "Above Average", "High")
row_order = c("Millennial", "Late Gen X", "Early Gen X", "Baby Boomer")
AgeCalories_table = AgeCalories_table[row_order, col_order]

AgeCalories_table
round(prop.table(AgeCalories_table),4) #Percent of Total table
round(prop.table(AgeCalories_table, 1),4) #Percent of Row table
round(prop.table(AgeCalories_table, 2),4) #Percent of column table
### No total row needed
