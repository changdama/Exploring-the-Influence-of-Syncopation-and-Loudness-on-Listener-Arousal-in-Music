---
title: "Final Project-Correlation analysis of Syncopation Score+RMS Score and Arousal Score in human emotions"
---

Abstract：The syncopation and loudness of the song often evoke different emotional changes of the listener. The analysis of the correlation between the three is worthy of certain importance in the field of music therapy or music perception. This study is based on a dataset of 100 pieces from the “Cocopops” corpus that contain human arousal and valence emotional tests. Based on HumdrumR, the number of syncopations, arousal value, and rms value of each song were analyzed, normalized to 0-100 respectively, to obtain the syncopation score, arousal score, and RMS score, and a multiple linear regression analysis was performed on the above three scores. Finally, the top 3 pieces with the highest and lowest syncopation scores were selected, and the syncopation account, arousal value and rms value were analyzed based on the measures or notes, respectively.


The “Cocopops” corpus in the *.varms.hum file are used, and there are a total of 100 of them, because these 100 files contain the arousal score, so that we can analyze the linear relationship between the syncopation score and the arousal score. However, the current progress is to calculate the different syncopation score of each of the 100 pieces.

```{r}
# load humdrumR and ggplot2
library(humdrumR)
library(ggplot2)
library(dplyr)
library(tidyr)
 


# load files
readHumdrum('./.*hum')->Cocopops

```
**1.Overview for all pieces**
*A.Calculate Syncopation Score of each piece*
i.Analyze the syncopation of each piece. First, obtain the duration of each song and convert them into numerical values. Group the data according to the "Piece".

ii.Second, use “syncopation(dur, meter = duple(5), levels = “all”, groupby = list())” in HumdrumR  to access songs in 4/4 time and obtain syncopations. The result is true and flase. True represents syncopations, and flase does not.
```{r}
# Calculate duration of each piece
dur_values <- Cocopops|>
  group_by(Piece) |>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()
# Calculate Syncopation of each piece based on duration
Synco <- dur_values |>
  group_by(Piece) |>
  summarise(Syncopation = list(syncopation(Duration, meter = duple(5), levels = "all", groupby = list())))

```
iii.Calculate the number of “true” in each piece, that is, the number of syncopations, and use “geom_col” in “ggplot” to plot a bar chart about piece and Syncopation_Count.
```{r}
results <- Synco |> 
  mutate(Syncopation_Count = sapply(Syncopation, function(x) sum(x)))
ggplot(results, aes(x = as.factor(Piece), y = Syncopation_Count)) +
  geom_col() +
  labs(title = "Syncopation Count per Piece", x = "Piece", y = "Syncopation Count") +
  theme_minimal()
ggsave("plot.jpg", width = 8, height = 6, dpi = 800)

```
iv.The Syncopation score for each piece is calculated by taking the Syncopation_Count statistics for all 100 pieces, arranging all the data from smallest to largest, and normalizing to [0,100] to obtain the corresponding score.
```{r}
min_valueS <- min(results$Syncopation_Count)
max_valueS <- max(results$Syncopation_Count)
S_Score_list <- results |> 
  mutate(Syncopation_Score = (results$Syncopation_Count - min_valueS) / 
                                        (max_valueS - min_valueS) * 100)
```
v.Plot all Syncopation_Score as scatter plot.
```{r}
print(S_Score_list$Syncopation_Score)
ggplot(S_Score_list, aes(x = Piece, y = Syncopation_Score)) +
  geom_point(color = "black", size = 3) +
  labs(
    x = "Piece", 
    y = "Syncopation Score", 
    title = "Syncopation Score by Piece"
  )

ggsave("Syncopation Score.png", width = 8, height = 6, dpi = 800)

```
*B.Calculate rms Score of each piece*
i.Design a function (RMS)that calculates the average of the total rms value of all rows in each song.

First, the data in the **rms spine of the corpus is filtered, and the remaining spine is removed, grouping each piece.

Second,Convert humdrum format to data.frame. Load all rms values into col1 and ensure that all data is numeric. Calculate the average of all values in col1, which is the average rms value of the piece.
```{r}
RMS<- function(data) {
    data |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) |>
    summarise(mean_col1 = mean(Col1, na.rm = TRUE)) |>
    ungroup()
}
```
ii.Iterate 100 pieces and calculate the rms mean value of all the songs.(Because using “as.data.frame” to calculate the mean causes the “group_by(Piece,Bar)” function in HumdrumR to be broken, if the input is the entire Cocopops corpus, the result is the unique mean of all pieces and all phrases.)
Extract the rms average of all pieces and write them to a list, convert to a data frame, and finally visualize them.
```{r}
RMS_list<- list()
for (i in 1:100) {
  RMS_result <- RMS(Cocopops[i])
  RMS_list[[i]] <- RMS_result$mean_col1
}
mean_col1_list <- lapply(RMS_list, function(x) as.numeric(x))

# Convert to data frame
mean_col1_df <- data.frame(mean_col1 = unlist(mean_col1_list))

# View results and visualization
print(mean_col1_df )

ggplot(mean_col1_df, aes(x = seq_along(mean_col1), y = mean_col1)) +
  geom_point(size = 2, color = "black") 
  labs(
    x = "Piece", 
    y = "RMSValue", 
    title = "RMSValue by Piece"
  ) +
  theme_minimal()
ggsave("RMSValue.png", width = 8, height = 6, dpi = 800)
```
iii.For subsequent correlation analysis, we also normalized the average rms value of all pieces from small to large to 0-100 as the rms score of each song and visualize them.
```{r}
min_value_means_col1 <- min(mean_col1_df$mean_col1)
max_value_means_col1 <- max(mean_col1_df$mean_col1)
Rms_Score = (mean_col1_df$mean_col1- min_value_means_col1) / (max_value_means_col1 - min_value_means_col1) * 100
Rms_Score_df <- data.frame(Piece = seq_along(Rms_Score), Rms_Score = Rms_Score)
ggplot(Rms_Score_df, aes(x = Piece, y = Rms_Score)) +
  geom_point(size = 3, color = "black") +  # 绘制散点图
  labs(
    x = "Piece", 
    y = "RMS Score", 
    title = "RMS Score Plot"
  ) +
  theme_minimal()
ggsave("RMS Score.png", width = 8, height = 6, dpi = 800)
```
*C.Calculate Arousal Score of each piece*
i.Design a function (AROUSAL)that calculates the average of the total arousal value of each piece.

ii.Since the arousal values come from four people, but in the corpus the four values are in one column, the average score for each phrase cannot be calculated. Therefore, the **.hum file must first be converted into a data frame, and then the data of one column  from four people into four columns of data from one person, so that the average of the four columns of data can be calculated as the arousal value of each musical phrase of  a piece. Admittedly, participants did not give scores to some musical phrases, so all “None” were changed to 0.Finally, the arousal value of each musical phrase was summed and then averaged to be used as the arousal value of the entire piece.
```{r}

AROUSAL <- function(data) {
  average_result <- data |>
    filter(Exclusive == "arousal") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = c("Col1", "Col2", "Col3", "Col4"), sep = " ") |>
    mutate(across(starts_with("Col"), ~ replace_na(as.numeric(.), 0))) |>  # Replace NA with 0
    rowwise() |>
    mutate(row_mean = mean(c_across(starts_with("Col")), na.rm = TRUE)) |> # Calculate the average of the rows
    ungroup()
  
  overall_mean <- average_result |>
    summarise(overall_row_mean = mean(row_mean, na.rm = TRUE))  # Calculate the overall average
  
  return(list(average_result = average_result, overall_mean = overall_mean))
}

```
iii.Iterate 100 pieces and calculate the arousal mean value of all the pieces.(Because using “as.data.frame” to calculate the mean causes the “group_by(Piece,Bar)” function in HumdrumR to be broken, if the input is the entire Cocopops corpus, the result is the unique mean of all pieces and all phrases.)
```{r}
AROUSAL_list <- list()
for (i in 1:100) {
  AROUSAL_result <- AROUSAL(Cocopops[i])
  AROUSAL_list[[i]] <- AROUSAL_result$overall_mean
}

```
iv.Extract the average arousal value(overall_row_mean) of all pieces and write them to a list, convert to a data frame, and finally visualize them.
```{r}
# Extract overall_mean and edit as a list
overall_mean_list <- lapply(AROUSAL_list, function(x) as.numeric(x$overall_row_mean))

# Convert to data frame
overall_mean_df <- data.frame(overall_mean = unlist(overall_mean_list))

# View results and visualization
print(overall_mean_df)
ggplot(overall_mean_df, aes(x = seq_along(overall_mean), y = overall_mean)) +
  geom_point(color = "blue", size = 3) +
  labs(x = "Index", y = "Overall Mean", title = "Scatter Plot of Arousal Value") +
  theme_minimal()

```
iii.For subsequent correlation analysis, we also normalized the average arousal value(overall mean) of all pieces from small to large to 0-100 as the arousal score of each song and visualize them.
```{r}
min_valueA <- min(overall_mean_df$overall_mean)
max_valueA <- max(overall_mean_df$overall_mean)
A_Score_list <- overall_mean_df |> 
  mutate(Arousal_Score = (overall_mean_df$overall_mean - min_valueA) / 
                                              (max_valueA - min_valueA) * 100)

print(A_Score_list$Arousal_Score)

ggplot(A_Score_list, aes(x = seq_along(Arousal_Score), y = Arousal_Score)) +
  geom_point(size = 3, color = "black") +
  labs(
    x = "Piece", 
    y = "Arousal Score", 
    title = "Arousal Score Per Piece"
  ) +
  theme_minimal()
ggsave("Arousal Score.png", width = 8, height = 6, dpi = 800)

```
*C.Linear Regression among Syncopation Score, Arousal Score and RMS score*
i. Use a multiple linear regression model to analyze the relationship between the Syncopation Score and the Arousal Score，and use this model to predict the predicted arousal score by entering the syncopation score.
```{r}
# Create data frame
data <- data.frame(
  S_Score = S_Score_list$Syncopation_Score,
  A_Score = A_Score_list$Arousal_Score
)

# Linear regression model
model <- lm(A_Score ~ S_Score, data = data)

# View model summary
summary(model)

# visualization
ggplot(data, aes(x = S_Score, y = A_Score)) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Linear Regression: Arousal_Score ~ Syncopation_Score",
    x = "Syncopation Score",
    y = "Arousal Score"
  ) +
  theme_minimal()
ggsave(
  filename = "linear_regressionSyncopation Score and the Arousal Score_.png",
  width = 8, height = 6, dpi = 800
)

# Model diagnosis chart



par(mfrow = c(2, 2))
plot(model)



# New data prediction

new_data <- data.frame(S_Score = c(35, 45, 55))
predictions <- predict(model, newdata = new_data)
print(data.frame(new_data, Predicted_Arousal_Score = predictions))
```
ii. Use a multiple linear regression model to analyze the relationship between the RMS Score and the Arousal Score，and use this model to predict the predicted arousal score by entering the RMS score.
```{r}
# Create data frame
datarms <- data.frame(
  R_Score = Rms_Score,
  A_Score = A_Score_list$Arousal_Score
)

# Linear regression model
model <- lm(A_Score_list$Arousal_Score ~ Rms_Score, datarms = datarms)

# View model summary
summary(model)

# visualization
ggplot(datarms, aes(x = Rms_Score, y = A_Score_list$Arousal_Score)) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Linear Regression: Arousal_Score ~ RMS_Score",
    x = "RMS Score",
    y = "Arousal Score"
  ) +
  theme_minimal()
ggsave(
  filename = "linear_regressionRMS Score and the Arousal Score_.png",
  width = 8, height = 6, dpi = 800
)

# Model diagnosis chart

par(mfrow = c(2, 2))  
plot(model)

# New data prediction
new_datarum <- data.frame(Rms_Score = c(35, 45, 55))
predictions <- predict(model, newdatarum = new_datarum)
print(data.frame(new_datarum, Predicted_Arousal_Score = predictions))
```
ii. Use a multiple linear regression model to analyze the relationship among the( RMS Score, Syncopation_Score) and the Arousal Score，and use this model to predict the predicted arousal score by entering the RMS score.
```{r}
# Create data frame
datam <- data.frame( S_Score_list$Syncopation_Score, Rms_Score,A_Score_list$Arousal_Score
)

# Linear regression model
model <- lm(A_Score_list$Arousal_Score ~ S_Score_list$Syncopation_Score + Rms_Score, datam = datam)

# View model summary
summary(model)

# # visualization
par(mfrow = c(2, 2))
plot(model)

# Visualize predicted values vs. actual values
library(ggplot2)
ggplot(datam, aes(x = predict(model), y = A_Score_list$Arousal_Score)) +
  geom_point(color = "black", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Syncopation_Score+Rms_Score vs Actual Arousal Score",
    x = "Syncopation_Score+Rms_Score",
    y = "Arousal_Score"
  ) +
  theme_minimal()
ggsave(
  filename = "linear_regressionRMS ScoreScycopation Score and the Arousal Score_.png",
  width = 8, height = 6, dpi = 800)

# Model prediction
new_datam <- data.frame(
  S_Score_list$Syncopation_Score = c(20, 30, 40),
  result_rms_Score = c(5, 10, 15)
)
predictions <- predict(model, newdatam = new_datam)
print(data.frame(new_datam, Predicted_Arousal_Score = predictions))

```
**3.Based on the scores of all pieces in the Syncopation_Score, locate the Top3 highest and Top3 lowest pieces, and analyze the arousal and rms of each bar and position one by one.**
*A.The piece of the lowest syncopation count-41(0)*
i.visualize the syncopation count based on each bar
```{r}
dur_values41 <- Cocopops[41]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()

Synco41 <- syncopation(dur_values41, meter = duple(5), levels = "all", groupby = list())

result41 <- Synco41 |>
  group_by(Piece, Bar) |>  
  count()

ggplot(result41, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "coral") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data41 <- AROUSAL(Cocopops[41])

average_result41 <- data41$average_result
result_table41 <- data.frame(average_result41)

result_table41_df <- data.frame(Index = seq_along(result_table41$row_mean), 
                                Row_Mean = result_table41$row_mean)


ggplot(result_table41_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)

```
iii.visualize the Rms score based on each note
```{r}
datarms41 <- Cocopops[41] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms41_df <- data.frame(Note = seq_along(datarms41$Col1), Rms_Value = datarms41$Col1)
ggplot(datarms41_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)

```

*B.The piece of the second lowest syncopation count-83(2)*
i.visualize the syncopation count based on each bar
```{r}
dur_values83 <- Cocopops[83]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()

Synco83 <- syncopation(dur_values83, meter = duple(5), levels = "all", groupby = list())


result83 <- Synco83 |>
  group_by(Piece, Bar) |>  
  count()|>
  ungroup()

ggplot(result83, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data83 <- AROUSAL(Cocopops[83])
average_result83 <- data83$average_result
result_table83 <- data.frame(average_result83)
result_table83_df <- data.frame(Index = seq_along(result_table83$row_mean), 
                                Row_Mean = result_table83$row_mean)


ggplot(result_table83_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)

```
iii.visualize the Rms score based on each note
```{r}
datarms83 <- Cocopops[83] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms83_df <- data.frame(Note = seq_along(datarms83$Col1), Rms_Value = datarms83$Col1)
ggplot(datarms83_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)
```
*c.The piece of the third lowest syncopation count-56(10)*
i.visualize the syncopation count based on each bar
```{r}
dur_values56 <- Cocopops[56]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()

Synco56 <- syncopation(dur_values56, meter = duple(5), levels = "all", groupby = list())


result56 <- Synco56 |>
  group_by(Piece, Bar) |>  
  count()|>
  ungroup()

ggplot(result56, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data56 <- AROUSAL(Cocopops[56])


average_result56 <- data56$average_result
result_table56 <- data.frame(average_result56)

result_table56_df <- data.frame(Index = seq_along(result_table56$row_mean), 
                                Row_Mean = result_table56$row_mean)


ggplot(result_table56_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)

```
iii.visualize the Rms score based on each note
```{r}
datarms56 <- Cocopops[56] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms56_df <- data.frame(Note = seq_along(datarms56$Col1), Rms_Value = datarms56$Col1)
ggplot(datarms83_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)
```
*d.The piece of the most higest syncopation count-14(487)*
i.visualize the syncopation count based on each bar
```{r}
dur_values14 <- Cocopops[14]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()
print(dur_values14)
Synco14 <- syncopation(dur_values14, meter = duple(5), levels = "all", groupby = list())
print(Synco14)

result14 <- Synco14 |>
  group_by(Piece, Bar) |>  
  count()|>
  ungroup()
print(result14)
ggplot(result14, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data14 <- AROUSAL(Cocopops[14])


average_result14 <- data14$average_result
result_table14 <- data.frame(average_result14)

result_table14_df <- data.frame(Index = seq_along(result_table14$row_mean), 
                                Row_Mean = result_table14$row_mean)


ggplot(result_table14_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)
```
iii.visualize the Rms score based on each note
```{r}
datarms14 <- Cocopops[14] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms14_df <- data.frame(Note = seq_along(datarms14$Col1), Rms_Value = datarms14$Col1)
ggplot(datarms14_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)
```
*e.The piece of the second higest syncopation count-100(419)*
i.visualize the syncopation count based on each bar
```{r}
dur_values100 <- Cocopops[100]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()

Synco100 <- syncopation(dur_values100, meter = duple(5), levels = "all", groupby = list())


result100 <- Synco100 |>
  group_by(Piece, Bar) |>  
  count()|>
  ungroup()

ggplot(result100, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data100 <- AROUSAL(Cocopops[100])


average_result100 <- data100$average_result
result_table100 <- data.frame(average_result100)

result_table100_df <- data.frame(Index = seq_along(result_table100$row_mean), 
                                Row_Mean = result_table100$row_mean)


ggplot(result_table100_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)
```
iii.visualize the Rms score based on each note
```{r}
datarms100 <- Cocopops[100] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms100_df <- data.frame(Note = seq_along(datarms100$Col1), Rms_Value = datarms100$Col1)
ggplot(datarms100_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)
```
*e.The piece of the third higest syncopation count-52(369)*
i.visualize the syncopation count based on each bar
```{r}
dur_values52 <- Cocopops[52]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()

Synco52 <- syncopation(dur_values52, meter = duple(5), levels = "all", groupby = list())


result52 <- Synco52 |>
  group_by(Piece, Bar) |>  
  count()|>
  ungroup()

ggplot(result52, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data52 <- AROUSAL(Cocopops[52])


average_result52 <- data52$average_result
result_table52 <- data.frame(average_result52)

result_table52_df <- data.frame(Index = seq_along(result_table52$row_mean), 
                                Row_Mean = result_table52$row_mean)


ggplot(result_table52_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)
```
iii.visualize the Rms score based on each note
```{r}
datarms52 <- Cocopops[52] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms52_df <- data.frame(Note = seq_along(datarms52$Col1), Rms_Value = datarms52$Col1)
ggplot(datarms52_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)
```
**2.Based on the scores of all pieces in the Arousal_Score, locate the  highest and Top3 lowest pieces, and analyze the syncopation,arousal and rms of each bar and position one by one.**
*A.The piece of the lowest arousal score-90(0)*
i.visualize the syncopation count based on each bar
```{r}
dur_values90 <- Cocopops[90]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()

Synco90 <- syncopation(dur_values90, meter = duple(5), levels = "all", groupby = list())


result90<- Synco90 |>
  group_by(Piece, Bar) |>  
  count()|>
  ungroup()

ggplot(result90, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data90<- AROUSAL(Cocopops[90])


average_result90 <- data90$average_result
result_table90 <- data.frame(average_result90)

result_table90_df <- data.frame(Index = seq_along(result_table90$row_mean), 
                                Row_Mean = result_table90$row_mean)


ggplot(result_table90_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)
```
iii.visualize the Rms score based on each note
```{r}
datarms90 <- Cocopops[90] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms90_df <- data.frame(Note = seq_along(datarms90$Col1), Rms_Value = datarms90$Col1)
ggplot(datarms90_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)
```
*B.The piece of the highest arousal score-65(100)*
i.visualize the syncopation count based on each bar
```{r}
dur_values65 <- Cocopops[65]|>
  mutate(Duration = as.numeric(as.character(duration(Token)))) |>
  ungroup()

Synco65 <- syncopation(dur_values65, meter = duple(5), levels = "all", groupby = list())


result65 <- Synco65 |>
  group_by(Piece, Bar) |>  
  count()|>
  ungroup()

ggplot(result65, aes(x = Bar, y = n)) +
  geom_boxplot(fill = "grey") +
  labs(
    x = "Bar ",
    y = "syncopation count(n)",

  ) +
  theme_minimal()
ggsave(
  filename = "syncopation count(Bar).png",
  width = 8, height = 6, dpi = 800)
```
ii.visualize the Arousal score based on each note
```{r}

data65<- AROUSAL(Cocopops[65])


average_result65 <- data65$average_result
result_table65 <- data.frame(average_result65)

result_table65_df <- data.frame(Index = seq_along(result_table65$row_mean), 
                                Row_Mean = result_table65$row_mean)


ggplot(result_table65_df, aes(x = Index, y = Row_Mean)) +
  geom_point(size = 3, shape = 1,color = "black") +
  labs(
    x = "Note",
    y = "Arousal value",

  ) +
  theme_minimal()
ggsave(
  filename = "Arousal value(Note).png",
  width = 8, height = 6, dpi = 800)
```
iii.visualize the Rms score based on each note
```{r}
datarms65 <- Cocopops[65] |>
    filter(Exclusive == "rms") |>
    group_by(Piece, Bar) |>
    removeEmptySpines() |>
    as.data.frame() |>
    separate(V1, into = "Col1", sep = " ") |>
    mutate(Col1 = as.numeric(Col1)) 

datarms65_df <- data.frame(Note = seq_along(datarms65$Col1), Rms_Value = datarms65$Col1)
ggplot(datarms65_df, aes(x = Note, y = Rms_Value)) +
  geom_point(shape = 1, size = 3, color = "black") +  
  labs(
    x = "Note",
    y = "Rms_Value",

  ) +
  theme_minimal()
ggsave(
  filename = "Rms_Value(Note).png",
  width = 8, height = 6, dpi = 800)
```



**4.Extension: linear regression of each song "grouped_by(Piece, Bar),take 14(syncopation score:100) as an example**
*Filter out the amount of syncopation, normalize 0-100*
```{R}
result14_true <- result14 |> 
  filter(Syncopation == "TRUE")

within(result14_true,line(result14_true$Bar, result14_true$n))


min_value_result14 <- min(result14_true$n, na.rm = TRUE)
max_value_result14 <- max(result14_true$n, na.rm = TRUE)
result14_S_Score = (result14_true$n - min_value_result14) / (max_value_result14 - min_value_result14) * 100

```
*Because the 14th syncopation score is calculated by dividing into 12 groups, the arousal score should also be divided into 12 groups accordingly for the subsequent linear regression.*
```{r}

# divide into 12 groups
num <- length(result_table14$row_mean) / 12
row_mean_split <- split(result_table14$row_mean, rep(1:12, each = num))
group_means14 <- sapply(row_mean_split, mean)
print(group_means14)

min_value_group_means14 <- min(group_means14)
max_value_group_means14 <- max(group_means14)
result14_A_Score = (group_means14 - min_value_group_means14) / (max_value_group_means14 - min_value_group_means14) * 100

```
linear regression of the 14th piece——syncopation score~~arousal score
```{r}
data <- data.frame(
  result14_S_Score = result14_S_Score,
  result14_A_Score = result14_A_Score
)

# Multiple Linear Regression Model
model <- lm(result14_S_Score ~ result14_A_Score, data = data)

# View Model Summary
summary(model)

# Visualizing regression results

ggplot(data, aes(x = result14_A_Score, y = result14_S_Score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Regression: Result14_S_Score ~ result14_A_Score",
    x = "result14_A_Score",
    y = "Result14 S_Score"
  ) +
  theme_minimal()

# Model predictions
new_data <- data.frame(result14_A_Score = c(20, 30, 40))
predictions <- predict(model, newdata = new_data)
print(data.frame(new_data, Predicted_S_Score = predictions))

# Residual diagnostics
par(mfrow = c(2, 2))
plot(model)
```
