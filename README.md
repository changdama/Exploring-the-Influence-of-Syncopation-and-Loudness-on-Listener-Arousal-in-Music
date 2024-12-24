# Exploring-the-Influence-of-Syncopation-and-Loudness-on-Listener-Arousal-in-Music
![Introduction](https://raw.githubusercontent.com/changdama/Exploring-the-Influence-of-Syncopation-and-Loudness-on-Listener-Arousal-in-Music/main/Fig/introduction.png)
**Examine the individual and combined effects of synco-pation and loudness on listener-reported arousal**

## Introduction ##
Music is a powerful tool for evoking emotions, capable of influencing mood, phys-iological states, and even behavior. Arousal, one of the primary dimensions of emo-tional response, reflects the intensity or energy level of an experience and is shaped by various musical features. Understanding the mechanisms behind arousal in music not only deepens our comprehension of human emotional processing but also has practical applications in fields such as music therapy, entertainment, and artificial intelligence-driven music composition. In this research, using syncopation and loudness to analyze arousal.
- **Syncopation**: Syncopation, marked by rhythmic irregularities and disruptions of expected patterns, evokes excitement and emotional tension by challenging the listener's predictive models (Huron & Ommen, 2006; 
                   Tan et al., 2019). Its impact on arousal depends on context and familiarity, with moderate syncopation often bringing more enjoyment than very low or high levels, supporting optimal complexity 
                   theories (Gómez et al., 2005).
- **Loudness**： Loudness, measured by RMS levels, strongly influences arousal, with louder music perceived as more intense and engaging, triggering heightened physiological and emotional responses (Hosken et al., 
                 2021). Unlike syncopation, it serves as a direct cue, consistently correlating with arousal across styles and listeners.
  
## Research Question and Hypothesis ##
### Question ###
- Does higher syncopation evoke greater arousal in listeners?
- Does increased loudness correlate with higher arousal levels?

### Hypothesis ###
- More syncopation within vocal parts evokes higher levels of arousal in listeners.
- Higher RMS levels (representing loudness) lead to increased levels of arousal.

## Method ##
### Datasets ###
- [![CoCoPops Corpus](https://img.shields.io/badge/CoCoPops-Corpus-blue)](https://github.com/Computational-Cognitive-Musicology-Lab/CoCoPops/tree/main/Billboard/Data)

- Filter ```*.varms.hum```: [![CoCoPops Dataset](https://img.shields.io/badge/CoCoPops-*.varms.hum-blue)](Datasets/CoCopops%20we%20will%20use/)

### Installation ###
- **Prerequsites**

  [![R](https://img.shields.io/static/v1?label=Language&message=R&color=blue&logo=R)](https://www.r-project.org/)
  [![RStudio](https://img.shields.io/static/v1?label=IDE&message=RStudio&color=blue&logo=RStudio)](https://posit.co/)

- **HumdrumR Installation**
```
install.packages('devtools')
devtools::install_github("Computational-Cognitive-Musicology-Lab/humdrumR")
git clone https://github.com/Computational-Cognitive-Musicology-Lab/humdrumR
devtools::install()
library(humdrumR)
```
### Key Features ###

🎶 **Syncopation Score** 
 - A measure of rhythmic complexity
 - Calculated as the normalized count of syncopations within vocal parts for each piece

🔊 **RMS Score**
 - Represents loudness
 - Computed as the average RMS value extracted from the ```**rms``` spine of each piece.

😊⚡ **Arousal Score** 
 - An overall mean value of each piece derived from listener-reported arousal levels in the ```**arousal``` spine.

🎶🔊**Combined RMS and Syncopation Score**
 - A composite score that integrates both RMS and syncopation metrics.

 ### Procedure ###

- **Load library**
  ```
   library(humdrumR)
   library(ggplot2)
   library(dplyr)
   library(tidyr)
  ```
- **Load Files**
  ```
  readHumdrum('./.*hum')->Cocopops
  ```
- **Calculating Syncopation Scores**
  - Analyze the syncopation of each piece. First, obtain the duration of each piece and convert them into numerical values. Group the data according to the 
    "Piece".
  ```
   dur_values <- Cocopops|>
       group_by(Piece) |>
       mutate(Duration = as.numeric(as.character(duration(Token)))) |>
   ungroup()
  ```
  
  - Using “syncopation(dur, meter = duple(5), levels = “all”, groupby = list())” in HumdrumR  to access songs in 4/4 time and obtain syncopations. The result is 
    true and flase. True represents syncopations, and flase does not.
    ```
    Synco <- dur_values |>
      group_by(Piece) |>
      summarise(Syncopation = list(syncopation(Duration, meter = duple(5), levels = "all", groupby = list())))
    ```

  - Calculate the number of “true” in each piece, that is, the number of syncopations, and use “geom_col” in “ggplot” to plot a bar chart about piece and 
    Syncopation_Count.
    ```
    results <- Synco |> 
               mutate(Syncopation_Count = sapply(Syncopation, function(x) sum(x)))
    ggplot(results, aes(x = as.factor(Piece), y = Syncopation_Count)) +
               geom_col() +
               labs(title = "Syncopation Count per Piece", x = "Piece", y = "Syncopation Count") + theme_minimal()
    ggsave("plot.jpg", width = 8, height = 6, dpi = 800)
    ```
    ![Plot](Fig/pic/pieces/plot.jpg)
    
  - Syncopation score for each piece is calculated by taking the Syncopation_Count statistics for all 100 pieces, arranging all the data from smallest to largest, and normalizing to [0,100] to obtain the 
    corresponding score, and make all Syncopation_Score as scatter plots.
    
    ```
    min_valueS <- min(results$Syncopation_Count)
    max_valueS <- max(results$Syncopation_Count)
    S_Score_list <- results |> 
    mutate(Syncopation_Score = (results$Syncopation_Count - min_valueS) / (max_valueS - min_valueS) * 100)

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
    ![Plot](Fig/pic/pieces/Syncopation%20Score.png)

 - **Calculating RMS Scores**
   - Design a function (RMS)that calculates the average of the total rms value of all rows in each song.First, the data in the **rms spine of the corpus is filtered, and the remaining spine is removed, grouping 
     each piece. Second,Convert humdrum format to data.frame. Load all rms values into col1 and ensure that all data is numeric. Calculate the average of all values in col1, which is the average rms value of the 
     piece.
     ```
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
   - Iterate 100 pieces and calculate the rms mean value of all the songs.(Because using “as.data.frame” to calculate the mean causes the “group_by(Piece,Bar)” function in HumdrumR to be broken, if the input is 
     the entire Cocopops corpus, the result is the unique mean of all pieces and all phrases.) Extract the rms average of all pieces and write them to a list, convert to a data frame, and finally visualize them.
     ```
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
   - For subsequent correlation analysis, we also normalized the average rms value of all pieces from small to large to 0-100 as the rms score of each song and visualize them.
     ```
     min_value_means_col1 <- min(mean_col1_df$mean_col1)
     max_value_means_col1 <- max(mean_col1_df$mean_col1)
     Rms_Score = (mean_col1_df$mean_col1- min_value_means_col1) / (max_value_means_col1 - min_value_means_col1) * 100
     Rms_Score_df <- data.frame(Piece = seq_along(Rms_Score), Rms_Score = Rms_Score)
     ggplot(Rms_Score_df, aes(x = Piece, y = Rms_Score)) +
         geom_point(size = 3, color = "black") +  
         labs(
           x = "Piece", 
           y = "RMS Score", 
           title = "RMS Score Plot"
         ) +
         theme_minimal()
     ggsave("RMS Score.png", width = 8, height = 6, dpi = 800)
     ```
     <img src="Fig/pic/pieces/RMS%20Score.png" alt="Plot" style="background-color: white; padding: 10px;">






