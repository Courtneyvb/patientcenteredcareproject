library(googlesheets4)
library(plyr)
library(dplyr)
library(lubridate)

survey_data_formatter <- function(){
  options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = TRUE,
    # specify auth tokens should be stored in a hidden directory ".secrets"
    gargle_oauth_cache = "./app_data/.secrets"
  )

  read_sheet('https://docs.google.com/spreadsheets/d/1V1b6-7YElXFuvsTR5kf58gtDCCXNNy0b-OII86-bs50/edit?gid=0#gid=0',
             sheet = 1, col_types = c('c'))|>
    mutate(trial = ifelse(datetime >= ymd('2024-08-12'), 'Trial', 'Baseline'))
}

dataLikertTrans <- function(df, qname){
  data <-
    df |>
    filter(!is.na(!!sym(qname)))
  if (qname == 'med_change'){
    data <-
      data |>
      mutate(Rating = gsub("\\:.*", "", !!sym(qname)))
  }else{
    data <-
      data |>
      mutate(Rating = !!sym(qname))
  }
  z  <- data  |>
    group_by(trial) |>
    count()

  data |>
    group_by(Rating, trial) |>
    count() |>
    ungroup(Rating) |>
    mutate('Proportion' = n/sum(n),
           'Percentage' = Proportion*100,
           trial = ifelse(trial == 'Baseline',
                          paste0(trial, ' (n=', z$n[z$trial=='Baseline'], ")"),
                          paste0(trial, ' (n=', z$n[z$trial=='Trial'], ")"))
    ) |>
    rename('Group'='trial',
           'Count'='n')
}

likertPlot <- function(data, levs = c("Excellent", "Good", "Fair", "Poor", "Very Poor"),
                         title = "Likert Plot", subtitle = "Description of Plot", tmargin = 0){
  library(plotly)
  library(dplyr)

  data <- data %>%
    mutate(
      Rating = factor(Rating, levels = levs[c(1,2,5,4,3)], ordered = TRUE)
    )
  neg_data <- dataSplit(data, levs[c(4,5)], levs[3])|> mutate(Percentage = -Percentage)
  pos_data <- dataSplit(data, levs[c(1,2)], levs[3])
  pos_fair <-  dataSplit(data, levs[3], levs[3])
  neg_fair <-  dataSplit(data, levs[3], levs[3]) |> mutate(Percentage = -Percentage)
  leg_data <- dataSplit(data, levs, levs[3]) |>
    mutate(
      Percentage = 0,
      Rating = factor(Rating, levels = levs),
      hovertemplate =   ifelse(Rating != levs[3],
                               paste0(Rating, "<br>",  round(abs(Percentage),1), "%",
                                      "<br>Count:", Count,"<extra></extra>"),
                               paste0(Rating, "<br>",  round(abs(Percentage)*2,1), "%",
                                      "<br>Count:", Count, "<extra></extra>")))
  # Create the Likert plot
  fig <- ggplot(data = data, aes(x = Percentage, y = Group, fill = Rating)) +
    geom_bar(stat = "identity", position = "stack") +
    
    # Reverses Legend 
    guides(fill = guide_legend(reverse = TRUE)) +

    # Custom colors to match the original
    scale_fill_manual(values = c("#87aca3", "#ddd9ce", "#000000","#953553","#7f7f7f")) +
    #fbaa60
    
    # Customize theme and layout
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 16,
                                 colour = "black"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 16,
                                 colour = "black"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5,
                                face = "bold",
                                size = 18,
                                colour = "black"),
      plot.subtitle = element_text(hjust = 0.5,
                                   face = "italic",
                                   size = 12)
      # panel.grid.major.y = element_blank(),
      # panel.grid.minor.y = element_blank()
    ) +

    # Reverse the y-axis to match the original order
    scale_y_discrete(limits = rev(unique(leg_data$Group))) +

    # Add title
    ggtitle(title, subtitle)


  fig
}

dataSplit <- function(data, levs, mid_lev){
  data |>
    filter(
      Rating %in% levs) |>
    mutate( Percentage = ifelse(Rating != mid_lev, Percentage, Percentage/2),
            hovertemplate = ifelse(Rating != mid_lev,
                                   paste0(Rating, "<br>",  round(abs(Percentage),1), "%",
                                          "<br>Count:", Count,"<extra></extra>"),
                                   paste0(Rating, "<br>",  round(abs(Percentage)*2,1), "%",
                                          "<br>Count:", Count, "<extra></extra>")),

    )
}

# Get data ----------------------------------------------------------------

dfSurvey <- survey_data_formatter()

data <- dataLikertTrans(
  dfSurvey |>
    mutate(
      overall_collapsed = case_when(
        overall == "10 - Exceptional" ~ "Exceptional (9 or 10)",
        overall == "9" ~ "Exceptional (9 or 10)",
        overall == "8" ~ "Exceeds Expectations (7 or 8)",
        overall == "7 - Exceeds Expectations" ~ "Exceeds Expectations (7 or 8)",
        overall == "6" ~ "Meets Expectations (5 or 6)",
        overall == "5 - Meets Expectations" ~ "Meets Expectations (5 or 6)",
        overall == "4" ~ "Below Expectations (3 or 4)",
        overall == "3 - Below Expectations" ~ "Below Expectations (3 or 4)",
        overall == "2" ~ "Needs Improvement (1 or 2)",
        overall == "1 - Needs Improvement" ~ "Needs Improvement (1 or 2)",
      )
    ),
  'overall_collapsed'
)
levs <- c("Exceptional (9 or 10)",
          "Exceeds Expectations (7 or 8)",
          "Meets Expectations (5 or 6)",
          "Below Expectations (3 or 4)",
          'Needs Improvement (1 or 2)')
likertPlot(data, levs, 
           title = "Patient Experience", 
           subtitle = "On a scale from 1-10, how was your overall experience today?",
             tmargin = 35)

ggsave("content/results/patient_experience_1.png", width = 12, height = 6)
