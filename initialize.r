# loads the *.csv into `df`,
# defines the functions,
# creates Question hierarchy in `layerQ`,

# Existing merge functions
merge_ResponseID <- function(char_vec) {
    char_vec <- str_replace(char_vec,"RESP025","RESP137")
    char_vec <- str_replace(char_vec,"RESP026","RESP172")
    char_vec <- str_replace(char_vec,"RESP029","RESP141")
    char_vec <- str_replace(char_vec,"RESP230","RESP020")
    char_vec <- str_replace(char_vec,"RESP231","RESP020")
    char_vec <- str_replace(char_vec,"RESP232","RESP020")
    char_vec <- str_replace(char_vec,"RESP196","RESP199")
    char_vec <- str_replace(char_vec,"RESP197","RESP199")
    char_vec <- str_replace(char_vec,"RESP198","RESP199")
    char_vec <- str_replace(char_vec,"RESP199","RESP199")
    char_vec <- str_replace(char_vec,"RESP200","RESP008")
    char_vec <- str_replace(char_vec,"RESP194","RESP005")
    char_vec <- str_replace(char_vec,"RESP195","RESP006")
    return(char_vec)
}

merge_Response <- function(ResponseID,Response) {
    idx <- str_detect(ResponseID,"RESP137")
    Response[idx] <- "Employed"
    idx <- str_detect(ResponseID,"RESP172")
    Response[idx] <- "Self-employed"
    idx <- str_detect(ResponseID,"RESP141")
    Response[idx] <- "Homemaker"
    idx <- str_detect(ResponseID,"RESP020")
    Response[idx] <- "$50,000+"
    idx <- str_detect(ResponseID,"RESP199")
    Response[idx] <- "A/A Native, Asian,Other"
    idx <- str_detect(ResponseID,"RESP008")
    Response[idx] <- "Multiracial"
    idx <- str_detect(ResponseID,"RESP005")
    Response[idx] <- "White"
    idx <- str_detect(ResponseID,"RESP006")
    Response[idx] <- "Black"
    # make all responses lower case
    Response <- tolower(Response)
    return(Response)
}

merge_BreakoutID <- function(char_vec) {
    char_vec <- str_replace(char_vec,"INCOME01","INCOME1")
    char_vec <- str_replace(char_vec,"INCOME02","INCOME2")
    char_vec <- str_replace(char_vec,"INCOME03","INCOME3")
    char_vec <- str_replace(char_vec,"INCOME04","INCOME4")
    char_vec <- str_replace(char_vec,"INCOME05","INCOME5")
    char_vec <- str_replace(char_vec,"INCOME06","INCOME5")
    char_vec <- str_replace(char_vec,"INCOME07","INCOME5")
    char_vec <- str_replace(char_vec,"RACE01","RACE1")
    char_vec <- str_replace(char_vec,"RACE02","RACE2")
    char_vec <- str_replace(char_vec,"RACE08","RACE3")
    char_vec <- str_replace(char_vec,"RACE04","RACE4")
    char_vec <- str_replace(char_vec,"RACE05","RACE4")
    char_vec <- str_replace(char_vec,"RACE06","RACE4")
    char_vec <- str_replace(char_vec,"RACE03","RACE4")
    char_vec <- str_replace(char_vec,"RACE07","RACE5")
    return(char_vec) 
}

merge_Break_Out <- function(BreakoutID,Break_Out) {
    idx <- str_detect(BreakoutID,"INCOME5")
    Break_Out[idx] <- "$50,000+"
    idx <- str_detect(BreakoutID,"RACE1")
    Break_Out[idx] <- "White"
    idx <- str_detect(BreakoutID,"RACE2")
    Break_Out[idx] <- "Black"
    idx <- str_detect(BreakoutID,"RACE3")
    Break_Out[idx] <- "Hispanic"
    idx <- str_detect(BreakoutID,"RACE4")
    Break_Out[idx] <- "A/A Native, Asian,Other"
    idx <- str_detect(BreakoutID,"RACE5")
    Break_Out[idx] <- "Multiracial"
    return(Break_Out)
}

# NEW: Generic aggregation function with granularity
aggregate_by_category <- function(qDf, category_id, granularity = 1) {
  
  base_data <- qDf |>
    filter(BreakOutCategoryID == category_id)
  
  # Granularity Level 1: Simple aggregate by breakout
  if (granularity == 1) {
    plotDf <- base_data |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response = Response,
              persons = persons,
              agg_ss = sum(persons)) |>
      group_by(Break_Out, Response) |>
      reframe(agg_ss = agg_ss,
              agg_persons = sum(persons),
              agg_percent = agg_persons * 100 / agg_ss,
              agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
              agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
              agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
  }
  
  # Granularity Level 2: Add Year breakdown
  else if (granularity == 2) {
    plotDf <- base_data |>
      select(Break_Out, Year, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out, Year) |>
      reframe(Response = Response,
              persons = persons,
              agg_ss = sum(persons)) |>
      group_by(Break_Out, Year, Response) |>
      reframe(agg_ss = agg_ss,
              agg_persons = sum(persons),
              agg_percent = agg_persons * 100 / agg_ss,
              agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
              agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
              agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
  }
  
  # Granularity Level 3: Add Location breakdown (top 12 states)
  else if (granularity == 3) {
    plotDf <- base_data |>
      select(Break_Out, Locationabbr, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out, Locationabbr) |>
      reframe(Response = Response,
              persons = persons,
              agg_ss = sum(persons)) |>
      group_by(Break_Out, Locationabbr, Response) |>
      reframe(agg_ss = agg_ss,
              agg_persons = sum(persons),
              agg_percent = agg_persons * 100 / agg_ss,
              agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
              agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
              agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
  }
  
  # Granularity Level 4: Show confidence intervals
  else if (granularity == 4) {
    plotDf <- base_data |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response = Response,
              persons = persons,
              agg_ss = sum(persons)) |>
      group_by(Break_Out, Response) |>
      reframe(agg_ss = agg_ss,
              agg_persons = sum(persons),
              agg_percent = agg_persons * 100 / agg_ss,
              agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
              agg_low_ci_limit = agg_percent - 2 * agg_percent_sdev,
              agg_high_ci_limit = agg_percent + 2 * agg_percent_sdev) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
  }
  
  return(plotDf)
}

# NEW: Generic plotting function with granularity support
create_granular_plot <- function(plotDf, x_var = "Break_Out", granularity = 1) {
  
  # Base plot
  if (granularity == 1) {
    p <- plotDf |>
      ggplot(aes(x = !!sym(x_var), y = agg_percent,
                 fill = Response, color = Response)) +
      geom_col(position = "fill") +
      theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust = 1),
            legend.position = "none")
  }
  
  # Faceted by Year
  else if (granularity == 2) {
    p <- plotDf |>
      ggplot(aes(x = !!sym(x_var), y = agg_percent,
                 fill = Response, color = Response)) +
      geom_col(position = "fill") +
      facet_wrap(~Year, ncol = 3) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7),
            legend.position = "bottom",
            legend.text = element_text(size = 8))
  }
  
  # Faceted by Location (top 12 states by sample size)
  else if (granularity == 3) {
    # Get top 12 locations by total sample
    top_locations <- plotDf |>
      group_by(Locationabbr) |>
      summarize(total = n()) |>
      arrange(desc(total)) |>
      head(12) |>
      pull(Locationabbr)
    
    p <- plotDf |>
      filter(Locationabbr %in% top_locations) |>
      ggplot(aes(x = !!sym(x_var), y = agg_percent,
                 fill = Response, color = Response)) +
      geom_col(position = "fill") +
      facet_wrap(~Locationabbr, ncol = 4) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
            legend.position = "bottom",
            legend.text = element_text(size = 7),
            strip.text = element_text(size = 8))
  }
  
  # With confidence intervals
  else if (granularity == 4) {
    p <- plotDf |>
      ggplot(aes(x = !!sym(x_var), y = agg_percent,
                 fill = Response, color = Response)) +
      geom_col(position = "dodge") +
      geom_errorbar(aes(ymin = agg_low_ci_limit, ymax = agg_high_ci_limit),
                    position = position_dodge(width = 0.9), width = 0.25) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            legend.position = "right")
  }
  
  return(p)
}

# load data
library(tidyverse)
library(utils)
options(dplyr.summarise.inform = FALSE)

fileList <- dir(recursive = TRUE)
fileName <- str_subset(fileList, "Prevalence.*[.]csv")
# Fallback if no CSV found (prevents crash if file is missing in some environments)
if(length(fileName) > 0) {
    df <- read_csv(fileName, show_col_types = FALSE)

    # creates question hierarchy in layerQ
    layerQ <- df |>
        select(Class, Topic, Question) |>
        distinct()

    # Create justQ for app.R - unique questions only
    justQ <- df |>
      select(Question) |>
      distinct() |>
      arrange(Question)
} else {
    # Placeholder if no data - allows app to launch but will be empty
    df <- data.frame(Question = character(), Class = character(), Topic = character())
    layerQ <- data.frame(Class = character(), Topic = character(), Question = character())
    justQ <- data.frame(Question = character())
}