rm(list=ls())

# Choose the maximum number of children to include in this analysis by
# setting max_children

# --- Load libraries --- 

library(purrr)
library(glmnet)
library(tidyverse)
library(AER)
library(lmtest)
library(readr)
library(stringr)
library(tidyverse)
library(purrr)
library(tibble)
library(stringi)
library(broom)
library(stargazer)
library(sandwich)
library(lmtest)
library(broom) # For cleaning data
library(quantreg)
library(tibble)
library(knitr)
library(rifreg) # For UQR
library(ivmodel) # For Weak Iv

### For BMA
library(BAS)
library(BMS)
library(modelsummary)



### Load the data

set.seed(100)

# Set working directory
setwd("C:/Users/nicho/OneDrive/Documents/research/Intergenerational Support/Data")

# Load data
df <- read_csv("SWB_data_working.csv")


### Clean the data

# Cleaning the variable names 
colnames(df) <- gsub("...45","", colnames(df) , fixed = T)
colnames(df)[45] <- "B32a. Child 1 - If yes, how much per month?"  
colnames(df)[64] <-"B32a. Child 2 - If yes, how much per month?"
colnames(df)<- gsub("B3. Think about your interactions and relationships with each child in the past six months.\n\n","", colnames(df),fixed = T)

# Clean the child gender names

colnames(df)[34] <- "B22. Child 1 - Gender"
colnames(df)[53] <- "B22. Child 2 - Gender"
colnames(df)[72] <- "B22. Child 3 - Gender"
colnames(df)[91] <- "B22. Child 4 - Gender"
colnames(df)[110]  <- "B22. Child 5 - Gender"

# Rename df$`C13. Do you belong to a religion or religious denomination? 
# If yes, which one? - Selected Choice` to religion

colnames(df)[150] <- "religion"


# Check for the distribution of children

df$num_children_total <-
  df$`B1. How many children do you have? - How many sons do you have?` +
  df$`B1. How many children do you have? - How many daughters do you have?`

child_dist <- df |>
  dplyr::count(num_children_total, name = "n") |>
  dplyr::mutate(percent = 100 * n / sum(n))

child_dist


# Construct a sampling approach dummy (=1 if conveience sampling)
# --- 244 observations are obtained through convenience sampling ---
df$sampling <- ifelse(
  nchar(as.character(df$`Please indicate the respondent's number.`)) == 8, 
  1, 
  0
) 

# --- Check if children number corresponds with birth order ---

# Child birth-year columns
birth_year_cols <- paste0("B23. Child ", 1:5, " - Year of Birth")

# Convert to numeric
birth_years <- df[, birth_year_cols]
birth_years <- as.data.frame(lapply(birth_years, as.numeric))

# Function: checks whether child records are ordered older-to-younger
check_birth_order <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA)
  all(diff(x) >= 0)
}

df$birth_order_old_to_young <- apply(birth_years, 1, check_birth_order)

# Overall check among respondents with at least two child birth years
df$n_child_birth_years <- rowSums(!is.na(birth_years))

table(df$birth_order_old_to_young[df$n_child_birth_years >= 2], useNA = "ifany")

mean(df$birth_order_old_to_young[df$n_child_birth_years >= 2], na.rm = TRUE)


### Construct or refine variables

# --- Demographics and core variables --- 

# Declare religion as a factor variable
df$religion <- factor(df$religion)
df$religion <- relevel(df$religion, ref = "No religion")

# Construct/refine variables
df <- df %>%
  mutate(
    # Gender (C1)
    gender = ifelse(`C1. Your gender:` == "Male", 1, 0),
    
    # Age (C3) - remove + and correct wrong age entry, i.e. 1955
    age_raw = as.numeric(str_remove_all(`C3. Your age in years:`, "[^0-9]")),
    age = ifelse(age_raw > 120, 2024 - age_raw, age_raw),

    # Ethnicity (C5)
    ethnicity = case_when(
      `C5. Your race: - Selected Choice` == "Chinese" ~ "Chinese",
      `C5. Your race: - Selected Choice` == "Malay" ~ "Malay",
      TRUE ~ "Other"
    ),
    
    # Marital status (C6)
    married = ifelse(`C6. Your current marital status:` == "Currently married", 1, 0),
    
    # Education group (C7)
    education = case_when(
      `C7. Your educational level:` %in% c("Secondary (‘O’ / ‘N’ level)") ~ "Secondary",
      `C7. Your educational level:` %in% c("Polytechnic diploma", "Post-secondary (non-tertiary): General & Vocational (‘A’ level)", "Professional qualification and other diploma") ~ "Polytechnic and A levels",
      `C7. Your educational level:` %in% c("Bachelor’s or equivalent", "Postgraduate diploma/certificate (excluding Master’s and Doctorate)", "Master’s and Doctorate or equivalent") ~ "Degree holders",
      TRUE ~ "Other"
    ),
    
    # Employment Status (C8) Recode employment status into dummy: 1 = Employed, 0 = Unemployed
    employed = case_when(
      `C8. Your employment status: - Selected Choice` %in% c(
        "Full-time employee (30 hours a week or more)",
        "Part-time employee",
        "Self-employed"
      ) ~ 1,
      `C8. Your employment status: - Selected Choice` %in% c(
        "Retired (record previous occupation), please specifiy:",
        "Homemaker/housewife",
        "Unemployed (able to work) (record previous occupation), please specify:",
        "Unemployed (unable to work) due to disability or other medical conditions (record previous occupation, if any), please specify:",
        "Others, please specify:"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Personal Income (C10)
    personal_income = case_when(
      `C9. Personal monthly income:` == "Below $1,000" ~ 500,
      `C9. Personal monthly income:` == "$1,000 - $1,999" ~ 1500,
      `C9. Personal monthly income:` == "$2,000 - $2,999" ~ 2500,
      `C9. Personal monthly income:` == "$3,000 - $3,999" ~ 3500,
      `C9. Personal monthly income:` == "$4,000 - $4,999" ~ 4500,
      `C9. Personal monthly income:` == "$5,000 - $6,999" ~ 6000,
      `C9. Personal monthly income:` == "$6,000 - $6,999" ~ 6500,
      `C9. Personal monthly income:` == "$7,000 - $7,999" ~ 7500,
      `C9. Personal monthly income:` == "$8,000 - $8,999" ~ 8500,
      `C9. Personal monthly income:` == "$9,000 - $9,999" ~ 9500,
      `C9. Personal monthly income:` == "$10,000 and above" ~ 11000,
      `C9. Personal monthly income:` == "Respondent refused to answer" ~ NA_real_,
      TRUE ~ NA_real_
  ),
  # Take the log of personal income
  log_personal_income = log(personal_income + 1),
  
  # Household income (C10)
  hh_income = case_when(
    `C10. Household monthly income:` == "Below $1,000" ~ 500,
    `C10. Household monthly income:` == "$1,000 - $1,999" ~ 1500,
    `C10. Household monthly income:` == "$2,000 - $2,999" ~ 2500,
    `C10. Household monthly income:` == "$3,000 - $3,999" ~ 3500,
    `C10. Household monthly income:` == "$4,000 - $4,999" ~ 4500,
    `C10. Household monthly income:` == "$5,000 - $6,999" ~ 6000,
    `C10. Household monthly income:` == "$6,000 - $6,999" ~ 6500,
    `C10. Household monthly income:` == "$7,000 - $7,999" ~ 7500,
    `C10. Household monthly income:` == "$8,000 - $8,999" ~ 8500,
    `C10. Household monthly income:` == "$9,000 - $9,999" ~ 9500,
    `C10. Household monthly income:` == "$10,000 and above" ~ 11000,
    `C10. Household monthly income:` == "Respondent refused to answer" ~ NA_real_,
    TRUE ~ NA_real_
  ),
  # Take the log of personal income
  log_hh_income = log(hh_income + 1),
  
  
  # Housing type (C11) - Classify housing type into broad categories
    housing = case_when(
        `C11. Is this house you are currently being interviewed? - Selected Choice` %in% c(
          "HDB 1- and 2-Room Flats", "HDB 3-room flats"
        ) ~ "1–3 rooms",
        `C11. Is this house you are currently being interviewed? - Selected Choice` == "HDB 4-room flats" ~ "4 rooms",
        `C11. Is this house you are currently being interviewed? - Selected Choice` == "HDB 5-room, 3 gen flats, and executive flats (e.g., executive apartments and executive maisonette)" ~ "5 rooms",
        `C11. Is this house you are currently being interviewed? - Selected Choice` %in% c(
          "Condominiums (including executive condo) and other apartments", "Landed properties"
        ) ~ "Private",
        TRUE ~ "Other"),
    
  # Home Ownership (C12)
  home_ownership = case_when(
    `C12. Do you own this house you are currently being interviewed? - Selected Choice` %in% c(
      "Self-owned", "Co-owned by self and spouse", "Co-owned by self and a child") ~ 1,
    TRUE ~ 0
  ),
  
  # C13 is on religion - declared as a factor earlier
  
  # C14. In general, how would you rate your health?
  # Health group
  health = case_when(
    `C14. In general, how would you rate your health?` %in% c("Very poor", "Poor") ~ 0,
    `C14. In general, how would you rate your health?` == "Fair" ~ 0,
    TRUE ~ 1
  ),
  
  # C15. Overall, in the last 30 days, how much difficulty did you have with moving around?
  mobility_difficulty = case_when(
    `C15. Overall, in the last 30 days, how much difficulty did you have with moving around?` %in% c("Moderate", "Severe", "Extreme / cannot do") ~ 1,
    `C15. Overall, in the last 30 days, how much difficulty did you have with moving around?` %in% c("None", "Mild") ~ 0,
    TRUE ~ NA_real_
  ),
  
  # C16. Do you have enough money to meet your needs?
  has_enough_money = case_when(
    `C16. Do you have enough money to meet your needs?` %in% c("Mostly", "Completely") ~ 1,
    `C16. Do you have enough money to meet your needs?` %in% c("Moderately", "A little", "None at all") ~ 0,
    TRUE ~ NA_real_
  ),
  
  # C18. In general, how satisfied are you with your relationship with your children?
  rel_satisfaction_score = case_when(
    `C18. In general, how satisfied are you with your relationship with your children?` == "Not at all" ~ 1,
    `C18. In general, how satisfied are you with your relationship with your children?` == "A little" ~ 2,
    `C18. In general, how satisfied are you with your relationship with your children?` == "Somewhat" ~ 3,
    `C18. In general, how satisfied are you with your relationship with your children?` == "Mostly" ~ 4,
    `C18. In general, how satisfied are you with your relationship with your children?` == "Almost completely" ~ 5,
    `C18. In general, how satisfied are you with your relationship with your children?` == "Completely" ~ 6,
    TRUE ~ NA_real_
  ),
  
  # C19. In general, how satisfied are you with your relationship with your friends?
  friend_satisfaction_score = case_when(
    `C19. In general, how satisfied are you with your relationship with your friends?` == "Not at all" ~ 1,
    `C19. In general, how satisfied are you with your relationship with your friends?` == "A little" ~ 2,
    `C19. In general, how satisfied are you with your relationship with your friends?` == "Somewhat" ~ 3,
    `C19. In general, how satisfied are you with your relationship with your friends?` == "Mostly" ~ 4,
    `C19. In general, how satisfied are you with your relationship with your friends?` == "Almost completely" ~ 5,
    `C19. In general, how satisfied are you with your relationship with your friends?` == "Completely" ~ 6,
    TRUE ~ NA_real_
  )
  
)

# --- Constructing the Number of Children --- 
# Number of sons and daughters
df = df %>% mutate(
  sons = as.numeric(`B1. How many children do you have? - How many sons do you have?`),
  daughters = as.numeric(`B1. How many children do you have? - How many daughters do you have?`),
  num_children = ifelse(is.na(sons) & is.na(daughters), NA, rowSums(cbind(sons, daughters), na.rm = TRUE))
)


# --- Constructing the Z relationship variables --- 

# Child 1
df <- df %>%
  mutate(
    closeness_child1 = case_when(
      `B31. Child 1 - How close are you to the child?` == "Very close" ~ 5,
      `B31. Child 1 - How close are you to the child?` == "Close" ~ 4,
      `B31. Child 1 - How close are you to the child?` == "Fair" ~ 3,
      `B31. Child 1 - How close are you to the child?` == "Not very close" ~ 2,
      `B31. Child 1 - How close are you to the child?` == "Not close at all" ~ 1,
      TRUE ~ NA_real_
    ),
    
    distance_km_child1 = case_when(
      `B28. Child 1 - Distance from your residence` == "Living together in the same house" ~ 0.01,
      `B28. Child 1 - Distance from your residence` == "Staying in the same or next block" ~ 0.1,
      `B28. Child 1 - Distance from your residence` == "Staying in the same housing estate" ~ 0.7,
      `B28. Child 1 - Distance from your residence` == "Within 20 minutes by walk" ~ 1.5,
      `B28. Child 1 - Distance from your residence` == "Within 20 minutes by car" ~ 8,
      `B28. Child 1 - Distance from your residence` == "Between 20 and 40 minutes by car" ~ 15,
      `B28. Child 1 - Distance from your residence` == "More than 40 minutes by car" ~ 30,
      `B28. Child 1 - Distance from your residence` == "Staying in a different country" ~ 300,
      TRUE ~ NA_real_
    )
  )

# Child 2
df <- df %>%
  mutate(
    closeness_child2 = case_when(
      `B31. Child 2 - How close are you to the child?` == "Very close" ~ 5,
      `B31. Child 2 - How close are you to the child?` == "Close" ~ 4,
      `B31. Child 2 - How close are you to the child?` == "Fair" ~ 3,
      `B31. Child 2 - How close are you to the child?` == "Not very close" ~ 2,
      `B31. Child 2 - How close are you to the child?` == "Not close at all" ~ 1,
      TRUE ~ NA_real_
    ),
    
    distance_km_child2 = case_when(
      `B28. Child 2 - Distance from your residence` == "Living together in the same house" ~ 0.01,
      `B28. Child 2 - Distance from your residence` == "Staying in the same or next block" ~ 0.1,
      `B28. Child 2 - Distance from your residence` == "Staying in the same housing estate" ~ 0.7,
      `B28. Child 2 - Distance from your residence` == "Within 20 minutes by walk" ~ 1.5,
      `B28. Child 2 - Distance from your residence` == "Within 20 minutes by car" ~ 8,
      `B28. Child 2 - Distance from your residence` == "Between 20 and 40 minutes by car" ~ 15,
      `B28. Child 2 - Distance from your residence` == "More than 40 minutes by car" ~ 30,
      `B28. Child 2 - Distance from your residence` == "Staying in a different country" ~ 300,
      TRUE ~ NA_real_
    )
  )

# Child 3
df <- df %>%
  mutate(
    closeness_child3 = case_when(
      `B31. Child 3 - How close are you to the child?` == "Very close" ~ 5,
      `B31. Child 3 - How close are you to the child?` == "Close" ~ 4,
      `B31. Child 3 - How close are you to the child?` == "Fair" ~ 3,
      `B31. Child 3 - How close are you to the child?` == "Not very close" ~ 2,
      `B31. Child 3 - How close are you to the child?` == "Not close at all" ~ 1,
      TRUE ~ NA_real_
    ),
    
    distance_km_child3 = case_when(
      `B28. Child 3 - Distance from your residence` == "Living together in the same house" ~ 0.01,
      `B28. Child 3 - Distance from your residence` == "Staying in the same or next block" ~ 0.1,
      `B28. Child 3 - Distance from your residence` == "Staying in the same housing estate" ~ 0.7,
      `B28. Child 3 - Distance from your residence` == "Within 20 minutes by walk" ~ 1.5,
      `B28. Child 3 - Distance from your residence` == "Within 20 minutes by car" ~ 8,
      `B28. Child 3 - Distance from your residence` == "Between 20 and 40 minutes by car" ~ 15,
      `B28. Child 3 - Distance from your residence` == "More than 40 minutes by car" ~ 30,
      `B28. Child 3 - Distance from your residence` == "Staying in a different country" ~ 300,
      TRUE ~ NA_real_
    )
  )

# Child 4
df <- df %>%
  mutate(
    closeness_child4 = case_when(
      `B31. Child 4 - How close are you to the child?` == "Very close" ~ 5,
      `B31. Child 4 - How close are you to the child?` == "Close" ~ 4,
      `B31. Child 4 - How close are you to the child?` == "Fair" ~ 3,
      `B31. Child 4 - How close are you to the child?` == "Not very close" ~ 2,
      `B31. Child 4 - How close are you to the child?` == "Not close at all" ~ 1,
      TRUE ~ NA_real_
    ),
    
    distance_km_child4 = case_when(
      `B28. Child 4 - Distance from your residence` == "Living together in the same house" ~ 0.01,
      `B28. Child 4 - Distance from your residence` == "Staying in the same or next block" ~ 0.1,
      `B28. Child 4 - Distance from your residence` == "Staying in the same housing estate" ~ 0.7,
      `B28. Child 4 - Distance from your residence` == "Within 20 minutes by walk" ~ 1.5,
      `B28. Child 4 - Distance from your residence` == "Within 20 minutes by car" ~ 8,
      `B28. Child 4 - Distance from your residence` == "Between 20 and 40 minutes by car" ~ 15,
      `B28. Child 4 - Distance from your residence` == "More than 40 minutes by car" ~ 30,
      `B28. Child 4 - Distance from your residence` == "Staying in a different country" ~ 300,
      TRUE ~ NA_real_
    )
  )

# Child 5
df <- df %>%
  mutate(
    closeness_child5 = case_when(
      `B31. Child 5 - How close are you to the child?` == "Very close" ~ 5,
      `B31. Child 5 - How close are you to the child?` == "Close" ~ 4,
      `B31. Child 5 - How close are you to the child?` == "Fair" ~ 3,
      `B31. Child 5 - How close are you to the child?` == "Not very close" ~ 2,
      `B31. Child 5 - How close are you to the child?` == "Not close at all" ~ 1,
      TRUE ~ NA_real_
    ),
    
    distance_km_child5 = case_when(
      `B28. Child 5 - Distance from your residence` == "Living together in the same house" ~ 0.01,
      `B28. Child 5 - Distance from your residence` == "Staying in the same or next block" ~ 0.1,
      `B28. Child 5 - Distance from your residence` == "Staying in the same housing estate" ~ 0.7,
      `B28. Child 5 - Distance from your residence` == "Within 20 minutes by walk" ~ 1.5,
      `B28. Child 5 - Distance from your residence` == "Within 20 minutes by car" ~ 8,
      `B28. Child 5 - Distance from your residence` == "Between 20 and 40 minutes by car" ~ 15,
      `B28. Child 5 - Distance from your residence` == "More than 40 minutes by car" ~ 30,
      `B28. Child 5 - Distance from your residence` == "Staying in a different country" ~ 300,
      TRUE ~ NA_real_
    )
  )



df <- df %>%
  mutate(
    avg_closeness = rowMeans(select(., starts_with("closeness_child")), na.rm = TRUE),
    avg_distance_km = rowMeans(select(., starts_with("distance_km_child")), na.rm = TRUE)
  )


# --- Define parent-level binary indicators of support --- 

# Child 1

df <- df %>%
  mutate(
    
    # For Financial Support
    
    amount_given1_clean = str_squish(`B32a. Child 1 - If yes, how much per month?`),
    amount_received1_clean = str_squish(`B35a. Child 1 - If yes, how much per month?`),
    
    give_financial1 = case_when(
      `B32. Child 1 - Did you offer the child economic support?` == "Yes" &
        amount_given1_clean == "Less than $200" ~ 100,
      amount_given1_clean == "$200-399" ~ 300,
      amount_given1_clean == "$400-599" ~ 500,
      amount_given1_clean == "$600-799" ~ 700,
      amount_given1_clean == "$800-999" ~ 900,
      amount_given1_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    receive_financial1 = case_when(
      `B35. Child 1 - Did the child give you economic support?` == "Yes" &
        amount_received1_clean == "Less than $200" ~ 100,
      amount_received1_clean == "$200-399" ~ 300,
      amount_received1_clean == "$400-599" ~ 500,
      amount_received1_clean == "$600-799" ~ 700,
      amount_received1_clean == "$800-999" ~ 900,
      amount_received1_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    # Instrumental support (housework)
    give_housework1_cont = case_when(
      `B33. Child 1 - How often did you help the child with housework?` == "Never" ~ 0,
      `B33. Child 1 - How often did you help the child with housework?` == "Once every few months" ~ 1/90,
      `B33. Child 1 - How often did you help the child with housework?` == "Once a month" ~ 1/30,
      `B33. Child 1 - How often did you help the child with housework?` == "2-3 days a month" ~ 2.5/30,
      `B33. Child 1 - How often did you help the child with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B33. Child 1 - How often did you help the child with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B33. Child 1 - How often did you help the child with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    # Instrumental support (grandchild care)
    give_grandchildcare1_cont = case_when(
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "Never" ~ 0,
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "Once every few months" ~ 1/90,
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "Once a month" ~ 1/30,
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "2-3 days a month" ~ 2.5/30,
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "Almost everyday" ~ 26/30,
      `B34. Child 1 - How often did you provide grandchild care to the child?` == "Not applicable (the child has no child)" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Final: max of the two, per your latest instruction
    give_instrumental1 = pmax(give_housework1_cont, give_grandchildcare1_cont, na.rm = TRUE),
    
    
    # Received instrumental support
    receive_housework1_cont = case_when(
      `B36. Child 1 - How often did the child help you with housework?` == "Never" ~ 0,
      `B36. Child 1 - How often did the child help you with housework?` == "Once every few months" ~ 1/90,
      `B36. Child 1 - How often did the child help you with housework?` == "Once a month" ~ 1/30,
      `B36. Child 1 - How often did the child help you with housework?` == "2-3 days a month" ~ 2.5/30,
      `B36. Child 1 - How often did the child help you with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B36. Child 1 - How often did the child help you with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B36. Child 1 - How often did the child help you with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    receive_care1_score = case_when(
      `B37. Child 1 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Never" ~ 1,
      `B37. Child 1 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Rarely" ~ 2,
      `B37. Child 1 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Sometimes" ~ 3,
      `B37. Child 1 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Often" ~ 4,
      `B37. Child 1 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Always" ~ 5,
      TRUE ~ NA_real_
    ),
    
    # Final: take the maximum of both
    receive_instrumental1 = pmax(receive_housework1_cont, receive_care1_score/5, na.rm = TRUE),

    # Emotional Support 
    receive_emotional1 = case_when(
      `B38. Child 1 - How often did the child spend time chatting with and listening to you?` == "Never" ~ 0,
      `B38. Child 1 - How often did the child spend time chatting with and listening to you?` == "Once every few months" ~ 1/90,
      `B38. Child 1 - How often did the child spend time chatting with and listening to you?` == "Once a month" ~ 1/30,
      `B38. Child 1 - How often did the child spend time chatting with and listening to you?` == "2-3 days a month" ~ 2.5/30,
      `B38. Child 1 - How often did the child spend time chatting with and listening to you?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B38. Child 1 - How often did the child spend time chatting with and listening to you?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B38. Child 1 - How often did the child spend time chatting with and listening to you?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    )
  )


### Code for Child 2

df <- df %>%
  mutate(
    
    # Clean amount inputs for financial support
    amount_given2_clean = str_squish(`B32a. Child 2 - If yes, how much per month?`),
    amount_received2_clean = str_squish(`B35a. Child 2 - If yes, how much per month?`),
    
    # Financial support
    give_financial2 = case_when(
      `B32. Child 2 - Did you offer the child economic support?` == "Yes" &
        amount_given2_clean == "Less than $200" ~ 100,
      amount_given2_clean == "$200-399" ~ 300,
      amount_given2_clean == "$400-599" ~ 500,
      amount_given2_clean == "$600-799" ~ 700,
      amount_given2_clean == "$800-999" ~ 900,
      amount_given2_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    receive_financial2 = case_when(
      `B35. Child 2 - Did the child give you economic support?` == "Yes" &
        amount_received2_clean == "Less than $200" ~ 100,
      amount_received2_clean == "$200-399" ~ 300,
      amount_received2_clean == "$400-599" ~ 500,
      amount_received2_clean == "$600-799" ~ 700,
      amount_received2_clean == "$800-999" ~ 900,
      amount_received2_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    # Instrumental support (housework)
    give_housework2_cont = case_when(
      `B33. Child 2 - How often did you help the child with housework?` == "Never" ~ 0,
      `B33. Child 2 - How often did you help the child with housework?` == "Once every few months" ~ 1/90,
      `B33. Child 2 - How often did you help the child with housework?` == "Once a month" ~ 1/30,
      `B33. Child 2 - How often did you help the child with housework?` == "2-3 days a month" ~ 2.5/30,
      `B33. Child 2 - How often did you help the child with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B33. Child 2 - How often did you help the child with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B33. Child 2 - How often did you help the child with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    give_grandchildcare2_cont = case_when(
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "Never" ~ 0,
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "Once every few months" ~ 1/90,
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "Once a month" ~ 1/30,
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "2-3 days a month" ~ 2.5/30,
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "Almost everyday" ~ 26/30,
      `B34. Child 2 - How often did you provide grandchild care to the child?` == "Not applicable (the child has no child)" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    give_instrumental2 = pmax(give_housework2_cont, give_grandchildcare2_cont, na.rm = TRUE),
    
    # Instrumental support (received)
    receive_housework2_cont = case_when(
      `B36. Child 2 - How often did the child help you with housework?` == "Never" ~ 0,
      `B36. Child 2 - How often did the child help you with housework?` == "Once every few months" ~ 1/90,
      `B36. Child 2 - How often did the child help you with housework?` == "Once a month" ~ 1/30,
      `B36. Child 2 - How often did the child help you with housework?` == "2-3 days a month" ~ 2.5/30,
      `B36. Child 2 - How often did the child help you with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B36. Child 2 - How often did the child help you with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B36. Child 2 - How often did the child help you with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    receive_care2_score = case_when(
      `B37. Child 2 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Never" ~ 1,
      `B37. Child 2 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Rarely" ~ 2,
      `B37. Child 2 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Sometimes" ~ 3,
      `B37. Child 2 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Often" ~ 4,
      `B37. Child 2 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Always" ~ 5,
      TRUE ~ NA_real_
    ),
    
    receive_instrumental2 = pmax(receive_housework2_cont, receive_care2_score/5, na.rm = TRUE),
    
    # Emotional support
    receive_emotional2 = case_when(
      `B38. Child 2 - How often did the child spend time chatting with and listening to you?` == "Never" ~ 0,
      `B38. Child 2 - How often did the child spend time chatting with and listening to you?` == "Once every few months" ~ 1/90,
      `B38. Child 2 - How often did the child spend time chatting with and listening to you?` == "Once a month" ~ 1/30,
      `B38. Child 2 - How often did the child spend time chatting with and listening to you?` == "2-3 days a month" ~ 2.5/30,
      `B38. Child 2 - How often did the child spend time chatting with and listening to you?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B38. Child 2 - How often did the child spend time chatting with and listening to you?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B38. Child 2 - How often did the child spend time chatting with and listening to you?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    )
  )


### Code for Child 3

df <- df %>%
  mutate(
    
    # Clean amount inputs for financial support
    amount_given3_clean = str_squish(`B32a. Child 3 - If yes, how much per month?`),
    amount_received3_clean = str_squish(`B35a. Child 3 - If yes, how much per month?`),
    
    # Financial support
    give_financial3 = case_when(
      `B32. Child 3 - Did you offer the child economic support?` == "Yes" &
        amount_given3_clean == "Less than $200" ~ 100,
      amount_given3_clean == "$200-399" ~ 300,
      amount_given3_clean == "$400-599" ~ 500,
      amount_given3_clean == "$600-799" ~ 700,
      amount_given3_clean == "$800-999" ~ 900,
      amount_given3_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    receive_financial3 = case_when(
      `B35. Child 3 - Did the child give you economic support?` == "Yes" &
        amount_received3_clean == "Less than $200" ~ 100,
      amount_received3_clean == "$200-399" ~ 300,
      amount_received3_clean == "$400-599" ~ 500,
      amount_received3_clean == "$600-799" ~ 700,
      amount_received3_clean == "$800-999" ~ 900,
      amount_received3_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    # Instrumental support (housework)
    give_housework3_cont = case_when(
      `B33. Child 3 - How often did you help the child with housework?` == "Never" ~ 0,
      `B33. Child 3 - How often did you help the child with housework?` == "Once every few months" ~ 1/90,
      `B33. Child 3 - How often did you help the child with housework?` == "Once a month" ~ 1/30,
      `B33. Child 3 - How often did you help the child with housework?` == "2-3 days a month" ~ 2.5/30,
      `B33. Child 3 - How often did you help the child with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B33. Child 3 - How often did you help the child with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B33. Child 3 - How often did you help the child with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    give_grandchildcare3_cont = case_when(
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "Never" ~ 0,
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "Once every few months" ~ 1/90,
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "Once a month" ~ 1/30,
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "2-3 days a month" ~ 2.5/30,
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "Almost everyday" ~ 26/30,
      `B34. Child 3 - How often did you provide grandchild care to the child?` == "Not applicable (the child has no child)" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    give_instrumental3 = pmax(give_housework3_cont, give_grandchildcare3_cont, na.rm = TRUE),
    
    # Instrumental support (received)
    receive_housework3_cont = case_when(
      `B36. Child 3 - How often did the child help you with housework?` == "Never" ~ 0,
      `B36. Child 3 - How often did the child help you with housework?` == "Once every few months" ~ 1/90,
      `B36. Child 3 - How often did the child help you with housework?` == "Once a month" ~ 1/30,
      `B36. Child 3 - How often did the child help you with housework?` == "2-3 days a month" ~ 2.5/30,
      `B36. Child 3 - How often did the child help you with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B36. Child 3 - How often did the child help you with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B36. Child 3 - How often did the child help you with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    receive_care3_score = case_when(
      `B37. Child 3 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Never" ~ 1,
      `B37. Child 3 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Rarely" ~ 2,
      `B37. Child 3 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Sometimes" ~ 3,
      `B37. Child 3 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Often" ~ 4,
      `B37. Child 3 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Always" ~ 5,
      TRUE ~ NA_real_
    ),
    
    receive_instrumental3 = pmax(receive_housework3_cont, receive_care3_score/5, na.rm = TRUE),
    
    
    # Emotional support
    receive_emotional3 = case_when(
      `B38. Child 3 - How often did the child spend time chatting with and listening to you?` == "Never" ~ 0,
      `B38. Child 3 - How often did the child spend time chatting with and listening to you?` == "Once every few months" ~ 1/90,
      `B38. Child 3 - How often did the child spend time chatting with and listening to you?` == "Once a month" ~ 1/30,
      `B38. Child 3 - How often did the child spend time chatting with and listening to you?` == "2-3 days a month" ~ 2.5/30,
      `B38. Child 3 - How often did the child spend time chatting with and listening to you?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B38. Child 3 - How often did the child spend time chatting with and listening to you?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B38. Child 3 - How often did the child spend time chatting with and listening to you?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    )
  )


### Code for Child 4

df <- df %>%
  mutate(
    
    # Clean amount inputs for financial support
    amount_given4_clean = str_squish(`B32a. Child 4 - If yes, how much per month?`),
    amount_received4_clean = str_squish(`B35a. Child 4 - If yes, how much per month?`),
    
    # Financial support
    give_financial4 = case_when(
      `B32. Child 4 - Did you offer the child economic support?` == "Yes" &
        amount_given4_clean == "Less than $200" ~ 100,
      amount_given4_clean == "$200-399" ~ 300,
      amount_given4_clean == "$400-599" ~ 500,
      amount_given4_clean == "$600-799" ~ 700,
      amount_given4_clean == "$800-999" ~ 900,
      amount_given4_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    receive_financial4 = case_when(
      `B35. Child 4 - Did the child give you economic support?` == "Yes" &
        amount_received4_clean == "Less than $200" ~ 100,
      amount_received4_clean == "$200-399" ~ 300,
      amount_received4_clean == "$400-599" ~ 500,
      amount_received4_clean == "$600-799" ~ 700,
      amount_received4_clean == "$800-999" ~ 900,
      amount_received4_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    # Instrumental support (housework)
    give_housework4_cont = case_when(
      `B33. Child 4 - How often did you help the child with housework?` == "Never" ~ 0,
      `B33. Child 4 - How often did you help the child with housework?` == "Once every few months" ~ 1/90,
      `B33. Child 4 - How often did you help the child with housework?` == "Once a month" ~ 1/30,
      `B33. Child 4 - How often did you help the child with housework?` == "2-3 days a month" ~ 2.5/30,
      `B33. Child 4 - How often did you help the child with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B33. Child 4 - How often did you help the child with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B33. Child 4 - How often did you help the child with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    give_grandchildcare4_cont = case_when(
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "Never" ~ 0,
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "Once every few months" ~ 1/90,
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "Once a month" ~ 1/30,
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "2-3 days a month" ~ 2.5/30,
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "Almost everyday" ~ 26/30,
      `B34. Child 4 - How often did you provide grandchild care to the child?` == "Not applicable (the child has no child)" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    give_instrumental4 = pmax(give_housework4_cont, give_grandchildcare4_cont, na.rm = TRUE),
    
    # Instrumental support (received)
    receive_housework4_cont = case_when(
      `B36. Child 4 - How often did the child help you with housework?` == "Never" ~ 0,
      `B36. Child 4 - How often did the child help you with housework?` == "Once every few months" ~ 1/90,
      `B36. Child 4 - How often did the child help you with housework?` == "Once a month" ~ 1/30,
      `B36. Child 4 - How often did the child help you with housework?` == "2-3 days a month" ~ 2.5/30,
      `B36. Child 4 - How often did the child help you with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B36. Child 4 - How often did the child help you with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B36. Child 4 - How often did the child help you with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    receive_care4_score = case_when(
      `B37. Child 4 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Never" ~ 1,
      `B37. Child 4 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Rarely" ~ 2,
      `B37. Child 4 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Sometimes" ~ 3,
      `B37. Child 4 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Often" ~ 4,
      `B37. Child 4 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Always" ~ 5,
      TRUE ~ NA_real_
    ),
    
    receive_instrumental4 = pmax(receive_housework4_cont, receive_care4_score/5, na.rm = TRUE),
    
    
    # Emotional support
    receive_emotional4 = case_when(
      `B38. Child 4 - How often did the child spend time chatting with and listening to you?` == "Never" ~ 0,
      `B38. Child 4 - How often did the child spend time chatting with and listening to you?` == "Once every few months" ~ 1/90,
      `B38. Child 4 - How often did the child spend time chatting with and listening to you?` == "Once a month" ~ 1/30,
      `B38. Child 4 - How often did the child spend time chatting with and listening to you?` == "2-3 days a month" ~ 2.5/30,
      `B38. Child 4 - How often did the child spend time chatting with and listening to you?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B38. Child 4 - How often did the child spend time chatting with and listening to you?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B38. Child 4 - How often did the child spend time chatting with and listening to you?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    )
  )

### Code for Child 5

df <- df %>%
  mutate(
    
    # Clean amount inputs for financial support
    amount_given5_clean = str_squish(`B32a. Child 5 - If yes, how much per month?`),
    amount_received5_clean = str_squish(`B35a. Child 5 - If yes, how much per month?`),
    
    # Financial support
    give_financial5 = case_when(
      `B32. Child 5 - Did you offer the child economic support?` == "Yes" &
        amount_given5_clean == "Less than $200" ~ 100,
      amount_given5_clean == "$200-399" ~ 300,
      amount_given5_clean == "$400-599" ~ 500,
      amount_given5_clean == "$600-799" ~ 700,
      amount_given5_clean == "$800-999" ~ 900,
      amount_given5_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    receive_financial5 = case_when(
      `B35. Child 5 - Did the child give you economic support?` == "Yes" &
        amount_received5_clean == "Less than $200" ~ 100,
      amount_received5_clean == "$200-399" ~ 300,
      amount_received5_clean == "$400-599" ~ 500,
      amount_received5_clean == "$600-799" ~ 700,
      amount_received5_clean == "$800-999" ~ 900,
      amount_received5_clean == "$1,000 and above" ~ 1100,
      TRUE ~ NA_real_
    ),
    
    # Instrumental support (housework)
    give_housework5_cont = case_when(
      `B33. Child 5 - How often did you help the child with housework?` == "Never" ~ 0,
      `B33. Child 5 - How often did you help the child with housework?` == "Once every few months" ~ 1/90,
      `B33. Child 5 - How often did you help the child with housework?` == "Once a month" ~ 1/30,
      `B33. Child 5 - How often did you help the child with housework?` == "2-3 days a month" ~ 2.5/30,
      `B33. Child 5 - How often did you help the child with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B33. Child 5 - How often did you help the child with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B33. Child 5 - How often did you help the child with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    give_grandchildcare5_cont = case_when(
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "Never" ~ 0,
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "Once every few months" ~ 1/90,
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "Once a month" ~ 1/30,
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "2-3 days a month" ~ 2.5/30,
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "Almost everyday" ~ 26/30,
      `B34. Child 5 - How often did you provide grandchild care to the child?` == "Not applicable (the child has no child)" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    give_instrumental5 = pmax(give_housework5_cont, give_grandchildcare5_cont, na.rm = TRUE),
    
    # Instrumental support (received)
    receive_housework5_cont = case_when(
      `B36. Child 5 - How often did the child help you with housework?` == "Never" ~ 0,
      `B36. Child 5 - How often did the child help you with housework?` == "Once every few months" ~ 1/90,
      `B36. Child 5 - How often did the child help you with housework?` == "Once a month" ~ 1/30,
      `B36. Child 5 - How often did the child help you with housework?` == "2-3 days a month" ~ 2.5/30,
      `B36. Child 5 - How often did the child help you with housework?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B36. Child 5 - How often did the child help you with housework?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B36. Child 5 - How often did the child help you with housework?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    ),
    
    receive_care5_score = case_when(
      `B37. Child 5 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Never" ~ 1,
      `B37. Child 5 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Rarely" ~ 2,
      `B37. Child 5 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Sometimes" ~ 3,
      `B37. Child 5 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Often" ~ 4,
      `B37. Child 5 - How often did the child take care of you (taking you to the doctor, etc.)?` == "Always" ~ 5,
      TRUE ~ NA_real_
    ),
    
    receive_instrumental5 = pmax(receive_housework5_cont, receive_care5_score/5, na.rm = TRUE),
    
    
    # Emotional support
    receive_emotional5 = case_when(
      `B38. Child 5 - How often did the child spend time chatting with and listening to you?` == "Never" ~ 0,
      `B38. Child 5 - How often did the child spend time chatting with and listening to you?` == "Once every few months" ~ 1/90,
      `B38. Child 5 - How often did the child spend time chatting with and listening to you?` == "Once a month" ~ 1/30,
      `B38. Child 5 - How often did the child spend time chatting with and listening to you?` == "2-3 days a month" ~ 2.5/30,
      `B38. Child 5 - How often did the child spend time chatting with and listening to you?` == "1-2 days a week" ~ (1.5 * 4.5)/30,
      `B38. Child 5 - How often did the child spend time chatting with and listening to you?` == "3-4 days a week" ~ (3.5 * 4.5)/30,
      `B38. Child 5 - How often did the child spend time chatting with and listening to you?` == "Almost everyday" ~ 26/30,
      TRUE ~ NA_real_
    )
  )


### This is where we decide if we use 4 or 5 children.
# If the child is present, but no support is given or received, we replace the
# support variables values containing NA with 0, and treat it as there is 
# no support.

max_children <- 4

child_nums <- 1:max_children

support_vars <- c(
  "give_financial", "receive_financial",
  "give_instrumental", "receive_instrumental",
  "receive_emotional"
)

for (i in child_nums) {
  gender_col <- paste0("B22. Child ", i, " - Gender")
  
  for (var in support_vars) {
    colname <- paste0(var, i)
    
    df[[colname]][!is.na(df[[gender_col]]) & is.na(df[[colname]])] <- 0
  }
}

sum_children <- function(var) {
  rowSums(
    dplyr::select(df, dplyr::all_of(paste0(var, child_nums))),
    na.rm = TRUE
  )
}

df <- df %>%
  mutate(
    receive_financial = sum_children("receive_financial"),
    give_financial = sum_children("give_financial"),
    receive_instrumental = sum_children("receive_instrumental"),
    give_instrumental = sum_children("give_instrumental"),
    receive_emotional = sum_children("receive_emotional")
  )


# The give and receive financial variable should be in log

df <- df %>%
  mutate(log_receive_financial = log(receive_financial+1),
         log_give_financial = log(give_financial+1))  





# --- Construct Well-being Measures --- 

# A1. CASP-style: Control
a1_map <- c("Often" = 1, "Sometimes" = 2, "Not Often" = 3, "Never" = 4)
a1_cols <- names(df)[startsWith(names(df), "A1.")]
a1_data <- df %>%
  select(all_of(a1_cols)) %>%
  mutate(across(everything(), ~ a1_map[.]))
# Reverse-coded items by index
a1_neg <- c(1, 2, 4, 6, 8, 9)
a1_data[a1_neg] <- 5 - a1_data[a1_neg]
df$wellbeing_a1_control <- rowMeans(a1_data, na.rm = TRUE)

# A2. Life Satisfaction (SWLS)
a2_map <- c(
  "Strongly disagree" = 1, "Disagree" = 2, "Somewhat disagree" = 3,
  "Neither agree nor disagree" = 4, "Somewhat agree" = 5,
  "Agree" = 6, "Strongly agree" = 7
)
a2_cols <- names(df)[startsWith(names(df), "A2.")]
a2_data <- df %>%
  select(all_of(a2_cols)) %>%
  mutate(across(everything(), ~ a2_map[.]))

df$wellbeing_a2_satisfaction <- rowMeans(a2_data, na.rm = TRUE)

# A3. WHO-5 Psychological Well-being
a3_map <- c(
  "All of the time" = 6, "Most of the time" = 5, "More than half of the time" = 4,
  "Less than half of the time" = 3, "Some of the time" = 2, "At no time" = 1
)
a3_cols <- names(df)[startsWith(names(df), "A3.")]
a3_data <- df %>%
  select(all_of(a3_cols)) %>%
  mutate(across(everything(), ~ a3_map[.]))

df$wellbeing_a3_affect <- rowMeans(a3_data, na.rm = TRUE)

# --- Normalize well-being indices to [0, 1] --- 
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
df <- df %>%
  mutate(
    wellbeing_a1_control_norm = normalize(wellbeing_a1_control),
    wellbeing_a2_satisfaction_norm = normalize(wellbeing_a2_satisfaction),
    wellbeing_a3_affect_norm = normalize(wellbeing_a3_affect)
  )




### Summary Statistics 
# --- Table 1. Generate Summary Statistics --- 

# Create dummies for education, housing and religion
df <- df %>%
  mutate(
    edu_degree = ifelse(education == "Degree holders", 1, 0),
    edu_poly = ifelse(education == "Polytechnic and A levels", 1, 0),
    edu_secondary = ifelse(education == "Secondary", 1, 0),
    edu_other = ifelse(education == "Other", 1, 0),
    house_13 = ifelse(housing == "1–3 rooms", 1, 0),
    house_4 = ifelse(housing == "4 rooms", 1, 0),
    house_5 = ifelse(housing == "5 rooms", 1, 0),
    house_private = ifelse(housing == "Private", 1, 0),
    house_other = ifelse(housing == "Other", 1, 0),
    religion_none = ifelse(religion == "No religion", 1, 0),
    religion_bud = ifelse(religion == "Buddhism", 1, 0),
    religion_cat = ifelse(religion == "Catholicism", 1, 0),
    religion_chr = ifelse(religion == "Christianity", 1, 0),
    religion_hin = ifelse(religion == "Hinduism", 1, 0),
    religion_isl = ifelse(religion == "Islam", 1, 0),
    religion_tao = ifelse(religion == "Taoism", 1, 0)
  )

df$religion %>% table()
# Define variable groups and labels
var_labels <- tibble::tribble(
  ~var, ~label, ~group,
  "wellbeing_a1_control_norm", "Control (A1)", "Subjective Well-Being Measures",
  "wellbeing_a2_satisfaction_norm", "Satisfaction (A2)", "Subjective Well-Being Measures",
  "wellbeing_a3_affect_norm", "Affect (A3)", "Subjective Well-Being Measures",
  
  "log_receive_financial", "Received Financial Support (log)", "Intergenerational Support Measures",
  "log_give_financial", "Given Financial Support (log)", "Intergenerational Support Measures",
  "receive_instrumental", "Received Instrumental Support", "Intergenerational Support Measures",
  "give_instrumental", "Given Instrumental Support", "Intergenerational Support Measures",
  "receive_emotional", "Received Emotional Support", "Intergenerational Support Measures",
  
  "age", "Age", "Demographic Controls",
  "gender", "Gender", "Demographic Controls",
  "married", "Married", "Demographic Controls",
  "num_children", "Number of Children", "Demographic Controls",
  "edu_degree", "Education: Degree", "Demographic Controls",
  "edu_poly", "Education: Poly/A Levels", "Demographic Controls",
  "edu_secondary", "Education: Secondary", "Demographic Controls",
  "edu_other", "Education: Other", "Demographic Controls",
  "employed", "Employed", "Demographic Controls",
  "religion_none", "Religion: No religion", "Demographic Controls",
  "religion_bud", "Religion: Buddhism","Demographic Controls",
  "religion_cat", "Religion: Catholicism", "Demographic Controls",
  "religion_chr", "Religion: Christianity", "Demographic Controls",
  "religion_hin", "Religion: Hinduism", "Demographic Controls",
  "religion_isl", "Religion: Islam", "Demographic Controls",
  "religion_tao", "Religion: Taoism", "Demographic Controls",
  "log_personal_income", "Personal Income (log)", "Demographic Controls",
  "house_13", "Housing: 1–3 Rooms", "Demographic Controls",
  "house_4", "Housing: 4 Rooms", "Demographic Controls",
  "house_5", "Housing: 5 Rooms", "Demographic Controls",
  "house_private", "Housing: Private", "Demographic Controls",
  "home_ownership", "Owns Home", "Demographic Controls",
  "health", "Self-Rated Health", "Demographic Controls",
  "mobility_difficulty", "Mobility Difficulty", "Demographic Controls",
  "has_enough_money", "Income Adequacy", "Demographic Controls",
  "friend_satisfaction_score", "Satisfaction with Friendships", "Demographic Controls",
  
  "avg_closeness", "Avg. Closeness to Children", "Relational Controls",
  "avg_distance_km", "Avg. Distance to Children (km)", "Relational Controls",
  "rel_satisfaction_score", "Satisfaction with Relationship with Children", "Relational Controls"
)

# Coerce all selected variables to numeric
df_num <- df %>%
  mutate(across(all_of(var_labels$var), ~as.numeric(as.character(.))))


# Generate summary statistics
summary_stats <- df_num %>%
  select(all_of(var_labels$var)) %>%
  summarise(across(everything(), list(
    Mean = ~mean(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE),
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), 
               names_to = c("var", "stat"), 
               names_pattern = "^(.*)_(Mean|SD|Min|Max)$") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  left_join(var_labels, by = "var") %>%
  arrange(factor(group, levels = unique(var_labels$group))) %>%
  select(Group = group, Variable = label, Mean, SD, Min, Max)

# Print LaTeX table
kable(summary_stats, format = "latex", booktabs = TRUE, digits = 2,
      caption = "Summary Statistics by Variable Group")






### Section 5: Results  

# Note: The outcomes are:
# "wellbeing_a1_control_norm", 
# "wellbeing_a2_satisfaction_norm", 
# "wellbeing_a3_affect_norm"


# --- Section 5.1.1: Baseline Regressions ---
# Table 2: Baseline Regression: Intergenerational Support and Well-being

model_base_a1 <- lm(wellbeing_a1_control_norm ~ 
                      log_receive_financial + log_give_financial +
                      receive_instrumental + give_instrumental +
                      receive_emotional + sampling, data = df)

model_base_a2 <- lm(wellbeing_a2_satisfaction_norm ~ 
                      log_receive_financial + log_give_financial +
                      receive_instrumental + give_instrumental +
                      receive_emotional + sampling, data = df)

model_base_a3 <- lm(wellbeing_a3_affect_norm ~ 
                      log_receive_financial + log_give_financial +
                      receive_instrumental + give_instrumental +
                      receive_emotional + sampling, data = df)


# Compute robust standard errors
se_base_a1 <- sqrt(diag(vcovHC(model_base_a1, type = "HC1")))
se_base_a2 <- sqrt(diag(vcovHC(model_base_a2, type = "HC1")))
se_base_a3 <- sqrt(diag(vcovHC(model_base_a3, type = "HC1")))

# Display in LaTeX with stargazer
stargazer(model_base_a1, model_base_a2, model_base_a3,
          se = list(se_base_a1, se_base_a2, se_base_a3),
          title = "Baseline Regression: Intergenerational Support and Well-being",
          column.labels = c("Control", "Satisfaction", "Affect"),
          covariate.labels = c("Receive Financial (log)", "Give Financial (log)", 
                               "Receive Instrumental", "Give Instrumental", 
                               "Receive Emotional",
                               "Sampling (Convenience = 1)"),
          dep.var.labels.include = FALSE,
          omit.stat = c("f", "ser"),
          type = "latex",
          digits = 3,
          no.space = TRUE)


### --- 5.1.2 Personal and Relational Controls ---
# Table 3: Regression with Controls: Intergenerational Support and Well-being --- 

# Personal income has many missing variables, due to respodents refusing to answer.
# We impute the missing income values using the average values of the others who 
# live in the housing type as the non-respondent. For instance, for people living in 
# landed properties, we use the average personal income of these individuals to
# impute the missing data for the person who also live in a landed property.

# First, check if personal_income and housing exist
if (!("personal_income" %in% names(df)) || !("housing" %in% names(df))) {
  stop("Ensure 'personal_income' and 'housing' columns exist in your dataframe.")
}

# Calculate mean personal income for each housing type, excluding missing values
mean_income_by_housing <- df %>%
  group_by(housing) %>%
  summarise(mean_income = mean(personal_income, na.rm = TRUE))

# Join the mean incomes back to the original dataset
df <- df %>%
  left_join(mean_income_by_housing, by = "housing")

# Impute missing personal income only where it is NA
df <- df %>%
  mutate(personal_income = if_else(is.na(personal_income), mean_income, personal_income)) %>%
  select(-mean_income)  # Clean up

# Create log version for regression
df <- df %>%
  mutate(log_personal_income = log(personal_income))


### Set Other, i.e. primary school, in education as the base group
df$education <- factor(df$education)
df$education <- relevel(df$education, ref = "Other")

### Set Other, i.e. smaller than 4 room, in education as the base group
df$housing <- factor(df$housing)
df$housing <- relevel(df$housing, ref = "Other")



# Estimate models
model_full_a1 <- lm(wellbeing_a1_control_norm ~ 
                      log_receive_financial + log_give_financial +
                      receive_instrumental + give_instrumental +
                      receive_emotional +
                      age + gender + married + num_children + 
                      education + employed + 
                      log_personal_income + housing + home_ownership + religion +
                      health + mobility_difficulty + has_enough_money + 
                      avg_closeness + avg_distance_km +
                      rel_satisfaction_score + friend_satisfaction_score + 
                      sampling, 
                    data = df)

model_full_a2 <- lm(wellbeing_a2_satisfaction_norm ~ 
                      log_receive_financial + log_give_financial +
                      receive_instrumental + give_instrumental +
                      receive_emotional +
                      age + gender + married + num_children + 
                      education + employed + 
                      log_personal_income + housing + home_ownership + religion +
                      health + mobility_difficulty + has_enough_money + 
                      avg_closeness + avg_distance_km +
                      rel_satisfaction_score + friend_satisfaction_score + 
                      sampling, 
                    data = df)

model_full_a3 <- lm(wellbeing_a3_affect_norm ~ 
                      log_receive_financial + log_give_financial +
                      receive_instrumental + give_instrumental +
                      receive_emotional +
                      age + gender + married + num_children + 
                      education + employed +
                      log_personal_income + housing + home_ownership + religion +
                      health + mobility_difficulty + has_enough_money + 
                      avg_closeness + avg_distance_km +
                      rel_satisfaction_score + friend_satisfaction_score +
                      sampling, 
                    data = df)

summary(model_full_a3)

# Compute robust standard errors (HC1)
se_full_a1 <- sqrt(diag(vcovHC(model_full_a1, type = "HC1")))
se_full_a2 <- sqrt(diag(vcovHC(model_full_a2, type = "HC1")))
se_full_a3 <- sqrt(diag(vcovHC(model_full_a3, type = "HC1")))

# Output with Stargazer
stargazer(model_full_a1, model_full_a2, model_full_a3,
          se = list(se_full_a1, se_full_a2, se_full_a3),
          type = "latex",
          title = "Baseline Regression with Controls: Intergenerational Support and Well-being",
          dep.var.labels = c("Control", "Satisfaction", "Affect"),
          covariate.labels = c(
            "Received Financial Support (log)",
            "Given Financial Support (log)",
            "Received Instrumental Support",
            "Given Instrumental Support",
            "Received Emotional Support",
            "Age",
            "Gender ($=1$ if male)",
            "Married ($=1$ if married)",
            "Number of Children",
            "Education: Degree ($=1$ if true)",
            "Education: Poly/A Levels ($=1$ if true)",
            "Education: Secondary ($=1$ if true)",
            "Employed ($=1$ if employed)",
            "Personal Income (log)",
            "Housing: 1–3 Rooms ($=1$ if true)",
            "Housing: 4 Rooms ($=1$ if true)",
            "Housing: 5 Rooms ($=1$ if true)",
            "Housing: Private ($=1$ if true)",
            "Owns Home ($=1$ if owned)",
            "Religion: Buddhism ($=1$ if true)",      
            "Religion: Catholicism ($=1$ if true)",             
            "Religion: Christianity ($=1$ if true)",            
            "Religion: Hinduism ($=1$ if true)",                  
            "Religion: Islam ($=1$ if true)",                      
            "Religion: Taoism ($=1$ if true)",                    
            "Self-Rated Health ($=1$ if good)",
            "Mobility Difficulty ($=1$ if severe or worse)",
            "Income Adequacy ($=1$ if adequate)",
            "Avg. Closeness to Children",
            "Avg. Distance to Children (km)",
            "Satisfaction with Relationship with Children",
            "Satisfaction with Friendships",
            "Sampling (Convenience = 1)"
          ),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE)



### --- 5.1.3 Relationships, Support and Well-Being ---
# Table 4: Closeness, Intergenerational Support and Well-being 

# Step 1: Normalize avg_closeness and rel_satisfaction_score to 0-1
df$norm_closeness <- (df$avg_closeness - 1) / (5 - 1)  # Scale 1–5
df$norm_rel_satisfaction <- (df$rel_satisfaction_score - 1) / (6 - 1)  # Scale 1–6

# Step 2: Create composite closeness index
df$closeness_index <- rowMeans(cbind(df$norm_closeness, df$norm_rel_satisfaction), na.rm = TRUE)

# Generate interactive terms between closeness and teh support variables

df$closeness_log_receive_financial  <- df$closeness_index * df$log_receive_financial
df$closeness_log_give_financial     <- df$closeness_index * df$log_give_financial
df$closeness_receive_instrumental      <- df$closeness_index * df$receive_instrumental
df$closeness_give_instrumental         <- df$closeness_index * df$give_instrumental
df$closeness_receive_emotional  <- df$closeness_index * df$receive_emotional


### --- 5.1.3 Regressions with interaction between closeness_index and support ---
model_closeness_1 <- lm(wellbeing_a1_control_norm ~ 
                          log_receive_financial +
                          closeness_log_receive_financial +
                          log_give_financial +
                          closeness_log_give_financial   +
                          receive_instrumental +
                          closeness_receive_instrumental   +
                          give_instrumental +
                          closeness_give_instrumental +
                          receive_emotional +
                          closeness_receive_emotional +
                          age + gender + married + num_children + 
                          education + employed + 
                          log_personal_income + 
                          housing + home_ownership + religion +
                          health +  mobility_difficulty +  
                          has_enough_money + 
                          avg_closeness + avg_distance_km +
                          rel_satisfaction_score +friend_satisfaction_score + 
                          sampling,
                        data = df)

model_closeness_2 <- lm(wellbeing_a2_satisfaction_norm ~ 
                          log_receive_financial +
                          closeness_log_receive_financial +
                          log_give_financial +
                          closeness_log_give_financial   +
                          receive_instrumental +
                          closeness_receive_instrumental   +
                          give_instrumental +
                          closeness_give_instrumental +
                          receive_emotional +
                          closeness_receive_emotional +
                          age + gender + married + num_children + 
                          education + employed +
                          log_personal_income + 
                          housing +home_ownership + religion +
                          health +  mobility_difficulty +  
                          has_enough_money + 
                          avg_closeness + avg_distance_km +
                          rel_satisfaction_score +friend_satisfaction_score +
                          sampling,
                        data = df)


model_closeness_3 <- lm(wellbeing_a3_affect_norm ~ 
                          log_receive_financial +
                          closeness_log_receive_financial +
                          log_give_financial +
                          closeness_log_give_financial   +
                          receive_instrumental +
                          closeness_receive_instrumental   +
                          give_instrumental +
                          closeness_give_instrumental +
                          receive_emotional +
                          closeness_receive_emotional +
                          age + gender + married + num_children +
                          education + employed + 
                          log_personal_income + 
                          housing +home_ownership + religion +
                          health +  mobility_difficulty +  
                          has_enough_money + 
                          avg_closeness + avg_distance_km +
                          rel_satisfaction_score +friend_satisfaction_score +
                          sampling,
                        data = df)


# Compute robust SEs (e.g., HC3)
se_closeness_1 <- sqrt(diag(vcovHC(model_closeness_1, type = "HC3")))
se_closeness_2 <- sqrt(diag(vcovHC(model_closeness_2, type = "HC3")))
se_closeness_3 <- sqrt(diag(vcovHC(model_closeness_3, type = "HC3")))

# Align SE vectors with coefficients in case of dropped variables
trimmed_se <- function(model, se_vec) {
  coefs <- coef(model)
  valid_names <- intersect(names(coefs), names(se_vec))
  se_vec[valid_names]
}

se_list <- list(
  trimmed_se(model_closeness_1, se_closeness_1),
  trimmed_se(model_closeness_2, se_closeness_2),
  trimmed_se(model_closeness_3, se_closeness_3)
)

# Stargazer with robust SEs
stargazer(model_closeness_1, model_closeness_2, model_closeness_3,
          type = "latex",
          se = se_list,
          title = "Interaction between Closeness Index and Support: Moderation Effects on Well-being",
          dep.var.labels = "Normalized Well-being Index",
          column.labels = c("Control", "Satisfaction", "Affect"),
          covariate.labels = c(
            "Received Financial Support (log)",
            "Closeness $\\times$ Received Financial (log)",
            "Given Financial Support (log)",
            "Closeness $\\times$ Given Financial (log)",
            "Received Instrumental Support",
            "Closeness $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Closeness $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Closeness $\\times$ Received Emotional Support",
            "Age",
            "Gender ($=1$ if male)",
            "Married ($=1$ if married)",
            "Number of Children",
            "Education: Degree ($=1$ if true)",
            "Education: Poly/A Levels ($=1$ if true)",
            "Education: Secondary ($=1$ if true)",
            "Employed ($=1$ if employed)",
            "Personal Income (log)",
            "Housing: 1–3 Rooms ($=1$ if true)",
            "Housing: 4 Rooms ($=1$ if true)",
            "Housing: 5 Rooms ($=1$ if true)",
            "Housing: Private ($=1$ if true)",
            "Owns Home ($=1$ if owned)",
            "Religion: Buddhism ($=1$ if true)",      
            "Religion: Catholicism ($=1$ if true)",             
            "Religion: Christianity ($=1$ if true)",            
            "Religion: Hinduism ($=1$ if true)",                  
            "Religion: Islam ($=1$ if true)",                      
            "Religion: Taoism ($=1$ if true)",  
            "Self-Rated Health ($=1$ if good)",
            "Mobility Difficulty ($=1$ if severe or worse)",
            "Income Adequacy ($=1$ if adequate)",
            "Avg. Closeness to Children",
            "Avg. Distance to Children (km)",
            "Satisfaction with Relationship with Children",
            "Satisfaction with Friendships",
            "Sampling (Convenience = 1)"
          ),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE)




### --- 5.1.4 Robustness Checks --- ###
# See Appendix B


### 5.2 Further Analysis 

### --- 5.2.1: Quantile Regression ---
# Do individuals with lower wellbeing benefit more?
#Table 5: Conditional Quantile Regression Results for Well-being Dimensions

# Support variables
support_vars <- c(
  "log_receive_financial",
  "closeness_log_receive_financial",
  "log_give_financial",
  "closeness_log_give_financial",
  "receive_instrumental",
  "closeness_receive_instrumental",
  "give_instrumental",
  "closeness_give_instrumental",
  "receive_emotional",
  "closeness_receive_emotional"
)

# Control variables (including gender)
controls <- c(
  "age", "gender", "married", "num_children", "education", "employed",
  "log_personal_income", "housing", "home_ownership", "religion",  "health", 
  "mobility_difficulty", "has_enough_money", "avg_closeness",
  "avg_distance_km", "rel_satisfaction_score", "friend_satisfaction_score",
  "sampling"
)

# Helper to get summary with robust SEs
get_summary <- function(model) summary(model, se = "boot")

# Helper to align coefficients and SEs
align_coef_se <- function(summary_obj, model) {
  se <- summary_obj$coefficients[, 2]
  coefs <- coef(model)
  common <- intersect(names(coefs), names(se))
  list(coef = coefs[common], se = se[common])
}

# Fit models across all outcomes and quantiles
taus <- c(0.25, 0.5, 0.75)
outcomes <- c("wellbeing_a1_control_norm", "wellbeing_a2_satisfaction_norm", "wellbeing_a3_affect_norm")

models <- list()
summaries <- list()
aligned <- list()

idx <- 1
for (outcome in outcomes) {
  for (tau in taus) {
    formula <- as.formula(paste(outcome, "~", paste(c(support_vars, controls), collapse = " + ")))
    model <- rq(formula, tau = tau, data = df)
    summary_model <- get_summary(model)
    aligned_model <- align_coef_se(summary_model, model)
    models[[idx]] <- model
    summaries[[idx]] <- summary_model
    aligned[[idx]] <- aligned_model
    idx <- idx + 1
  }
}

# Extract coef and se lists for stargazer
coef_list <- lapply(aligned, function(x) x$coef)
se_list <- lapply(aligned, function(x) x$se)

# Label columns
dep_labels <- rep(c("Control", "Satisfaction", "Affect"), each = 3)
col_labels <- paste(dep_labels, rep(c("(τ=0.25)", "(τ=0.5)", "(τ=0.75)"), 3), sep = " ")

# Generate LaTeX table
stargazer(models[[1]], models[[2]], models[[3]],
          type = "latex",
          title = "Quantile Regression for Well-being Dimensions (Robust SEs)",
          column.labels = col_labels[1:3],
          dep.var.caption = "Dependent Variables",
          dep.var.labels.include = FALSE,
          coef = coef_list[1:3],
          se = se_list[1:3],
          covariate.labels = c(
            "Received Financial Support (log)",
            "Closeness $\\times$ Received Financial",
            "Given Financial Support (log)",
            "Closeness $\\times$ Given Financial",
            "Received Instrumental Support",
            "Closeness $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Closeness $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Closeness $\\times$ Received Emotional"
          ),
          omit.stat = c("n", "ll", "aic", "bic"),
          no.space = TRUE,
          digits = 3
)



# Generate LaTeX table
stargazer(models[[4]], models[[5]], models[[6]],
          type = "latex",
          title = "Quantile Regression for Well-being Dimensions (Robust SEs)",
          column.labels = col_labels[4:6],
          dep.var.caption = "Dependent Variables",
          dep.var.labels.include = FALSE,
          coef = coef_list[4:6],
          se = se_list[4:6],
          covariate.labels = c(
            "Received Financial Support (log)",
            "Closeness $\\times$ Received Financial",
            "Given Financial Support (log)",
            "Closeness $\\times$ Given Financial",
            "Received Instrumental Support",
            "Closeness $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Closeness $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Closeness $\\times$ Received Emotional"
          ),
          omit.stat = c("n", "ll", "aic", "bic"),
          no.space = TRUE,
          digits = 3
)

# Generate LaTeX table
stargazer(models[[7]], models[[8]], models[[9]],
          type = "latex",
          title = "Quantile Regression for Well-being Dimensions (Robust SEs)",
          column.labels = col_labels[7:9],
          dep.var.caption = "Dependent Variables",
          dep.var.labels.include = FALSE,
          coef = coef_list[7:9],
          se = se_list[7:9],
          covariate.labels = c(
            "Received Financial Support (log)",
            "Closeness $\\times$ Received Financial",
            "Given Financial Support (log)",
            "Closeness $\\times$ Given Financial",
            "Received Instrumental Support",
            "Closeness $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Closeness $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Closeness $\\times$ Received Emotional"
          ),
          omit.stat = c("n", "ll", "aic", "bic"),
          no.space = TRUE,
          digits = 3
)



### --- 5.2.2: Gender ---
# Table 6: Closeness as a Moderator of Support-Well-being Links, by Gender

# Define model formula
model_formula <- wellbeing ~ 
  log_receive_financial +
  closeness_log_receive_financial +
  log_give_financial +
  closeness_log_give_financial +
  receive_instrumental +
  closeness_receive_instrumental +
  give_instrumental +
  closeness_give_instrumental +
  receive_emotional +
  closeness_receive_emotional +
  age + married + num_children + education + employed +
  log_personal_income + housing + home_ownership +  religion +
  health + mobility_difficulty + has_enough_money +
  avg_closeness + avg_distance_km +
  rel_satisfaction_score + friend_satisfaction_score + sampling

# Estimate models by gender using the same data frame
model_a1_m <- lm(update(model_formula, wellbeing_a1_control_norm ~ .), data = df, subset = gender == 1)
model_a2_m <- lm(update(model_formula, wellbeing_a2_satisfaction_norm ~ .), data = df, subset = gender == 1)
model_a3_m <- lm(update(model_formula, wellbeing_a3_affect_norm ~ .), data = df, subset = gender == 1)

model_a1_f <- lm(update(model_formula, wellbeing_a1_control_norm ~ .), data = df, subset = gender == 0)
model_a2_f <- lm(update(model_formula, wellbeing_a2_satisfaction_norm ~ .), data = df, subset = gender == 0)
model_a3_f <- lm(update(model_formula, wellbeing_a3_affect_norm ~ .), data = df, subset = gender == 0)

# Get robust standard errors  
robust_se <- function(model) coeftest(model, vcov = vcovHC(model, type = "HC1"))

se_list <- list(
  sqrt(diag(vcovHC(model_a1_m, type = "HC1"))),
  sqrt(diag(vcovHC(model_a1_f, type = "HC1"))),
  sqrt(diag(vcovHC(model_a2_m, type = "HC1"))),
  sqrt(diag(vcovHC(model_a2_f, type = "HC1"))),
  sqrt(diag(vcovHC(model_a3_m, type = "HC1"))),
  sqrt(diag(vcovHC(model_a3_f, type = "HC1")))
)


# Return trimmed coef and se vectors with matching names
extract_aligned_coef_se <- function(model) {
  se_vec <- sqrt(diag(vcovHC(model, type = "HC1")))
  coef_vec <- coef(model)
  
  valid_names <- intersect(names(coef_vec), names(se_vec))
  
  list(
    coef = coef_vec[valid_names],
    se = se_vec[valid_names]
  )
}

models_gender <- list(model_a1_m, model_a1_f, model_a2_m, model_a2_f, model_a3_m, model_a3_f)

# Generate aligned coef and se lists
aligned <- lapply(models_gender, extract_aligned_coef_se)
coef_list <- lapply(aligned, `[[`, "coef")
se_list   <- lapply(aligned, `[[`, "se")

stargazer(models_gender,
          coef = coef_list,
          se = se_list,
          type = "latex",
          title = "Moderating Role of Closeness on Support and Well-being by Gender",
          column.labels = c("Male", "Female", "Male", "Female", "Male", "Female"),
          dep.var.labels = "Normalized Well-being Index",
          covariate.labels = c(
            "Received Financial Support (log)",
            "Closeness $\\times$ Received Financial",
            "Given Financial Support (log)",
            "Closeness $\\times$ Given Financial",
            "Received Instrumental Support",
            "Closeness $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Closeness $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Closeness $\\times$ Received Emotional"
          ),
          omit.stat = c("f", "ser"),
          omit = "^(age|married|num_children|education|employed|log_personal_income|housing|home_ownership|health|mobility_difficulty|has_enough_money|avg_closeness|avg_distance_km|rel_satisfaction_score|friend_satisfaction_score)$",
          no.space = TRUE,
          digits = 3)



### --- 5.2.3: Co-residence and the Relational Context of Support ---
## Table 7: Closeness as a Moderator of Support-Well-being Links, by Co-Residence

# Create a binary indicator for co-residence
df$coreside <- ifelse(
  rowSums(
    cbind(
      df$`B28. Child 1 - Distance from your residence` == "Living together in the same house",
      df$`B28. Child 2 - Distance from your residence` == "Living together in the same house",
      df$`B28. Child 3 - Distance from your residence` == "Living together in the same house",
      df$`B28. Child 4 - Distance from your residence` == "Living together in the same house",
      df$`B28. Child 5 - Distance from your residence` == "Living together in the same house"
    ),
    na.rm = TRUE
  ) > 0,
  1, 0
)


# Define model formula
model_formula <- wellbeing ~ 
  log_receive_financial +
  closeness_log_receive_financial +
  log_give_financial +
  closeness_log_give_financial +
  receive_instrumental +
  closeness_receive_instrumental +
  give_instrumental +
  closeness_give_instrumental +
  receive_emotional +
  closeness_receive_emotional +
  age + married + num_children + education + employed + 
  log_personal_income + housing + home_ownership +  religion +
  health + mobility_difficulty + has_enough_money +
  avg_closeness + 
  rel_satisfaction_score + friend_satisfaction_score + sampling

# Estimate models by gender using the same data frame
model_a1_coreside <- lm(update(model_formula, wellbeing_a1_control_norm ~ .), data = df, subset = coreside == 1)
model_a2_coreside <- lm(update(model_formula, wellbeing_a2_satisfaction_norm ~ .), data = df, subset = coreside == 1)
model_a3_coreside <- lm(update(model_formula, wellbeing_a3_affect_norm ~ .), data = df, subset = coreside == 1)

model_a1_ncoreside <- lm(update(model_formula, wellbeing_a1_control_norm ~ .), data = df, subset = coreside == 0)
model_a2_ncoreside <- lm(update(model_formula, wellbeing_a2_satisfaction_norm ~ .), data = df, subset = coreside == 0)
model_a3_ncoreside <- lm(update(model_formula, wellbeing_a3_affect_norm ~ .), data = df, subset = coreside == 0)

# Get robust standard errors  
robust_se <- function(model) coeftest(model, vcov = vcovHC(model, type = "HC1"))

se_list <- list(
  sqrt(diag(vcovHC(model_a1_m, type = "HC1"))),
  sqrt(diag(vcovHC(model_a1_f, type = "HC1"))),
  sqrt(diag(vcovHC(model_a2_m, type = "HC1"))),
  sqrt(diag(vcovHC(model_a2_f, type = "HC1"))),
  sqrt(diag(vcovHC(model_a3_m, type = "HC1"))),
  sqrt(diag(vcovHC(model_a3_f, type = "HC1")))
)


# Return trimmed coef and se vectors with matching names
extract_aligned_coef_se <- function(model) {
  se_vec <- sqrt(diag(vcovHC(model, type = "HC1")))
  coef_vec <- coef(model)
  
  valid_names <- intersect(names(coef_vec), names(se_vec))
  
  list(
    coef = coef_vec[valid_names],
    se = se_vec[valid_names]
  )
}

models_coreside <- list(model_a1_coreside, model_a1_ncoreside, model_a2_coreside, model_a2_ncoreside, model_a3_coreside, model_a3_ncoreside)

# Generate aligned coef and se lists
aligned <- lapply(models_coreside, extract_aligned_coef_se)
coef_list <- lapply(aligned, `[[`, "coef")
se_list   <- lapply(aligned, `[[`, "se")

stargazer(models_coreside,
          coef = coef_list,
          se = se_list,
          type = "latex",
          title = "Moderating Role of Closeness on Support and Well-being by Coresidence",
          column.labels = c("Coreside", "No", "Coreside", "No", "Coreside", "No"),
          dep.var.labels = "Normalized Well-being Index",
          covariate.labels = c(
            "Received Financial Support (log)",
            "Closeness $\\times$ Received Financial",
            "Given Financial Support (log)",
            "Closeness $\\times$ Given Financial",
            "Received Instrumental Support",
            "Closeness $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Closeness $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Closeness $\\times$ Received Emotional"
          ),
          omit.stat = c("f", "ser"),
          omit = "^(age|married|num_children|education|employed|log_personal_income|housing|home_ownership|health|mobility_difficulty|has_enough_money|avg_closeness|avg_distance_km|rel_satisfaction_score|friend_satisfaction_score)$",
          no.space = TRUE,
          digits = 3)




### --- Some Diagnostics - List of variables to check for missing --- 
vars_to_check <- c(
  "log_receive_financial",
  "closeness_log_receive_financial",
  "log_give_financial",
  "closeness_log_give_financial",
  "receive_instrumental",
  "closeness_receive_instrumental",
  "give_instrumental",
  "closeness_give_instrumental",
  "receive_emotional",
  "closeness_receive_emotional",
  "age",
  "married",
  "num_children",
  "education",
  "employed",
  "log_personal_income",
  "housing",
  "home_ownership",
  "health",
  "mobility_difficulty",
  "has_enough_money",
  "avg_closeness",
  "rel_satisfaction_score",
  "friend_satisfaction_score"
)

# Filter for gender == 1 and check for missingness
missing_obs <- df %>%
  filter(gender == 0) %>%
  filter(if_any(all_of(vars_to_check), is.na))

# View number or inspect rows
n_missing <- nrow(missing_obs)
print(n_missing)
head(missing_obs)






### Appendix B: Further Details on Robustness Checks

# --- Inverse-Probability Selection Weighting (IPW) ---


# Table B1: Inverse-Probability Selection Weights
# We construct the inverse probability weights using a logit model
# on the sampling indicator. The weights are P(S=1)/p_hat and
# P(S=0)/(1-p_hat)

# --- 1) Stabilized inverse-probability-of-selection weights (sampling: 1=convenience) ---
# Model the probability of being in the convenience sample given observables (use your existing X,Z)
sel_formula <- sampling ~ age + gender + married + num_children +
  education + employed + log_personal_income +
  housing + home_ownership + religion +
  health + mobility_difficulty + has_enough_money +
  avg_closeness + avg_distance_km + rel_satisfaction_score +
  friend_satisfaction_score

ps_sel <- glm(sel_formula, data = df, family = binomial())

# Predicted probs (bounded to avoid 0/1)
p_hat <- pmin(pmax(predict(ps_sel, type = "response"), 1e-6), 1 - 1e-6)
p_bar <- mean(df$sampling == 1, na.rm = TRUE)  # marginal rate

# Stabilized weights
w_sel_raw <- ifelse(df$sampling == 1, p_bar / p_hat, (1 - p_bar) / (1 - p_hat))

# Mild trimming for overlap (adjust cutoffs if needed)
trim <- function(w, p = c(0.01, 0.99)) {
  q <- quantile(w, p, na.rm = TRUE)
  pmin(pmax(w, q[1]), q[2])
}
df$w_sel <- trim(w_sel_raw, p = c(0.01, 0.99))

# Optional: quick diagnostics
cat("Selection weights summary:\n"); print(summary(df$w_sel))
cat("Share convenience:", round(p_bar, 3), "\n")


# --- 3) IPW versions (same formulas, just add weights = w_sel) ---
model_closeness_1_ipw <- update(model_closeness_1, weights = w_sel)
model_closeness_2_ipw <- update(model_closeness_2, weights = w_sel)
model_closeness_3_ipw <- update(model_closeness_3, weights = w_sel)

# --- 4) Robust SEs (HC3) aligned to each model ---
hc3_aligned <- function(mod) {
  se <- sqrt(diag(vcovHC(mod, type = "HC3")))
  se[names(coef(mod))]
}

se_closeness_1     <- hc3_aligned(model_closeness_1)
se_closeness_2     <- hc3_aligned(model_closeness_2)
se_closeness_3     <- hc3_aligned(model_closeness_3)

se_closeness_1_ipw <- hc3_aligned(model_closeness_1_ipw)
se_closeness_2_ipw <- hc3_aligned(model_closeness_2_ipw)
se_closeness_3_ipw <- hc3_aligned(model_closeness_3_ipw)

# helper to align names (in case any term drops)
trimmed_se <- function(model, se_vec) {
  v <- se_vec[names(coef(model))]
  v[!is.na(v)]
}

# --- 5) LaTeX table for the IPW models (separate from your baseline table) ---
stargazer(model_closeness_1_ipw, model_closeness_2_ipw, model_closeness_3_ipw,
          type = "latex",
          se = list(
            trimmed_se(model_closeness_1_ipw, se_closeness_1_ipw),
            trimmed_se(model_closeness_2_ipw, se_closeness_2_ipw),
            trimmed_se(model_closeness_3_ipw, se_closeness_3_ipw)
          ),
          title = "Interaction between Closeness and Support: Selection-IPW Weighted Estimates",
          dep.var.labels = "Normalized Well-being Index",
          column.labels = c("Control", "Satisfaction", "Affect"),
          covariate.labels = c(
            "Received Financial Support (log)",
            "Closeness $\\times$ Received Financial (log)",
            "Given Financial Support (log)",
            "Closeness $\\times$ Given Financial (log)",
            "Received Instrumental Support",
            "Closeness $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Closeness $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Closeness $\\times$ Received Emotional Support",
            "Age",
            "Gender ($=1$ if male)",
            "Married ($=1$ if married)",
            "Number of Children",
            "Education: Degree ($=1$ if true)",
            "Education: Poly/A Levels ($=1$ if true)",
            "Education: Secondary ($=1$ if true)",
            "Employed ($=1$ if employed)",
            "Personal Income (log)",
            "Housing: 1–3 Rooms ($=1$ if true)",
            "Housing: 4 Rooms ($=1$ if true)",
            "Housing: 5 Rooms ($=1$ if true)",
            "Housing: Private ($=1$ if true)",
            "Owns Home ($=1$ if owned)",
            "Religion: Buddhism ($=1$ if true)",      
            "Religion: Catholicism ($=1$ if true)",             
            "Religion: Christianity ($=1$ if true)", 
            "Religion: Hinduism ($=1$ if true)",
            "Religion: Islam ($=1$ if true)",                      
            "Religion: Taoism ($=1$ if true)",  
            "Self-Rated Health ($=1$ if good)",
            "Mobility Difficulty ($=1$ if severe or worse)",
            "Income Adequacy ($=1$ if adequate)",
            "Avg. Closeness to Children",
            "Avg. Distance to Children (km)",
            "Satisfaction with Relationship with Children",
            "Satisfaction with Friendships",
            "Sampling (Convenience = 1)"
          ),
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE)



# --- Bayesian Model Averaging (BMA) for the Interaction Model ---
# Table B2: Bayesian Model Averaging

# Variable sets
ys <- c("wellbeing_a1_control_norm",
        "wellbeing_a2_satisfaction_norm",
        "wellbeing_a3_affect_norm")

always_support <- c("log_receive_financial", "closeness_log_receive_financial",
                    "log_give_financial", "closeness_log_give_financial",
                    "receive_instrumental", "closeness_receive_instrumental",
                    "give_instrumental", "closeness_give_instrumental",
                    "receive_emotional", "closeness_receive_emotional")

always_rel    <- c("avg_closeness","avg_distance_km","rel_satisfaction_score")
always_other  <- c("sampling")

# exclude religion_hin as there is not enough variation
vary_demo <- c("age","gender","married","num_children",
               "edu_degree", "edu_poly", "edu_secondary",  
               "employed", "log_personal_income",
               "house_13", "house_4", "house_5", "house_private",
               "home_ownership", "religion_none",
               "religion_bud",  
               "religion_cat", "religion_chr",   
               "religion_isl", "religion_tao", 
               "health","mobility_difficulty","has_enough_money",
               "friend_satisfaction_score")

support_labels <- c(
  "Received Financial Support (log)",
  "Closeness $\\times$ Received Financial (log)",
  "Given Financial Support (log)",
  "Closeness $\\times$ Given Financial (log)",
  "Received Instrumental Support",
  "Closeness $\\times$ Received Instrumental",
  "Given Instrumental Support",
  "Closeness $\\times$ Given Instrumental",
  "Received Emotional Support",
  "Closeness $\\times$ Received Emotional Support"
)


# (Optional) keep only variables we need to avoid accidental coercions
keep_vars <- unique(c(ys, always_support, always_rel, always_other, vary_demo))
dat <- df %>% select(all_of(keep_vars)) %>% na.omit()

# ---- helper: build BMS design with fixed regs ----
# BMS expects a matrix with y in the first column, followed by regressors.
# We will order columns as: [always_support, always_rel, always_other, vary_demo]
X_order <- c(always_support, always_rel, always_other, vary_demo)
stopifnot(all(X_order %in% names(dat)))  # sanity check

# Indices (within the design matrix) that should be forced in every model
fixed_idx <- seq_len(length(always_support) + length(always_rel) + length(always_other))

# Containers for stargazer of support vars
coef_list <- list(); se_list <- list()
dep_labels <- c("A1: Perceived Control (norm.)",
                "A2: Life Satisfaction (norm.)",
                "A3: Affect (norm.)")

# Container for PIPs of demographics (to report separately)
pip_demo <- list()

# BMA loop over outcomes 
for (j in seq_along(ys)) {
  yvar <- ys[j]
  
  # Build y|X
  design_df <- cbind(y = dat[[yvar]], dat[, X_order, drop = FALSE])
  
  # IMPORTANT: BMS uses mcmc (not iter)
  bma_fit <- bms(design_df,
                 burn     = 20000,
                 mcmc     = 120000,   # <- was iter=
                 nmodel   = 200000,
                 g        = "UIP",
                 mprior   = "uniform",
                 user.int = FALSE,
                 fixed.reg = fixed_idx)
  
  # Pull coefficient matrix; DO NOT pass incl.prob=
  summ <- coef(bma_fit, std.coefs = FALSE, exact = FALSE)
  # See what's actually there
  # print(colnames(summ))
  
  # Robustly locate the columns we need
  cm <- colnames(summ)
  postmean_col <- if ("postmean" %in% cm) "postmean" else cm[grep("post.?mean", cm, ignore.case=TRUE)][1]
  postsd_col   <- if ("postsd"   %in% cm) "postsd"   else cm[grep("post.?sd", cm, ignore.case=TRUE)][1]
  pip_col      <- if ("inclprob" %in% cm) "inclprob" else cm[grep("incl.?prob|pip", cm, ignore.case=TRUE)][1]
  
  if (any(is.na(c(postmean_col, postsd_col, pip_col))))
    stop("Unexpected coef() columns: ", paste(cm, collapse=", "))
  
  # Keep the 10 support rows in your specified order (some may be missing)
  missing_sup <- setdiff(always_support, rownames(summ))
  if (length(missing_sup))
    warning("Support vars missing in coef(): ", paste(missing_sup, collapse=", "))
  
  sup_rows <- summ[match(always_support, rownames(summ)), , drop = FALSE]
  
  coef_list[[j]] <- sup_rows[, postmean_col]
  se_list[[j]]   <- sup_rows[, postsd_col]
  
  # PIPs for demographics (your names must match rows in `summ`)
  demo_rows <- summ[match(vary_demo, rownames(summ)), , drop = FALSE]
  pip_demo[[j]] <- data.frame(
    variable  = rownames(demo_rows),
    PIP       = as.numeric(demo_rows[, pip_col]),
    PostMean  = as.numeric(demo_rows[, postmean_col]),
    PostSD    = as.numeric(demo_rows[, postsd_col]),
    row.names = NULL
  )
  
  cat("\n====", yvar, "====\nTop demographics by PIP:\n")
  ord <- order(-demo_rows[, pip_col])
  print(head(demo_rows[ord, c(pip_col, postmean_col, postsd_col)], 10))
}


# Build stargazer with BMA-averaged support coefficients 
# stargazer doesn't "know" BMA; we pass the coefficient vectors/se's directly.
# (We also add a note that PIPs for these support variables are 1 because they were forced-in.)

# Create dummy models to feed into stargazer, as models are required in the 
# first argument

dummy1 <- lm(reformulate(always_support, response = "wellbeing_a1_control_norm"), data = dat)
dummy2 <- lm(reformulate(always_support, response = "wellbeing_a2_satisfaction_norm"), data = dat)
dummy3 <- lm(reformulate(always_support, response = "wellbeing_a3_affect_norm"), data = dat)


stargazer(dummy1, dummy2, dummy3,
          type = "latex",                      # change to "text" or "html" as you prefer
          coef = coef_list,
          se = se_list,
          dep.var.labels = dep_labels,
          column.labels = dep_labels,
          covariate.labels = support_labels,
          keep.stat = c("n"),
          star.cutoffs = c(0.10, 0.05, 0.01),  # stars based on normal approx using coef/se
          notes = c("Entries are Bayesian model-averaged posterior means with posterior SD in parentheses.",
                    "Stars use a normal approximation based on posterior mean/posterior SD.",
                    "Support variables were forced in all models (PIP = 1 by design)."),
          notes.align = "l",
          digits = 3,
          title = "Bayesian Model Averaged Effects of Intergenerational Support on Well‑Being"
)


# A compact table of demographic PIPs per outcome 
# You can export or print these to review robustness of demographics.
pip_table <- function(pip_list, outcome_names) {
  out <- pip_list
  names(out) <- outcome_names
  
  for (k in seq_along(out)) {
    cat("\n--- PIPs for demographics:", names(out)[k], "---\n")
    print(
      out[[k]] %>%
        arrange(desc(PIP)) %>%
        mutate(
          PIP = round(PIP, 3),
          PostMean = round(PostMean, 3),
          PostSD = round(PostSD, 3)
        )
    )
  }
}
pip_table(pip_demo, dep_labels)



# --- Identification by Conditional Heteoskedasticity IVs ---
# Table B4: Second-Stage IV Regression using Lewbel’s Instruments

# We omit religion as they are mainly statistically insignificant
# Step 0: Define all candidate exogenous variables
candidate_exog <- c("age", "gender", "married", "num_children",
                    "edu_degree", "edu_poly", "edu_secondary",
                    "employed", "log_personal_income",
                   "house_13", "house_4", "house_5", "house_private",
                   "home_ownership", "religion_bud", "religion_cat",
                   "religion_chr", "religion_hin", "religion_isl",
                   "religion_tao", "health", "mobility_difficulty", 
                   "has_enough_money", "friend_satisfaction_score")

### Religion categories are statistically insignificant - alternative set
#candidate_exog <- c("age", "gender", "married", "num_children",
#                    "edu_degree", "edu_poly", "edu_secondary",
#                    "employed", "log_personal_income",
#                    "house_13", "house_4", "house_5", "house_private", 
#                    "home_ownership", "health", "mobility_difficulty", 
#                    "has_enough_money", "friend_satisfaction_score")

# Step 1: LASSO selection for each endogenous variable
endog_vars <- c("log_receive_financial", "log_give_financial",
                "receive_instrumental", "give_instrumental", "receive_emotional")

lasso_selected <- list()

for (v in endog_vars) {
  set.seed(12345)  
  x <- model.matrix(reformulate(candidate_exog), data = df)[, -1]
  y <- df[[v]]
  cvfit <- cv.glmnet(x, y, alpha = 1)
  coef_idx <- which(coef(cvfit, s = "lambda.min")[-1] != 0)
  selected <- colnames(x)[coef_idx]
  lasso_selected[[v]] <- selected
}


# Step 2: Construct Lewbel instruments for each endogenous variable
lewbel_instr_all <- c()
for (v in endog_vars) {
  exog_v <- lasso_selected[[v]]
  for (x in exog_v) {
    df[[paste0("c_", x)]] <- df[[x]] - mean(df[[x]], na.rm = TRUE)
    res <- resid(lm(df[[v]] ~ ., data = df[, exog_v]))
    zname <- paste0("Z_", v, "_", x)
    df[[zname]] <- df[[paste0("c_", x)]] * res
    lewbel_instr_all <- c(lewbel_instr_all, zname)
  }
}

# Step 3: Fit first-stage regressions and construct interactions
first_stage_fits <- list()

for (v in endog_vars) {
  # Ensure some exogenous variables were selected; fallback if empty
  selected_vars <- lasso_selected[[v]]
  if (length(selected_vars) == 0) {
    warning(paste("LASSO selected nothing for", v, "- using all candidate_exog as fallback"))
    selected_vars <- candidate_exog
  }
  
  # Recalculate residual for Lewbel instrument construction
  res <- resid(lm(df[[v]] ~ ., data = df[, selected_vars, drop = FALSE]))
  
  # Construct instruments
  z_vars <- c()
  for (x in selected_vars) {
    c_name <- paste0("c_", x)
    z_name <- paste0("Z_", v, "_", x)
    if (!c_name %in% names(df)) {
      df[[c_name]] <- df[[x]] - mean(df[[x]], na.rm = TRUE)
    }
    df[[z_name]] <- df[[c_name]] * res
    z_vars <- c(z_vars, z_name)
  }
  
  # Combined instrument set
  inst <- c(selected_vars, z_vars)
  inst <- intersect(inst, names(df))  # ensure all exist
  
  # Check for valid instrument set
  if (length(inst) == 0) {
    warning(paste("No instruments found for", v))
    next
  }
  
  # Run first stage and generate interaction term
  first_stage_fits[[v]] <- lm(df[[v]] ~ ., data = df[, inst, drop = FALSE])
  df[[paste0("int_", v)]] <- fitted(first_stage_fits[[v]]) * df$avg_closeness
}

# Step 4: Run second-stage IV regressions for each outcome
ordered_rhs <- c(
  "log_receive_financial", "int_log_receive_financial",
  "log_give_financial", "int_log_give_financial",
  "receive_instrumental", "int_receive_instrumental",
  "give_instrumental", "int_give_instrumental",
  "receive_emotional", "int_receive_emotional",
  "age", "gender", "married", "num_children",
  "edu_degree", "edu_poly", "edu_secondary",
  "employed", "log_personal_income",
  "house_13", "house_4", "house_5", "house_private",
  "home_ownership",  "religion_cat", "religion_chr", 
  "religion_hin", "religion_isl", "religion_tao",
  "health", "mobility_difficulty", "has_enough_money",
  "rel_satisfaction_score", "friend_satisfaction_score",
  "avg_closeness", "avg_distance_km", "sampling"
)


# Filter only those variables that exist in df
rhs <- ordered_rhs[ordered_rhs %in% colnames(df)]

outcomes <- c("wellbeing_a1_control_norm", "wellbeing_a2_satisfaction_norm", "wellbeing_a3_affect_norm")
iv_models <- list()

for (y in outcomes) {
  instruments_all <- unique(c(unlist(lasso_selected), lewbel_instr_all))
  iv_formula <- as.formula(paste(y, "~", paste(rhs, collapse = " + "), "|", paste(instruments_all, collapse = " + ")))
  iv_models[[y]] <- ivreg(iv_formula, data = df)
}

# Step 5: Robust SEs
se_list <- lapply(iv_models, function(m) sqrt(diag(vcovHC(m, type = "HC1"))))


# Step 6: Stargazer Output 
covariate_labels <- c(
  "Received Financial Support (log)",
  "Closeness $\\times$ Received Financial (log)",
  "Given Financial Support (log)",
  "Closeness $\\times$ Given Financial (log)",
  "Received Instrumental Support",
  "Closeness $\\times$ Received Instrumental",
  "Given Instrumental Support",
  "Closeness $\\times$ Given Instrumental",
  "Received Emotional Support",
  "Closeness $\\times$ Received Emotional Support",
  "Age",
  "Gender ($=1$ if male)",
  "Married ($=1$ if married)",
  "Number of Children",
  "Education: Degree ($=1$ if true)",
  "Education: Poly/A Levels ($=1$ if true)",
  "Education: Secondary ($=1$ if true)",
  "Employed ($=1$ if employed)",
  "Personal Income (log)",
  "Housing: 1–3 Rooms ($=1$ if true)",
  "Housing: 4 Rooms ($=1$ if true)",
  "Housing: 5 Rooms ($=1$ if true)",
  "Housing: Private ($=1$ if true)",
  "Owns Home ($=1$ if owned)",
  "Religion: Buddhism ($=1$ if true)",      
  "Religion: Catholicism ($=1$ if true)",             
  "Religion: Christianity ($=1$ if true)",            
  "Religion: Hinduism ($=1$ if true)",                  
  "Religion: Islam ($=1$ if true)",                      
  "Religion: Taoism ($=1$ if true)",  
  "Self-Rated Health ($=1$ if good)",
  "Mobility Difficulty ($=1$ if severe or worse)",
  "Income Adequacy ($=1$ if adequate)",
  "Avg. Closeness to Children",
  "Avg. Distance to Children (km)",
  "Satisfaction with Relationship with Children",
  "Satisfaction with Friendships"
)
stargazer(iv_models[[1]], iv_models[[2]], iv_models[[3]],
          type = "latex",
          title = "Lewbel IV Estimation with LASSO-Selected Instruments",
          column.labels = c("Control (A1)", "Satisfaction (A2)", "Affect (A3)"),
          se = se_list,
          covariate.labels = covariate_labels,
          dep.var.labels = "Normalized Well-being Index",
          omit.stat = c("f", "ser"),
          digits = 3,
          no.space = TRUE)



# Step 7: Weak-IV Estimates and CLR CI 

# Containers
coef_weak  <- matrix(NA, nrow=length(endog_vars), ncol=length(outcomes), dimnames=list(endog_vars, outcomes))
se_weak    <- coef_weak
pval_weak  <- coef_weak
cset_weak  <- coef_weak


for (y in outcomes) {
  for (v in endog_vars) {
    message("AR Test: ", y, " ~ ", v)
    
    # Define the interaction version of v
    v_int <- paste0("int_", v)
    
    # Exclude the current variable and its interaction from X
    other_vars <- setdiff(endog_vars, v)
    other_ints <- paste0("int_", other_vars)
    
    X_vars <- c(other_vars, other_ints, "avg_closeness", "avg_distance_km", 
                "rel_satisfaction_score", "friend_satisfaction_score")
    
    ivmod_tmp <- ivmodel(
      Y = df[[y]],
      D = df[[v]],  # single endogenous regressor of interest
      Z = as.matrix(df[, instruments_all]),
      X = model.matrix(~ . -1, data = df[, X_vars]),
      intercept = TRUE
    )
    coef_weak[v,y] <- ivmod_tmp$Fuller$point.est
    se_weak[v,y] <- ivmod_tmp$Fuller$std.err
    pval_weak <- ivmod_tmp$Fuller$p.value
    cset_weak[v,y] <-ivmod_tmp$CLR$ci.info
    print(summary(ivmod_tmp, weakiv = "AR"))
  }
}

# After this, we may save the confidence sets cset_weak for reporting.
# The confidence set is in cset_weak




### --- Appendix C: Unconditional quantile regression ---
# Table C1: Unconditional Quantile Regression Results for Well-being Dimensions
# Define variables
support_vars <- c(
  "log_receive_financial", "closeness_log_receive_financial",
  "log_give_financial", "closeness_log_give_financial",
  "receive_instrumental", "closeness_receive_instrumental",
  "give_instrumental", "closeness_give_instrumental",
  "receive_emotional", "closeness_receive_emotional"
)

controls <- c(
  "age", "gender", "married", "num_children", "education", "employed",
  "log_personal_income", "housing", "home_ownership", "religion", "health",
  "mobility_difficulty", "has_enough_money", "avg_closeness",
  "avg_distance_km", "rel_satisfaction_score", "friend_satisfaction_score", "sampling"
)

outcomes <- c("wellbeing_a1_control_norm", "wellbeing_a2_satisfaction_norm", "wellbeing_a3_affect_norm")
taus <- c(0.25, 0.5, 0.75)

# Prepare containers
models <- list()
coef_list <- list()
se_list <- list()
idx <- 1

# Loop over outcomes and quantiles
for (outcome in outcomes) {
  formula_str <- paste(outcome, "~", paste(c(support_vars, controls), collapse = " + "))
  
  for (tau in taus) {
    model <- rifreg::rifreg(
      formula = as.formula(formula_str),
      data = df,
      statistic = "quantiles",
      probs = tau
    )
    
    models[[idx]] <- model$rif_lm[[1]]
    coef_list[[idx]] <- coef(model$rif_lm[[1]])
    se_list[[idx]] <- sqrt(diag(vcov(model$rif_lm[[1]])))  # robust SE
    idx <- idx + 1
  }
}

# Label columns
dep_labels <- rep(c("Control", "Satisfaction", "Affect"), each = 3)
col_labels <- paste(dep_labels, rep(c("(τ=0.25)", "(τ=0.5)", "(τ=0.75)"), 3), sep = " ")

# Generate stargazer tables in blocks of 3
for (i in seq(1, 9, by = 3)) {
  stargazer(models[[i]], models[[i+1]], models[[i+2]],
            type = "latex",
            title = "Unconditional Quantile Regression for Well-being Dimensions (RIF-OLS)",
            column.labels = col_labels[i:(i+2)],
            dep.var.caption = "Dependent Variables",
            dep.var.labels.include = FALSE,
            coef = coef_list[i:(i+2)],
            se = se_list[i:(i+2)],
            covariate.labels = c(
              "Received Financial Support (log)",
              "Closeness $\\times$ Received Financial",
              "Given Financial Support (log)",
              "Closeness $\\times$ Given Financial",
              "Received Instrumental Support",
              "Closeness $\\times$ Received Instrumental",
              "Given Instrumental Support",
              "Closeness $\\times$ Given Instrumental",
              "Received Emotional Support",
              "Closeness $\\times$ Received Emotional"
            ),
            omit.stat = c("n", "ll", "aic", "bic"),
            no.space = TRUE,
            digits = 3
  )
}




############# For the Revision #############

### --- Reviewer 1 Comments on Heterogeneity --- 
### Appendix D: Additional Heterogeneity Analyses
#3 Table D1: Closeness as a Moderator of Support-Well-being Links, by Income Adequacy
# Table D2: Closeness as a Moderator of Support-Well-being Links, by Self-Rated Health
#
# Purpose:
# To address reviewer concern about boundary conditions of
# intergenerational support, we estimate the main interaction
# model separately by:
#   (1) Socioeconomic status: perceived income adequacy
#   (2) Health status: self-rated health
#
# Note:
# We do not estimate gender x co-residence models because the fully crossed 
# cells are too small, especially male non-coresiding parents.



# Labels for focal support variables

support_labels <- c(
  "Received Financial Support (log)",
  "Closeness $\\times$ Received Financial",
  "Given Financial Support (log)",
  "Closeness $\\times$ Given Financial",
  "Received Instrumental Support",
  "Closeness $\\times$ Received Instrumental",
  "Given Instrumental Support",
  "Closeness $\\times$ Given Instrumental",
  "Received Emotional Support",
  "Closeness $\\times$ Received Emotional"
)



# Controls for Appendix D models

appendix_d_controls <- c(
  "age",
  "gender",
  "married",
  "num_children",
  "education",
  "employed",
  "log_personal_income",
  "housing",
  "home_ownership",
  "religion",
  "health",
  "mobility_difficulty",
  "has_enough_money",
  "avg_closeness",
  "avg_distance_km",
  "rel_satisfaction_score",
  "friend_satisfaction_score",
  "sampling"
)



# Function to estimate Appendix D subgroup models

run_appendix_d_models <- function(data,
                                  split_var,
                                  split_values,
                                  split_labels,
                                  table_title,
                                  out_file = NULL) {
  
  # Remove subgroup-defining variable from controls
  controls_use <- setdiff(appendix_d_controls, split_var)
  
  rhs_vars <- c(support_vars, controls_use)
  
  model_formula_d <- as.formula(
    paste("wellbeing ~", paste(rhs_vars, collapse = " + "))
  )
  
  outcomes <- c(
    "wellbeing_a1_control_norm",
    "wellbeing_a2_satisfaction_norm",
    "wellbeing_a3_affect_norm"
  )
  
  models_d <- list()
  
  # Order:
  # Control:      group 1, group 2
  # Satisfaction: group 1, group 2
  # Affect:       group 1, group 2
  idx <- 1
  
  for (outcome in outcomes) {
    for (g in split_values) {
      models_d[[idx]] <- lm(
        update(model_formula_d, as.formula(paste(outcome, "~ ."))),
        data = data,
        subset = !is.na(data[[split_var]]) & data[[split_var]] == g
      )
      idx <- idx + 1
    }
  }
  
  # Aligned coefficients and HC1 robust standard errors
  aligned_d <- lapply(models_d, extract_aligned_coef_se)
  coef_list_d <- lapply(aligned_d, `[[`, "coef")
  se_list_d   <- lapply(aligned_d, `[[`, "se")
  
  # Print subgroup counts for transparency
  cat("\n============================================================\n")
  cat("Subgroup counts for:", split_var, "\n")
  print(table(data[[split_var]], useNA = "ifany"))
  cat("============================================================\n\n")
  
  stargazer(
    models_d,
    coef = coef_list_d,
    se = se_list_d,
    type = "latex",
    title = table_title,
    column.labels = rep(split_labels, times = 3),
    dep.var.labels = "Normalized Well-being Index",
    covariate.labels = support_labels,
    keep = paste0("^", support_vars, "$"),
    omit.stat = c("f", "ser"),
    no.space = TRUE,
    digits = 3,
    out = out_file
  )
  
  return(models_d)
}


# Table D1: Closeness as a Moderator of Support-Well-being Links,
# by Income Adequacy

models_income_adequacy <- run_appendix_d_models(
  data = df,
  split_var = "has_enough_money",
  split_values = c(1, 0),
  split_labels = c("Adequate", "Not Adequate"),
  table_title = "Table D1: Closeness as a Moderator of Support-Well-being Links, by Income Adequacy",
  out_file = "table_D1_income_adequacy.tex"
)


# Table D2: Closeness as a Moderator of Support-Well-being Links,
#by Self-Rated Health

models_health <- run_appendix_d_models(
  data = df,
  split_var = "health",
  split_values = c(1, 0),
  split_labels = c("Good Health", "Fair/Poor Health"),
  table_title = "Table D2: Closeness as a Moderator of Support-Well-being Links, by Self-Rated Health",
  out_file = "table_D2_self_rated_health.tex"
)





### --- Reviewer 2 Sensitivity Analysis --- 
# Use support variables from the closest or weakest child
# Keep the parent-level sample. Only replace averaged support variables.

child_nums <- 1:4

# Select closest or weakest child among observed children only
get_selected_child <- function(data, type = c("closest", "weakest")) {
  type <- match.arg(type)
  
  closeness_mat <- as.matrix(data[, paste0("closeness_child", child_nums)])
  
  apply(closeness_mat, 1, function(x) {
    if (all(is.na(x))) return(NA_integer_)
    
    if (type == "closest") {
      return(which.max(replace(x, is.na(x), -Inf)))
    } else {
      return(which.min(replace(x, is.na(x), Inf)))
    }
  })
}

# Extract selected child's value
get_selected_value <- function(data, varname, selected_child) {
  mat <- as.matrix(data[, paste0(varname, child_nums)])
  out <- rep(NA_real_, nrow(data))
  ok <- !is.na(selected_child)
  out[ok] <- mat[cbind(which(ok), selected_child[ok])]
  out
}

# Construct selected-child support variables
make_selected_child_df <- function(data, type = c("closest", "weakest")) {
  type <- match.arg(type)
  selected_child <- get_selected_child(data, type)
  
  data %>%
    mutate(
      selected_child = selected_child,
      
      receive_financial_selected =
        get_selected_value(., "receive_financial", selected_child),
      give_financial_selected =
        get_selected_value(., "give_financial", selected_child),
      receive_instrumental_selected =
        get_selected_value(., "receive_instrumental", selected_child),
      give_instrumental_selected =
        get_selected_value(., "give_instrumental", selected_child),
      receive_emotional_selected =
        get_selected_value(., "receive_emotional", selected_child),
      
      # Treat missing support for the selected child as zero,
      # consistent with the parent-level support construction.
      receive_financial_selected =
        ifelse(is.na(receive_financial_selected), 0, receive_financial_selected),
      give_financial_selected =
        ifelse(is.na(give_financial_selected), 0, give_financial_selected),
      receive_instrumental_selected =
        ifelse(is.na(receive_instrumental_selected), 0, receive_instrumental_selected),
      give_instrumental_selected =
        ifelse(is.na(give_instrumental_selected), 0, give_instrumental_selected),
      receive_emotional_selected =
        ifelse(is.na(receive_emotional_selected), 0, receive_emotional_selected),
      
      log_receive_financial_selected = log(receive_financial_selected + 1),
      log_give_financial_selected = log(give_financial_selected + 1),
      
      closeness_log_receive_financial_selected =
        closeness_index * log_receive_financial_selected,
      closeness_log_give_financial_selected =
        closeness_index * log_give_financial_selected,
      closeness_receive_instrumental_selected =
        closeness_index * receive_instrumental_selected,
      closeness_give_instrumental_selected =
        closeness_index * give_instrumental_selected,
      closeness_receive_emotional_selected =
        closeness_index * receive_emotional_selected
    )
}

df_closest_child <- make_selected_child_df(df, "closest")
df_weakest_child <- make_selected_child_df(df, "weakest")

### --- Additive models --- ###

model_base_a1_closest <- lm(
  wellbeing_a1_control_norm ~ 
    log_receive_financial_selected +
    log_give_financial_selected +
    receive_instrumental_selected +
    give_instrumental_selected +
    receive_emotional_selected +
    sampling,
  data = df_closest_child
)

model_base_a2_closest <- update(
  model_base_a1_closest,
  wellbeing_a2_satisfaction_norm ~ .
)

model_base_a3_closest <- update(
  model_base_a1_closest,
  wellbeing_a3_affect_norm ~ .
)

model_base_a1_weakest <- update(
  model_base_a1_closest,
  data = df_weakest_child
)

model_base_a2_weakest <- update(
  model_base_a1_weakest,
  wellbeing_a2_satisfaction_norm ~ .
)

model_base_a3_weakest <- update(
  model_base_a1_weakest,
  wellbeing_a3_affect_norm ~ .
)


### --- Interaction models --- ###

model_closeness_1_closest <- lm(
  wellbeing_a1_control_norm ~ 
    log_receive_financial_selected +
    closeness_log_receive_financial_selected +
    log_give_financial_selected +
    closeness_log_give_financial_selected +
    receive_instrumental_selected +
    closeness_receive_instrumental_selected +
    give_instrumental_selected +
    closeness_give_instrumental_selected +
    receive_emotional_selected +
    closeness_receive_emotional_selected +
    age + gender + married + num_children +
    education + employed +
    log_personal_income +
    housing + home_ownership + religion +
    health + mobility_difficulty +
    has_enough_money +
    avg_closeness + avg_distance_km +
    rel_satisfaction_score + friend_satisfaction_score +
    sampling,
  data = df_closest_child
)

model_closeness_2_closest <- update(
  model_closeness_1_closest,
  wellbeing_a2_satisfaction_norm ~ .
)

model_closeness_3_closest <- update(
  model_closeness_1_closest,
  wellbeing_a3_affect_norm ~ .
)

model_closeness_1_weakest <- update(
  model_closeness_1_closest,
  data = df_weakest_child
)

model_closeness_2_weakest <- update(
  model_closeness_1_weakest,
  wellbeing_a2_satisfaction_norm ~ .
)

model_closeness_3_weakest <- update(
  model_closeness_1_weakest,
  wellbeing_a3_affect_norm ~ .
)


### --- Stargazer: closest vs weakest child, additive models --- 
# Table E1: Sensitivity Analysis: Additive Models Using Relationally Closest 
# and Weakest Child Support Values

models_child_base <- list(
  model_base_a1_closest,
  model_base_a2_closest,
  model_base_a3_closest,
  model_base_a1_weakest,
  model_base_a2_weakest,
  model_base_a3_weakest
)

keep_base <- c(
  "log_receive_financial_selected",
  "log_give_financial_selected",
  "receive_instrumental_selected",
  "give_instrumental_selected",
  "receive_emotional_selected"
)

coef_base <- lapply(models_child_base, function(m) coef(m)[keep_base])

se_base <- lapply(models_child_base, function(m) {
  sqrt(diag(vcovHC(m, type = "HC3")))[keep_base]
})

stargazer(models_child_base,
          coef = coef_base,
          se = se_base,
          type = "latex",
          title = "Sensitivity Analysis: Additive Models Using Closest and Weakest Child",
          column.labels = c("Closest", "Closest", "Closest",
                            "Weakest", "Weakest", "Weakest"),
          dep.var.labels = "Normalized Well-being Index",
          covariate.labels = c(
            "Received Financial Support (log)",
            "Given Financial Support (log)",
            "Received Instrumental Support",
            "Given Instrumental Support",
            "Received Emotional Support"
          ),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          digits = 3)


### --- Stargazer: closest vs weakest child, interaction models --- ###
# Table E2: Sensitivity Analysis: Interaction Models Using Relationally 
# Closest and Weakest Child Support Variables

models_child_interact <- list(
  model_closeness_1_closest,
  model_closeness_2_closest,
  model_closeness_3_closest,
  model_closeness_1_weakest,
  model_closeness_2_weakest,
  model_closeness_3_weakest
)

keep_interact <- c(
  "log_receive_financial_selected",
  "closeness_log_receive_financial_selected",
  "log_give_financial_selected",
  "closeness_log_give_financial_selected",
  "receive_instrumental_selected",
  "closeness_receive_instrumental_selected",
  "give_instrumental_selected",
  "closeness_give_instrumental_selected",
  "receive_emotional_selected",
  "closeness_receive_emotional_selected"
)

coef_interact <- lapply(models_child_interact, function(m) coef(m)[keep_interact])

se_interact <- lapply(models_child_interact, function(m) {
  sqrt(diag(vcovHC(m, type = "HC3")))[keep_interact]
})

stargazer(models_child_interact,
          coef = coef_interact,
          se = se_interact,
          type = "latex",
          title = "Sensitivity Analysis: Interaction Models Using Closest and Weakest Child",
          column.labels = c("Closest", "Closest", "Closest",
                            "Weakest", "Weakest", "Weakest"),
          dep.var.labels = "Normalized Well-being Index",
          covariate.labels = c(
            "Received Financial Support (log)",
            "Relationship Quality $\\times$ Received Financial",
            "Given Financial Support (log)",
            "Relationship Quality $\\times$ Given Financial",
            "Received Instrumental Support",
            "Relationship Quality $\\times$ Received Instrumental",
            "Given Instrumental Support",
            "Relationship Quality $\\times$ Given Instrumental",
            "Received Emotional Support",
            "Relationship Quality $\\times$ Received Emotional"
          ),
          omit.stat = c("f", "ser"),
          no.space = TRUE,
          digits = 3)