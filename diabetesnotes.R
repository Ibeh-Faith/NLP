library(tidyverse)
library(magrittr)
library(bigrquery)
library(bigrquery)
library(stringr)
library(caret)
library(dplyr)


con <- DBI::dbConnect(drv = bigquery(),
                      project = "learnclinicaldatascience")
patient <- tbl(con, "course4_data.diabetes_goldstandard") %>%
  collect()
notes <- tbl(con, "course4_data.diabetes_notes") %>% 
  collect()

##Feature Engineering ###Key Window Technique

#First, create text windows for notes containing the word linked to diabetes, using window size of 20.

extract_text_window <- function(dataframe, keyword, half_window_size) {
  dataframe %>% 
    group_by(NOTE_ID) %>% 
    mutate(WORDS = TEXT) %>% 
    separate_rows(WORDS, sep = "[ \n]+") %>% 
    mutate(INDEX = seq(from = 1, to = n(), by = 1.0),
           WINDOW_START = case_when(INDEX - half_window_size < 1 ~ 1, TRUE ~ INDEX - half_window_size), 
           WINDOW_END = case_when(INDEX + half_window_size > max(INDEX) ~ max(INDEX), TRUE ~ INDEX + half_window_size), 
           WINDOW = word(string = TEXT, start = WINDOW_START, end = WINDOW_END, sep = "[ \n]+")) %>% 
    ungroup() %>% 
    filter(str_detect(string = WORDS, pattern = regex(keyword, ignore_case = TRUE)))
}

diabetes_notes_window <- notes %>%
  extract_text_window(keyword = "(?<![a-zA-Z])diabet(es|ic)?(?![a-zA-z])", half_window_size = 10)


#Then, negation is removed, identifying searches that look out for family history or words such as ‘no’ or ‘negative’ diabetes signs.

diabetes_notes1 <- diabetes_notes_window %>%
  mutate(EXCLUDE = case_when(
    str_detect(WINDOW, regex(pattern = "no history of (?<![a-zA-Z])diabet(es|ic)?(?![a-zA-z])", ignore_case = TRUE)) ~1,  
    str_detect(WINDOW, regex(pattern = "den(ies|y)? any comorbid complications", ignore_case = TRUE)) ~1, 
    str_detect(WINDOW, regex(pattern = "family history", ignore_case = TRUE)) ~1, 
    str_detect(WINDOW, regex(pattern = "negative for (?<![a-zA-Z])diabet(es|ic)?(?![a-zA-z])", ignore_case = TRUE)) ~1, 
    str_detect(WINDOW, regex(pattern = "(father|mother) (also)? (?<![a-zA-Z])diabet(es|ic)?(?![a-zA-z])", ignore_case = TRUE)) ~1, 
    str_detect(WINDOW, regex(pattern = "no weakness, numbness or tingling", ignore_case = TRUE)) ~1, TRUE ~0)) %>%
  filter(EXCLUDE !=1) 

#Then, notes indicating diabetic patients with diabetic complications are identified, using keywords such as the signs and symptoms in REGEX, while categorizing the three different types of diabetes into their own types.

diabetes_complic <- diabetes_notes1 %>%
  mutate(COMPLICATIONS = case_when(
    str_detect(WINDOW, regex("(?<![a-zA-Z])neuropath(y|ic)?(?![a-zA-z])", ignore_case = TRUE)) ~ "neuropathy", 
    str_detect(WINDOW, regex("diabetic nerve pain", ignore_case = TRUE)) ~ "neuropathy", 
    str_detect(WINDOW, regex("tingling", ignore_case = TRUE)) ~ "neuropathy", 
    str_detect(WINDOW, regex("(?<![a-zA-Z])nephropathy(?![a-zA-z])", ignore_case = TRUE)) ~"nephropathy", 
    str_detect(WINDOW, regex("renal (insufficiency|disease)", ignore_case = TRUE)) ~"nephropathy", 
    str_detect(WINDOW, regex("(?<![a-zA-Z])retinopath(y|ic)?(?![a-zA-z])", ignore_case = TRUE)) ~"retinopathy", TRUE~"")) %>%
  arrange(desc(COMPLICATIONS))

pred_neuro <- diabetes_complic %>% 
  filter(COMPLICATIONS == "neuropathy") %>%
  distinct(NOTE_ID) %>% 
  mutate(predicted_neuropathy=1)

pred_nephro <- diabetes_complic %>% 
  filter(COMPLICATIONS == "nephropathy") %>%
  distinct(NOTE_ID) %>% 
  mutate(predicted_nephropathy=1)

pred_retino <- diabetes_complic %>% 
  filter(COMPLICATIONS == "retinopathy") %>%
  distinct(NOTE_ID) %>% 
  mutate(predicted_retinopathy=1)

##Review the accuracy of the technique Calculating the sensitivity and specificity of the technique, by comparing the gold standard population and the predicted results from the text processing of clinical notes.

getStats <- function(df, ...){
  df %>%
    select_(.dots = lazyeval::lazy_dots(...)) %>%
    mutate_all(funs(factor(., levels = c(1,0)))) %>% 
    table() %>% 
    confusionMatrix()
}

patient %>%
  left_join(pred_neuro) %>%
  left_join(pred_nephro) %>%
  left_join(pred_retino) %>%
  mutate(predicted_neuropathy=coalesce(predicted_neuropathy,0),
         predicted_nephropathy=coalesce(predicted_nephropathy,0),
         predicted_retinopathy=coalesce(predicted_retinopathy,0)) %>%
  mutate(predicted_complic=case_when(predicted_neuropathy==1|predicted_nephropathy==1|predicted_retinopathy==1 ~ 1,
                                     TRUE ~ 0)) %>%
  select(c(NOTE_ID, ANY_DIABETIC_COMPLICATION, predicted_complic)) %>%
  collect() %>%
  getStats(predicted_complic, ANY_DIABETIC_COMPLICATION)  

##Review the accuracy of the technique Calculating the sensitivity and specificity of the technique, by comparing the gold standard population and the predicted results from the text processing of clinical notes.

getStats <- function(df, ...){
  df %>%
    select_(.dots = lazyeval::lazy_dots(...)) %>%
    mutate_all(funs(factor(., levels = c(1,0)))) %>% 
    table() %>% 
    confusionMatrix()
  ##Review the accuracy of the technique Calculating the sensitivity and specificity of the technique, by comparing the gold standard population and the predicted results from the text processing of clinical notes.
  
  getStats <- function(df, ...){
    df %>%
      select_(.dots = lazyeval::lazy_dots(...)) %>%
      mutate_all(funs(factor(., levels = c(1,0)))) %>% 
      table() %>% 
      confusionMatrix()
  }
  
  patient %>%
    left_join(pred_neuro) %>%
    left_join(pred_nephro) %>%
    left_join(pred_retino) %>%
    mutate(predicted_neuropathy=coalesce(predicted_neuropathy,0),
           predicted_nephropathy=coalesce(predicted_nephropathy,0),
           predicted_retinopathy=coalesce(predicted_retinopathy,0)) %>%
    mutate(predicted_complic=case_when(predicted_neuropathy==1|predicted_nephropathy==1|predicted_retinopathy==1 ~ 1,
                                       TRUE ~ 0)) %>%
    select(c(NOTE_ID, ANY_DIABETIC_COMPLICATION, predicted_complic)) %>%
    collect() %>%
    getStats(predicted_complic, ANY_DIABETIC_COMPLICATION)  
  
  ##Review the accuracy of the technique Calculating the sensitivity and specificity of the technique, by comparing the gold standard population and the predicted results from the text processing of clinical notes.
  
  getStats <- function(df, ...){
    df %>%
      select_(.dots = lazyeval::lazy_dots(...)) %>%
      mutate_all(funs(factor(., levels = c(1,0)))) %>% 
      table() %>% 
      confusionMatrix()
  }
  
  patient %>%
    left_join(pred_neuro) %>%
    left_join(pred_nephro) %>%
    left_join(pred_retino) %>%
    mutate(predicted_neuropathy=coalesce(predicted_neuropathy,0),
           predicted_nephropathy=coalesce(predicted_nephropathy,0),
           predicted_retinopathy=coalesce(predicted_retinopathy,0)) %>%
    mutate(predicted_complic=case_when(predicted_neuropathy==1|predicted_nephropathy==1|predicted_retinopathy==1 ~ 1,
                                       TRUE ~ 0)) %>%
    select(c(NOTE_ID, ANY_DIABETIC_COMPLICATION, predicted_complic)) %>%
    collect() %>%
    getStats(predicted_complic, ANY_DIABETIC_COMPLICATION)  
  
  patient %>%
    left_join(pred_neuro) %>%
    left_join(pred_nephro) %>%
    left_join(pred_retino) %>%
    mutate(predicted_neuropathy=coalesce(predicted_neuropathy,0),
           predicted_nephropathy=coalesce(predicted_nephropathy,0),
           predicted_retinopathy=coalesce(predicted_retinopathy,0)) %>%
    mutate(predicted_complic=case_when(predicted_neuropathy==1|predicted_nephropathy==1|predicted_retinopathy==1 ~ 1,
                                       TRUE ~ 0)) %>%
    select(c(NOTE_ID, ANY_DIABETIC_COMPLICATION, predicted_complic)) %>%
    collect() %>%
    getStats(predicted_complic, ANY_DIABETIC_COMPLICATION)  
  
  # <!-- From the results, 22 cases were true positives and 110 were true negatives (correctly identified) while 4 were false positives and 5 were false negatives. -->
  #   
  #   <!-- In conclusion, the accuracy of this keyword window technique used is 0.9362, while the sensitivity is 0.8148 and specificity is 0.9649. There is a balance of sensitivity and specificity, with a relatively high accuracy.  -->
  #   <!-- Therefore, this method can be used to classify such clinical notes in the future. We could try to use the note section technique as an alternative method where we use the context provided by the note section to understand  -->
  #   <!-- the meaning of the keyword matches in the text. -->