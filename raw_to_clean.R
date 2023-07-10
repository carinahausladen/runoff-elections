# A script to clean the raw data from Bouton et al. 2022 
# 
# Bouton, L., Gallego, J., Llorente-Saguer, A., & Morton, R. (2022). Run-off Elections in the Laboratory. The Economic Journal, 132(641), 106–146. https://doi.org/10.1093/ej/ueab051
#
# Prepared by Joel Levin, Carina Hausladen, and Sean Hu

library(tidyverse)
library(haven)
library(zTree)
library(readxl)
library(psych)

dir <- "bouton"
setwd(dir)

# options(rstudio.help.showDataPreview = FALSE)

source("https://raw.githubusercontent.com/joelmlevin/public_r_functions/main/fix_names.R")

# importing the clean data, for reference: 
clean <- read_dta("replication/BGLM_Data.dta") 
names(clean) <- tolower(names(clean)) 

# there is a typo in which they misspell 'disagreement' in the `treatn` variable, which causes issues later. correcting
clean <- clean %>% 
  mutate(treatn = str_replace(treatn, "Disaggreement", "Disagreement"))

# importing the variable definitions
definitions <- read_excel("replication/BGLM_codebook.xlsx", 
                          skip = 4)

# importing the raw data
# note that the files are stored as csvs, but they appear to actually be xls files that have been named incorrectly.
# we will first convert these back to xls files, then use the `zTree` package to load them
# note: before running this code, copy the existing folder 'raw data' in its existing directory and call it 'raw data xls'

old_files <- list.files("replication/additional files/raw data xls/")[1:35]
old_files
old_paths <- paste0("replication/additional files/raw data xls/", old_files)
old_paths

new_files <- str_replace_all(old_files, "csv", "xls")
new_files
new_paths <- paste0("replication/additional files/raw data xls/", new_files)
new_paths

file.rename(from = old_paths, to = new_paths)

# generating a vector of session ids to iterate over
# there are two ways to get this. 
# below we show that both yield the same result

# 1. from the `session_id` variable in the clean data
clean_sessionids <- unique(clean$sessionid)
# 2. from the raw data filenames
raw_sessionids <- list.files("replication/additional files/raw data/")[1:35] %>%
  str_remove(".csv")

identical(
  sort(clean_sessionids),
  sort(raw_sessionids)
)

# since they're the same, we'll just use one
session_ids <- clean_sessionids

# 2. Import functions #####

# 2.1 A function to import and format globals data from each xls --------
get_globals <- function(session_id) {
  
  raw <- zTreeTables(paste0("replication/additional files/raw data xls/", session_id, ".xls"))
  
  raw$globals %>%
    as_tibble() %>%
    fix_names() %>%
    rename(sessionid = date)
  
}

# testing
# get_globals("180419_1649")
# get_globals("170922_1043") 
# get_globals("170920_1023") 

# creating a globals dataset by iterating across session ids

# globals <- map_dfr(session_ids, get_globals)
# write_rds(globals, "globals.rds")

globals <- read_rds("globals.rds")
globals

# note that all of these global variables are actually at the session level (n = 35), not at the session X period level, which is what we appear to have in this dataset
# we show this as follows, based on variance estimates of each numerical variable at the session level

globals %>%
  group_by(sessionid) %>%
  summarise(across(period:pcb, sd)) %>% 
  psych::describe()

# therefore, we just use one observation from each session and drop the period variable

globals <- globals %>%
  select(-period) %>%
  group_by(sessionid) %>%
  summarise(across(numperiods:pcb, first))

globals

# 2.2 A function to import and format subject data from each xls --------
get_subjects <- function(session_id) {
  
  raw <- zTreeTables(paste0("replication/additional files/raw data xls/", session_id, ".xls"))
  
  raw$subjects %>%
    as_tibble() %>%
    fix_names() %>%
    rename(sessionid = date)
  
}

# iterating across session ids

# subjects <- map_dfr(session_ids, get_subjects)
# write_rds(subjects, "subjects.rds")

subjects <- read_rds("subjects.rds")
subjects

# 3. Creating a dataset with both subject and globals data

both_wide <- subjects %>%
  left_join(globals, by = "sessionid") %>%
  arrange(sessionid, group)

names(both_wide)

# there are some strange (suspected) floating point arithmetic things going on here, which are making values that appear equal to not compute as being equal. this causes lots of issues with both matching on numerical values and writing functional statements (e.g, creating variables based on values of other variables)
# to address this, we'll round every numerical value in the dataset

both_wide <- both_wide %>%
  mutate(across(where(is.numeric), \(x) round(x, 6))) %>%
  select(-treatment) # a variable that always takes a value of 1 (useless) and has a confusing name

clean <- clean %>%
  mutate(across(where(is.numeric), \(x) round(x, 6)))
  


# 3 Comparing variables across the original clean data and our assembled dataset

names(clean) %>% sort()
names(both_wide) %>% sort()

cbind(
  names(clean),
  names(clean) %in% names(both_wide) 
)

# selecting only variables that need to be matched across clean and raw datasets
needs_matching <- both_wide %>%
  select(!any_of(names(clean)))

# 3.1 using means to identify variables that are the same

# 2xN tables of variables and their means
cleanmeans <- clean %>%
  ungroup() %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything()) %>%
  mutate(value = round(value, 6))

cleanmeans %>% print(n=100)

rawmeans <- needs_matching %>%
  ungroup() %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(value = round(value, 6))

rawmeans %>% print(n=200)
# 
# cleanmeans$name %>% sort()
# rawmeans$name %>% sort()
# 
# test <- full_join(cleanmeans, 
#           rawmeans,
#           by = "value") 
# 
# test %>% arrange(value) %>% print(n=100)
# 
# test$name.x %in% test$name.y
# 
# cleannamestest <- test$name.x[1:38]
# 
# test %>%
#   filter(name.y %in% test$name.y)
# 
# test %>% print(n=200)

# matching these tables based on mean values
match_means <- full_join(cleanmeans, 
                         rawmeans,
                         by = "value") %>% 
  rename(name_clean = name.x,
         name_raw = name.y) %>% 
  drop_na()

# looking at the matches to make sure that they are face valid. they all appear to be, with the possible exception of `pab` in the clean data
match_means %>%
  print(n=100)

# manually updating these
both_wide_renamed <- both_wide %>%
  rename(dta = type_a,
         dtb = type_b,
         dtc = type_c,
         ra = r_a,
         rb = r_b,
         rc = r_c,
         da = a,
         db = b,
         dc = c,
         win1rn = winner_1r
         )

# identifying which variables are in the clean dataset but not in our new, compiled dataset
name_diagnostics <- 
  cbind(tolower(names(clean)), 
        tolower(names(clean)) %in% names(both_wide_renamed)) %>% 
  as_tibble() %>%
  rename(name_from_clean = V1,
         in_compiled = V2) 

needtomatch <- name_diagnostics %>%
  filter(in_compiled == FALSE) %>%
  .$name_from_clean

# these are variables that appear in the clean data but not in the compiled (raw) dataset
needtomatch

# dwa:dwc are dummies for who they voted for [can't infer for runoff conditions, bc simulation]
# dwx is are dummies for who won the election (we need the simulations for this)
# pay: never used in Figures.do or Tables.do --> we do not replicate
# igroupt: used for Figure 4 and 8 only --> we do not replicate

# adding 'tenperiods' variable
both_wide_renamed <- both_wide_renamed %>%
  mutate(tenperiods = cut(period, breaks = seq(0, 60, by = 10), 
                          labels = seq(10, 60, by = 10), 
                          include.lowest = TRUE, right = FALSE))

# adding an 'sid' variable, which uniquely identifies participants
both_wide_renamed <- both_wide_renamed %>%
  unite(sid, sessionid, subject, remove = FALSE) %>%
  mutate(sid = factor(sid))

# adding an 'igroup' variable, which uniquely identifies groups
both_wide_renamed <- both_wide_renamed %>%
  unite(igroup, sessionid, group, remove = FALSE) %>%
  mutate(igroup = factor(igroup))

# names(both_wide)
# 
# unique(clean$env)
# unique(clean$treatn)
# unique(clean$treat)
# unique(clean$thr)
# unique(clean$dwa)
# unique(clean$dwb)
# unique(clean$dwc)
# unique(clean$pay)
# unique(clean$igroup)
# unique(clean$igroupt)
# unique(clean$sid)
# unique(clean$threshold)
# unique(clean$fiveperiods)

# because there is no treatment assignment variable in the raw data, we want to check to see whether the authors' treatment assignment is consistent with what's in the raw data
# the raw data include a dummy variable, `secondround`, that denotes whether there was a second round election in each period, which should only happen in runoff treatments
# below, we confirm that this is true

shouldhavesecondround <- clean %>%
  select(sessionid, treatn) %>%
  unique() %>%
  mutate(shouldsecondround = ifelse(str_detect(treatn, "Runoff") == TRUE, 1, 0)) %>%
  filter(shouldsecondround == 1) %>%
  .$sessionid

shouldhavesecondround

inferred_treatment <- both_wide %>%
  select(sessionid, secondround) %>%
  group_by(sessionid) %>%
  summarise(prop_2n = mean(secondround)) %>%
  mutate(inferred_treatment = case_when(prop_2n > 0 ~ "Runoff",
                                        prop_2n == 0 ~ "Plurality")) %>%
  select(sessionid, inferred_treatment)

# identifying baseline versus low disagreement
# parameters are listed in Tables 1 and 4
# note that the conditional probabilities (`pab`, etc.) are only useful for differentiating between baseline-runoff and no upset-runoff.
# conditional probabilities should not be used to distinguish between other conditions because they are *always missing* in plurality conditions 

inferred_parameters <- both_wide_renamed %>%
  select(sessionid, ra, rb, rc, pab, pac, pbc) %>%
  unique() %>%
  mutate(
    inferred_parameters = case_when(
    ra == .34 & 
      rb == .22 & 
      rc == .44 &
      (pac == 76 | is.na(pac)) ~ "Baseline", 
    ra == .43 & 
      rb == .13 ~ "Low Disagreement",
    ra == .37 & 
      rb == .24 & 
      rc == .39 ~ "Small Minority",
    ra == .34 & 
      rb == .22 & 
      rc == .44 & 
      pac == 99 ~ "No Upset")) %>%
  select(sessionid, inferred_parameters)

# creating an object with all of the inferred variable names...
inferred_everything <- inner_join(inferred_treatment, inferred_parameters, by = "sessionid") %>%
  unite("treatment", inferred_treatment:inferred_parameters, sep = " - ", remove = FALSE) %>%
  mutate(treat = case_when(
    treatment == "Plurality - Baseline" ~ "P_B",
    treatment == "Plurality - Low Disagreement" ~ "P_LD",
    treatment == "Plurality - Small Minority" ~ "P_SM",
    treatment == "Runoff - Baseline" ~ "R_B",
    treatment == "Runoff - Low Disagreement" ~ "R_LD",
    treatment == "Runoff - Small Minority" ~ "R_SM",
    treatment == "Runoff - No Upset" ~ "R_NU",
  ))

inferred_everything

# we appear to be missing one runoff, small minority group
inferred_everything %>% group_by(treatment) %>% count()
inferred_everything %>% group_by(treat) %>% count()

# are the names the same as in the clean dataset?
# yes! (after typo correction in the clean dataset)
identical(
  sort(unique(inferred_everything$treatment)),
  sort(unique(clean$treatn))
)

# merging this into the main dataset
both_wide_renamed <- inferred_everything %>%
  select(sessionid, treatment, treat) %>%
  right_join(both_wide_renamed, by = "sessionid")

both_wide_renamed %>%
  group_by(treatment) %>% count()

both_wide_renamed %>% group_by(group, treatment) %>% count()

# adding a `threshold`, based on table G3 in the manuscript
# flag
# it appears that Table G3 has a few errors:
# 1. the leftmost column has values: "P_B, P_LD, P_SM, R_NU", but there are columns for both runoff and plurality. 
#    they almost certainly meant not to include the letter before the underscore, which represents election type (runoff or plurality) in their notation.      
# 2. there is a value for no upset/plurality, which is a condition that doesn't exist. for no upset, only a runoff condition exists  
# below, we'll create this variable based on our understanding of what the authors intended this table to communicate.

# unique(clean$threshold)

both_wide_renamed <- both_wide_renamed %>%
  mutate(threshold = case_when(
    treat == "P_B" ~ 1.0236,
    treat == "P_LD" ~ 1.7322,
    treat == "P_SM" ~ 0.9616,
    treat == "R_B" ~ 0.5845,
    treat == "R_LD" ~ 1.0607,
    treat == "R_SM" ~ 0.4786,
    treat == "R_NU" ~ 0.8345
  )) 

names(clean)
unique(clean$env)
unique(clean$rule)
names(both_wide_renamed)
both_wide_renamed$treat

clean_vars <- names(clean)
clean_vars 

both_wide_renamed$type[both_wide_renamed$type == 1] <- 'a'
both_wide_renamed$type[both_wide_renamed$type == 2] <- 'b'
both_wide_renamed$type[both_wide_renamed$type == 3] <- 'c'

"we need to fill NAs. socio demographics were recorded only for period=60 participant"
both_wide_renamed %>%
  group_by(sessionid, subject) %>%
  fill(gender, age, year, risk, trust, experiments, politicosity, .direction = "downup") ->both_wide_renamed

# making sure that we have every variable in the clean dataset, and that they are named consistently
output <- both_wide_renamed %>% 
  rename(treatn = treatment) %>% # renaming
  separate(treat, c("rule", "env"), remove = FALSE) %>% # creating "rule" and "env" variables
  select(any_of(names(clean)))
  
# only variables that are short enough for stata
# shortenough <- cbind(
#   names(output),
#   nchar(names(output))
# ) %>%
#   as_tibble() %>%
#   filter(V2 <= 32) %>%
#   .$V1





output %>%
  write_dta("compiled_dataset.dta")

