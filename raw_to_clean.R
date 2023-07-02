library(tidyverse)
library(haven)
library(zTree)
library(readxl)

dir <- "/Users/joelmlevin/Documents/Projects/replication/bouton"
setwd(dir)

# options(rstudio.help.showDataPreview = FALSE)

source("https://raw.githubusercontent.com/joelmlevin/public_r_functions/main/fix_names.R")

# importing the clean data, for reference: 
clean <- read_dta("replication/BGLM_Data.dta") 
names(clean) <- tolower(names(clean)) 

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
globals <- map_dfr(session_ids, get_globals)
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

subjects <- map_dfr(session_ids, get_subjects)

subjects

# 3. Creating a dataset with both subject and globals data

both_wide <- subjects %>%
  left_join(globals, by = "sessionid") %>%
  arrange(sessionid, group, )

names(both_wide)

# there are some strange (suspected) floating point arithmetic things going on here, which are making values that appear equal to not compute as being equal. this causes lots of issues with both matching on numerical values and writing functional statements (e.g, creating variables based on values of other variables)
# to address this, we'll round every numerical value in the dataset

both_wide <- both_wide %>%
  mutate(across(where(is.numeric), \(x) round(x, 4)))

# 3 Comparing variables across the original clean data and our assembled dataset

names(clean) %>% sort()
names(both_wide) %>% sort()

cbind(
  names(clean),
  names(clean) %in% names(both_wide) 
)

needs_matching <- both_wide %>%
  select(!any_of(names(clean)))

# 3.1 using means to identify variables that are the same

# 2xN tables of variables and their means
cleanmeans <- clean %>%
  ungroup() %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  summarise(across(where(is.numeric), round, 3)) %>%
  pivot_longer(cols = everything())

cleanmeans %>% print(n=100)

rawmeans <- needs_matching %>%
  ungroup() %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  summarise(across(where(is.numeric), round, 3)) %>%
  pivot_longer(cols = everything())

rawmeans %>% print(n=200)

# matching these tables based on mean values
match_means <- left_join(cleanmeans, 
                         rawmeans,
                         by = "value") %>%
  rename(name_clean = name.x,
         name_raw = name.y) %>%
  drop_na()

# looking at the matches to make sure that they are face valid.
# they appear to be
match_means %>%
  # arrange(value) %>%
  print(n=200)

both_wide_renamed <- both_wide %>%
  rename(dta = type_a,
         dtb = type_b,
         dtc = type_c,
         ra = r_a,
         rb = r_b,
         rc = r_c,
         win1rn = winner_1r
         
         )

globals 
unique(clean$rule)

name_diagnostics <- cbind(tolower(names(clean)),
      tolower(names(clean)) %in% names(both_wide)) %>%
  as_tibble() %>%
  rename(name_from_clean = V1,
         in_compiled = V2) 

needtomatch <- name_diagnostics %>%
  filter(in_compiled == FALSE) %>%
  .$name_from_clean

# these are variables that appear in the clean data but not in the compiled (raw) dataset
needtomatch

# variables that are constructed:
# dwa:dwc are dummies for who they voted for
  
# env is baseline or low agreement
# rule is electoral rule
# treat and treatn are indicators for the treatment
# dwx is are dummies for who won the election (we need the simulations for this)
# pay is the payoff, which is depending on electoral outcomes (which depend on sims)
# igroup is the group indicator
# igroupt: NOT SURE 
# sid: NOT SURE
# threshold is: the minimal percentage of sincere voting by tB-voters that makes sincere voting a best response for those voters under run-off (pp127, also see Table G3)
  # NOT IN RAW DATA
# the nperiods variables are just coarsened period # variables

names(both_wide)

unique(clean$env)
unique(clean$treatn)
unique(clean$treat)
unique(clean$thr)
unique(clean$dwa)
unique(clean$dwb)
unique(clean$dwc)
unique(clean$pay)
unique(clean$igroup)
unique(clean$igroupt)
unique(clean$sid)
unique(clean$threshold)
unique(clean$fiveperiods)

both_wide %>%
  group_by(sessionid) %>%
  mutate(prop_second_round = mean(secondround)) %>%
  ungroup() %>%
  transmute(treatment = ifelse(prop_second_round == 0, "Plurality", "Runoff")) %>%
  count(treatment)


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

inferred_parameters <- both_wide_renamed %>%
  select(sessionid, ra, rb, rc, pac) %>%
  unique() %>%
  mutate(
    inferred_parameters = case_when(
    ra == .34 & rb == .22 & pac == 76 ~ "Baseline", # need to work on integrating the conditional ps into these
    ra == .43 & rb == .13 ~ "Low Dis",
    ra == .37 & rb == .24 ~ "Small minority",
    ra == .34 & rb == .22 & rc == .44 & pac == 99 ~ "No upset")) %>%
  select(sessionid, inferred_parameters)

inferred_parameters %>% group_by(inferred_parameters) %>% count()

inferred_parameters %>%
  group_by(inferred_parameters) %>% count()
  filter(between(ra, .42, .44)) %>%
  count()

options(digits=10)
both_wide_renamed$ra
unique(both_wide_renamed$ra)
str(both_wide_renamed$ra)

both_wide_renamed %>%
  filter(ra == .34 & rb == 0.22) %>%
  select(ra, rb)

both_wide_renamed %>%
  unite("raandb", ra, rb) %>%
  .$raandb %>%
  unique()

unique(both_wide_renamed$ra)
unique(both_wide_renamed$rb)
unique(inferred_parameters$inferred_parameters)
  

inferred_parameters %>% group_by(inferred_parameters) %>% count()

inferred_parameters
inferred_treatment

# merging them into the main df
both_wide_renamed %>%
  left_join(inferred_parameters, by = "sessionid") %>%
  left_join(inferred_treatment, by = "sessionid") %>%
  unite("inferred_condition", inferred_treatment, inferred_parameters) %>%
  group_by(inferred_condition) %>%
  count()

# so, we are now comfortable using the authors' treatment variable in building our raw dataset
treatment_identifiers <- clean %>%
  select(sessionid, treatn) %>%
  unique()

both_wide_2 <- both_wide %>% left_join(treatment_identifiers, by = "sessionid")  

unique(both_wide_2$treatn)

