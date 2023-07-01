library(dplyr)
library(readstata13)
library(gt)

# Load the data
df <- read.dta13("BGLM_Data.dta")


# --------------------------- Table 2
# Filter and aggregate data
df %>% 
  filter(env %in% c("B", "LD")) %>%
  group_by(treat, type) %>%
  summarise(across(c(da, db, dc), mean, na.rm = TRUE)) %>%
  
  mutate(across(c(da, db, dc), ~.x*100, .names = "{.col}"),
         across(c(da, db, dc), ~round(.x, 2), .names = "{.col}")) %>%
  
  separate(treat, into = c("vote", "treat"), sep = "_") %>%
  mutate(treat = ifelse(treat == "B", "Baseline", "Low Disagreement")) %>%
  
  gt(rowname_col = c("vote"))

# create your gt table with group labels
gt(df) %>%
  tab_header(
    title = "Treatment Results"
  ) %>%
  cols_label(
    Treatment = " ",
    Type = "Type",
    chr = "Character",
    da = "da",
    db = "db",
    dc = "dc"
  ) %>%
  tab_stubhead(label = "Treatment")