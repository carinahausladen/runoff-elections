# --------------------------- Tables 3 & 6
rm(list=ls())
library(dplyr)
library(tidyr)
library(purrr)
library(readstata13)
library(gt)

set.seed(42)
df <- read.dta13("/Users/carinaines/Documents/GitHub/runoff-elections/BGLM_Data.dta")

# Arrange the df and average frequencies played by each subject in a group.
df %>% 
  group_by(igroup, subject, type) %>% 
  mutate(across(c(da, db, dc), 
                list("30" = ~mean(.x[period > 30], na.rm = TRUE),
                     "60" = mean), 
                .names = "{.col}{.fn}")) %>%
  ungroup() %>%
  select(!c(da,db,dc))->df


# Generate variables
df %>%
  mutate(
    dad = (type == "a" | type == "b"),
    dbd = 0,
    dcd = (type == "c"),
    dadb = 0,
    dbdb = (type == "a" | type == "b"),
    dcdb = (type == "c"),
    das = (type == "a"),
    dbs = (type == "b"),
    dcs = (type == "c"))->df


# collapse
df %>%
  group_by(treat, env, rule, sessionid, igroup, subject, type, ra, rb, rc, thr, pAB, pAC, pBC) %>%
  summarise(da30 = mean(da30, na.rm = TRUE),
            db30 = mean(db30, na.rm = TRUE),
            dc30 = mean(dc30, na.rm = TRUE),
            da60 = mean(da60, na.rm = TRUE),
            db60 = mean(db60, na.rm = TRUE),
            dc60 = mean(dc60, na.rm = TRUE),
            dad = mean(dad, na.rm = TRUE),
            dbd = mean(dbd, na.rm = TRUE),
            dcd = mean(dcd, na.rm = TRUE),
            dadb = mean(dadb, na.rm = TRUE),
            dbdb = mean(dbdb, na.rm = TRUE),
            dcdb = mean(dcdb, na.rm = TRUE),
            das = mean(das, na.rm = TRUE),
            dbs = mean(dbs, na.rm = TRUE),
            dcs = mean(dcs, na.rm = TRUE), .groups="drop")->df

# use all df if action was not played (only happens with one subject)
df %>%
  mutate(
    aux = is.na(da30) + is.na(db30) + is.na(dc30),
    da30 = if_else(aux > 0, da60, da30),
    db30 = if_else(aux > 0, db60, db30),
    dc30 = if_else(aux > 0, dc60, dc30)
    ) %>%
  select(-aux)->df


# Rename second round probs
df %>%
  rename(da = da60, 
         db = db60, 
         dc = dc60,
         dah = da30, 
         dbh = db30, 
         dch = dc30) %>%
  mutate(
    p12 = pAB,
    p21 = 1 - pAB,
    p13 = pAC,
    p31 = 1 - pAC,
    p23 = pBC,
    p32 = 1 - pBC)->df

# reshape the df "--> column ra got lost here"
df %>%
  mutate(typen = case_when(
    type == "b" ~ 2,
    type == "c" ~ 3,
    TRUE ~ 1
  )) %>%
  select(-type) %>%
  pivot_wider(
    id_cols = c(igroup, subject,ra,rb),
    names_from = typen,
    values_from = c(da, db, dc, dah, dbh, dch, dad, dbd, dcd, dadb, dbdb, dcdb, das, dbs, dcs),
    names_prefix = "typen"
  )->df

# define new variables and starting values
obs <- 10000 
df$aux <- NA 
df$t <- 0 

#//----------------------------------- We use 1,2,3 instead of abc since it allows to use loops
for(j in 1:3){
  df[paste0("v", j)] <- 0
  df[paste0("v", j, "h")] <- 0
  df[paste0("v", j, "d")] <- 0
  df[paste0("v", j, "s")] <- 0
  df[paste0("w", j)] <- 0
  df[paste0("w", j, "h")] <- 0
  df[paste0("w", j, "d")] <- 0
  df[paste0("w", j, "s")] <- 0
  df[paste0("u", j)] <- 0
  df[paste0("prob", j)] <- 0
  df[paste0("prob", j, "h")] <- 0
  df[paste0("prob", j, "d")] <- 0
  df[paste0("prob", j, "s")] <- 0
}


for(i in 1:obs){
  # Assign type to each subject
  df$aux <- runif(nrow(df)) 
  df$t[df$aux <= df$ra] <- 1 
  df$t[df$aux > df$ra & df$aux <= df$ra + df$rb] <- 2 
  df$t[df$aux > df$ra + df$rb] <- 3 
  
  # assign vote
  df$aux <- runif(nrow(df)) 
  
  # Reinitialize vote to zero
  df[c("v1", "v2", "v3", "v1h", "v2h", "v3h", "v1d", "v3d", "v1s", "v2s", "v3s")] <- 0
  
  
  for(j in 1:3){
    j=1
    # Empirical frequencies
    # All periods
    df$v1 <- ifelse(df$t == j & df$aux <= df[[paste0("da", j)]], 1, df$v1)
    df$v2 <- ifelse(df$t == j & df$aux > df[[paste0("da", j)]] & df$aux <= df[[paste0("da", j)]] + df[[paste0("db", j)]], 1, df$v2)
    df$v3 <- ifelse(df$t == j & df$aux > df[[paste0("da", j)]] + df[[paste0("db", j)]], 1, df$v3)
    
    # Only half periods
    df$v1h <- ifelse(df$t == j & df$aux <= df[[paste0("dah", j)]], 1, df$v1h)
    df$v2h <- ifelse(df$t == j & df$aux > df[[paste0("dah", j)]] & df$aux <= df[[paste0("dah", j)]] + df[[paste0("dbh", j)]], 1, df$v2h)
    df$v3h <- ifelse(df$t == j & df$aux > df[[paste0("dah", j)]] + df[[paste0("dbh", j)]], 1, df$v3h)
    
    # Theory
    # Duverger (on alt. A )
    df$v1d <- ifelse(df$t == j & df$aux <= df[[paste0("dad", j)]], 1, df$v1d)
    df$v3d <- ifelse(df$t == j & df$aux > df[[paste0("dad", j)]], 1, df$v3d)
    
    # Sincere
    df$v1s <- ifelse(df$t == j & df$aux <= df[[paste0("das", j)]], 1, df$v1s)
    df$v2s <- ifelse(df$t == j & df$aux > df[[paste0("das", j)]] & df$aux <= df[[paste0("das", j)]] + df[[paste0("dbs", j)]], 1, df$v2s)
    df$v3s <- ifelse(df$t == j & df$aux > df[[paste0("das", j)]] + df[[paste0("dbs", j)]], 1, df$v3s)
  }
  
}

#####################

#----------------- Aggregate Results
for(j in 1:3) {
  # Add votes
  df <- df %>%
    group_by(igroup) %>%
    mutate(!!paste0("t", j) := sum(!!sym(paste0("v", j))),
           !!paste0("t", j, "h") := sum(!!sym(paste0("v", j, "h"))),
           !!paste0("t", j, "d") := sum(!!sym(paste0("v", j, "d"))),
           !!paste0("t", j, "s") := sum(!!sym(paste0("v", j, "s"))))
  
  # Normalize
  df[[paste0("t", j)]] <- df[[paste0("t", j)]] / 12
  df[[paste0("t", j, "h")]] <- df[[paste0("t", j, "h")]] / 12
  df[[paste0("t", j, "d")]] <- df[[paste0("t", j, "d")]] / 12
  df[[paste0("t", j, "s")]] <- df[[paste0("t", j, "s")]] / 12
}

df <- ungroup(df)

# Insert noise in case of plurality for the tie breaking

for(j in 1:3) {
  df[[paste0("t", j)]] <- ifelse(df$rule == "P", df[[paste0("t", j)]] + 0.001*(3-j), df[[paste0("t", j)]])
  df[[paste0("t", j, "h")]] <- ifelse(df$rule == "P", df[[paste0("t", j, "h")]] + 0.001*(3-j), df[[paste0("t", j, "h")]])
  df[[paste0("t", j, "d")]] <- ifelse(df$rule == "P", df[[paste0("t", j, "d")]] + 0.001*(3-j), df[[paste0("t", j, "d")]])
  df[[paste0("t", j, "s")]] <- ifelse(df$rule == "P", df[[paste0("t", j, "s")]] + 0.001*(3-j), df[[paste0("t", j, "s")]])
}

#// Maximal share
df$tmax <- with(df, pmax(t1, t2, t3, na.rm = TRUE))
df$thmax <- with(df, pmax(t1h, t2h, t3h, na.rm = TRUE))
df$tdmax <- with(df, pmax(t1d, t2d, t3d, na.rm = TRUE))
df$tsmax <- with(df, pmax(t1s, t2s, t3s, na.rm = TRUE))

# // first round victories
for(j in 1:3){
  df[[paste0("w", j)]] <- ifelse(df[[paste0("t", j)]] == df$tmax & df[[paste0("t", j)]] > df$thr, 1, 0)
  df[[paste0("w", j, "h")]] <- ifelse(df[[paste0("t", j, "h")]] == df$thmax & df[[paste0("t", j, "h")]] > df$thr, 1, 0)
  df[[paste0("w", j, "d")]] <- ifelse(df[[paste0("t", j, "d")]] == df$tdmax & df[[paste0("t", j, "d")]] > df$thr, 1, 0)
  df[[paste0("w", j, "s")]] <- ifelse(df[[paste0("t", j, "s")]] == df$tsmax & df[[paste0("t", j, "s")]] > df$thr, 1, 0)
}


df$r2 <- 1 - df$w1 - df$w2 - df$w3
df$r2h <- 1 - df$w1h - df$w2h - df$w3h
df$r2d <- 1 - df$w1d - df$w2d - df$w3d
df$r2s <- 1 - df$w1s - df$w2s - df$w3s

# // substract noise
for(j in 1:3) {
  df[[paste0("t", j)]] <- ifelse(df$rule == "P", df[[paste0("t", j)]] - 0.001*(3-j), df[[paste0("t", j)]])
  df[[paste0("t", j, "h")]] <- ifelse(df$rule == "P", df[[paste0("t", j, "h")]] - 0.001*(3-j), df[[paste0("t", j, "h")]])
  df[[paste0("t", j, "d")]] <- ifelse(df$rule == "P", df[[paste0("t", j, "d")]] - 0.001*(3-j), df[[paste0("t", j, "d")]])
  df[[paste0("t", j, "s")]] <- ifelse(df$rule == "P", df[[paste0("t", j, "s")]] - 0.001*(3-j), df[[paste0("t", j, "s")]])
}


    