library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(Lahman)
library(ISLR)
library(caret)
library(data.table)

#year variables are the condensed game log files from Retrosheet.com including the variables of interest for this project

#variables include a special gameid, date, and for both home and away teams: score, team, team game number, at bats, hits, doubles, triples, homeruns, RBIs, sacrifice hits, sacrifice flies, hit by pitches, walks, intentional walks, strikeouts, sacrifice bunts, caught stealing, grounded into double plays, and left on base
year2019 <- read_csv("C:/Users/jorda/OneDrive/Desktop/Thesis/year2019.csv")

#renaming doubles and triples for conveinence
year2019 <- year2019 %>%
  rename(home_doubles = home_2B,
         away_doubles = away_2B,
         home_triples = home_3B,
         away_triples = away_3B)

#below are functions that help write out all the functions to create new variables for models

onbase <- function(df, H, BB, HBP, AB, SF) {
  H <- enquo(H)
  BB <- enquo(BB)
  HBP <- enquo(HBP)
  AB <- enquo(AB)
  SF <- enquo(SF)
  out <- mutate(df, OBP := (!!H + !!BB + !!HBP)/(!!AB + !!BB + !!HBP + !!SF))
  return(out)
}
slugging <- function(df, s, d, t, hr, ab){
  s  <- enquo(s)
  d <- enquo(d)
  t <- enquo(t)
  hr <- enquo(hr)
  ab <- enquo(ab)
  out <- mutate(df, SLG := ((!!s) + (2 * !!d) + (3 * !!t) + (4 * !!hr)) / (!!ab))
  return(out)
}

#weighted on base average
wonbase <- function(df, nibb, hbp, s, d, t, hr, ab, bb, ibb, sf) {
  nibb <- enquo(nibb)
  hbp <- enquo(hbp) 
  s  <- enquo(s)
  d  <- enquo(d)
  t  <- enquo(t)
  hr  <- enquo(hr)
  ab  <- enquo(ab)
  bb  <- enquo(bb)
  ibb <- enquo(ibb) 
  sf  <- enquo(sf)
  out <- mutate(df, wOBA := ((.69 * !!nibb) + (.72 * !!hbp) + (.88 * !!s) + (1.247 * !!d) + (1.578 * !!t) + (2.031 * !!hr)) /  (!!ab + !!bb - !!ibb + !!sf + !!hbp))
  return(out)
}

#weighted on base plus slugging
wops <- function(df, woba, slg) {
  woba <- enquo(woba)
  slg <- enquo(slg)
  out <- mutate(df, wOPS := !!woba + !!slg)
  return(out) 
}

#pythagorean expectation
pe <- function(df, rs, ra) {
  rs <- enquo(rs)
  ra <- enquo(ra)
  out <- mutate(df, PE := ((!!rs)^2) / ((!!rs)^2 + (!!ra)^2))
  return(out)
}

#walk to strikeout ratio
bbsor <- function(df, bb, so) {
  bb <- enquo(bb)
  so <- enquo(so)
  out <- mutate(df, BBSOr := !!bb / !! so)
  return(out)
}

#list of teams to run through the function
teamsab <- c("SEA", "PIT", "ARI", "COL", "SLN", "ATL", "SFN", "NYN", "CHA", "CLE", "BAL",
             "ANA", "BOS", "HOU", "CHN", "DET", "MIL", "PHI", "MIN", "WAS", "CIN", "TEX",
             "NYA", "TOR", "KCA", "MIA", "LAN", "TBA", "SDN", "OAK")

data2019 <- sapply(teamsab, simplify = FALSE, FUN = function(t) {
  
  away_games <- filter(year2019, away_team == t) %>% 
    select(date, home_team, home_score, starts_with("away"), SpecID) #
  
  colnames(away_games)[2:3] <- c("opponent", "opponent_score")
  colnames(away_games) <- sub("away_", "", colnames(away_games))
  
  away_games$singles <- away_games$H -    (away_games$doubles +     away_games$triples +     away_games$HR)
  away_games$NIBB <-    away_games$BB -   (away_games$IBB +    away_games$HBP)
  away_games$TB <-      away_games$singles + (2*away_games$doubles) + (3*away_games$triples) + (4*away_games$HR)
  away_games$RS <-      away_games$score
  away_games$RA <-      away_games$opponent_score
  away_games["home"] <- 0
  away_games <- onbase(  away_games, H, BB, HBP, AB, SF)
  away_games <- slugging(away_games, singles, doubles, triples, HR, AB)
  away_games <- wonbase( away_games, NIBB, HBP, singles, doubles, triples, HR, AB, BB, IBB, SF)
  away_games <- wops(    away_games, wOBA, SLG)

  
  home_games <- filter(year2019, home_team == t) %>%
    select(date, away_team, away_score, starts_with("home"), SpecID)#how to include SpecID in this function
  
  colnames(home_games)[2:3] <- c("opponent", "opponent_score")
  colnames(home_games) <- sub("home_", "", colnames(home_games))
  home_games$singles <- home_games$H -    (home_games$doubles +     home_games$triples +     home_games$HR)
  home_games$NIBB <-    home_games$BB -   (home_games$IBB +    home_games$HBP)
  home_games$TB <-      home_games$singles + (2*home_games$doubles) + (3*home_games$triples) + (4*home_games$HR)
  home_games$RS <-      home_games$score
  home_games$RA <-      home_games$opponent_score
  home_games["home"] <- 1
  home_games <- onbase(  home_games, H, BB, HBP, AB, SF)
  home_games <- slugging(home_games, singles, doubles, triples, HR, AB)
  home_games <- wonbase( home_games, NIBB, HBP, singles, doubles, triples, HR, AB, BB, IBB, SF)
  home_games <- wops(    home_games, wOBA, SLG)
  
  bind_rows(away_games, home_games) %>% 
    arrange(date)
})


#summing all the data over each iteration, dividing by game number to get the per game average, and shifting so that all 
final2019 <- lapply(data2019, function(x){
  
  x$ID <- seq.int(nrow(x))
  x$RS <- shift(cumsum(x$RS))
  x$RA <- shift(cumsum(x$RA))
  x$BB <- shift(cumsum(x$BB))
  x$HR <- shift(cumsum(x$HR)/x$ID)
  x$SO <- shift(cumsum(x$SO))
  x$TB <- shift(cumsum(x$TB)/x$ID)
  x$OBP <- shift(cumsum(x$OBP)/x$ID)
  x$SLG <- shift(cumsum(x$SLG)/x$ID)
  x$wOBA <- shift(cumsum(x$wOBA)/x$ID)
  x$wOPS <- shift(cumsum(x$wOPS)/x$ID)
  x <- pe(x, RS, RA)
  x <- bbsor(x, BB, SO)
  x
})

#combining the lists
final2019 <- bind_rows(final2019)

#filtering for home and away, selecting and renaming to home and away variables again
home2019 <- final2019 %>%
  filter(home == 1)

home2019 <- home2019 %>%
  select(SpecID, HR, BBSOr, TB, OBP, SLG, wOBA, wOPS, PE) %>%
  rename(home_HR = HR,
         home_BBSOr = BBSOr,
         home_TB = TB,
         home_OBP = OBP,
         home_SLG = SLG,
         home_wOBA = wOBA,
         home_wOPS = wOPS,
         home_PE = PE)


away2019 <- final2019 %>%
  filter(home == 0)

away2019 <- away2019 %>%
  select(SpecID, HR, BBSOr, TB, OBP, SLG, wOBA, wOPS, PE) %>%
  rename(away_HR = HR,
         away_BBSOr = BBSOr,
         away_TB = TB,
         away_OBP = OBP,
         away_SLG = SLG,
         away_wOBA = wOBA,
         away_wOPS = wOPS,
         away_PE = PE)

#creating the final dataframe

df2019 <- year2019 %>%
  select(SpecID, date, home_team, away_team, home_score, away_score)


#joing back together to create final data
df2019 <- df2019 %>%
  left_join(home2019, by = "SpecID")

df2019 <- df2019 %>%
  left_join(away2019, by = "SpecID")

#removing the rows with na values, or no previous game data available
df2019 <- na.omit(df2019)

#introducing score differential
df2019$scorediff <- (df2019$home_score - df2019$away_score)

#normalizing function, to be used on total bases
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalizing total bases, as the variable was much higher than others
df2019$away_TB <- normalize(df2019$away_TB)
df2019$home_TB <- normalize(df2019$home_TB)

#creating csv function
#write_csv(df2019, path = "C:/Users/jorda/OneDrive/Desktop/Thesis/df2019", col_names = TRUE)