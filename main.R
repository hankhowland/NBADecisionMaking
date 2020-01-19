library("tidyverse")
narrowed <- data.frame() ##dataframe to add plays I want to
##function to find plays I want from a csv and add them to "narrowed"##############################
narrow_add <- function(x) {

  ##lessthan2possessions and 2 or less point differential
  time<-x[which(as.numeric(x$PCTIMESTRING) < 5000 & x$PERIOD >= 4 
  ), ]


  ##fixing scores
  for (y in seq_len(dim(time)[1]-1)){
    print(y)
    if (is.na(time$SCOREMARGIN[y+1]) & time$GAME_ID[y+1] == time$GAME_ID[y]){
      time$SCOREMARGIN[y+1] <- time$SCOREMARGIN[y]
    }
  }
  
  ##eliminating nonshots
  shots <- time[which((time$EVENTMSGTYPE == 1 | time$EVENTMSGTYPE == 2) & time$PCTIMESTRING <= 1440
  ), ]

  ##removing columns
  

  ##negative SCOREMARGIN means visitors(team2) are winning
  
  ##narrow to shots that the losing team is shooting and it was a 2 point game before the shot
  ##(if a miss, scoremargin = 2 if 2pt = TIE if 3pt = 1)
  want = data.frame()

  want <- rbind(want, shots[which((shots$EVENTMSGTYPE==2 & !is.na(shots$HOMEDESCRIPTION) & shots$SCOREMARGIN == -2) | (shots$SCOREMARGIN==2 & !is.na(shots$VISITORDESCRIPTION) & shots$EVENTMSGTYPE == 2)
  ), ]) ##misses

  want <- rbind(want, shots[which(shots$EVENTMSGTYPE == 1 & !grepl("3PT", shots$HOMEDESCRIPTION, fixed=FALSE) & !grepl("3PT", shots$VISITORDESCRIPTION, fixed=FALSE) & shots$SCOREMARGIN == 'TIE'
  ), ]) ##twos

  want <- rbind(want, shots[which(shots$EVENTMSGTYPE==1 & grepl("3PT", shots$HOMEDESCRIPTION, fixed=TRUE) & shots$SCOREMARGIN==1
  ), ]) ##home 3s

  want <- rbind(want, shots[which(shots$EVENTMSGTYPE==1 & grepl("3PT", shots$VISITORDESCRIPTION, fixed=TRUE) & shots$SCOREMARGIN==-1
  ), ]) ##away 3s

  narrowed <- rbind(narrowed, want)
  rm("want", "time", "shots")
}
 
threes <- narrowed[which(grepl("3PT", narrowed$HOMEDESCRIPTION, fixed=FALSE) | grepl("3PT", narrowed$VISITORDESCRIPTION,fixed=FALSE)
), ] 
twos <- narrowed[which( !grepl("3PT", narrowed$HOMEDESCRIPTION, fixed=FALSE) & !grepl("3PT", narrowed$VISITORDESCRIPTION,fixed=FALSE)
), ]
twomakes <- twos[which(twos$EVENTMSGTYPE==1
), ]
threemakes <- threes[which(threes$EVENTMSGTYPE==1
), ]

rm("X2002_03_pbp")

model <- lm(SSI_NBA_Sheet1$`2018 value` ~ SSI_NBA_Sheet1$Population + SSI_NBA_Sheet1$`2017-18 wins`)
model
confint(model, level=.95)

#####salaries###############################################################################
library(tidyverse)
narrowed <- narrowed %>% rename(Player = PLAYER1_NAME)
narrowed1 <- merge(narrowed,NBA_season1718_salary, by="Player")
narrowed1 <- narrowed1 %>% rename(Salaries = season17_18)
divide <- function(x){
  y <- x/100000
  return(y)
}
narrowed1$smaller <-lapply(narrowed1$Salaries, divide)
salaries_logit <- glm((grepl("3PT", HOMEDESCRIPTION, fixed=FALSE) | grepl("3PT", VISITORDESCRIPTION,fixed=FALSE)) == TRUE ~ Salaries, data=narrowed1, family=binomial(link="logit"))
salaries_logit2 <- glm((grepl("3PT", HOMEDESCRIPTION, fixed=FALSE) | grepl("3PT", VISITORDESCRIPTION,fixed=FALSE)) == TRUE ~ smaller2, data=narrowed1, family=binomial(link="logit"))
salaries_logit2
narrowed1$smaller2 <- as.numeric(narrowed1$smaller)
summary(salaries_logit2)

salaries2018 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2018),]
salaries2017 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2017),]
salaries2016 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2016),]
salaries2015 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2015),]
salaries2014 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2014),]
salaries2013 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2013),]
salaries2012 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2012),]
salaries2011 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2011),]
salaries2010 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2010),]
salaries2009 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2009),]
salaries2008 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2008),]
salaries2007 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2007),]
salaries2006 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2006),]
salaries2005 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2005),]
salaries2004 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2004),]
salaries2003 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2003),]
salaries2002 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2002),]
salaries2001 <- nba_salaries_1990_to_2018[which(nba_salaries_1990_to_2018$season_end == 2001),]

shots_18$Player <- str_split(shots_18$Player, , n = 2)



#####playoffs######################################################################################
playoff_twos <- Behind_by_2_Playoffs_Sheet1[which(grepl("2-pt", Behind_by_2_Playoffs_Sheet1$Description, fixed=FALSE)
), ]
playoff_threes <- Behind_by_2_Playoffs_Sheet1[which(grepl("3-pt", Behind_by_2_Playoffs_Sheet1$Description, fixed=FALSE)),]
stakes <- prop.test(x = c(73, 859), n = c(73+110, 859+1753))
stakes

#####regular season#################################################################################
twos <- behind_by_2_regular_season_Sheet1[which(grepl("2-pt", behind_by_2_regular_season_Sheet1$Description, fixed=FALSE)
), ]
threes <- behind_by_2_regular_season_Sheet1[which(grepl("3-pt", behind_by_2_regular_season_Sheet1$Description, fixed=FALSE)
), ]

####team performance###############################################################################
after_18 <- behind_by_2_regular_season_after_all_star_break_Sheet1[1:47,]
after_17 <- behind_by_2_regular_season_after_all_star_break_Sheet1[158:191,]
after_16 <- behind_by_2_regular_season_after_all_star_break_Sheet1[302:344,]
after_15 <- behind_by_2_regular_season_after_all_star_break_Sheet1[441:481,]
after_14 <- behind_by_2_regular_season_after_all_star_break_Sheet1[597:644,]
after_13 <- behind_by_2_regular_season_after_all_star_break_Sheet1[697:739,]
after_12 <- behind_by_2_regular_season_after_all_star_break_Sheet1[856:926,]
after_11 <- behind_by_2_regular_season_after_all_star_break_Sheet1[993:1032,]
after_10 <- behind_by_2_regular_season_after_all_star_break_Sheet1[1132:1172,]
after_09 <- behind_by_2_regular_season_after_all_star_break_Sheet1[1262:1321,]
after_08 <- behind_by_2_regular_season_after_all_star_break_Sheet1[1432:1457,]
after_07 <- behind_by_2_regular_season_after_all_star_break_Sheet1[1552:1617,]
after_06 <- behind_by_2_regular_season_after_all_star_break_Sheet1[1706:1761,]
after_05 <- behind_by_2_regular_season_after_all_star_break_Sheet1[1869:1917,]
after_04 <- behind_by_2_regular_season_after_all_star_break_Sheet1[2017:2059,]
after_03 <- behind_by_2_regular_season_after_all_star_break_Sheet1[2143:2192,]
after_02 <- behind_by_2_regular_season_after_all_star_break_Sheet1[2287:2357,]
after_01 <- behind_by_2_regular_season_after_all_star_break_Sheet1[2461:2510,]

NBA_records_17_18 <- NBA_records_at_all_star_break_Sheet1_1_ %>% rename(Team = X2)
after_18 <- after_18 %>% rename(Abbr = Tm)
records_18 <- merge(NBA_records_17_18,Team_names_and_abbreviations_Sheet1_1_, by="Team")
after_18 <- merge(after_18,records_18, by="Abbr")

mergerec <- function(records,shots){
records <- merge(records,Team_names_and_abbreviations_Sheet1_1_, by="Team")
shots <- shots %>% rename(Abbr = Tm)
shots <- merge(records,shots,by="Abbr")
return(shots)
}

after_16 <- mergerec(records_15_16_Sheet1,after_16)
after_15 <- mergerec(records_14_15_Sheet1,after_15)
  after_15 <- after_15 %>% rename(Abbr = Tm)
  records_14_15_Sheet1 <- merge(records_14_15_Sheet1,Team_names_and_abbreviations_Sheet1_1_, by="Team")
  after_15 <- merge(records_14_15_Sheet1,after_15,by="Abbr")
after_14 <- mergerec(records13_14_Sheet1,after_14)

after_18 <- mergerec(records_17_18_Sheet1,after_18)
after_16 <- mergerec(records_15_16_Sheet1,after_16)
after_16 <- mergerec(records_15_16_Sheet1,after_16)
after_16 <- mergerec(records_15_16_Sheet1,after_16)

after <- data.frame()
after <- rbind(after,after_16)
after <- rbind(after,after_17)
after <- rbind(after,after_18)
after <- rbind(after,after_15)
after <- rbind(after,after_14)

            #######get records for each month and shots separated by year, merge by team and month, then combine to one big dataset
after$Pre <- gsub('-','',after$Pre)
after$Pre = substr(after$Pre,1,nchar(after$Pre)-2)
record_logit <- glm(grepl("3-pt", Description, fixed=FALSE) == TRUE ~ Pre, data=after, family=binomial(link="logit"))

############monthly record#########################################
    ##test for 1 season
record_to_lists <- function(col){
  col <- str_split(col, "-", n = 2)
  return(col)
}
NBA_monthly_records_Sheet1_3_$Nov <- record_to_lists(NBA_monthly_records_Sheet1_3_$Nov)
NBA_monthly_records_Sheet1_3_$Oct <- record_to_lists(NBA_monthly_records_Sheet1_3_$Oct)
NBA_monthly_records_Sheet1_3_$Dec <- record_to_lists(NBA_monthly_records_Sheet1_3_$Dec)
NBA_monthly_records_Sheet1_3_$Jan <- record_to_lists(NBA_monthly_records_Sheet1_3_$Jan)
NBA_monthly_records_Sheet1_3_$Feb <- record_to_lists(NBA_monthly_records_Sheet1_3_$Feb)
NBA_monthly_records_Sheet1_3_$Mar <- record_to_lists(NBA_monthly_records_Sheet1_3_$Mar)
NBA_monthly_records_Sheet1_3_$Apr <- record_to_lists(NBA_monthly_records_Sheet1_3_$Apr)


colw <- vector()
colL <- vector()
for(i in seq_len(506)) {
   colw[i] <- as.numeric(NBA_monthly_records_Sheet1_3_$Apr[[i]][[1]])
}
for(i in seq_len(506)) {
  colL[i] <- as.numeric(NBA_monthly_records_Sheet1_3_$Apr[[i]][[2]])
}
for(i in seq_len(506)) {
  NBA_monthly_records_Sheet1_3_$Apr[i] <- colw[i]/(colw[i] + colL[i])
}
backup <- NBA_monthly_records_Sheet1_3_

behind_by_2_regular_season_Sheet1$date <- substr(behind_by_2_regular_season_Sheet1$Date,6,7)
behind_by_2_regular_season_Sheet1$month <- as.numeric(behind_by_2_regular_season_Sheet1$date)

backup <- backup %>% rename('11',Dec)
records_18 <- backup[1:30,]
records_17 <- backup[31:60,]
records_16 <- backup[61:90,]
records_15 <- backup[91:120,]
records_14 <- backup[121:150,]
records_13 <- backup[151:180,]

records_11 <- backup[181:210,]
records_10 <- backup[211:240,]
records_09 <- backup[241:270,]
records_08 <- backup[271:300,]
records_07 <- backup[301:330,]
records_06 <- backup[331:360,]
records_05 <- backup[361:390,]
records_04 <- backup[391:419,]
records_03 <- backup[420:448,]
records_02 <- backup[449:477,]
records_01 <- backup[478:506,]

shots_18 <- behind_by_2_regular_season_Sheet1[1:157,]
shots_17 <- behind_by_2_regular_season_Sheet1[158:301,]
shots_16 <- behind_by_2_regular_season_Sheet1[302:440,]
shots_15 <- behind_by_2_regular_season_Sheet1[441:596,]
shots_14 <- behind_by_2_regular_season_Sheet1[597:696,]
shots_13 <- behind_by_2_regular_season_Sheet1[697:856,]
shots_12 <- behind_by_2_regular_season_Sheet1[857:992,]
shots_11 <- behind_by_2_regular_season_Sheet1[993:1131,]
shots_10 <- behind_by_2_regular_season_Sheet1[1132:1261,]
shots_09 <- behind_by_2_regular_season_Sheet1[1262:1431,]
shots_08 <- behind_by_2_regular_season_Sheet1[1432:1551,]
shots_07 <- behind_by_2_regular_season_Sheet1[1552:1705,]
shots_06 <- behind_by_2_regular_season_Sheet1[1706:1868,]
shots_05 <- behind_by_2_regular_season_Sheet1[1869:2016,]
shots_04 <- behind_by_2_regular_season_Sheet1[2017:2142,]
shots_03 <- behind_by_2_regular_season_Sheet1[2143:2286,]
shots_02 <- behind_by_2_regular_season_Sheet1[2287:2460,]
shots_01 <- behind_by_2_regular_season_Sheet1[2461:2612,]

records_18 <- merge(Team_names_and_abbreviations_Sheet1_1_, records_18, by="Team")
records_18 <- records_18 %>% rename(Tm = Abbr)
shots_18 <- merge(records_18,shots_18,by="Tm")
shots_18$Perc <- 1
for (i in seq_len(dim(shots_18)[1])){
  if (shots_18$date[i] == '12'){
    shots_18$Perc[i] <- shots_18$Nov[i]
  }
  if (shots_18$date[i] == '01'){
    shots_18$Perc[i] <- shots_18$Dec[i]
  }
  if (shots_18$date[i] == '02'){
    shots_18$Perc[i] <- shots_18$Jan[i]
  }
  if (shots_18$date[i] == '03'){
    shots_18$Perc[i] <- shots_18$Feb[i]
  }
  if (shots_18$date[i] == '04'){
    shots_18$Perc[i] <- shots_18$Mar[i]
  }
}

finish <- function(df,df2){
  df <- merge(Team_names_and_abbreviations_Sheet1_1_, df, by="Team")
  df <- df %>% rename(Tm = Abbr)
  df2 <- merge(df,df2,by="Tm")
  df2$Perc <- 1
  for (i in seq_len(dim(df2)[1])){
    if (df2$date[i] == '12'){
      df2$Perc[i] <- df2$Nov[i]
    }
    if (df2$date[i] == '01'){
      df2$Perc[i] <- df2$Dec[i]
    }
    if (df2$date[i] == '02'){
      df2$Perc[i] <- df2$Jan[i]
    }
    if (df2$date[i] == '03'){
      df2$Perc[i] <- df2$Feb[i]
    }
    if (df2$date[i] == '04'){
      df2$Perc[i] <- df2$Mar[i]
    }
  }
return(df2)
}


shots_17 <- finish(records_17,shots_17)
shots_16 <- finish(records_16,shots_16)
shots_15 <- finish(records_15,shots_15)
shots_14 <- finish(records_14,shots_14)
shots_13 <- finish(records_13,shots_13)

shots_11 <- finish(records_11,shots_11)
shots_10 <- finish(records_10,shots_10)
shots_09 <- finish(records_09,shots_09)
shots_08 <- finish(records_08,shots_08)
shots_07 <- finish(records_07,shots_07)
shots_06 <- finish(records_06,shots_06)
shots_05 <- finish(records_05,shots_05)
shots_04 <- finish(records_04,shots_04)
shots_03 <- finish(records_03,shots_03)
shots_02 <- finish(records_02,shots_02)
shots_01 <- finish(records_01,shots_01)

shots_record <- data.frame()
shots_record <- rbind(shots_record,shots_18)
shots_record <- rbind(shots_record,shots_17)
shots_record <- rbind(shots_record,shots_16)
shots_record <- rbind(shots_record,shots_15)
shots_record <- rbind(shots_record,shots_14)
shots_record <- rbind(shots_record,shots_13)

shots_record <- rbind(shots_record,shots_11)
shots_record <- rbind(shots_record,shots_10)
shots_record <- rbind(shots_record,shots_09)
shots_record <- rbind(shots_record,shots_08)
shots_record <- rbind(shots_record,shots_07)
shots_record <- rbind(shots_record,shots_06)
shots_record <- rbind(shots_record,shots_05)
shots_record <- rbind(shots_record,shots_04)
shots_record <- rbind(shots_record,shots_03)
shots_record <- rbind(shots_record,shots_02)
shots_record <- rbind(shots_record,shots_01)

shots_noones <- shots_record[which(shots_record$Perc != 1),]
shots_noones$Percentage <- as.numeric(shots_noones$Perc) * 100
record_logit <- glm(grepl("3-pt", Description, fixed=FALSE) == TRUE ~ Percentage, data=shots_noones, family=binomial(link="logit"))
summary(record_logit)
