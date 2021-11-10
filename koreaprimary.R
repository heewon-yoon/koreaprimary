setwd("~/Desktop/Yoon/7. 2021Fall/miniAPSA/Analysis")

library("readxl")
library("tidyverse")

# csm dataset
#---------------------------
# 19th
mp19 <- read_excel("19csm_mp.xlsx")
colnames(mp19) <- c("municipality", "district", "party", "n.cand", "csm", "cand1", "cand2",
                    "cand3", "final.cand", "win", "voteshare", "turnout", "etc",
                    "incumbent", "incumb.party", "etc2")

table(mp19$csm)
mp19$primary <- ifelse(mp19$csm %in% c("경선", "경선(야권단일화경선)"), 1, 0)
mp19$inc.race <- ifelse(mp19$cand1 == mp19$incumbent |
                          mp19$cand2 == mp19$incumbent |
                          mp19$cand3 == mp19$incumb.party, 1, 0)


sp19 <- read_excel("19csm_sp.xlsx")
colnames(sp19) <- c("municipality", "district", "party", "n.cand", "csm", "cand1", "cand2",
                    "cand3", "final.cand", "win", "voteshare", "turnout", "etc",
                    "incumbent", "incumb.party", "etc2")

table(sp19$csm)
sp19$primary <- ifelse(sp19$csm %in% c("후보자추천", "후보자추천(전략지역)"), 0, 1)
sp19$inc.race <- ifelse(sp19$cand1 == sp19$incumbent |
                          sp19$cand2 == sp19$incumbent |
                          sp19$cand3 == sp19$incumb.party, 1, 0)

csm19 <- rbind(mp19, sp19)


# 20th
mp20 <- read_excel("20csm_mp.xlsx")
colnames(mp20) <- c("municipality", "district", "party", "n.cand", "csm", "cand1", "cand2",
                    "cand3", "final.cand", "win", "voteshare", "turnout", "etc",
                    "incumbent", "incumb.party", "etc2")
table(mp20$csm)
mp20$primary <- ifelse(mp20$csm %in% c("경선", "원외경선", "현역경선"), 1, 0)
mp20$inc.race <- ifelse(mp20$cand1 == mp20$incumbent |
                          mp20$cand2 == mp20$incumbent |
                          mp20$cand3 == mp20$incumb.party, 1, 0)

# inc.race = whether there was incumbent in the party (not whether it's an incumbent race)

sp20 <- read_excel("20csm_sp.xlsx")
colnames(sp20) <- c("municipality", "district", "party", "n.cand", "csm", "cand1", "cand2",
                    "cand3", "cand4", "final.cand", "win", "voteshare", "turnout", "etc",
                    "incumbent", "incumb.party", "etc2")

table(sp20$csm)
sp20$primary <- ifelse(sp20$csm %in% c("경선"), 1, 0)
sp20$inc.race <- ifelse(sp20$cand1 == sp20$incumbent |
                          sp20$cand2 == sp20$incumbent |
                          sp20$cand3 == sp20$incumb.party, 1, 0)

mp20$cand4 <- NA
csm20 <- rbind(mp20, sp20)
#-----------------------------


# pr vote dataset
#---------------------------
mp_pr <- read_excel("prvote_mp.xlsx")
colnames(mp_pr) <- c("municipality", "district", "c17", "c18", "c19")
mp_pr$pr19 <- (mp_pr$c17+mp_pr$c18)/2
mp_pr$pr20 <- (mp_pr$c18+mp_pr$c19)/2
mp_pr$party19 <- "민주통합당"
mp_pr$party20 <- "더불어민주당"

sp_pr <- read_excel("prvote_sp.xlsx")
colnames(sp_pr) <- c("municipality", "district", "c17", "c18", "c19")
sp_pr$pr19 <- (sp_pr$c17+sp_pr$c18)/2
sp_pr$pr20 <- (sp_pr$c18+sp_pr$c19)/2
sp_pr$party19 <- "새누리당"
sp_pr$party20 <- "새누리당"

pr <- rbind(mp_pr, sp_pr)
#---------------------------


# number of wins dataset
#---------------------------
winparty <- read_excel("winparty.xlsx")
colnames(winparty) <- c("municipality", "district", "c15", "c16", "c17", "c18",
                        "c19", "shift0", "shift1")

mparty <- c("새정치국민회의", "통합민주당", "새천년민주당", "갑-열린우리당", "새천년민주당",
            "열린우리당", "장안구-열린우리당", "처인구-통합민주당", "통합민주당")
sparty <- c("신한국당", "자유민주연합", "한나라당", "갑-한나라당", "을-한나라당", "팔달구-한나라당",
            "한나라당", "한나라당", "양평,가평-한나라당", "이천,여주-한나라당", "자유선진당",
            "친박연대", "한나라당")

winparty$m15 <- ifelse(winparty$c15 %in% mparty, 1, 0)
winparty$m16 <- ifelse(winparty$c16 %in% mparty, 1, 0) 
winparty$m17 <- ifelse(winparty$c17 %in% mparty, 1, 0) 
winparty$m18 <- ifelse(winparty$c18 %in% mparty, 1, 0) 
winparty$m19 <- ifelse(winparty$c19 %in% mparty, 1, 0) 

winparty$s15 <- ifelse(winparty$c15 %in% sparty, 1, 0)
winparty$s16 <- ifelse(winparty$c16 %in% sparty, 1, 0) 
winparty$s17 <- ifelse(winparty$c17 %in% sparty, 1, 0) 
winparty$s18 <- ifelse(winparty$c18 %in% sparty, 1, 0) 
winparty$s19 <- ifelse(winparty$c19 %in% sparty, 1, 0) 

# number of wins for 19th
winparty$safe19_m <- winparty$m15 + winparty$m16 + winparty$m17 + winparty$m18
winparty$safe19_s <- winparty$s15 + winparty$s16 + winparty$s17 + winparty$s18

# number of wins for 20th
winparty$safe20_m <- winparty$m16 + winparty$m17 + winparty$m18 + winparty$m19
winparty$safe20_s <- winparty$s16 + winparty$s17 + winparty$s18 + winparty$s19
#---------------------------


# mapping
#---------------------------
map <- read_excel("mapping.xlsx")
map$new <- ifelse(is.na(map$new)==T, 0, map$new)

# add pre- post- redistricting
csm19 <- left_join(csm19, map, 
                   by = c("municipality" = "municipality",
                          "district" = "district19"))

csm20 <- left_join(csm20, map,
                   by = c("municipality" = "municipality",
                          "district" = "district20"))
#---------------------------




# add safeness to csm dataset
# winparty: districts based on pre- redistricting (19th)
#---------------------------
csm19 <- left_join(csm19 %>% select(municipality, district, district20, new, party, 
                                    n.cand, csm, primary, inc.race),
                   winparty %>% select(municipality, district, safe19_m, safe19_s), 
                   by = c("municipality" = "municipality",
                          "district" = "district"))

csm20 <- left_join(csm20 %>% select(municipality, district19, district, new, party, 
                                    n.cand, csm, primary, inc.race),
                   winparty %>% select(municipality, district, safe20_m, safe20_s),
                   by = c("municipality" = "municipality",
                          "district19" = "district"))
#---------------------------


# add pr to dataset
# pr: districts based on post- redistricting (20th)
#---------------------------
csm19 <- left_join(csm19, 
                   pr %>% select(municipality, district, pr19, party19),
                   by = c("municipality" = "municipality",
                          "district20" = "district",
                          "party" = "party19"))

csm20 <- left_join(csm20,
                   pr %>% select(municipality, district, pr20, party20),
                   by = c("municipality" = "municipality",
                          "district" = "district",
                          "party" = "party20"))
#---------------------------


# clean and re-code data
#---------------------------
csm19$election19 <- 1
csm20$election19 <- 0

# unified variable of 'safe'
# safe: number of wins for the party in the district in the past 4 elections
csm19$safe <- ifelse(csm19$party == "민주통합당", csm19$safe19_m, NA)
csm19$safe <- ifelse(csm19$party == "새누리당", csm19$safe19_s, csm19$safe)

csm20$safe <- ifelse(csm20$party == "더불어민주당", csm20$safe20_m, NA)
csm20$safe <- ifelse(csm20$party == "새누리당", csm20$safe20_s, csm20$safe)

# unified party name
# rep: saenuri (rep) party = 1, minjoo (dem) party = 0
csm19$rep <- ifelse(csm19$party == "새누리당", 1, 0)
csm20$rep <- ifelse(csm20$party == "새누리당", 1, 0)

# combine data
colnames(csm19) <- c("municipality", "district19", "district20", "new", "party", "n.cand",
                     "csm", "primary", "inc.race", "safe_m", "safe_s", "pr", "election19",
                     "safe", "rep")
colnames(csm20) <- c("municipality", "district19", "district20", "new", "party", "n.cand",
                     "csm", "primary", "inc.race", "safe_m", "safe_s", "pr", "election19",
                     "safe", "rep")

korcsm <- rbind(csm19, csm20) %>%
  select(municipality, district19, district20, new, rep, n.cand, csm, primary,
         inc.race, pr, safe, election19)

#---------------------------

# csm over years
#---------------------------
sp_cent <- c(184, 245, 184, 110, 162)
sp_decent <- c(28, 0, 48, 141, 73)
sp <- cbind(sp_cent, sp_decent)

dp_cent <- c(160, 202, 140, 179, 123)
dp_decent <- c(83, 0, 86, 57, 96)
dp <- cbind(dp_cent, dp_decent)

over <- data.frame(rbind(sp, dp))
over$election <- c("17", "18", "19", "20", "21", "17", "18", "19", "20", "21")
over$party <- c("rep", "rep", "rep", "rep", "rep", "dem", "dem", "dem", "dem", "dem")
colnames(over) <- c("notpri", "pri", "election", "party")

over$pct.pri <- over$pri/(over$pri + over$notpri)
over$pct.notpri <- over$notpri/(over$pri + over$notpri)

pool <- over %>% group_by(election) %>%
  summarize(notpri.total = sum(notpri),
            pri.total = sum(pri))

pool$pct.pri <- pool$pri.total/(pool$pri.total+pool$notpri.total)*100
pool$pct.notpri <- pool$notpri.total/(pool$pri.total+pool$notpri.total)*100


sum_pri <- tapply(over$pri, over$election, sum)
sum_notpri <- tapply(over$notpri, over$election, sum)
pct_pri <- sum_pri/(sum_pri+sum_notpri)*100
pct_notpri <- sum_notpri/(sum_pri+sum_notpri)*100

# pooled
plot(names(pct_pri), pct_pri, type="l", ylim = c(0,100))
lines(names(pct_notpri), pct_notpri)

# just primary for different parties
plot(over$election[over$party == "rep"], over$pct.pri[over$party == "rep"]*100, type = "l")
lines(over$election[over$party == "dem"], over$pct.pri[over$party == "dem"]*100)

#---------------------------


# prior csm
#---------------------------
## csm in 18th to 19th
sp_pool <- read_excel("sppool.xlsx") 
sp_pool <- rename(sp_pool,
                  "c19" = "19th")
sp_pool <- sp_pool %>% filter(c19 == 1)
sp_pool <- rename(sp_pool,
                  "municipality"= "시도", 
                  "district" = "선거구명")
sp_pool$rep <- 1
sp_pool$election19 <- 1

korcsm <- left_join(korcsm, sp_pool %>% select(municipality, district, rep, election19, priorcsm), 
                    by = c("municipality" = "municipality", 
                           "district19" = "district",
                           "rep" = "rep",
                           "election19" = "election19"))


dp_pool <- read_excel("dppool.xlsx")
dp_pool <- rename(dp_pool,
                  "c19" = "19th")
dp_pool <- dp_pool %>% filter(c19 == 1)
dp_pool <- rename(dp_pool, 
                  "municipality"= "시도", 
                  "district" = "선거구명")
dp_pool$rep <- 0
dp_pool$election19 <- 1

korcsm <- left_join(korcsm, dp_pool %>% select(municipality, district, rep, election19, priorcsm),
                    by = c("municipality" = "municipality", 
                           "district19" = "district",
                           "rep" = "rep",
                           "election19" = "election19"))

korcsm$priorcsm <- coalesce(korcsm$priorcsm.x, korcsm$priorcsm.y)

# just priorcsm for 19th
korcsm19 <- korcsm %>% filter(election19 == 1)


## csm in 19th to 20th
# add priorcsm for 20th
korcsm20 <- left_join(korcsm %>% filter(election19 == 0), 
                      korcsm %>% filter(election19 == 1) %>% select(municipality, rep, district20, primary),
                      by = c("municipality" = "municipality",
                             "district20" = "district20",
                             "rep" = "rep"))

# merge two
korcsm19 <- korcsm19 %>% select(-c(priorcsm.x, priorcsm.y))
korcsm20 <- korcsm20 %>% select(-c(priorcsm.x, priorcsm.y, priorcsm))
korcsm20 <- rename(korcsm20, 
                   "primary" = "primary.x",
                   "priorcsm" = "primary.y")

korcsm <- rbind(korcsm19, korcsm20)
#---------------------------

colnames(korcsm)
# primary
# safe, pr
# rep, election19, priorcsm
# district? incumbent? 


# district fixed effect? 
# cluster std errors?


