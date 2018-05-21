setwd("~/Desktop/900")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(reshape,foreign,dplyr)

SD <- read.dta("~/Panel1.dta")

SD$ds <- as.numeric(SD$dataset)
SD$ds[SD$ds == 10] <- 8
SD$ds[SD$ds == 11] <- 9
SD$ds[SD$ds == 12] <- 10
SD$ds[SD$ds == 13] <- 11
SD$ds[SD$ds == 14] <- 12
SD$ds[SD$ds == 15] <- 13
SD$ds[SD$ds == 16] <- 14

SD$sd <- 0
SD$sd[SD$ds == 11] <- 1
SD$co[SD$case_outcome == "liberal"] <- 0
SD$co[SD$case_outcome == "conservative"] <- 1
SD$party <- as.numeric(SD$party_pres)
SD$party[SD$party == 2] <- 0
SD$female[SD$sex == "female"] <- 1
SD$female[SD$sex == "male"] <- 0

SD$fempan <- SD$female_on_panel
SD$jn <- as.numeric(SD$judge_number)
SD$cv <- SD$conserve_vote
SD$lc_cv <- SD$lc_conserve
SD$min <- SD$Minority
SD$maj <- SD$Majority
SD$year <- SD$dec_year
SD$age <- SD$birth_yr

SD <- SD %>%
  group_by(pan) %>%
  mutate(numjudge = n())
#SD <- arrange(SD, pan)
SD <- SD %>%
  group_by(jn) %>% 
  mutate(conIDvote = ifelse(cv==1 & jcs>0, 1, 0)) %>% 
  mutate(libIDvote = ifelse(cv==0 & jcs<0, 1, 0))
SD$IDvote <- SD$conIDvote+SD$libIDvote
#SD <- arrange(SD, jn)

SD1 <- SD[ , -which(names(SD) %in% c("num","sex", "female_on_panel", "judge_number", "conserve_vote", 
                               "Minority", "Majority", "dec_year", "birth_yr", "party_pres", "lc_conserve",
                               "cite", "dataset", "black", "sum", "non_white", "reverse", "conIDvote", "libIDvote",
                               "judge_name"))]


SD1 = as.data.frame(SD1)
### CREATE PI and mfPI ###
for(i in 1:14){
  SD1[,paste0("MINovOP_",i)] <- ifelse(SD1$jcs>0 & SD1$lc_cv==0 & SD1$min==1 & SD1$ds!=i, 1, 
                               ifelse(SD1$jcs<0 & SD1$lc_cv==1 & SD1$min==1 & SD1$ds!=i, 1, 0))
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("MINovOP_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  my.sum <- function(x) ifelse( !all(is.na(x)), sum(x, na.rm=T), NA)
  SD1<-SD1 %>% group_by(jn) %>% mutate(SUM=my.sum(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("sumMINovOP_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  SD1[,paste0("psit_",i)] <- ifelse(SD1$co==1 & SD1$cv==1 & SD1$jcs>0 & SD1$lc_cv==0 & SD1$min==1 & SD1$ds!=i, 1, 
                              ifelse(SD1$co==0 & SD1$cv==0 & SD1$jcs<0 & SD1$lc_cv==1 & SD1$min==1 & SD1$ds!=i, 1, 0))
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("psit_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% group_by(jn) %>% mutate(SUM=my.sum(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("sumpsit_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  SD1 <- as.data.frame(SD1)
  SD1[,paste0("PI_",i)] <- SD1[,paste0("sumpsit_",i)]/SD1[,paste0("sumMINovOP_",i)]

  SD1 <- as.data.frame(SD1)
  SD1[,paste0("femPI_",i)] <- ifelse(SD1$female==1, SD1[,paste0("PI_",i)],NA)

  .curcol<-colnames(SD1)[colnames(SD1)==paste0("femPI_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
  SD1 <-SD1 %>% group_by(pan) %>% mutate(SUM=my.max(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("mfPI_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  SD1 <- as.data.frame(SD1)
}


### CREATE pdi ###
for(i in 1:14){
  SD1[,paste0("MAJupOP_",i)] <- ifelse(SD1$jcs>0 & SD1$lc_cv==1 & SD1$maj==1 & SD1$ds!=i, 1, 
                                 ifelse(SD1$jcs<0 & SD1$lc_cv==0 & SD1$maj==1 & SD1$ds!=i, 1, 0))
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("MAJupOP_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% group_by(jn) %>% mutate(SUM=my.sum(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("sumMAJupOP_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  
  SD1[,paste0("av_",i)] <- ifelse(SD1$jcs>0 & SD1$cv==0 & SD1$lc_cv==1 & SD1$maj==1 & SD1$ds!=i, 1, 
                            ifelse(SD1$jcs<0 & SD1$cv==1 & SD1$lc_cv==0 & SD1$maj==1 & SD1$ds!=i, 1, 0))
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("av_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% group_by(jn) %>% mutate(SUM=my.sum(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("sumav_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  
  SD1[,paste0("pdi_",i)] <- SD1[,paste0("sumav_",i)]/SD1[,paste0("sumMAJupOP_",i)]
  SD1 <- as.data.frame(SD1)
  }
class(SD1)
SD1 <- arrange(SD1, pan, jn)

#### Standardize Variables
my.mean <- function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T), NA)
my.sd2 <- function(x) ifelse( !all(is.na(x)), 2*sd(x, na.rm=T), NA)

for(i in 1:14){
#PI
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("PI_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% mutate(SUM=my.mean(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("mn_PI_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("PI_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% mutate(SUM=my.sd2(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("sd2_PI_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol

  SD1[,paste0("PI_2SD_",i)] <- (SD1[,paste0("PI_",i)] - SD1[,paste0("mn_PI_",i)])/SD1[,paste0("sd2_PI_",i)]
  SD1 <- as.data.frame(SD1)
#mfPI
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("mfPI_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% mutate(SUM=my.mean(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("mn_mfPI_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("mfPI_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% mutate(SUM=my.sd2(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("sd2_mfPI_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  
  SD1[,paste0("mfPI_2SD_",i)] <- (SD1[,paste0("mfPI_",i)] - SD1[,paste0("mn_mfPI_",i)])/SD1[,paste0("sd2_mfPI_",i)]
  SD1 <- as.data.frame(SD1)
#pdi
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("pdi_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% mutate(SUM=my.mean(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("mn_pdi_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  
  .curcol<-colnames(SD1)[colnames(SD1)==paste0("pdi_",i)]
  colnames(SD1)[colnames(SD1)==.curcol]<-"MIN"
  SD1<-SD1 %>% mutate(SUM=my.sd2(MIN))
  colnames(SD1)[colnames(SD1)=="SUM"]<-paste0("sd2_pdi_",i)
  colnames(SD1)[colnames(SD1)=="MIN"]<-.curcol
  
  SD1[,paste0("pdi_2SD_",i)] <- (SD1[,paste0("pdi_",i)] - SD1[,paste0("mn_pdi_",i)])/SD1[,paste0("sd2_pdi_",i)]
  SD1 <- as.data.frame(SD1)
}
# year
  SD1<-SD1 %>% mutate(mn_yr=my.mean(year))
  SD1<-SD1 %>% mutate(sd2_yr=my.sd2(year))
  SD1$yr_2SD <- (SD1$year - SD1$mn_yr)/SD1$sd2_yr
# age
  SD1<-SD1 %>% mutate(mn_age=my.mean(age))
  SD1<-SD1 %>% mutate(sd2_age=my.sd2(age))
  SD1$age_2SD <- (SD1$age - SD1$mn_age)/SD1$sd2_age
# jcs
  SD1<-SD1 %>% mutate(mn_jcs=my.mean(jcs))
  SD1<-SD1 %>% mutate(sd2_jcs=my.sd2(jcs))
  SD1$jcs_2SD <- (SD1$jcs - SD1$mn_jcs)/SD1$sd2_jcs  
  
# Catergorize mfPI_2SD into mfPI3
    for(i in 1:14){
    SD1[,paste0("mfPI3_",i)] <- ifelse(is.na(SD1[,paste0("mfPI_2SD_",i)]), 0,
                                       ifelse(SD1[,paste0("mfPI_2SD_",i)]<0, 1,
                                              ifelse(SD1[,paste0("mfPI_2SD_",i)]>0, 2, NA)))
  }

# My Model
Regress  <- glm(cv ~
    lc_cv 
    + factor(mfPI3_11)
    + PI_2SD_11 
    + pdi_2SD_11 
    + jcs_2SD 
    + yr_2SD 
    + age_2SD,
    family = binomial("logit"),
    data=subset(SD1, numjudge<=3 &female==0 &ds==11)
    )
summary(Regress)
  
  
  

