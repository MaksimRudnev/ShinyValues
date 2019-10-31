library(LittleHelpers) # Installation  devtools::install_github("maksimrudnev/LittleHelpers")

# # Get the data
ess1 <- download_ess(1, user="maksim.rudnev@gmail.com")
ess2 <- download_ess(2, user="maksim.rudnev@gmail.com")
ess3 <- download_ess(3, user="maksim.rudnev@gmail.com")
ess4 <- download_ess(4, user="maksim.rudnev@gmail.com")
ess5 <- download_ess(5, user="maksim.rudnev@gmail.com")
ess6 <- download_ess(6, user="maksim.rudnev@gmail.com")
ess7 <- download_ess(7, user="maksim.rudnev@gmail.com")
ess8 <- download_ess(8, user="maksim.rudnev@gmail.com")
ess9 <- download_ess(9, user="maksim.rudnev@gmail.com")
#
#
# # Additional countries
# Russia round 7
library(httr)
auten<-POST("http://www.europeansocialsurvey.org/user/login?u=maksim.rudnev@gmail.com")
data.file <- GET("http://www.europeansocialsurvey.org/docs/related_studies/CESSI_ESS7/ESS7RU_main_data.zip")
writeBin(content(data.file, "raw"), paste(tempdir(), "russia07.zip"))
path<-utils::unzip(paste(tempdir(), "russia07.zip"))
russia7<-haven::read_spss(path[1])
rm(auten, data.file, path)
russia7$essround <- rep(7, nrow(russia7))
russia7$pspwght  <- rep(1, nrow(russia7))
names(russia7)<-tolower(names(russia7))

# Russia round 9
russia9 <- read_sav("~/Dropbox/STAT/European Social Survey/data/ESS9/ESS9rus.sav")
label_table(russia9)
russia9$essround <- rep(9, nrow(russia9))
# 

all.rounds <- list(ess1=ess1, ess2=ess2, ess3=ess3, ess4=ess4, ess5=ess5, ess6=ess6, ess7=ess7, ess8=ess8, russia7=russia7, ess9=ess9, russia9 = russia9)
 
# save(all.rounds, file = "data/all.rounds.Rdata")
# load("extradata/all.rounds.Rdata")
all.rounds$russia7$stflife[all.rounds$russia7$stflife %in% c(77,88,99)]<-NA
all.rounds$russia7$happy[all.rounds$russia7$happy %in% c(77,88,99)]<-NA
all.rounds$russia7$freehms[all.rounds$russia7$freehms %in% c(8, 9)]<-NA
for(v in values$items) all.rounds$russia7[,v][all.rounds$russia7[,v] %in% c(8, 9)]<-NA
# 
items.to.select <- c("cntry", "essround", values$items, "dweight", #"pspwght",
                     "yrbrn", "happy", "stflife", "freehms")

ess1_9 <- Reduce("rbind", list(
  all.rounds$ess1[,items.to.select],
  all.rounds$ess2[,items.to.select],
  all.rounds$ess3[,items.to.select],
  all.rounds$ess4[,items.to.select],
  all.rounds$ess5[,items.to.select],
  all.rounds$ess6[,items.to.select],
  all.rounds$ess7[,items.to.select],
  all.rounds$ess8[,items.to.select],
  all.rounds$russia7[,items.to.select],
  all.rounds$russia9[,items.to.select],
  all.rounds$ess9[,items.to.select]
))


# save(ess1_9, file="extradata/ess1_9.Rdata")
# load(file="extradata/ess1_9.Rdata")
#crosstab("cntry", "essround", drop_labs(untibble(ess1_9)))
# Compute value indices

ess1_9 <- as.data.frame(ess1_9, stringsAsFactors =F)
ess1_9 <- ess_values(ess1_9, v2=T, v4=T, v10=T, center=T, abbr=T)

# Compute weighted country means and standard errors 
library("survey")
# ess1_8$essround <- as.factor(ess1_8$essround)
# ess1_8$cntry <- as.factor(as.character(ess1_8$cntry))

#s.w <- svydesign(ids = ~1, data = ess1_9, weights = ess1_9[,"pspwght"])

s.w <- svydesign(ids = ~1, data = ess1_9, weights = ess1_9[,"dweight"])

tab<-svyby(formula= ~ Conservation_Openness + Self_Enhancement_Self_Transcendence + Openness + Conserv + Self_Trans + Self_Enhance + 
             SE + CO + TR + BE + UN + SD + ST + HE + AC + PO + stflife + happy, 
           by= ~ cntry + essround,
           design = s.w, 
           FUN = svymean,
           na.rm = TRUE,
           na.rm.all=TRUE, 
           multicore=T
)

# Shape data for ggplot

library("reshape") 
tb<-cbind(    
  melt(tab[,c("cntry", "essround", values$two.abbr, values$four.abbr, values$ten.abbr,  "happy", "stflife"#, "cohort"
  )],
  id.vars=c("cntry", "essround"#, "cohort"
  )), 
  se=melt(tab[,c("cntry", "essround",  #"cohort",
                 paste0("se.", values$two.abbr), 
                 paste0("se.", values$four.abbr),
                 paste0("se.", values$ten.abbr),
                 "se.happy", "se.stflife")
              ],
          id.vars=c("cntry", "essround"#, "cohort"
          ))$value)

tb$upper<-tb$value+tb$se*1.96
tb$lower<-tb$value-tb$se*1.96


tb$essround<-2000+tb$essround*2

#tb<-tb[!tb$cntry %in% c("LU", "AL"),]
tb$cntry<-as.character(tb$cntry)




tb$variable<- factor(tb$variable, levels=c(values$ten.abbr, values$four.abbr, values$two.abbr #, "happy", "stflife"
))

save(tab, file="data/tb2.Rdata")