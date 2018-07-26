library(LittleHelpers)

# # Get the data
# ess1 <- download_ess(1, user="maksim.rudnev@gmail.com")
# ess2 <- download_ess(2, user="maksim.rudnev@gmail.com")
# ess3 <- download_ess(3, user="maksim.rudnev@gmail.com")
# ess4 <- download_ess(4, user="maksim.rudnev@gmail.com")
# ess5 <- download_ess(5, user="maksim.rudnev@gmail.com")
# ess6 <- download_ess(6, user="maksim.rudnev@gmail.com")
# ess7 <- download_ess(7, user="maksim.rudnev@gmail.com")
# ess8 <- download_ess(8, user="maksim.rudnev@gmail.com")
# 
# # Additional countries
# 
# library(httr)
# auten<-POST("http://www.europeansocialsurvey.org/user/login?u=maksim.rudnev@gmail.com")
# data.file <- GET("http://www.europeansocialsurvey.org/docs/related_studies/CESSI_ESS7/ESS7RU_main_data.zip")
# writeBin(content(data.file, "raw"), paste(tempdir(), "russia07.zip"))
# path<-utils::unzip(paste(tempdir(), "russia07.zip"))
# russia7<-haven::read_spss(path[1])
# rm(auten, data.file, path)
# 
# russia7$essround <- rep(7, nrow(russia7))
# russia7$pspwght  <- rep(1, nrow(russia7))
# names(russia7)<-tolower(names(russia7))
# 
# ess1_8 <- Reduce("rbind", list(
#   ess1[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   ess2[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   ess3[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   ess4[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   ess5[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   ess6[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   ess7[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   ess8[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")],
#   russia7[,c("cntry", "essround", values$items, "dweight", "pspwght", "yrbrn")]
# ))
# 
#save(ess1_8, file="ess1_8.Rdata")
load(file="data/ess1_8.Rdata")
#crosstab("cntry", "essround", drop_labs(untibble(ess1_8)))
# Compute value indices
ess1_8 <- as.data.frame(ess1_8, stringsAsFactors =F)
ess1_8 <- ess_values(ess1_8, v2=T, v4=T, v10=T, center=T, abbr=T)

# Compute weighted country means and standard errors 
library("survey")
# ess1_8$essround <- as.factor(ess1_8$essround)
# ess1_8$cntry <- as.factor(as.character(ess1_8$cntry))

s.w <- svydesign(ids = ~1, data = ess1_8, weights = ess1_8[,"pspwght"])

tab<-svyby(formula= ~ Conservation_Openness + Self_Enhancement_Self_Transcendence + Openness + Conserv + Self_Trans + Self_Enhance + 
             SE + CO + TR + BE + UN + SD + ST + HE + AC + PO, 
           by= ~ cntry + essround,
           design = s.w, 
           FUN = svymean,
           na.rm = TRUE,
           na.rm.all=TRUE#, multicore=T
           )



# Shape data for ggplot

library("reshape") 
tb<-cbind(    
   melt(tab[,c("cntry", "essround", values$two.abbr, values$four.abbr, values$ten.abbr)],id.vars=c("cntry", "essround")), 
se=melt(tab[,c("cntry", "essround", 
               paste0("se.", values$two.abbr), 
               paste0("se.", values$four.abbr),
               paste0("se.", values$ten.abbr))],
        id.vars=c("cntry", "essround"))$value)

tb$upper<-tb$value+tb$se*1.96
tb$lower<-tb$value-tb$se*1.96


tb$essround<-2000+tb$essround*2

#tb<-tb[!tb$cntry %in% c("LU", "AL"),]
tb$cntry<-as.character(tb$cntry)




tb$variable<- factor(tb$variable, levels=c(values$ten.abbr, values$four.abbr, values$two.abbr))

# tb$cntry<- factor(tb$cntry, levels=
#                   c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia",
#                      "Spain", "Finland", "France", "United Kingdom", "Greece", "Croatia", "Hungary", "Ireland", "Israel",
#                      "Iceland", "Italy", "Lithuania", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal",
#                      "Russia", "Sweden", "Slovenia", "Slovakia", "Turkey", "Ukraine")
# )

tab<-tb
save(tab, file="data/tb.Rdata")



# geo map
load("data/tb.Rdata")
tab


library(mapdata)
library(sf)
#library(lwgeom)



#d <- map("worldMapEnv", fill = T, plot = F)
d <- map('world', fill = TRUE, col = 1:10, plot=F)
mapBase <- st_as_sf(d)
#mapBase <- st_make_valid(mapBase) #takes a long time
europe <- st_crop(mapBase, xmin = -15, xmax = 35, ymin = 35, ymax = 60)
europe$ID[europe$ID=="UK"]<-"United Kingdom"
save(europe, file="data/europe.Rdata")


translation.countries <- read.delim("data/translation_cntry.txt", fileEncoding = "UTF-16", colClasses = "character")

cropMap$cntry <- sapply(cropMap$ID, function(x) if(any(translation.countries$English==x)) translation.countries[translation.countries$English==x, "cntry"]  else NA,  USE.NAMES =F  )

dt<-merge(cropMap, tab[tab$essround==2006 & tab$variable=="Self_Trans",c("cntry","value")], by="cntry", all.x=T)

# Europe
ggplot(dt) + 
  geom_sf(aes(fill=value, label=ID), color="gray30")+
  theme_void()+labs(fill="that value")+
  theme(panel.grid = element_line(colour = "transparent"),
        legend.title = element_text(size = 12))



spainMap <- ggplot(cropMap,
                   aes(fill = factor(ifelse(cropMap$ID == "Spain", 1, 2)))) +
  geom_sf() +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  scale_fill_manual(values = c("darkgrey", "lightgrey"),
                    labels = c("Spain", "Not Spain")) +
  theme_bw()
  

spainMap


#### Additional, legacy and stuff



# rm(dat, dat2, value.indices, s.w, val.names, weight)
# 
# 
# 
# 
# levels(tb$variable)[c(1, 11, 13:16)]<-c("Self-Direction", "Openness to change", 
#                                          "Self-Enhancement","Self-Transcendence","Conservation - Openness to change",    
#                                          "Self-Enhancement - Self-Transcendence")
# 
# 
# levels(tab$cntry)<-c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia",
#                      "Spain", "Finland", "France", "United Kingdom", "Greece", "Croatia", "Hungary", "Ireland", "Israel",
#                      "Iceland", "Italy", "Lithuania", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal",
#                      "Russia", "Sweden", "Slovenia", "Slovakia", "Turkey", "Ukraine")
# 
# tab$cntry<-factor(tab$cntry, levels(tab$cntry)[order(levels(tab$cntry))])
# tab$variable<- factor(tab$variable, levels=c(
#   
#   "Security"                             
#   , "Conformity"                           
#   , "Tradition" 
#   , "Benevolence" 
#   , "Universalism"
#   , "Self-Direction" 
#   , "Stimulation"   
#   , "Hedonism" 
#   , "Achievement" 
#   ,"Power"                                
#   
#   , "Openness to change"                   
#   , "Conservation"                         
#   , "Self-Enhancement"                     
#   , "Self-Transcendence"                   
#   , "Conservation - Openness to change"    
#   , "Self-Enhancement - Self-Transcendence"
#   
# ))
# 
# 
# 



