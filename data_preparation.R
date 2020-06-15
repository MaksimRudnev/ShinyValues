library(LittleHelpers) # Installation  devtools::install_github("maksimrudnev/LittleHelpers")

# # Get the data
ess1 <- download_ess(1, user="maksim.rudnev@gmail.com", clean = F)
ess2 <- download_ess(2, user="maksim.rudnev@gmail.com", clean = F)
ess3 <- download_ess(3, user="maksim.rudnev@gmail.com", clean = F)
ess4 <- download_ess(4, user="maksim.rudnev@gmail.com", clean = F)
ess5 <- download_ess(5, user="maksim.rudnev@gmail.com", clean = F)
ess6 <- download_ess(6, user="maksim.rudnev@gmail.com", clean = F)
ess7 <- download_ess(7, user="maksim.rudnev@gmail.com", clean = F)
ess8 <- download_ess(8, user="maksim.rudnev@gmail.com", clean = F)
ess9 <- download_ess(9, user="maksim.rudnev@gmail.com", version = "02", clean = F)
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






# ~~~~~~ Offline version ~~~~~~~~~~~ #####
library(haven)
ess1 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS1/ESS1e06_4.sav")
ess2 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS2/ESS2e03_4.sav")
ess3 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS3/ESS3e03_6.sav")
ess4 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS4/ESS4e04_5.sav")
ess5 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS5/ESS5e03_3.sav")
ess6 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS6/ESS6e02_2.sav")
ess7 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS7/ESS7e02_1.sav")
ess8 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS8/ESS8e02_1.sav")
ess9 <- read_sav("ESS9e02.sav")
russia7 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS7_RUS_with adds/Datafile_ESS7.sav")
russia9 <- read_sav("/Users/maksimrudnev/OneDrive/DATA/European Social Survey/Data/ESS9_RUS/ESS9rus.sav")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Temporary adjustments for local Russia files 

russia7$essround <- rep(7, nrow(russia7))
russia7$pspwght  <- rep(1, nrow(russia7))
russia7$pweight  <- rep(0, nrow(russia7))
names(russia7)<-tolower(names(russia7))
russia7$stflife[russia7$stflife %in% c(77,88,99)]<-NA
russia7$happy[russia7$happy %in% c(77,88,99)]<-NA
russia7$freehms[russia7$freehms %in% c(8, 9)]<-NA
for(v in values$items) russia7[,v][russia7[,v] %in% c(8, 9)]<-NA

russia9$essround <- rep(9, nrow(russia9))
russia9$pweight <- rep(0, nrow(russia9))
russia9$pspwght <- rep(1, nrow(russia9))

#ess9$pspwght  <- rep(1, nrow(ess9))

all.rounds <- list(ess1=ess1, 
                   ess2=ess2, 
                   ess3=ess3, 
                   ess4=ess4, 
                   ess5=ess5, 
                   ess6=ess6, 
                   ess7=ess7, 
                   ess8=ess8, 
                   ess9=ess9, 
                   russia7 = russia7, 
                   russia9 = russia9
                   )
 
# save(all.rounds, file = "extradata/all.rounds.Rdata")
# load("extradata/all.rounds.Rdata")

# 
items.to.select <- c("cntry", "essround", values$items, "dweight", "pspwght", "pweight", "idno"
                    )

ess1_9 <- Reduce("rbind", lapply(all.rounds, function(x) drop_labs(untibble(x[,items.to.select]))))
#   all.rounds$ess1[,items.to.select],
#   all.rounds$ess2[,items.to.select],
#   all.rounds$ess3[,items.to.select],
#   all.rounds$ess4[,items.to.select],
#   all.rounds$ess5[,items.to.select],
#   all.rounds$ess6[,items.to.select],
#   all.rounds$ess7[,items.to.select],
#   all.rounds$ess8[,items.to.select],
#   all.rounds$russia7[,items.to.select],
#   all.rounds$russia9[,items.to.select],
#   all.rounds$ess9[,items.to.select]
# ))


# save(ess1_9, file="extradata/ess1_9_withRussia_and_weights.Rdata")
# crosstab("cntry", "essround", drop_labs(untibble(ess1_9)))
# Compute value indices

ess1_9 <- as.data.frame(ess1_9, stringsAsFactors =F)
ess1_9 <- ess_values(ess1_9, v2=T, v4=T, v10=T, center=T, abbr=T)

# Compute weighted country means and standard errors 
library("survey")
# ess1_8$essround <- as.factor(ess1_8$essround)
# ess1_8$cntry <- as.factor(as.character(ess1_8$cntry))

s.w <- svydesign(ids = ~1, data = ess1_9, weights = ess1_9[,"pspwght"])

#s.w <- svydesign(ids = ~1, data = ess1_9, weights = ess1_9[,"dweight"])

tab<-svyby(formula= ~ Conservation_Openness + Self_Enhancement_Self_Transcendence + Openness + Conserv + Self_Trans + Self_Enhance + 
             SE + CO + TR + BE + UN + SD + ST + HE + AC + PO, 
           by= ~ cntry + essround,
           design = s.w, 
           FUN = svymean,
           na.rm = TRUE,
           na.rm.all=TRUE, 
           multicore=T
)



# ######## Save data for d3 #############
# 
# ##x .      y .    year . col . xSE .  ySE .  name
# tab$essround<-2000+tab$essround*2
# d3.data.general <- reshape2::melt(tab, id.vars = c("cntry", "essround"), measure.vars = c(values$ten.abbr, values$four.abbr, values$two.abbr))
# d3.data.general <- cbind(d3.data.general, 
#                          se=reshape2::melt(tab, id.vars = c("cntry", "essround"), 
#                                         measure.vars = paste("se", c(values$ten.abbr, values$four.abbr, values$two.abbr), sep = "."))[,4]
# )
# 
# write.table(d3.data.general, 
#             file="/Users/maksimrudnev/Dropbox/STAT/R-functions/ShinyValues/values_d3/longvalues.csv", 
#             quote = T, row.names = F, col.names=F, sep=",")
# 
# 
# d3data.map <- tab[,c("Conservation_Openness", "Self_Enhancement_Self_Transcendence", 
#                   "se.Conservation_Openness", "se.Self_Enhancement_Self_Transcendence",
#                   "essround", "col", "cntry")]
# d3data.10values <- tab[, c(values$ten.abbr, paste0("se.", values$ten.abbr), "essround", "col", "cntry")]
# 
# tr <-  read.delim(file="data/translation_cntry.txt")
# d3data.map$cntry      <- replace_by_table(d3data.map$cntry, tr, "cntry", "English")
# d3data.10values$cntry <- replace_by_table(d3data.10values$cntry, tr, "cntry", "English")
# # 
# # 
# write.table(d3data.map, 
#             file="/Users/maksimrudnev/Dropbox/STAT/R-functions/ShinyValues/values_d3/countryvalues.csv", 
#             quote = T, row.names = F, col.names=F, sep=",")
# write.table(d3data.10values, 
#             file="/Users/maksimrudnev/Dropbox/STAT/R-functions/ShinyValues/values_d3/10values.csv", 
#             quote = T, row.names = F, col.names=F, sep=",")


# Shape data for ggplot

library("reshape") 
tb<-cbind(    
  melt(tab[,c("cntry", "essround", values$two.abbr, values$four.abbr, values$ten.abbr)],
       id.vars=c("cntry", "essround")), 
  se=melt(tab[,c("cntry", "essround",
                 paste0("se.", values$two.abbr), 
                 paste0("se.", values$four.abbr),
                 paste0("se.", values$ten.abbr)
  )
  ],
  id.vars=c("cntry", "essround"
  ))$value)

tb$upper<-tb$value+tb$se*1.96
tb$lower<-tb$value-tb$se*1.96


tb$essround<-2000+tb$essround*2
tb$cntry<-as.character(tb$cntry)
tb$variable<- factor(tb$variable, levels=c(values$ten.abbr, values$four.abbr, values$two.abbr))


# Save data 
tab <- tb
save(tab, file="data/tb2.Rdata") # with Russia round9
#save(tab, file="data/tb1.Rdata") # without Russia round9


# Test ----###################

d= d[d$cntry=="ME",]

# Plot 1 - by country #####

ggplot(d[d$cntry=="ME",], aes(essround,y = value))+ 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill=variable),  alpha=.2) + 
  geom_line(aes(color=variable, size=variable))+
  coord_cartesian(xlim = c(min(d$essround), max(d$essround) + 3)) +
  geom_point(aes(color=variable, shape=variable), size=3) +
  geom_label_repel(
    data = subset(d, essround==max(essround)),
    aes(label = str_wrap(variable, 20), fill=variable), segment.colour = "grey60", col="black", nudge_x = 1.1, size= 5, alpha=.8)+
  scale_x_continuous(breaks=unique(d$essround), minor_breaks =F) +
  # labs(x =     translation.tab[translation.tab$element=="x.round", lang$lang],
  #      y =     translation.tab[translation.tab$element=="y.value", lang$lang],
  #      caption=translation.tab[translation.tab$element=="copyright.caption", lang$lang], 
  #      title=  translation.countries[translation.countries$cntry==input$show_countries, lang$lang], 
  #      fill="", shape="", color="", size="")+
  
  # scale_fill_manual(values=selectedData1()$clr_line)+
  # scale_color_manual(values=selectedData1()$clr_line)+
  # scale_shape_manual(values=selectedData1()$ln_type)+
  # scale_size_manual(values=selectedData1()$ln_width)+
  
  theme_minimal()+theme(panel.grid.minor = element_blank(),
                        axis.line.x = element_line(color="black", size = .5),
                        axis.line.y = element_line(color="black", size = 0.5),
                        axis.title.x = element_text(face="bold", size=14),
                        axis.title.y = element_text(face="bold", size=14),
                        axis.text.x = element_text(size = 12),
                        #legend.text = element_text(size = 12),
                        legend.position = "none",
                        plot.title= element_text(face="bold", size=16))

# Plot 2 #####
h <- ggplot(tab2, aes(essround, y = value, fill=cntry))
h + geom_ribbon(aes(ymin = lower, ymax = upper)) + 
  geom_line(aes(color=cntry #, linetype=cntry
  ), size=2)+
  geom_point(aes(color=cntry), shape=21, size=5, fill="white")+
  geom_point(aes(color=cntry, shape=cntry), size=3)+
  coord_cartesian(xlim = c(min(tab2$essround), max(tab2$essround) + 3)) +
  geom_label_repel(
    data = dat.labs,
    aes(label = str_wrap(cntry.lab, 20), color=cntry),nudge_x = 1.1, size= 5, fill="white",
    min.segment.length = unit(15, "points"))+ 
  
  
  scale_fill_manual(values=selectedData2()$clr)+
  scale_color_manual(values=selectedData2()$clr_line)+
  # #scale_linetype_manual(values=selectedData2()$ln_type)+
  scale_shape_manual(values=selectedData2()$shapes)+
  
  scale_x_continuous(breaks=unique(tab2$essround), minor_breaks =F)+
  labs(title =translation.tab[translation.tab$element== input$show_vals2, lang$lang],
       x=translation.tab[translation.tab$element=="x.round", lang$lang], 
       caption=translation.tab[translation.tab$element=="copyright.caption", lang$lang], 
       fill="", shape="", color="",
       y="")+
  
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        #legend.text = element_text(size = 12),
        legend.position = "none",
        axis.text.x = element_text(size = 12),
        plot.title= element_text(face="bold", size=16)
  )


# Plot 3 #####
ggplot(d$means, aes(Conservation_Openness,
                    Self_Enhancement_Self_Transcendence#,
                    #color=cntry, 
                    #label=cntry.lab
), cex=1) +
  geom_point(shape=19, size=7, alpha=.6)+
  geom_text_repel(aes(label=cntry.lab),
                  size=5, point.padding = unit(7, "points"),
                  box.padding= unit(5, "points"),#nudge_x = .1,
                  min.segment.length = unit(16, "points"),
                  segment.colour = "#00000066")+ #common labels
  #    annotate("rect", xmin = -Inf, xmax = +Inf,   ymin = 0.42, ymax = 0.50,   fill = "gray", alpha=0.3) +
  #    annotate("rect", xmin = -0.55, xmax = -0.47,   ymin = -Inf, ymax = +Inf,   fill = "gray", alpha=0.3)+
  scale_x_continuous(breaks = seq(-1.5, 0.5, by = 0.5), limits=c(-1.5, 0.5))+
  scale_y_continuous(breaks = seq(0, 2, by = 0.5), limits=c(0, 2))