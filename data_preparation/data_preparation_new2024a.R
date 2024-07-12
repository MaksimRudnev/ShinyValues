library(LittleHelpers) # Installation  devtools::install_github("maksimrudnev/LittleHelpers")
library(dplyr)
# ~~~~~~ Offline version ~~~~~~~~~~~ #####
library(haven)
#data.folder = "/Users/maksimrudnev/Library/Mobile Documents/com~apple~CloudDocs/DATA/European Social Survey/Data/R1-10 fullest 2022/"
data.folder = "/Users/maksimrudnev/Library/Mobile Documents/com~apple~CloudDocs/DATA/European Social Survey/Data/R1-10 fullest 2023/"
ess.l <- lapply(setNames(nm=list.files(data.folder)), function(x) read_sav(paste0(data.folder, x)))

r.11.path = "/Users/maksimrudnev/Library/Mobile Documents/com~apple~CloudDocs/DATA/European Social Survey/Data/ESS11/ESS11.sav"
r11 <- read_sav(r.11.path)

comm.vars = lapply(append(ess.l,list(r11=r11)), colnames) %>%
  melt() %>% select(1) %>% table() %>% as.data.frame %>%
  arrange(desc(Freq)) %>% filter(Freq>11)  %>% select(1) %>% unlist %>% as.character

#label_table(ess.l$ESS5ATe1_1.sav[,comm.vars])

extra.vars = c('happy', 'stflife', 'health', 'hincfel', 'aesfdrk', 'pplfair', 'sclmeet',
               'stfgov', 'polintr', 'lrscale',
               'gincdif', 'imwbcnt', 'freehms',
               'rlgdgr')
               
extra.vars.rev = c('health', 'hincfel', 'aesfdrk',
                  "gincdif", "polintr")        

# stflife	How satisfied with life as a whole
# health	Subjective general health (Reversed)
# hincfel	Feeling about household's income nowadays (Reversed)
# aesfdrk	Feeling of safety of walking alone in local area after dark (reversed)


# pplfair	Most people try to take advantage of you, or try to be fair
# pplhlp	Most of the time people helpful or mostly looking out for themselves
# ppltrst	Most people can be trusted or you can't be too careful

# sclact	Take part in social activities compared to others of same age
# sclmeet	How often socially meet with friends, relatives or colleagues
# stfeco	How satisfied with present state of economy in country
# stfgov	How satisfied with the national government


# gincdif	Government should reduce differences in income levels (reversed)
# imwbcnt	Immigrants make country worse or better place to live
# freehms

# agea

# imbgeco	Immigration bad or good for country's economy
# imdfetn	Allow many/few immigrants of different race/ethnic group from majority (rev)
# impcntr	Allow many/few immigrants from poorer countries outside Europe (rev)
# imsmetn	Allow many/few immigrants of same race/ethnic group as majority (rev)
# imueclt	Country's cultural life undermined or enriched by immigrants
# iorgact	Allowed to influence policy decisions about activities of organisation
# lrscale	Placement on left right scale (right)

# pray	How often pray apart from at religious services (rev)
# rlgatnd	How often attend religious services apart from special occasions (rev)
# rlgdgr	How religious are you
# 

# polintr	How interested in politics (rev)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# adjustments for  Russia files 
# r7
names(ess.l$ESS7RU_main_data.sav)<-tolower(names(ess.l$ESS7RU_main_data.sav))
ess.l$ESS7RU_main_data.sav %<>%
  mutate(essround = 7,
         pspwght = 1,
         pweight = 1#,
         # stflife = car::Recode(stflife, "c(77,88,99) = NA"),
         # happy   = car::Recode(happy,   "c(77,88,99) = NA"),
         # freehms = car::Recode(freehms, "c(8,9) = NA")
         )

for(v in c('health', 'hincfel', 'polintr', 'gincdif', 'freehms'))
  ess.l$ESS7RU_main_data.sav[[v]] %<>% car::Recode("c(8,9) = NA")
for(vv in c('pplfair', 'sclmeet', 'stfgov', 'lrscale', 'imwbcnt'))
  ess.l$ESS7RU_main_data.sav[[vv]] %<>% car::Recode("c(77,88,99) = NA")
  
         
for(v in values$items) ess.l$ESS7RU_main_data.sav[,v][ess.l$ESS7RU_main_data.sav[,v] %in% c(8, 9)]<-NA

# r9
ess.l$`data_RSS wave9-Russia_Eng labels_international ver 01.sav` <- 
  dplyr::mutate(ess.l$`data_RSS wave9-Russia_Eng labels_international ver 01.sav`, 
                essround = 9,
                pweight = 0,
                pspwght = 1)

ess.l$`Максиму_R-10_ База данных РСИ-ESS-HSE.sav` <- 
          dplyr::mutate(ess.l$`Максиму_R-10_ База данных РСИ-ESS-HSE.sav`, 
               essround = 10,
               pweight = 0,
               pspwght = 1)

# adjustments for round 11
r11$pspwght = 1
names(r11)[names(r11) %in% paste0(values$items, "a")] <- gsub("a$", "", names(r11)[names(r11) %in% paste0(values$items, "a")])

ess.l$essr11 <- r11

ess.l <- lapply(setNames(nm = names(ess.l)), function(x) {
  if (x == "ESS10_self_completion.sav") {
    ess.l[[x]]$mode = "self-completion"
    for(v in values$items)  ess.l[[x]][[v]] <- NA
    ess.l[[x]]
  } else {
    ess.l[[x]]$mode = "f2f"
    ess.l[[x]]
  }
})

#save(ess.l, file="extradata/ess1_10_list.Rdata")
# some common variables
items.to.select <- c("cntry", "essround", values$items, "dweight", "pspwght",  "idno", 'yrbrn', 
                     # "happy", "stflife", "freehms", 
                     extra.vars,
                     "mode"
                     #"pweight",
)



# all common variables
# vars.in.ess <- unname(unlist(lapply(ess.l, names)))
# comm.vars = names(table(vars.in.ess))[table(vars.in.ess)==10]
# items.to.select <- comm.vars


ess1_11 <- Reduce("rbind", lapply(ess.l, function(x) unhaven(x[,items.to.select])))

 # save(ess1_11, file="extradata/ess1_11_list.Rdata")
# crosstab("cntry", "essround", drop_labs(untibble(ess1_11)))
# load("extradata/ess1_10_withRussia_merged.Rdata")

# reverse extra vars
for(v in extra.vars.rev) ess1_11[,v] <- max(ess1_11[,v], na.rm = T) - ess1_11[,v] + 1
rm(v)


# Compute value indices

ess1_11 <- ess_values(ess1_11, v2=T, v4=T, v10=T, center=T, abbr=T)
ess1_11 <- ess_values(ess1_11, v2=F, v4=T, v10=T, center=F, abbr=T, suffix = ".non")


# Compute weighted country means and standard errors 
library("survey")


#table(ess1_11$cntry, ess1_11$essround, is.na(ess1_11[,"pspwght"]))
ess1_11[is.na(ess1_11[,"pspwght"]),"pspwght"] <- 1 # BG, CZ, EE, FI, FR, HR, HU, 
#table(ess1_11$cntry, ess1_11$essround, is.na(ess1_11[,"dweight"]))
ess1_11[is.na(ess1_11[,"dweight"]),"dweight"] <- 1 #(LT, LV, RO)

table(ess1_11$cntry, ess1_11$essround, !is.na(ess1_11[,extra.vars[[9]]]))

#s.w <- svydesign(ids = ~1, data = ess1_10, weights = ess1_10[,"pspwght"])
s.w <- svydesign(ids = ~1, data = ess1_11, weights = ess1_11[,"dweight"])

tab<-svyby(formula= ~ 
             Conservation_Openness + Self_Enhancement_Self_Transcendence + 
             Openness + Conserv + Self_Trans + Self_Enhance + 
             SE + CO + TR + BE + UN + SD + ST + HE + AC + PO +  
             SE.non + CO.non + TR.non + BE.non + UN.non + SD.non + ST.non + HE.non + AC.non + PO.non + Openness.non + Conserv.non + Self_Trans.non + Self_Enhance.non + #Conservation_Openness.non + Self_Enhancement_Self_Transcendence.non +
           yrbrn +happy +stflife +health +hincfel +aesfdrk +pplfair +sclmeet +stfgov +polintr + #iorgact +
             lrscale +gincdif +imwbcnt +freehms +rlgdgr + mrat,
           by= ~ cntry + essround,
           design = s.w, 
           FUN = svymean,
           na.rm = TRUE,
           na.rm.all=TRUE, 
           vartype = "ci",
           multicore=T
)



# Shape data for ggplot

library("reshape2") 
tb <-
  melt(tab, id.vars = c("cntry", "essround")) %>%
  mutate(
    param.kind = ifelse(
      grepl("ci_l\\.", variable),
      "lower",
      ifelse(grepl("ci_u\\.", variable), "upper", "est")
    ),
    variable = gsub("ci_l\\.|ci_u\\.", "", variable)
  ) %>%
  dcast(cntry + essround + variable ~ param.kind, value.var = "value") %>%
  rename(value = "est")
  


# tb.est <- melt(tab[,    !grepl("se\\.", names(tab))],  id.vars=c("cntry", "essround"))
# tb.se <-  melt(tab[,c(1,2,grep("se\\.", names(tab)))], id.vars=c("cntry", "essround"))
# tb.est$variable <- as.character(tb.est$variable)
# tb.se$variable <- gsub("se\\.", "", as.character(tb.se$variable))
# tb = merge(tb.est, tb.se, by = names(tb.est)[-4], all = T)
# names(tb)[4:5]<- c("value", "se")
# tb$upper<-tb$value+tb$se*1.96
# tb$lower<-tb$value-tb$se*1.96


tb$essround<-2000+tb$essround*2
tb$essround[tb$essround==2020]<-2021
tb$essround[tb$essround==2022]<-2023
tb$cntry<-as.character(tb$cntry)



# Save data 
#tab <- subset(tb, !variable %in% c("happy", "freehms", "stflife"))
#save(tab, file="data/tb2.Rdata") 
tb.extra <- subset(tb, variable %in% c(extra.vars, "yrbrn", "mrat"))
tb.extra$variable<- factor(tb.extra$variable, 
                            levels=c(extra.vars, "yrbrn", "mrat"))
saveRDS(tb.extra, "data/tb.extra.rds")

tb.values <- subset(tb, !variable %in% c(extra.vars, "yrbrn", "mrat"))
tb.values %<>% mutate(centered = !grepl("\\.non", variable),
                      variable = gsub("\\.non", "", variable))
tb.values$variable<- factor(tb.values$variable, 
                            levels=c(values$ten.abbr, values$four.abbr, values$two.abbr))
saveRDS(tb.values, "data/tb3.rds")




# Tableau

load("data/tb3.Rdata") # write.csv(tab, file = "data/tb2.csv")
translation.tab <- as.data.frame(read_delim(file="data/translation_elements.txt", 
                                            col_types="cccc",
                                            col_names=T, delim="\t", quote="", locale=locale(encoding="UTF-8")
))

translation.countries <- as.data.frame(read_delim(file="data/translation_cntry.txt",
                                                  col_types="cccc",
                                                  col_names=T, delim="\t", quote="", locale=locale(encoding="UTF-8")
))


tab1 = merge(tab, translation.countries, by = "cntry", all.x = T)
melt(tab1, id.vars = c("essround", "variable", ))

library(tidyr)
tab1 = pivot_longer(tab1[,-1], 
                    c("English",    "Russian",  "Portuguese"),
                    names_to = "language",
                    values_to = "cntry")

translation.tab1 = melt(translation.tab, id.vars = "element",variable.name = "language", value.name = "valur.lab")
tab1 = merge(tab1, translation.tab1, by.x = c("variable", "language"), by.y = c("element", "language"), all.x = T)

tab1$value_order = ifelse(tab1$variable %in% LittleHelpers::values$ten.abbr, "ten values", 
                          ifelse(tab1$variable %in% LittleHelpers::values$four.abbr,  "four higher order",
                                 ifelse(tab1$variable %in% LittleHelpers::values$two.abbr,  "two axes", NA)))


head(tab1)
write.csv(tab1[, -c(1)], file = "data/tb3.csv", row.names = F)

tab2 = 
  filter(tab1, value_order %in% "two axes") %>%
  select(language, variable, essround, cntry, value, se, upper, lower) %>%
  melt(id.vars = c("essround", "cntry", "variable", "language"), 
       variable.name = "measure", 
       value.name = "score") %>%
  dcast(language + essround + cntry ~ variable + measure, value.var = "score")

write.csv(tab2, file = "Tableau/two_axes.csv", row.names = F)


# Ind / Means
sample.ind = ess1_10[
  unlist(lapply(unique(ess1_10$cntry), function(x) sample(which(ess1_10$cntry==x), 100 ))),
  #sample(1:nrow(ess1_10), 500),
  c("cntry", "Conservation_Openness", "Self_Enhancement_Self_Transcendence")]

means = aggregate(ess1_10[, c("Conservation_Openness", "Self_Enhancement_Self_Transcendence")], 
                  list(cntry = ess1_10$cntry), mean, na.rm = T)

means.ind =  rbind(
          cbind(sample.ind, type = "indidivuals"),
          cbind(means, type = "means"))

library(readr)
translation.countries <- as.data.frame(readr::read_delim(file="data/translation_cntry.txt",
                                                  col_types="cccc",
                                                  col_names=T, delim="\t", quote="", locale=locale(encoding="UTF-8")
))


means.ind = merge(means.ind, translation.countries[, 1:2], by = "cntry", all.x = T)


write.csv(means.ind, file = "Tableau/ind_mean.csv", row.names = F)


