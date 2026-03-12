library(LittleHelpers) # Installation  devtools::install_github("maksimrudnev/LittleHelpers")
library(reshape2)
library(dplyr)

# ~~~~~~ Read data ~~~~~~~~~~~ #####
library(haven)
library(magrittr)
library(lubridate)
#data.folder = "/Users/maksimrudnev/Library/Mobile Documents/com~apple~CloudDocs/DATA/European Social Survey/Data/R1-10 fullest 2022/"
data.folder = "/Users/maksimrudnev/Library/Mobile Documents/com~apple~CloudDocs/DATA/European Social Survey/Data/R1-11 fullest 2025/"
ess.l <- lapply(setNames(nm=list.files(data.folder)), function(x) read_sav(paste0(data.folder, x)))


# adjustments for  Russia files ####
# r7
names(ess.l$ESS7RU_main_data.sav)<-tolower(names(ess.l$ESS7RU_main_data.sav))
ess.l$ESS7RU_main_data.sav %<>%
  mutate(essround = 7,
         pspwght = 1,
         pweight = 0#,
         # stflife = car::Recode(stflife, "c(77,88,99) = NA"),
         # happy   = car::Recode(happy,   "c(77,88,99) = NA"),
         # freehms = car::Recode(freehms, "c(8,9) = NA")
         )

for(v in c('health', 'hincfel', 'polintr', 'gincdif', 'freehms', values$items))
  ess.l$ESS7RU_main_data.sav[[v]] %<>% car::Recode("c(8,9) = NA")
for(vv in c('pplfair', 'sclmeet', 'stfgov', 'lrscale', 'imwbcnt'))
  ess.l$ESS7RU_main_data.sav[[vv]] %<>% car::Recode("c(77,88,99) = NA")
  

# r9
ess.l$`data_RSS wave9-Russia_Eng labels_international ver 01.sav` <- 
  dplyr::mutate(ess.l$`data_RSS wave9-Russia_Eng labels_international ver 01.sav`, 
                essround = 9,
                pweight = 0,
                pspwght = 1)
# r10
ess.l$`Максиму_R-10_ База данных РСИ-ESS-HSE.sav` <- 
          dplyr::mutate(ess.l$`Максиму_R-10_ База данных РСИ-ESS-HSE.sav`, 
               essround = 10,
               pweight = 0,
               pspwght = 1,
               inwyye = 2022)



# r11
names(ess.l$`База данных РСИ 2024.sav`) <- sapply(names(ess.l$`База данных РСИ 2024.sav`), function(x)
  if( x %in% paste0(values$items, "a")) 
    return(gsub("a$", "", x)) 
  else 
    x
  )

# all(values$items %in% names(ess.l$`База данных РСИ 2024.sav`))

ess.l$`База данных РСИ 2024.sav` <- 
  dplyr::mutate(ess.l$`База данных РСИ 2024.sav`, 
                essround = 11,
                pweight = 0,
                pspwght = 1,
                inwyye = 2024)



# adjustments for round 11
# r11$pspwght = 1


apply(ess.l[[3]][ess.l[[3]]$essround==10, paste0(values$items, "a")], 2, 
      function(x) c(values=sum(!is.na(x))/length(x)))

apply(ess.l[[3]][ess.l[[3]]$essround==11, values$items], 2, 
      function(x) c(values=sum(!is.na(x))/length(x)))

for(v in values$items)
  ess.l[[3]][[v]] = ifelse(ess.l[[3]]$essround == 11, ess.l[[3]][[paste0(v,"a")]], ess.l[[3]][[v]])


# ess.l$essr11 <- r11
# 
# ess.l <- lapply(setNames(nm = names(ess.l)), function(x) {
#   if (x == "ESS10_self_completion.sav") {
#     ess.l[[x]]$mode = "self-completion"
#     for(v in values$items)  ess.l[[x]][[v]] <- NA
#     ess.l[[x]]
#   } else {
#     ess.l[[x]]$mode = "f2f"
#     ess.l[[x]]
#   }
# })


table(ess.l[[3]]$cntry, ess.l[[3]]$impdiff,
ess.l[[3]]$impdiffa, useNA = "a") %>% as.data.frame %>% filter(Freq >0) %>%
  dcast(Var1 + Var2 ~ Var3)


# year of interview ####

# fixing interview date in the combined file
ess.l[[3]]$inwyye = ifelse(is.na(ess.l[[3]]$inwyye), year(as_date(ess.l[[3]]$inwde)), ess.l[[3]]$inwyye)
# fixing interview date in Italy r2
ess.l$`ESS2IT-2.sav`$inwyye = ess.l$`ESS2IT-2.sav`$inwyr
# fixing interview date in Ukraine r10
ess.l$`ESS Round 10 Ukraine.sav`$inwyye = year(as_date(ess.l$`ESS Round 10 Ukraine.sav`$inwde))

# checks
all(sapply(ess.l, function(x) any(c("inwyye")   %in% colnames(x))))

# checks = fix  later the missing date by using default year of the round
lapply(ess.l, function(x) sum(is.na(x$inwyye))/nrow(x)) 
table(ess.l$ESS4AT.sav$inwyye, useNA  = "a")
table(ess.l[[3]]$inwyye, useNA  = "a")

table(is.na(ess.l[[3]]$inwyye), ess.l[[3]]$mode, useNA  = "a")


# add the survey mode
ess.l = lapply(ess.l, function(x) { 
  if(!"mode" %in% colnames(x) ) {
    x$mode = 1
  }
  return(x)
})

# add interview language
ess.l = lapply(ess.l, function(x) { 
  if(!any(grepl("lnghom", colnames(x) ) )) {
    x$lnghom = NA
  }
  
  x$lnghom = apply(x[,grepl("lnghom", colnames(x))], 1, 
                   function(y) {
                     y[y==""]<-NA
                     ifelse(all(is.na(y)), NA, na.omit(y)[[1]])
                     
                     })
  
  return(x)
})

# EE = ess.l$`ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.sav` %>% filter(cntry=="EE") 
# table(EE$essround, EE$lnghom, useNA = "a")

# If Germany is East
ess.l$`ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.sav` %<>% 
  mutate(
  regionde_char = as.character(lab_to_fac(regionde)), 
  region_char = as.character(lab_to_fac(region))) %>%  
  mutate(regionde_char = ifelse(is.na(regionde_char), 
                                region_char, regionde_char)) %>%
  mutate(East_germ = ifelse(cntry=="DE", 
                            regionde_char %in% c(
                              "Berlin",                
                              "Brandenburg",           
                              "Mecklenburg-Vorpommern",
                              "Sachsen",               
                              "Sachsen-Anhalt",        
                              "Thüringen"
                            ),
                            NA))

ess.l = lapply(ess.l, function(x) {
  if(!any(grepl("East_germ", colnames(x) ) )) {
    x$East_germ = NA
  }
  x
  })


# Common variables to extract

comm.vars = lapply(ess.l, colnames) %>%
  melt() %>% select(1) %>% table() %>% as.data.frame %>%
  arrange(desc(Freq)) %>% filter(Freq>11) %>% select(1) %>% unlist %>% as.character



#label_table(ess.l$ESS5ATe1_1.sav[,comm.vars])

extra.vars = c('happy', 'stflife', 'health', 'hincfel', 'aesfdrk', 'pplfair', 'sclmeet',
               'stfgov', 'polintr', 'lrscale',
               'gincdif', 'imwbcnt', 'freehms',
               'rlgdgr', 'lnghom', "East_germ")

extra.vars.rev = c('health', 'hincfel', 'aesfdrk',
                   "gincdif", "polintr") 


all(extra.vars %in% comm.vars)
all(extra.vars.rev %in% comm.vars)
sapply(ess.l, function(x) any(c("mode")   %in% colnames(x)))

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


#save(ess.l, file="extradata/ess1_10_list.Rdata")
# some common variables
items.to.select <- c("cntry", "essround", 'inwyye',
                     values$items, "dweight", "pspwght",  "idno", 
                     'yrbrn', "gndr", "eduyrs", 
                     extra.vars,
                     "mode",
                     "pweight"
)



# all common variables
# vars.in.ess <- unname(unlist(lapply(ess.l, names)))
# comm.vars = names(table(vars.in.ess))[table(vars.in.ess)==10]
# items.to.select <- comm.vars


ess1_11 <- Reduce("rbind", lapply(ess.l, function(x) unhaven(x[,items.to.select])))

save(ess1_11, file="extradata/ess1_11_df.Rdata")
# crosstab("cntry", "essround", drop_labs(untibble(ess1_11)))

   

# reverse extra vars
for(v in extra.vars.rev) ess1_11[,v] <- max(ess1_11[,v], na.rm = T) - ess1_11[,v] + 1
rm(v)


# Compute value indices

ess1_11 <- ess_values(ess1_11, v2=T, v4=T, v10=T, center=T, abbr=T)
ess1_11 <- ess_values(ess1_11, v2=F, v4=T, v10=T, center=F, abbr=T, suffix = ".non")


# Make the year of the survey 

ess1_11$year = ifelse(is.na(ess1_11$inwyye), 
                      ifelse(ess1_11$essround == 10, 2021, 
                             ifelse(ess1_11$essround == 11, 2023,
                                    ess1_11$essround*2 + 2000)),
                      ess1_11$inwyye)

ess1_11 %<>% 
  group_by(essround, cntry) %>% 
  mutate(modal.year  = median(year)) %>%
  ungroup()

table(
      ess1_11$modal.year, 
      ess1_11$essround, ess1_11$cntry=="DE")



# Compute weighted country means and standard errors 
library("survey")

#table(ess1_11$cntry, ess1_11$essround, is.na(ess1_11[,"pspwght"]))
ess1_11[is.na(ess1_11[,"pspwght"]),"pspwght"] <- 1 # BG, CZ, EE, FI, FR, HR, HU, 
#table(ess1_11$cntry, ess1_11$essround, is.na(ess1_11[,"dweight"]))
ess1_11[is.na(ess1_11[,"dweight"]),"dweight"] <- 1 #(LT, LV, RO)

table(ess1_11$cntry, ess1_11$essround, is.na(ess1_11[,"Openness"]))

table(ess1_11$cntry, ess1_11$essround, !is.na(ess1_11[,extra.vars[[9]]]))

#s.w <- svydesign(ids = ~1, data = ess1_10, weights = ess1_10[,"pspwght"])
s.w <- svydesign(ids = ~1, data = ess1_11, weights = ess1_11[,"dweight"])

tab<-svyby(formula= ~ 
             Conservation_Openness + Self_Enhancement_Self_Transcendence + 
             Openness + Conserv + Self_Trans + Self_Enhance + 
             SE + CO + TR + BE + UN + SD + ST + HE + AC + PO +  
             SE.non + CO.non + TR.non + BE.non + UN.non + SD.non + ST.non + HE.non + AC.non + PO.non + Openness.non + Conserv.non + Self_Trans.non + Self_Enhance.non + 
             #Conservation_Openness.non + Self_Enhancement_Self_Transcendence.non +
           yrbrn +happy +stflife +health +hincfel +aesfdrk +pplfair +sclmeet +stfgov +polintr + #iorgact +
             lrscale +gincdif +imwbcnt +freehms +rlgdgr + mrat,
           by= ~ cntry + essround + modal.year,
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
  melt(tab, id.vars = c("cntry", "essround", "modal.year")) %>%
  mutate(
    param.kind = ifelse(
      grepl("ci_l\\.", variable),
      "lower",
      ifelse(grepl("ci_u\\.", variable), "upper", "est")
    ),
    variable = gsub("ci_l\\.|ci_u\\.", "", variable)
  ) %>%
  dcast(cntry + essround + variable + modal.year ~ param.kind, value.var = "value") %>%
  rename(value = "est")
  


# tb.est <- melt(tab[,    !grepl("se\\.", names(tab))],  id.vars=c("cntry", "essround"))
# tb.se <-  melt(tab[,c(1,2,grep("se\\.", names(tab)))], id.vars=c("cntry", "essround"))
# tb.est$variable <- as.character(tb.est$variable)
# tb.se$variable <- gsub("se\\.", "", as.character(tb.se$variable))
# tb = merge(tb.est, tb.se, by = names(tb.est)[-4], all = T)
# names(tb)[4:5]<- c("value", "se")
# tb$upper<-tb$value+tb$se*1.96
# tb$lower<-tb$value-tb$se*1.96


tb$cntry<-as.character(tb$cntry)

# this is a temporary fix, should rename the variable
tb$essround = tb$modal.year

## Save data  ####
#tab <- subset(tb, !variable %in% c("happy", "freehms", "stflife"))
#save(tab, file="data/tb2.Rdata") 
tb.extra <- subset(tb, variable %in% c(extra.vars, "yrbrn", "mrat"))
tb.extra$variable<- factor(tb.extra$variable, 
                            levels=c(extra.vars, "yrbrn", "mrat"))
saveRDS(tb.extra, "shinyapps.io/data/tb.extra.rds")

tb.values <- subset(tb, !variable %in% c(extra.vars, "yrbrn", "mrat"))
tb.values %<>% mutate(centered = !grepl("\\.non", variable),
                      variable = gsub("\\.non", "", variable))
tb.values$variable<- factor(tb.values$variable, 
                            levels=c(values$ten.abbr, values$four.abbr, values$two.abbr))
saveRDS(tb.values, "shinyapps.io/data/tb3.rds")

# ANALYSES #####

# Profiles ####


tb.values %>% 
  filter(essround==2023 & #cntry=="NL" & 
           centered & variable %in% values$ten.abbr) %>%
  #mutate(
    #variable = translation.tab[match(variable, translation.tab$element), "Russian"],
    #variable = factor(as.character(variable), levels = as.character(variable)[order(value)])) %>%
ggplot(aes(value, variable))+
  geom_col(fill = "tomato", size = 3, alpha = .7)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, color = "blue")+
  #geom_point(color = "tomato", size = 3)+
  geom_text(aes(label = f(value,2)), #nudge_y = .2, 
            size = 3)+
  #labs(x = "", y = "", title = "Нидерланды")+
  theme_mr()+facet_wrap(~cntry, scales = "free")


# difference
tb.values %>% 
  filter(essround==2021 & centered & variable %in% c("PO", "UN")) %>%
  dcast(cntry ~ variable, value.var = "value") %>%
  mutate( diff =  UN - PO,
    cntry = translation.countries[match(cntry, translation.countries$cntry), "Russian"], 
    cntry = factor(cntry, levels = cntry[order(diff, decreasing = F)]), 
    filler = cntry == "Россия") %>%
  ggplot(aes(diff, cntry))+
  geom_col(aes(fill = filler), size = 3, alpha = .7, show.legend = F)+
  geom_text(aes(label = f(diff,2)), nudge_x = .1, size = 3)+
  scale_fill_manual(values = c("tomato", "royalblue"))+
  labs(x = "", y = "", title = "Универсализм минус\nВласть-богатство")+
  theme_mr()




# Tableau #####
 
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


# Russia invariance ######

load("extradata/ess1_11_list.Rdata")

russia = ess1_11[ess1_11$cntry=="RU",]


library(LittleHelpers); library(reshape2); library(MIE)

russia = ess_values(russia)


original.categories = melt(values$codes.for.ten) %>%
  mutate(item.wording = sapply(value, function(x) names(russia)[grepl(paste0(x,"\\."), names(russia) )]))


# ideal.value.model = paste(apply(melt(values$codes.for.ten)[,2:1], 1, paste, collapse = "=~"), collapse = ";\n")
ideal.value.model =  
           "TRADCONSE=~impsafe;
            TRADCONSE=~ipstrgv;
            TRADCONSE=~ipfrule;
            TRADCONSE=~ipbhprp;
            TRADCONSE=~ipmodst;
            TRADCONSE=~imptrad;
            STRANS=~iphlppl;
            STRANS=~iplylfr;
            STRANS=~ipeqopt;
            STRANS=~ipudrst;
            STRANS=~impenv;
            Self.Direction=~ipcrtiv;
            Self.Direction=~impfree;
            Stimulation =~ impdiff;
            Stimulation =~ ipadvnt;
            Hedonism =~ ipgdtim;
            Hedonism =~ impfun;
            POACH =~ ipshabt;
            POACH =~ ipsuces;
            POACH =~ imprich;
            POACH =~ iprspot"

m1 = cfa(ideal.value.model, russia, estimator = "mlr", missing = "ml", std.lv = T)
summary(m1, fit = T, std = T, est = F)
lavInspect(m1, "cor.lv")

library(dynamic)
dyn.cutoff = cfaHB(m1)



mg1 = globalMI(ideal.value.model, russia, 
          group = "essround",
          estimator = "mlr", missing = "ml")



# alignment ####
russia$cntry_n = as.numeric(as.factor(russia$cntry))
write.table(russia[, c("cntry_n", values$items, "dweight", "yrbrn")], quote = F, row.names = F, col.names = F, file = "data_preparation/russia_1_11.dat")

runAlignment("Security BY impsafe ipstrgv;
  Conformity BY ipfrule ipbhprp;
  Tradition BY ipmodst imptrad;
  Benevolence BY iphlppl iplylfr;
  Universalism BY ipeqopt ipudrst impenv;
  SelfDirection BY ipcrtiv impfree;
  Stimulation BY impdiff ipadvnt;
  Hedonism BY ipgdtim impfun;
  Achievement BY ipshabt ipsuces;
  Power BY imprich iprspot;", 
             group = "essround", 
             dat = russia,
             sim.samples =NULL, 
             processors = 6)


al.all = extractAlignment("~/Library/Mobile Documents/com~apple~CloudDocs/R-functions/ShinyValues/Basic_Values/data_preparation/free.out")

al.loadings = al.all$summary %>%
  mutate(param = rownames(.)) %>%
  filter(!grepl("Intercept", param)) %>%
  mutate(f = trimws(gsub("Loadings  |(by.+)", "", param)),
         item = trimws(tolower(gsub(".*by", "", param)))) %>%
  dcast(item ~ f, value.var = "AlignedParameter") %>%
  mutate(across(2:7, as.numeric)) %>% 
  mutate(category = original.categories[match(item,original.categories$value), "L1"], .before = 1) 

al.loadings %>%
   arrange(category) %>%
  mutate(across(3:8, function(x) sig_to_bold(f(x,2), bold.thresh = .5))) %>%
  df_to_viewer(row = F, kable.options=list(escape=FALSE, align = c("l", "l", rep("r", 6 ))))

al.loadings %>%  melt(id.vars = c("item", "category")) %>%
  mutate(category = factor(as.character(category), levels = values$ten)) %>%
  ggplot(aes(category, value, group = variable, color = variable))+geom_point()+
  facet_wrap(~variable)+theme_bw()+geom_hline(yintercept= 0)


al.params = MplusAutomation::readModels("~/Library/Mobile Documents/com~apple~CloudDocs/R-functions/ShinyValues/Basic_Values/data_preparation/free.out", "parameters")


al.params$parameters$unstandardized %>% 
  filter(grepl("WITH", paramHeader)) %>% 
  mutate(eststar = eststar(est, pval)) %>%
  dcast(paramHeader + param ~ Group, value.var = "eststar")


al.all$mean.comparison %>% melt(id.vars = "Group.value") %>%
  filter(variable == "Factor.mean") %>%
  ggplot(aes(as.numeric(Group.value), value, color = L1, group = L1))+geom_line()+
  facet_wrap(~L1)

# correlations #####
cors = lapply(setNames(nm=3:11), function(i)
  cor(russia[russia$essround==i,values$items], use = "complete"))

pcors = lapply(setNames(nm=3:11), function(i)
  corpcor::cor2pcor(cor(russia[russia$essround==i,values$items], use = "complete")))

pcor.var.and.trend = 
  melt(pcors) %>% dcast(Var1 + Var2 ~ L1, value.var = "value") %>% #str
  filter(Var1 != Var2) %>%
  mutate(sd_ = apply(.[,-c(1,2)], 1, sd), 
         slope_ = apply(.[,-c(1,2)], 1, function(x) {
           essround = as.numeric(names(x))
           x = unlist(x)
           #coef(lm(x ~ essround))[[2]]
           cor(x, essround)
           }),
         M_ = rowMeans(.[,-c(1,2)])
         ) %>%
  mutate(Var1 = values$items[as.numeric(Var1)],
         Var2 = values$items[as.numeric(Var2)]) %>%
  arrange(desc(slope_)) %>% select(Var1, Var2, sd_, slope_, M_) %>%
  filter(!(duplicated(sd_)|duplicated(slope_)))

df_to_viewer(pcor.var.and.trend)


melt(pcors) %>% filter(Var1!=Var2 & !duplicated(value)) %>%
  mutate(Var1 = values$items[as.numeric(Var1)],
         Var2 = values$items[as.numeric(Var2)]) %>%
  filter(Var1 %in% values$codes.for.four$Self.Transcendence & 
           Var2 %in% values$codes.for.four$Self.Transcendence) %>%
  ggplot(aes(as.numeric(L1), value))+
  geom_line()+geom_point()+scale_x_continuous(breaks = 3:11)+
  facet_grid(rows = vars(Var1), cols = vars(Var2))


melt(cors) %>% filter(Var1!=Var2 & !duplicated(value)) %>%
  mutate(Var1 = values$items[as.numeric(Var1)],
         Var2 = values$items[as.numeric(Var2)]) %>%
  filter(Var1 %in% values$codes.for.four$Openness.to.Change & 
           Var2 %in% values$codes.for.four$Openness.to.Change) %>%
ggplot(aes(as.numeric(L1), value))+
  geom_line()+geom_point()+scale_x_continuous(breaks = 3:11)+
  facet_grid(rows = vars(Var1), cols = vars(Var2))


plot(sapply(3:11, function(i)
       psych::alpha(russia[russia$essround==i,values$codes.for.four$Openness.to.Change])$total$raw_alpha), type = "line", xlab = "essround", ylab = "Cronbach's alpha for openness")

plot(sapply(3:11, function(i)
  psych::alpha(russia[russia$essround==i,values$codes.for.four$Openness.to.Change])$total$raw_alpha))

russia %>% group_by(essround) %>%
  summarize(across(all_of(values$items), mean, na.rm = T)) %>%
  melt(id.vars = "essround") %>%
  filter(variable %in% values$codes.for.four$Openness) %>%
ggplot(aes(essround, value, color = variable))+geom_line()+scale_x_continuous(breaks = 3:11)

## cor indices #####
cors.indices = lapply(setNames(nm=3:11), function(i)
  cor(russia[russia$essround==i,values$four], use = "complete"))

cors.indices.trends = 
melt(cors.indices) %>% dcast(Var1 + Var2 ~ L1, value.var = "value") %>% #str
  filter(Var1 != Var2) %>%
  mutate(sd_ = apply(.[,-c(1,2)], 1, sd), 
         slope_ = apply(.[,-c(1,2)], 1, function(x) {
           essround = as.numeric(names(x))
           x = unlist(x)
           #coef(lm(x ~ essround))[[2]]
           cor(abs(x), essround)
         }),
         M_ = rowMeans(.[,-c(1,2)])
  ) %>%
  arrange(desc(slope_)) %>% select(Var1, Var2, sd_, slope_, M_) %>%
  filter(!(duplicated(sd_)|duplicated(slope_)))

arrange(cors.indices.trends) %>% df_to_viewer()

melt(cors.indices)  %>% 
  filter(Var1 != Var2) %>%
  # filter(Var1 %in% c("Tradition", 'Universalism', "Stimulation", "Hedonism", "Conformity") & 
  #        Var2 %in% c("Tradition", 'Universalism', "Stimulation", "Hedonism", "Conformity")) %>%
  ggplot(aes(as.numeric(L1), value, color = value>0))+
  geom_line()+geom_point()+scale_x_continuous(breaks = 3:11)+
  facet_grid(rows = vars(Var1), cols = vars(Var2))

## cors across cohorts, indices ####
options("scipen"=999)
russia$cohort = cut(russia$yrbrn, breaks = c(1900, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2020),
                    ordered_result = T, dig.lab = 0)
#russia$cohort_n = as.numeric(russia$cohort)
russia %<>% group_by(cohort) %>% mutate(cohort_n = round(mean(yrbrn, na.rm = T)))
table(russia$cohort, russia$cohort_n)
cors.indices.cohort = 
  lapply(setNames(nm=sort(unique(russia$essround))), function(r)
    lapply(setNames(nm=sort(unique(russia$cohort_n))), function(i) {
      val.subset = russia[russia$cohort_n==i & russia$essround==r, values$items]
      if(nrow(na.omit(val.subset))<2) 
        matrix(rep(NA, 16), nrow=4, dimnames = list(values$four, values$four) ) 
      else
       cor(val.subset[,values$four],  use = "complete")
}))

cors.indices.cohort.df = lapply(cors.indices.cohort, function(y) 
  lapply(y, function(x) { x[lower.tri(x, diag=T)]=NA; return(x)})) %>%
  melt %>% filter(!is.na(value))  %>% rename(cohort = "L2", essround = "L1")

cors.indices.trends.cohorts = 
  cors.indices.cohort.df %>% 
   group_by(Var1, Var2) %>%  
   summarize(cors_round = cor(abs(value), as.numeric(essround)),
             cors_cohort = cor(abs(value), as.numeric(cohort)))

df_to_viewer(cors.indices.trends.cohorts, row = F)

summary(lm(abs(value) ~ as.numeric(essround) + as.numeric(cohort), cors.indices.cohort.df %>% mutate(value = psych::fisherz(value))))


ggplot(cors.indices.cohort.df, 
       aes(as.numeric(cohort), value, color = as.numeric(essround)))+
  #geom_line()+
  geom_point(alpha = .5)+
  #scale_x_continuous(transform = "reverse", breaks = 8:1)+
  facet_grid(rows = vars(Var1), cols = vars(Var2))

# lms

OP_ST = lm(Openness.to.Change ~ Self.Transcendence*yrbrn*essround, russia)
OP_SE = lm(Openness.to.Change ~ Self.Enhancement*yrbrn*essround, russia)
CO_SE = lm(Conservation ~ Self.Enhancement*yrbrn*essround, russia)
CO_ST = lm(Conservation ~ Self.Transcendence*yrbrn*essround, russia)

texreg::screenreg(list(OP_ST, CO_ST, OP_SE, CO_SE))
effects::Effect(c("Self.Transcendence","yrbrn","essround"), OP_ST) %>% as.data.frame %>%
  ggplot(aes(Self.Transcendence, fit, color = yrbrn, group = yrbrn))+geom_line()+ geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha= .3)+facet_wrap(~essround)+ggtitle("OP_ST")
effects::Effect(c("Self.Enhancement","yrbrn","essround"), CO_SE) %>% as.data.frame %>%
  ggplot(aes(Self.Enhancement, fit, color = yrbrn, group = yrbrn))+geom_line()+facet_wrap(~essround)
effects::Effect(c("Self.Transcendence","yrbrn","essround"), CO_ST) %>% as.data.frame %>%
  ggplot(aes(Self.Transcendence, fit, color = yrbrn, group = yrbrn))+geom_line()+facet_wrap(~essround)
effects::Effect(c("Self.Enhancement","yrbrn","essround"), OP_SE) %>% as.data.frame %>%
  ggplot(aes(Self.Enhancement, fit, color = yrbrn, group = yrbrn))+
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha= .3)+
  geom_line()+facet_wrap(~essround)+
  ggtitle("OP_SE")



long.russia = russia %>% ungroup %>%
  select(all_of(values$four), yrbrn, essround) %>%
  melt(id.vars = c("yrbrn", "essround"))

lmer1 = lmer(value ~ 1 + (1|yrbrn) + (1|essround),long.russia)

ranef(lmer1)

## glm nets ####
library(glmnet)
glms = lapply(setNames(nm=values$ten), function(dv)
  lapply(setNames(nm=c(3:11)), function(x) {
    
     IV = values$ten[values$ten != dv]
     cat(dv, x)
     xy = russia %>%  
       select(-all_of(values$ten)) %>% ess_values(center = F, v4 = F, v10 = T, v2 = F) %>%
       dplyr::filter(essround == x) %>% select(all_of(values$ten)) %>% na.omit
  glmnet(xy[IV],
         xy[[dv]],
         alpha = 1 # 1=lasso regr
  )
   }))

    cofs %>% 
      mutate_all(function(x) ifelse(abs(x)<0.01, "", f(x,2))) %>%
      df_to_viewer()

# ALL vals
all.ten.coords = lapply(setNames(nm=names(glms)), function(this.v) {
   
   cofs = Reduce(cbind, lapply(glms[[this.v]], coef, s = .01)) %>% 
      as.data.frame.matrix %>% set_names(names(glms[[this.v]]))

   # perfect circle
    perfect.cors = sin(pi/2*seq(1, 5, by = .42))
    perfect.cors100 = rep(perfect.cors, 2)
    perfect.cors.m = matrix(perfect.cors100[c(11:20, 10:19, 9:18, 8:17, 
                                              7:16, 6:15, 5:14, 4:13, 
                                              3:12, 2:11)],
           nrow = 10, dimnames = list(values$ten,values$ten))

    
    # get the max coef
    norm.max = max(abs(cofs[-1, ]))
    # adjust perfect matrix with the max value
    perfect.cors.m.norm = perfect.cors.m*norm.max
    
    # initial coords
    perfect.cors.m.norm[this.v,values$ten] <- cofs[values$ten,names(cofs)[1]]
    perfect.cors.m.norm[values$ten, this.v] <- cofs[values$ten,names(cofs)[1]]
    diag(perfect.cors.m.norm) = norm.max
    # compute coords
    coords1 = as.data.frame.matrix(cmdscale(dist(perfect.cors.m.norm)))
    
    hed.coords = lapply(setNames(nm=names(cofs)[-1]), function(v) {
      
      # replace col and row for particular value
      perfect.cors.m.norm[this.v, values$ten] <- cofs[values$ten,v]
      perfect.cors.m.norm[values$ten, this.v] <- cofs[values$ten,v]
      diag(perfect.cors.m.norm) = norm.max
      # compute cors
      coords = as.data.frame.matrix(cmdscale(dist(perfect.cors.m.norm)))
      # rotate towards the initial coords
    
      coords.rot <- targetQ(as.matrix(coords), 
                            Target = as.matrix(coords1)
      )
      coords.rot = as.data.frame.matrix(coords.rot$loadings)
      
      if(coords.rot["Universalism","V1"]>0) coords.rot[,"V1"] = -coords.rot[,"V1"] 
      if(coords.rot["Universalism","V2"]<0) coords.rot[,"V2"] = -coords.rot[,"V2"] 
      
      coords.rot$labels = row.names(coords.rot)
      coords.rot

    })
    # swapping to align
    if(coords1["Universalism","V1"]>0) coords1[,"V1"] = -coords1[,"V1"] 
    if(coords1["Universalism","V2"]<0) coords1[,"V2"] = -coords1[,"V2"] 
    coords1$labels = row.names(coords1)
    hed.coords = append(hed.coords, list(`3`=coords1))
})



melt(all.ten.coords, id.vars = "labels") %>% dcast(labels + L1 + L2 ~ variable) %>%
  ggplot(aes(V1, V2, color = as.numeric(L2) ))+geom_point()+
  geom_line(aes(group = labels))+
  geom_text(aes(label = substr(labels, 1, 2)), nudge_x = .1)+
  coord_equal()+facet_wrap(~L1)


# manual rot

ten.raw.by.round = lapply(3:11, function(rnd)
russia %>%  
  select(-all_of(values$ten)) %>% ess_values(center = F, v4 = F, v10 = T, v2 = F) %>%
  dplyr::filter(essround == rnd) %>% select(all_of(values$ten)) %>% na.omit %>% cor)

coords1 = as.data.frame.matrix(cmdscale(dist(ten.raw.by.round[[1]])))
plot(coords1)
all.coords.rot = lapply(2:9, function(rnd) {
  coords = as.data.frame.matrix(cmdscale(dist(ten.raw.by.round[[rnd]])))
# rotate towards the initial coords
  coords.rot <- targetQ(as.matrix(coords), Target = as.matrix(coords1))
  coords.rot <- as.data.frame.matrix(coords.rot$loadings)
})
all.coords.rot = append( list(coords1), all.coords.rot)

all.coords.rot = 
lapply(all.coords.rot, function(x) {
  if(x["Universalism", "V1"]>0) x[, "V1"]= - x[, "V1"]
  if(x["Universalism", "V2"]<0) x[, "V2"]= - x[, "V2"]
  x
  })

lapply(all.coords.rot, function(x) x %>% mutate(labels = row.names(.)) ) %>%
melt(id.vars = "labels") %>% dcast(labels + L1 ~ variable) %>%
  mutate(L1 = factor(L1, levels = 1:9)) %>%
  filter(L1 %in% c(1,3,6, 9)) %>%
  ggplot(aes(V1, V2, color = labels ))+
  #geom_point()+
  geom_line(aes(group = labels))+
  geom_text(aes(label = L1))+
  coord_equal()



 # indices network #####
network_RUS_ten_pool <- estimateNetwork(
                                ess_values(russia[russia$essround %in% 3:11,values$items], v10 = T, 
                                           center = F)[, values$ten],
                               default = "EBICglasso", # 
                               threshold = T,
                               corMethod = "spearman",
                               missing = "listwise")
network_RUS_ten <- lapply(setNames(nm=3:11), function(i)
  estimateNetwork(ess_values(russia[russia$essround %in% i,values$items], v10 = T, 
                             center = F)[, values$ten],
                  default = "EBICglasso", # 
                  threshold = T,
                  corMethod = "spearman",
                  missing = "pairwise"))

plot(network_RUS_ten_pool,
     weighted = T, signed = T,
     layout = "spring", maximum=.2)

net_layout <- averageLayout(network_RUS_ten$`3`,network_RUS_ten$`4`,network_RUS_ten$`5`,network_RUS_ten$`6`,network_RUS_ten$`7`,network_RUS_ten$`8`,network_RUS_ten$`9`,network_RUS_ten$`10`,network_RUS_ten$`11`,layout = "spring")

plot(network_RUS_ten$`3`,
     weighted = T, signed = T,
     layout = net_layout, 
     maximum= .2)

plot(network_RUS_ten$`11`,
     weighted = T, signed = T,
     layout = net_layout, maximum= .2)


nct_test_3_11 <- NCT(network_RUS_ten$`9`, 
                     network_RUS_ten$`10`,
                    it = 1000,
                    gamma = .5,
                    #binary.data = F,
                    test.edges = T,
                    p.adjust.methods = "BH",
                    progressbar = T,
                    weighted = TRUE,
                    abs = F,
                    test.centrality=TRUE,
                    centrality = "strength"
                    )

summary(nct_test_3_11)

nct_test_3_11$einv.pvals %>% filter(`p-value` < .05) %>% arrange(desc(`Test statistic E`)) %>%
  df_to_viewer(row = F, digits = 3)

nct_test_3_11$diffcen.pval %>% as.data.frame %>% arrange(strength)

## EGA invariance ####
ega_invariance <- invariance(
  data = russia[russia$essround %in% 3:11, values$ten],
  group = russia$essround, 
  ncores = 8, seed = 1, 
  loading.method = "revised", model = "glasso",
  corr = "spearman", 
  na.data = "listwise"
)
ega_invariance
plot(ega_invariance$configural.results$item_stability)
plot(ega_invariance, base = 2)

ega_invariance$results = lapply(ega_invariance$results, function(x) x %>% mutate(item = row.names(.)))

melt(ega_invariance$results, id.vars= c("Membership", "sig", "Direction", "item")) %>%
  filter(variable == "p_BH") %>% 
  mutate(Direction = paste0(Direction, sig) ) %>%
  dcast(item + Membership ~ L1, value.var = "Direction")  %>%  
  select(item, Membership, c("3-4", '4-5', '5-6', '6-7', '8-7', '9-8','9-10', '10-11')) %>%
  arrange(Membership) %>%
  df_to_viewer()





#  Networks #####
library(qgraph); library(dplyr)


# Network estimation
library(bootnet)
items.reduced = unlist(values$codes.for.ten)
items.reduced <- items.reduced[!items.reduced %in% c("impfree")] # , "iprspot"
network_RUS <- estimateNetwork(russia[russia$essround %in% 3:11, 
                                      items.reduced],
                                  default = "EBICglasso", # 
                               lambda = .6,
                                  threshold = F,
                                  corMethod = "spearman",
                                  missing = "listwise")
centrality(network_RUS, R2 = T)

plot(network_RUS,
     weighted = T, signed = T,
     layout = "spring", 
     maximum = .7,
     #labels = 1:21,
     label.cex = 1, 
     legend.cex = 0.4, # scalar of the legend
     #legend.mode = 'style2', # default is 'style1'
     groups = original.categories$L1[original.categories$value %in% items.reduced],
     #nodeNames = original.categories$value, 
     palette = "rainbow",
     title = "Russia pooled")

network_RUS_byround <- lapply(setNames(nm=3:11), function(i)
  estimateNetwork(russia[russia$essround == i, items.reduced],
                               default = "TMFG", # EBICglasso
                               #threshold = F,
                               refit = T, 
                               lambda = .5,
                               #corMethod = "spearman",
                               graphType = "pcor",
                               missing = "pairwise"))

net_layout21 <- averageLayout(network_RUS_byround$`3`,network_RUS_byround$`4`,network_RUS_byround$`5`,network_RUS_byround$`6`,network_RUS_byround$`7`,network_RUS_byround$`8`,network_RUS_byround$`9`,network_RUS_byround$`10`,network_RUS_byround$`11`,
                              layout.par = list(groups = original.categories$L1[original.categories$value %in% items.reduced]),
                              layout = "spring")


plot(network_RUS_byround$`11`,
     weighted = T, signed = T,
     layout = a11$layout.orig,
     #layout = network_RUS_byround$`11`,
     #layout = net_layout21, 
     maximum = max(sapply(network_RUS_byround, function(x) max(abs(x$graph)))),
      label.cex = 1, 
      legend.cex = 0.4, # scalar of the legend
     # legend.mode = 'style2', # default is 'style1'
     groups = original.categories$L1[original.categories$value %in% items.reduced],
     palette = "rainbow",
     title = "Russia r11")


a11 = plot(network_RUS_byround$`3`,
           weighted = T, signed = T,
           layout = layout_with_kk, repulsion = .5,
           #layout = net_layout21, 
           maximum = max(sapply(network_RUS_byround, function(x) max(abs(x$graph)))),
           label.cex = 1, 
           legend.cex = 0.4, # scalar of the legend
           #legend.mode = 'style2', # default is 'style1'
           groups = original.categories$L1[original.categories$value %in% items.reduced],
           palette = "rainbow",
           title = "Russia r3")


melt(network_RUS_byround$`3`$graph - network_RUS_byround$`11`$graph) %>%
  arrange(value) %>% filter(!duplicated(value)) %>% head



library(NetworkComparisonTest)
nct_test <- NCT(network_RUS_byround$`3`, 
                network_RUS_byround$`11`,
                it = 1000,
                gamma = .5,
                #binary.data = F,
                test.edges = T,
                # edges=list(c(1,2),  # compares 1->2 edge 
                #            c(3,6)), # compares 3->6 edge 
                p.adjust.methods = c("none", "holm", "hochberg", "hommel", 
                                     "bonferroni", "BH", "BY", "fdr"),
                progressbar = T,
                weighted = TRUE,
                abs = F,
                test.centrality=TRUE,
                centrality = "strength", verbose = F
)
summary(nct_test)

# statistic E is simply an abs difference in edges
nct_test$einv.pvals %>% #filter(`p-value` < .01) %>% 
  arrange(desc(`Test statistic E`)) %>% 
  df_to_viewer(row = F, digits = 3)

nct_test$diffcen.pval %>% as.data.frame %>% arrange(strength)






plot(network_IL_new, #weighted = T, signed = T,
     layout = net_layout, #' *fixed layout*
     maximum = .7, #' *fixed edge width*
     labels = 1:21,
     label.cex = 1, 
     legend.cex = 0.4, # scalar of the legend
     legend.mode = 'style2', # default is 'style1'
     groups = original.categories$L1[1:21],
     nodeNames = original.categories$item.wording[1:21], 
     
     #theme = "style2", details = T,
     graph = "glasso", refit = T,
     title = "Israel at-war")


# EGAnet ########
# see https://www.mdpi.com/2624-8611/3/3/32
library(EGAnet)

ru.list = split(russia[, items.reduced], russia$essround)

ega1 = EGA(ru.list$`3`)
ega2 = EGA(ru.list$`11`)

CFA(ega1, ru.list$`3`, estimator = "ml")
CFA(ega2, ru.list$`11`, estimator = "ml")

# network loadings
net.loads(ega1)
net.loads(ega2)

t(rbind(
  entropyFit(ru.list$`3`, ega1$wc),
  entropyFit(ru.list$`11`, ega2$wc))) %>% round(2)

# hierEGA(IL_old, "revised", model = "glasso") # experimental

#LCT	Loadings Comparison Test - should it be factor or network model?
LCT(ru.list$`3`, corr = "spearman", na.data = "listwise", model = "glasso",
    seed = 1244)
LCT(ru.list$`11`, corr = "spearman", na.data = "listwise", model = "glasso",
    seed = 1244)

# invariance
ega_invariance <- invariance(
  data = russia[, items.reduced],
  group = russia$essround, 
  ncores = 8, seed = 1, 
  loading.method = "revised", model = "glasso",
  corr = "spearman", 
  na.data = "listwise",
  algorithm = "walktrap", uni.method = "expand"
)
ega_invariance
plot(ega_invariance$configural.results$item_stability)
plot(ega_invariance, p_type = "p_BH")

ega_invariance$configural.results

items.reduced[! items.reduced %in% names(ega_invariance$configural.results$stable_items)]

ega_invariance$results = lapply(ega_invariance$results, function(x) x %>% mutate(item = row.names(.)))

melt(ega_invariance$results, id.vars= c("Membership", "sig", "Direction", "item")) %>%
  filter(variable == "p_BH") %>% 
  mutate(Direction = paste0(Direction, sig) ) %>%
  dcast(item + Membership ~ L1, value.var = "Direction")  %>% 
  select(item, Membership, c("3-4", '4-5', '5-6', '6-7', '8-7', '9-8','9-10', '10-11')) %>%
  arrange(Membership) %>%
  df_to_viewer()



ega_invariance10_11 <- invariance(
  data = russia %>% dplyr::filter(essround %in% 10:11) %>% select(all_of(unname(items.reduced))),
  group = russia$essround[russia$essround %in% 10:11], 
  ncores = 8, seed = 1, 
  loading.method = "revised", model = "glasso",
  corr = "spearman", 
  na.data = "listwise",
  algorithm = "walktrap", uni.method = "expand"
)
plot(ega_invariance10_11)

