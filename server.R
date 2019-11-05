
#setwd("~/Dropbox/STAT/R-functions/ShinyValues/Basic_Values")
#load("data/tab.pspweight.R")
#load("data/tab.dweight.R")
#load("data/tab.pspweight_rus.R")

load("data/tb2.Rdata")
#load("data/europe.Rdata")
#tab<-tab[tab$variable!="stflife" & tab$variable!="happy",]
tab$variable<-droplevels(tab$variable)

require("ggplot2")
require("reshape2")
require("ggrepel")
require("stringr")
#require("sf")
require("readr")

translation.tab <- as.data.frame(read_delim(file="data/translation_elements.txt", 
                              col_types="cccc",
                              col_names=T, delim="\t", quote="", locale=locale(encoding="UTF-8")
                              ))

translation.countries <- as.data.frame(read_delim(file="data/translation_cntry.txt",
                              col_types="cccc",
                              col_names=T, delim="\t", quote="", locale=locale(encoding="UTF-8")
                              ))



values <- list(
  ten.abbr =  c("SE", "CO", "TR", "BE", "UN", "SD", "ST", "HE", "AC", "PO"),
  four.abbr = c("Openness", "Conserv", "Self_Trans", "Self_Enhance"),
  two.abbr = c("Conservation_Openness", "Self_Enhancement_Self_Transcendence"))




shinyServer(
function(input, output, session) {
  
  gg_color_hue <- function(n, brightness=70, beginning=0) {
    hues = seq(beginning, beginning+360-15, length = n )
    hcl(h = hues, l = brightness, c = 200)
  }
  #barplot(1:10, col = gg_color_hue(10, 50, 72))
  #plot(rep(1,10), col=hcl(h = seq(15, 375, length = n + 1), l = 70, c = 200), pch=19, cex=10)
 
 
  
# Translate elements to some language
  #store name of language
  lang <- reactiveValues(lang="English")
  
  # add the language selector to UI
   output$language.selector <- renderUI(
   selectInput('lang', NULL, #translation.tab[translation.tab$element=="lang.selector", lang$lang],
               choices=names(translation.tab)[-1], selected = "English")
   )
   
   # watch if selector changes values
   observeEvent(input$lang, {
     lang$lang = input$lang
   })
   
   # # produce an output to use in UI elements
   # lapply(translation.tab$element, function(x) {
   #   el.name <- paste("lang.", x, sep="")
   #   output[[el.name]] <- renderText( translation.tab[translation.tab$element==x,lang$lang]
   #                                    )
   # })
   
   # produce an output to use for countries in UI
   # lapply(translation.countries$cntry, function(x) {
   #   el.name <- paste("lang.c.", x, sep="")
   #   output[[el.name]] <- renderText( translation.countries[translation.countries$cntry==x,lang$lang]
   #   )
   # })
   
   # produce UI  elements in target language
     ## Tab1 
   output$lang.general.title <- renderUI(
     tags$span(translation.tab[translation.tab$element=="general.title",lang$lang])
   )
   
   output$lang.val10.header <- renderUI(
     actionLink("tenValues.link", label=translation.tab[translation.tab$element=="val10.header", lang$lang])
   )
   
   output$lang.val4.header <- renderUI(
     actionLink("fourValues.link",  label=translation.tab[translation.tab$element=="val4.header", lang$lang])
   )
   
   output$lang.val2.header <- renderUI(
     actionLink("twoValues.link",  label=translation.tab[translation.tab$element=="val2.header", lang$lang])
   )
     
   ## Tab names
   output$lang.tab1.name <- renderUI(
     tags$span(translation.tab[translation.tab$element=="tab1.name",lang$lang])
   )
   
   output$lang.tab2.name <- renderUI(
     tags$span(translation.tab[translation.tab$element=="tab2.name",lang$lang])
   )
   
   output$lang.tab3.name <- renderUI(
     tags$span(translation.tab[translation.tab$element=="tab3.name",lang$lang])
   )
   
   output$lang.geomap.tab <- renderUI(
     tags$span(translation.tab[translation.tab$element=="geomap.tab",lang$lang])
   )
   
   
   # Tab 2
   output$lang.multiple.countries.selector <- renderUI( 
      tags$html(strong(span(translation.tab[translation.tab$element=="multiple.countries.selector",lang$lang])))
   )
   
   output$lang.showNorth <- renderUI( 
   actionLink("showNorth", translation.tab[translation.tab$element=="north",lang$lang] #"North"
   ))
   
   output$lang.showEast <- renderUI( 
   actionLink("showEast", translation.tab[translation.tab$element=="east",lang$lang]  #"East"
   ))
   
   output$lang.showWest <- renderUI( 
   actionLink("showWest", translation.tab[translation.tab$element=="west",lang$lang] # "West"
   ))
   
   output$lang.showSouth <- renderUI( 
   actionLink("showSouth", translation.tab[translation.tab$element=="south",lang$lang] #"South"
   ))
   
   # Tab 3
   output$tab3.slider <- renderUI( 
     
   sliderInput(inputId="round", 
               label=translation.tab[translation.tab$element=="year.slider",lang$lang], #"Year of survey",
               
               
               min=min(tab$essround), # 2002
               max=max(tab$essround), # 2018
               value=2018,
               step=2, round=TRUE, animate=T, sep="")
   )
   
   output$help.tab3.slider <- renderUI( 
   helpText(translation.tab[translation.tab$element=="hint.year.slider",lang$lang])
   )
   
   output$tab4.slider <- renderUI(
     sliderInput("round.tab4", translation.tab[translation.tab$element=="year.slider2",lang$lang],
                 min=min(tab$essround), # 2002
                 max=max(tab$essround), # 2016
                 value=2006,
                 step=2, round=TRUE, animate=T, sep="")
   )
   
   # Tab 1 functionality
   
   list.of.countries <- reactive({
     a <- translation.countries[translation.countries$cntry %in% unique(tab$cntry),]
     a <- a[order(a[,lang$lang]),]
     b<- as.list(a$cntry)
     names(b) <- a[,lang$lang]
     b 
   })
   
   output$country.selector.tab.1 <- renderUI(
   selectInput('show_countries', #'Choose country to show:',
               label=translation.tab[translation.tab$element=="country.selector", lang$lang],
               choices=list.of.countries(), #levels(tab$cntry),
               selected = store.tab1$selected.countries
               )
   )
   
   observeEvent(input$show_countries, {
     store.tab1$selected.countries<-input$show_countries
     })
 
  
# Functionality of section links in the first tab ####  
  store.tab1 <- reactiveValues (
    ten.values = NULL,
    #ten.values.checked = FALSE,
    four.values = NULL,
    #four.values.checked = FALSE,
    two.values = values$two.abbr,
    #two.values.checked = FALSE,
    #lang = "English",
    selected.countries = "AT"
  )
  

  
  # Selectors of values on tab 1 ####
   output$ten.values.selector.tab.1 <- renderUI({
     checkboxGroupInput('show_vals_a', NULL,
                        choiceValues= values$ten.abbr,
                        choiceNames = unname(sapply(values$ten.abbr, function(x) translation.tab[translation.tab$element==x,lang$lang])),
                        selected = store.tab1$ten.values
     )
   })
   
   output$four.values.selector.tab.1 <- renderUI({
     checkboxGroupInput('show_vals_b', NULL,
                        choiceValues= values$four.abbr,
                        choiceNames = unname(sapply(values$four.abbr, function(x) translation.tab[translation.tab$element==x,lang$lang])),
                        selected = store.tab1$four.values
     )
   })
   
   output$two.values.selector.tab.1 <- renderUI({
     checkboxGroupInput('show_vals_c', NULL,
                        choiceValues= values$two.abbr,
                        choiceNames = unname(sapply(values$two.abbr, function(x) translation.tab[translation.tab$element==x,lang$lang])),
                        selected = store.tab1$two.values
     )
   })
   
   
   # Store values selected in the groups
   observeEvent(input$show_vals_a, {
     store.tab1$ten.values <- input$show_vals_a
   })
   
   observeEvent(input$show_vals_b, {
     store.tab1$four.values <- input$show_vals_b
   })
   
   observeEvent(input$show_vals_c, {
     store.tab1$two.values <- input$show_vals_c
   })
   
   # Behavior of group selection links
  observeEvent(input$tenValues.link, {
    
    if(any(!values$ten.abbr %in% store.tab1$ten.values )) {
      store.tab1$ten.values <- values$ten.abbr
      #store.tab1$ten.values.checked <- TRUE 

    } else {
      store.tab1$ten.values <- NULL
      #store.tab1$ten.values.checked <- FALSE
    }
  })
  
    observeEvent(input$fourValues.link, {
      if(any(!values$four.abbr %in% store.tab1$four.values ) ) {
        
        store.tab1$four.values <- values$four.abbr
        #store.tab1$four.values.checked <- TRUE 
        
      } else {
        store.tab1$four.values <- NULL
        #store.tab1$four.values.checked <- FALSE
      }   
    })
    
    # If two values group is clicked then...
    observeEvent(input$twoValues.link, {
      if( any(!values$two.abbr %in% store.tab1$two.values ) ) {
        
        store.tab1$two.values <- values$two.abbr
        #store.tab1$two.values.checked <- TRUE
   
      } else {
        store.tab1$two.values <- NULL
        #store.tab1$two.values.checked <- FALSE 
      }   
    })
    

    
    # Functionality of section links in the SECOND tab ####  
selector.tab.2 <- reactiveValues(countries=c("RU", "BE", "UK", "SE", "ES"),
                                 values="SE")
    
    output$countries.selector.tab.2 <- renderUI({
      checkboxGroupInput('show_countries2', NULL,
                         choices = list.of.countries(),
                         #choiceNames = 
                         #choiceValues = 
                         #levels(tab$cntry), 
                         selected = selector.tab.2$countries
                         )
      
    })
    
    observeEvent(input$show_countries2, {
      selector.tab.2$countries <- input$show_countries2
    })
    
    observeEvent(input$showEast, {
      selector.tab.2$countries <- list.of.countries()[list.of.countries() %in% 
                  c("RU", "HR", "LV", "RO", "LT",  "XK", "PL", "SI", "EE", "SK", "UA", "BG", "HU", "CZ", "RS")]
    })
    
    observeEvent(input$showNorth, {
      selector.tab.2$countries <- list.of.countries()[list.of.countries() %in%  c("DK", "FI", "NO", "SE", "IS")]
    })
    
    observeEvent(input$showWest, {
      selector.tab.2$countries <- list.of.countries()[list.of.countries() %in%  c("BE", "CH", "FR", "GB", "DE","NL", "AT",  "IE")]
    })
    
    observeEvent(input$showSouth, {
      selector.tab.2$countries <- list.of.countries()[list.of.countries() %in%  c("ES",   "GR", "IL", "IT", "CY", "PT",  "TR")]
    })
    
    # observeEvent(input$showEast, {
    #     updateCheckboxGroupInput(
    #       session, 'show_countries2', 
    #       choices = list.of.countries(),
    #       selected = list.of.countries()[list.of.countries() %in% 
    #                                        c("RU", "HR", "LV", "RO", "LT",  "XK", "PL", "SI", "EE", "SK", "UA", "BG", "HU", "CZ")]
    #     ) })
    #     
    # observeEvent(input$showNorth, {
    #     updateCheckboxGroupInput(
    #       session, 'show_countries2', 
    #       choices = list.of.countries(),
    #       selected = list.of.countries()[list.of.countries() %in% 
    #                                        c("DK", "FI", "NO", "SE", "IS")]
    #       
    #     )})
    # 
    # observeEvent(input$showWest, {
    #   updateCheckboxGroupInput(
    #     session, 'show_countries2', 
    #     choices = list.of.countries(),
    #     selected = list.of.countries()[list.of.countries() %in% 
    #                                      c("BE", "CH", "FR", "GB", "DE","NL", "AT",  "IE")]
    #     
    #   )})
    # 
    # observeEvent(input$showSouth, {
    #   updateCheckboxGroupInput(
    #     session, 'show_countries2', 
    #     choices = list.of.countries(),
    #     selected = list.of.countries()[list.of.countries() %in% 
    #                                      c("ES",   "GR", "IL", "IT", "CY", "PT",  "TR")]
    #     
    #   )})
    # 
    # observeEvent(input$reset, {
    #   updateCheckboxGroupInput(
    #     session, 'show_countries2', choices = list.of.countries(), #unique(tab$cntry), #levels(tab$cntry)
    #     selected = NULL
    #   )
    # })
    
    observeEvent(input$reset, {
      selector.tab.2$countries <- NULL
    })
    
    value.names <- reactive({
    # a <- translation.tab$element[translation.tab$element %in% c(values$ten.abbr, values$four.abbr, values$two.abbr)]
    # names(a)<- translation.tab[,lang$lang][translation.tab$element %in% c(values$ten.abbr, values$four.abbr, values$two.abbr)]
    # a
      
      ten <-  values$ten.abbr
      names(ten) <- sapply(values$ten.abbr, function(x) translation.tab[translation.tab$element==x, lang$lang])
      four <- values$four.abbr
      names(four) <- sapply(values$four.abbr, function(x) translation.tab[translation.tab$element==x, lang$lang])
      two <- values$two.abbr
      names(two) <- sapply(values$two.abbr, function(x) translation.tab[translation.tab$element==x, lang$lang])
      
      a<-list(ten, four, two)
      names(a) <- c(translation.tab[,lang$lang][translation.tab$element=="val10.header"], 
                    translation.tab[,lang$lang][translation.tab$element=="val4.header"],
                    translation.tab[,lang$lang][translation.tab$element=="val2.header"]
                    )
      a
      
    })
    
    
  output$value.selector.tab.2 <- renderUI({
    selectInput('show_vals2', textOutput("lang.value.selector", inline=T), #'Choose value:',
                choices=value.names(),
                selected = selector.tab.2$values
                #  choices=levels(tab$variable), selected = levels(tab$variable)[1]
    )
  })
  
  observeEvent(input$show_vals2, {
    selector.tab.2$values <- input$show_vals2
  })
    
  
  # Functioning of UI in TAB 4 ######
  
  output$values.selector.tab4 <- renderUI({
    selectInput("values.selector4", textOutput("lang.values.selector.tab4"),
                choices=value.names(),
                selected = selector.tab.2$values)
    
  })
  
  observeEvent(input$values.selector4, {
    selector.tab.2$values <- input$values.selector4
  })
  
  
  # Preparing data TAB 1 #### 
  #Combine the selected variables into a new data frame
  selectedData1 <- reactive({

  #  tab[tab$variable %in% input$show_vals & tab$cntry==input$show_countries,]
    #tab$variable <- as.factor(tab$variable)
    ta<- tab[tab$cntry==input$show_countries,] 
    #ta$cntry <- rep(input$show_countries, nrow(ta))
    clr<-c(gg_color_hue(10, 70), "#0000ff", "#996633", "#ac3973", "#00cccc", "#FFFF00", "#737373", "#FFFFFF")
    names(clr)<-levels(ta$variable)
    
    all_clr_line <-c(gg_color_hue(11, 50, 72)[1:10], "#9178F5", "#00CD00", "#FF4040", "#2E9DFF", "#FF00FF", "#00CCFF")
    names(all_clr_line)<-levels(ta$variable)
    
    ln_type <- c(rep(c(15,16), 100)[1:10], rep(32, 6))
    names(ln_type)<-levels(ta$variable)
    
    ln_width <- c(rep(1, 10), 2,2,2,2, 3,3)
    names(ln_width)<-levels(ta$variable)

    
    ta<- ta[ta$variable %in% c(input$show_vals_a, input$show_vals_b,input$show_vals_c),]
    
    
    
    clr<-clr[names(clr) %in% unique(ta$variable)]
    ln_type<-ln_type[names(ln_type) %in% unique(ta$variable)]
    clr_line <-all_clr_line[names(all_clr_line) %in% unique(ta$variable)]
    ln_width <-ln_width[names(ln_width) %in% unique(ta$variable)]
    
    list(ta=ta, clr=clr, clr_line=clr_line, ln_type=ln_type, ln_width=ln_width, all_clr_line=all_clr_line)
    
  })
  
  #output$tabl<-renderTable(selectedData1()$ta )
    
    
# Preparing data for the 2 tab ####   
  selectedData2 <- reactive({
    
    # select values
    ta<- tab[tab$variable==input$show_vals2,]
    
    clr<-gg_color_hue(length(unique(ta$cntry)), 60)
    clr<-c(clr[seq(1, length(clr), 3)], clr[seq(2, length(clr), 3)], clr[seq(3, length(clr), 3)])
    clr<-paste(clr, "16", sep="") #add transparency
    names(clr)<-unique(ta$cntry)
    
    
    clr_line <-gg_color_hue(length(unique(ta$cntry)), 50)
    clr_line<-c(clr_line[seq(1, length(clr_line), 3)], clr_line[seq(2, length(clr_line), 3)], clr_line[seq(3, length(clr_line), 3)])
    names(clr_line)<-unique(ta$cntry)
    
    ln_type <- rep(c("solid","93","31"), 100)[1:length(unique(ta$cntry))]
    names(ln_type)<-unique(ta$cntry)
    
    
    # select countries
    ta<- ta[ta$cntry %in% input$show_countries2,]
    
    ta$cntry.lab <- sapply(ta$cntry, function(x) translation.countries[translation.countries==x, lang$lang])
    
    shapes <- substr(as.character(unique(ta$cntry.lab)),1,1)[1:length(unique(ta$cntry.lab))]
    names(shapes)<-unique(ta$cntry)
    
    
    clr<-clr[names(clr) %in% unique(ta$cntry)]
    ln_type<-ln_type[names(ln_type) %in% unique(ta$cntry)]
    clr_line <-clr_line[names(clr_line) %in% unique(ta$cntry)]
    shapes <-shapes[names(shapes) %in% unique(ta$cntry)]
    
    
    
    list(ta=ta, clr=clr, clr_line=clr_line, ln_type=ln_type, shapes=shapes)
    
  })
  
    # Preparing data for the third tab #### 
  selectedData3 <- reactive({
    
    d<-tab[tab$essround==input$round & (tab$variable %in% values$two.abbr ),]
                                          
    d1 <- list( means=dcast(d, "cntry + essround ~ variable", value.var="value"),
          lower=dcast(d, "cntry + essround ~ variable", value.var="lower"),
          upper=dcast(d, "cntry + essround ~ variable", value.var="upper")
        )
    
    d1$means$cntry.lab <- sapply(d1$means$cntry, function(x) translation.countries[translation.countries==x, lang$lang])
    d1$lower$cntry.lab <- sapply(d1$lower$cntry, function(x) translation.countries[translation.countries==x, lang$lang])
    d1$upper$cntry.lab <- sapply(d1$upper$cntry, function(x) translation.countries[translation.countries==x, lang$lang])
    
    d1
    
  })
  
  # Preparing data for TAB 4 #####
  # 
  # selectedData4 <- reactive({
  #   
  #   europe$cntry <- sapply(europe$ID, 
  #                           function(x) {
  #                             if(any(translation.countries$English==x)) {
  #                                 translation.countries[translation.countries$English==x, "cntry"]
  #                                 } else {
  #                                   NA
  #                                 }
  #                             },  USE.NAMES =F  
  #                           )
  #   
  #   dt<-merge(europe, tab[tab$essround==input$round.tab4 & tab$variable==input$values.selector4,
  #                          c("cntry","value")], by="cntry", all.x=T)
  # 
  #   dt
  # })
  
 
  output$downloadButton <-  downloadHandler(
      filename = "data.csv",
      content = function(file) {
        write.csv(selectedData1()$ta, file, row.names = FALSE)
      }
    )
  
  

  # Plot 1 ####
  output$plot1 <- renderPlot({
    if(nrow(selectedData1()$ta)>0) {
      d<-selectedData1()$ta
      
      #d$variable <- unname(sapply(as.character(d$variable), function(x) translation.tab[translation.tab==x, lang$lang]))
      d$variable.lab <- factor(d$variable, 
             levels=levels(d$variable), 
             labels=sapply(levels(d$variable), function(x) translation.tab[translation.tab==x, lang$lang])
             )
      
    h <- ggplot(d, aes(essround,y = value))
    h + geom_ribbon(aes(ymin = lower, ymax = upper, fill=variable),  alpha=.2) + 
      geom_line(aes(color=variable, size=variable))+
      coord_cartesian(xlim = c(min(d$essround), max(d$essround) + 3)) +
      geom_point(aes(color=variable, shape=variable), size=3) +
      
      geom_label_repel(
        data = subset(d, essround==max(essround)),
        aes(label = str_wrap(variable.lab, 20), fill=variable), segment.colour = "grey60", col="black", nudge_x = 1.1, size= 5, alpha=.8)+
      
      scale_x_continuous(breaks=unique(d$essround), minor_breaks =F) +
      labs(x =     translation.tab[translation.tab$element=="x.round", lang$lang],
           y =     translation.tab[translation.tab$element=="y.value", lang$lang],
           caption=translation.tab[translation.tab$element=="copyright.caption", lang$lang], 
           title=  translation.countries[translation.countries$cntry==input$show_countries, lang$lang], 
           fill="", shape="", color="", size="")+
      
      scale_fill_manual(values=selectedData1()$clr_line)+
      scale_color_manual(values=selectedData1()$clr_line)+
      scale_shape_manual(values=selectedData1()$ln_type)+
      scale_size_manual(values=selectedData1()$ln_width)+
      
      theme_minimal()+theme(panel.grid.minor = element_blank(),
                            axis.line.x = element_line(color="black", size = .5),
                            axis.line.y = element_line(color="black", size = 0.5),
                            axis.title.x = element_text(face="bold", size=14),
                            axis.title.y = element_text(face="bold", size=14),
                            axis.text.x = element_text(size = 12),
                            #legend.text = element_text(size = 12),
                            legend.position = "none",
                            plot.title= element_text(face="bold", size=16))
    } else {
      qplot(1, 1, geom="text", label=translation.tab[translation.tab$element=="choose.some.values",lang$lang])+xlim(0,2)+
        geom_segment(aes(x=0.6, y=1, xend=0.2, yend=1), arrow=arrow(), size=1)+  theme_void()
    }
    
  })
  
  
  
  
  # Plot 2 ####  
  output$plot2 <- renderPlot({
    
    tab2<-selectedData2()$ta
    if(nrow(tab2)>0) {
      #print(isolate(tab2))
    last_rounds <- aggregate(tab2$essround, list(tab2$cntry.lab),  max)
    dat.labs <- tab2[paste(tab2$cntry.lab, tab2$essround) %in% paste(last_rounds$Group.1, last_rounds$x), ]
    #print(isolate(dat.labs))
    
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

    } else {
      qplot(1, 1, geom="text", label=translation.tab[translation.tab$element=="choose.some.values",lang$lang])+xlim(0,2)+
        geom_segment(aes(x=0.6, y=1, xend=0.2, yend=1), arrow=arrow(), size=1)+  theme_void()
    }
    
  })

  
  
  # Plot 3 map #### 
  vals <- reactiveValues(
    hovered = 0,
    clicked = 0
  )
  
  output$plot3 <- renderPlot({
    d<- selectedData3()
    # name_x<-names(d$means)[3]
    # name_y<-names(d$means)[4]
    # names(d$means)[3:4]<-c("x", "y")

  
  g<-ggplot(d$means, aes(Conservation_Openness,
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
  
  
  # Behavior on hover: if hover on point, then draw rects with CIs
  if(vals$hovered!=0) {

    upper<-d$upper[d$upper$cntry==as.character(vals$hovered),values$two.abbr]
    lower<-d$lower[d$lower$cntry==as.character(vals$hovered),values$two.abbr]

    g<-g+geom_rect(aes(xmin=-Inf, 
                       xmax=Inf,
                       ymin=lower[, values$two.abbr[2]],
                       ymax=upper[, values$two.abbr[2]]
                       ), 
                  col=NA, 
                  fill="#00000001"
                  )+
         geom_rect(aes(xmin=lower[, values$two.abbr[1]],
                       xmax=upper[, values$two.abbr[1]],
                       ymin=-Inf,
                       ymax=Inf
                       ), 
                   col=NA, fill="#00000001"
                   )
  }
  
  if (vals$clicked!=0 ) {
  
    #rect_coord<-d$means[d$means$cntry==as.character(vals$clicked),3:4]

    
    d.r<-tab[tab$cntry==as.character(vals$clicked) & (tab$variable %in% values$two.abbr), ]
    
    if(sum(d.r$essround==input$round)==0)  { 
      #vals$clicked=0 
      showNotification(paste("Data for", input$round, "year in ", vals$clicked, "is not available."))
    } else {
    
    d.r<-dcast(d.r, "cntry + essround ~ variable", value.var="value")  
    
    

    g<- g + geom_point(data = d.r[d.r$essround==input$round,],        
                       aes(x = d.r[d.r$essround==input$round,values$two.abbr[1]], 
                           y = d.r[d.r$essround==input$round,values$two.abbr[2]]#,
                           #label=NULL
                             ), 
                       size=8, colour="blue", shape=1, fill="red") + 
      
      # big clicked country label
      # geom_text_repel(data = d.r[d.r$essround==input$round,],        
      #           aes(x = d.r[d.r$essround==input$round,3], 
      #               y = d.r[d.r$essround==input$round,4],
      #               label=translation.countries[translation.countries==cntry, lang$lang]), 
      #           size=8, colour="green", nudge_x=1, nudge_y=1) +
            
           #  arrow line 
            geom_path(data = d.r, aes(x = d.r[,3], y = d.r[,4]), 
                      arrow=arrow(type="open", angle=20, length=unit(14, "points")),
                      size=1.5, colour="tomato1")+
           
           # year of survey labels
            geom_text_repel(data = d.r, 
                      aes(x = d.r[,3], y = d.r[,4], label=d.r$essround),
                      colour="tomato3", size=3)
    }
  } 
    g  +  theme_minimal()+
      theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5),
            axis.title.x = element_text(face="bold", size=14),
            axis.title.y = element_text(face="bold", size=14),
            panel.grid = element_blank(),
            legend.position="none",
            plot.title= element_text(face="bold", size=16))+
      coord_fixed(ratio = 1)+
      labs(title=input$round,
           x=translation.tab[translation.tab=="Conservation_Openness", lang$lang],
           y=translation.tab[translation.tab=="Self_Enhancement_Self_Transcendence", lang$lang],
           caption=translation.tab[translation.tab$element=="copyright.caption", lang$lang], 
           fill="", shape="", color="")
  
    
  })
  
  
 
  
#  output$plot_hoverinfo <- renderPrint({
#    vals$hovered
    
   #  print(paste("Hover (throttled):"))
   # input$plot_hover
#  })
  
  observeEvent(input$plot_click, {
  
  d<-selectedData3()$means
  
  res <- nearPoints(d, xvar=names(d)[3], yvar=names(d)[4], coordinfo=input$plot_click, maxpoint=1, allRows=F)

  vals$clicked<-ifelse(nrow(res)==1, as.character(res$cntry), 0)
  
  })
  
  
  observeEvent(input$plot_hover, {
    
    d<-selectedData3()$means
    
    res <- nearPoints(d, xvar=names(d)[3], yvar=names(d)[4], coordinfo=input$plot_hover, maxpoint=1, allRows=F)
    
    vals$hovered<-ifelse(nrow(res)==1, as.character(res$cntry), 0)
    rm(d)
  })
  
  
  # Plot 4 geo map ######
  output$plot4 <- renderPlot({
   
  ggplot(selectedData4()) + 
    geom_sf(aes(fill=value), color="gray60")+
    scale_fill_gradient(  low="white", high=selectedData1()$all_clr_line[input$values.selector4] )+
      #geom_text(aes(label=ID))+
    theme_void()+labs(fill=  str_wrap( translation.tab[translation.tab==input$values.selector4, lang$lang], 20),
                      caption=translation.tab[translation.tab$element=="copyright.caption", lang$lang])+
    theme(panel.grid = element_line(colour = "transparent"),
          legend.title = element_text(size = 12),
          plot.background = element_rect(fill="transparent"))
  })
  
}) # close shinyServer
