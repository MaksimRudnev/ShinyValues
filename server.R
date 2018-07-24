
#setwd("~/Dropbox/STAT/R-functions/ShinyValues/Basic_Values")
#load("data/tab.pspweight.R")
load("data/tab.dweight.R")
#load("data/tab.pspweight_rus.R")
library("ggplot2")
library("reshape2")
library("ggrepel")
library("stringr")

shinyServer(
function(input, output, session) {
  
  gg_color_hue <- function(n, brightness=70, beginning=0) {
    hues = seq(beginning, beginning+360-15, length = n )
    hcl(h = hues, l = brightness, c = 200)
  }
  #barplot(1:10, col = gg_color_hue(10, 50, 72))
  #plot(rep(1,10), col=hcl(h = seq(15, 375, length = n + 1), l = 70, c = 200), pch=19, cex=10)
 
  observeEvent(input$reset, {
    updateCheckboxGroupInput(
      session, 'show_countries2', choices = levels(tab$cntry),
      selected = NULL
    )
  })
  
# Functionality of section links in the first tab ####  
  store <- reactiveValues (
    ten_values_checked = FALSE,
    four_values_checked = FALSE,
    two_values_checked = TRUE
  )
  
  observeEvent(input$tenValues, {
    if(!store$ten_values_checked) {
    updateCheckboxGroupInput(
      session, 'show_vals_a', choices = levels(tab$variable)[1:10],
      selected = levels(tab$variable)[1:10]
    )
      store$ten_values_checked=TRUE 
      
      #uncheck four values
      updateCheckboxGroupInput(
        session, 'show_vals_b', choices = levels(tab$variable)[11:14],
        selected = NULL
      )
      store$four_values_checked=FALSE
      
      #uncheck two values
      updateCheckboxGroupInput(
        session, 'show_vals_c', choices = levels(tab$variable)[15:16],
        selected = NULL
      )
      store$two_values_checked=FALSE
      
    } else {
      updateCheckboxGroupInput(
        session, 'show_vals_a', choices = levels(tab$variable)[1:10],
        selected = NULL
      )
      store$ten_values_checked=FALSE
    }
  })
    
    observeEvent(input$fourValues, {
      if(!store$four_values_checked) {
        updateCheckboxGroupInput(
          session, 'show_vals_b', choices = levels(tab$variable)[11:14],
          selected = levels(tab$variable)[11:14]
        )
        store$four_values_checked=TRUE 
        
        #uncheck ten values
        updateCheckboxGroupInput(
          session, 'show_vals_a', choices = levels(tab$variable)[1:10],
          selected = NULL
        )
        store$ten_values_checked=FALSE
        
        #uncheck two values
        updateCheckboxGroupInput(
          session, 'show_vals_c', choices = levels(tab$variable)[15:16],
          selected = NULL
        )
        store$two_values_checked=FALSE
        
      } else {
        updateCheckboxGroupInput(
          session, 'show_vals_b', choices = levels(tab$variable)[11:14],
          selected = NULL
        )
        store$four_values_checked=FALSE
      }   
    
   
  })
    
    # If two values group is clicked then...
    observeEvent(input$twoValues, {
      if(store$two_values_checked) {
        
        #uncheck two values
        updateCheckboxGroupInput(
          session, 'show_vals_c', choices = levels(tab$variable)[15:16],
          selected = NULL
        )
        store$two_values_checked=FALSE
        
      } else {
        
        #check the two values
        updateCheckboxGroupInput(
          session, 'show_vals_c', choices = levels(tab$variable)[15:16],
          selected = levels(tab$variable)[15:16]
        )
        store$two_values_checked=TRUE 
        
        #uncheck ten values
        updateCheckboxGroupInput(
          session, 'show_vals_a', choices = levels(tab$variable)[1:10],
          selected = NULL
        )
        store$ten_values_checked=FALSE
        
        #uncheck four values
        updateCheckboxGroupInput(
          session, 'show_vals_b', choices = levels(tab$variable)[11:14],
          selected = NULL
        )
        store$four_values_checked=FALSE
     
      }   
      
      
    })
    
    # Functionality of section links in the SECOND tab ####  

    
    observeEvent(input$showEast, {
        updateCheckboxGroupInput(
          session, 'show_countries2', choices = levels(tab$cntry),
          selected = levels(tab$cntry)[c(3,4,6,8, 13, 18,21,23, 24, 25, 30)]
        ) })
        
    observeEvent(input$showNorth, {
        updateCheckboxGroupInput(
          session, 'show_countries2', choices = levels(tab$cntry),
          selected = levels(tab$cntry)[c(7,9,14,20,27)]
        )})
    
    observeEvent(input$showWest, {
      updateCheckboxGroupInput(
        session, 'show_countries2', choices = levels(tab$cntry),
        selected = levels(tab$cntry)[c(1,2,10,11,15,19, 28,31)]
      )})
    
    observeEvent(input$showSouth, {
      updateCheckboxGroupInput(
        session, 'show_countries2', choices = levels(tab$cntry),
        selected = levels(tab$cntry)[c(5, 12, 16,17,22,26,29)]
      )})
    
  
  # Preparing data for the first tab #### 
  #Combine the selected variables into a new data frame
  selectedData1 <- reactive({

  #  tab[tab$variable %in% input$show_vals & tab$cntry==input$show_countries,]
    
    ta<- tab[tab$cntry==input$show_countries,]  
    clr<-c(gg_color_hue(10, 70), "#0000ff", "#996633", "#ac3973", "#00cccc", "#FFFF00", "#737373", "#FFFFFF")
    names(clr)<-levels(ta$variable)
    
    clr_line <-c(gg_color_hue(11, 50, 72)[1:10], "#9178F5", "#00CD00", "#FF4040", "#2E9DFF", "#FF00FF", "#00CCFF")
    names(clr_line)<-levels(ta$variable)
    
    ln_type <- c(rep(c(15,16), 100)[1:10], rep(32, 6))
    names(ln_type)<-levels(ta$variable)
    
    ln_width <- c(rep(1, 10), 2,2,2,2, 3,3)
    names(ln_width)<-levels(ta$variable)

    
    ta<- ta[ta$variable %in% c(input$show_vals_a, input$show_vals_b,input$show_vals_c),]
    
    
    
    clr<-clr[names(clr) %in% unique(ta$variable)]
    ln_type<-ln_type[names(ln_type) %in% unique(ta$variable)]
    clr_line <-clr_line[names(clr_line) %in% unique(ta$variable)]
    ln_width <-ln_width[names(ln_width) %in% unique(ta$variable)]
    
    list(ta=ta, clr=clr, clr_line=clr_line, ln_type=ln_type, ln_width=ln_width)
    
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
    
    
    shapes <- substr(as.character(unique(ta$cntry)),1,1)[1:length(unique(ta$cntry))]
    names(shapes)<-unique(ta$cntry)
    
    # select countries
    ta<- ta[ta$cntry %in% input$show_countries2,]
    
    
    clr<-clr[names(clr) %in% unique(ta$cntry)]
    ln_type<-ln_type[names(ln_type) %in% unique(ta$cntry)]
    clr_line <-clr_line[names(clr_line) %in% unique(ta$cntry)]
    shapes <-shapes[names(shapes) %in% unique(ta$cntry)]
    
    
    list(ta=ta, clr=clr, clr_line=clr_line, ln_type=ln_type, shapes=shapes)
    
  })
  
    # Preparing data for the third tab #### 
  selectedData3 <- reactive({
    
    d<-tab[tab$essround==input$round & (tab$variable==levels(tab$variable)[15]|tab$variable==levels(tab$variable)[16]), ]
    list(means=dcast(d, "cntry + essround ~ variable", value.var="value"),
    lower=dcast(d, "cntry + essround ~ variable", value.var="lower"),
    upper=dcast(d, "cntry + essround ~ variable", value.var="upper"))
    
    
  })
  

  # Plot 1 ####
  output$plot1 <- renderPlot({
    if(nrow(selectedData1()$ta)>0) {
    
    h <- ggplot(selectedData1()$ta, aes(essround,y = value))
    h + geom_ribbon(aes(ymin = lower, ymax = upper, fill=variable),  alpha=.2) + 
      geom_line(aes(color=variable, size=variable))+
      coord_cartesian(xlim = c(min(selectedData1()$ta$essround), max(selectedData1()$ta$essround) + 3)) +
      geom_point(aes(color=variable, shape=variable), size=3) +
      
      geom_label_repel(
        data = subset(selectedData1()$ta, essround==max(essround)),
        aes(label = str_wrap(variable, 20), fill=variable), segment.colour = "grey60", col="black", nudge_x = 1.1, size= 5, alpha=.8)+
      
      scale_x_continuous(breaks=unique(selectedData1()$ta$essround), minor_breaks =F) +
      labs(x="ESS round", caption="(c) ESS/Rudnev", title=input$show_countries, 
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
      qplot(1, 1, geom="text", label="Choose some values")+xlim(0,2)+
        geom_segment(aes(x=0.6, y=1, xend=0.2, yend=1), arrow=arrow(), size=1)+  theme_void()
    }
    
  })
  
  # Plot 2 ####  
  output$plot2 <- renderPlot({
    
    tab2<-selectedData2()$ta
    if(nrow(tab2)>0) {
    last_rounds <- aggregate(tab2$essround, list(tab2$cntry),  max)
    
    
    h <- ggplot(tab2, aes(essround, y = value, fill=cntry))
    h + geom_ribbon(aes(ymin = lower, ymax = upper)) + 
      geom_line(aes(color=cntry #, linetype=cntry
                    ), size=2)+
      geom_point(aes(color=cntry), shape=21, size=5, fill="white")+
      geom_point(aes(color=cntry, shape=cntry), size=3)+
      coord_cartesian(xlim = c(min(tab2$essround), max(tab2$essround) + 3)) +
      geom_label_repel(
        data = tab2[paste(tab2$cntry, tab2$essround) %in% paste(last_rounds$Group.1, last_rounds$x), ],
        aes(label = str_wrap(cntry, 20), color=cntry),nudge_x = 1.1, size= 5, fill="white",
        min.segment.length = unit(15, "points"))+ 
      
      
      scale_fill_manual(values=selectedData2()$clr)+
      scale_color_manual(values=selectedData2()$clr_line)+
      #scale_linetype_manual(values=selectedData2()$ln_type)+
      scale_shape_manual(values=selectedData2()$shapes)+
      
      scale_x_continuous(breaks=unique(selectedData2()$ta$essround), minor_breaks =F)+
      labs(y = input$show_vals2, x="ESS round", caption="(c) ESS/Rudnev", fill="", shape="", color="")+
      
      theme_minimal()+
      theme(panel.grid.minor = element_blank(),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            axis.title.x = element_text(face="bold", size=14),
            axis.title.y = element_text(face="bold", size=14),
            #legend.text = element_text(size = 12),
            legend.position = "none",
            axis.text.x = element_text(size = 12)
            )

    } else {
      qplot(1, 1, geom="text", label="Choose some countries")+xlim(0,2)+
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
    name_x<-names(d$means)[3]
    name_y<-names(d$means)[4]
    names(d$means)[3:4]<-c("x", "y")

  #print(d)
  g<-ggplot(d$means, aes(x,
                         y,
                   #color=cntry, 
                   label=cntry), cex=1) +
    geom_point(shape=19, size=7, alpha=.6)+
    geom_text_repel(size=5, point.padding = unit(7, "points"),
                    box.padding= unit(5, "points"),#nudge_x = .1,
                    min.segment.length = unit(16, "points"),
                    segment.colour = "#00000066")+ #common labels
    
#    annotate("rect", xmin = -Inf, xmax = +Inf,   ymin = 0.42, ymax = 0.50,   fill = "gray", alpha=0.3) +
#    annotate("rect", xmin = -0.55, xmax = -0.47,   ymin = -Inf, ymax = +Inf,   fill = "gray", alpha=0.3)+
    scale_x_continuous(breaks = seq(-1.5, 0.5, by = 0.5), limits=c(-1.5, 0.5))+
    scale_y_continuous(breaks = seq(0, 2, by = 0.5), limits=c(0, 2))+
    theme_minimal()+
    theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5),
          axis.title.x = element_text(face="bold", size=14),
          axis.title.y = element_text(face="bold", size=14),
          panel.grid = element_blank(),
          legend.position="none",
          plot.title= element_text(face="bold", size=16))+
    coord_fixed(ratio = 1)+
    labs(title=input$round,
         x=name_x, y=name_y,
         caption="(c) ESS/Rudnev", fill="", shape="", color="")
  
  
  if(vals$hovered!=0  ) {
    d<- selectedData3()
    names(d$lower)[3:4]<-c("x", "y")
    names(d$upper)[3:4]<-c("x", "y")
    upper<-d$upper[d$upper$cntry==as.character(vals$hovered),3:4]
    lower<-d$lower[d$lower$cntry==as.character(vals$hovered),3:4]
    # showNotification(    paste(lower$`Self-Enhancement - Self-Transcendence`,
    #                            upper$`Self-Enhancement - Self-Transcendence`,
    #                            vals$hovered))
   # g+geom_rect(aes(xmin=-1,xmax=0,ymin=0,ymax=1), fill="black")
    
    g<-g+geom_rect(aes(xmin=-Inf, xmax=Inf,
                                   ymin=lower$y,
                                   ymax=upper$y
    ), col=NA, fill="#00000001")+
     geom_rect(aes(xmin=lower$x,
                   xmax=upper$x,
                                  ymin=-Inf,
                                  ymax=Inf
     ), col=NA, fill="#00000001")
    

  }
  
  if (vals$clicked!=0  ) {
  
    #rect_coord<-d$means[d$means$cntry==as.character(vals$clicked),3:4]

    
    d.r<-tab[tab$cntry==as.character(vals$clicked) & (tab$variable==levels(tab$variable)[15]|
                                                      tab$variable==levels(tab$variable)[16]), ]
    
    if(sum(d.r$essround==input$round)==0)  { 
      #vals$clicked=0 
      showNotification(paste("Data for", input$round, "year in ", vals$clicked, "is not available."))
    } else {
    
    d.r<-dcast(d.r, "cntry + essround ~ variable", value.var="value")  
    
    #Get color of the currect country
    clr<-ggplot_build(g)$data[[2]][ggplot_build(g)$data[[2]]$label==as.character(vals$clicked),"colour"]
    
    #Set alpha for existing graphics to .9

    g<- g + #annotate("rect", xmin=-1.5, xmax=0.5, ymin=0, ymax=2,alpha = .6, fill="white")+
     
      #geom_point(fill="00000001")+
      geom_point(data = d.r[d.r$essround==input$round,],        # how arrow line looks like
                 aes(x = d.r[d.r$essround==input$round,3], 
                     y = d.r[d.r$essround==input$round,4]), size=8, colour="tomato1") + 
      
      geom_path(data = d.r, aes(x = d.r[,3], y = d.r[,4]), 
              arrow=arrow(type="open", angle=20, length=unit(14, "points")),
              size=1.7, colour="tomato1")+
     geom_text_repel(data = d.r, aes(x = d.r[,3], y = d.r[,4], label=d.r$essround), colour="tomato3", size=3)
    }
  } 
    g
  
    
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
  
  
})
