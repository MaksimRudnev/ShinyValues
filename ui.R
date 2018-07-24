
#load("data/tab.pspweight.R")
#load("data/tab.pspweight_rus.R")
load("data/tab.dweight.R")

fluidPage(tags$head(
  #@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700'); #move it to style when needed
  tags$style(HTML("
                  
                  
                  h1 {
                  font-family: 'Tahoma', cursive;
                  font-weight: 900;
                  line-height: 1.1;
                  color: #525050;
                  }

                  a.action-button {
                   font-weight: 500;
                   color: #000;
                  }

                div.shiny-input-checkboxgroup {
                    margin-top: 0px;
                    padding-top: 0px;
}
                  
                  "))
  ),
  headerPanel('Trends in basic values in European countries'),
sidebarLayout(
 
  sidebarPanel(
    conditionalPanel(  #TAB 1
      'input.dataset === "by country"',
      selectInput('show_countries', 'Choose country to show:',
                  levels(tab$cntry), selected = levels(tab$cntry)[1]),
      actionLink("tenValues", "Basic values:"), 
    checkboxGroupInput('show_vals_a', NA,
                       levels(tab$variable)[1:10], selected = NULL),
    
    actionLink("fourValues", "Higher-order values:"), 
    checkboxGroupInput('show_vals_b', NA,
                       levels(tab$variable)[11:14], selected = NULL),
    
    actionLink("twoValues", 'Higher-order value dimensions:'),
    checkboxGroupInput('show_vals_c', NA,
                       levels(tab$variable)[15:16], selected = levels(tab$variable)[c(15,16)])
    
    
    #numericInput('clusters', 'Cluster count', 3,
    #             min = 1, max = 9)
  ),
  
  conditionalPanel(  # TAB 2
    'input.dataset === "by value"',
      #helpText('Click the column header to sort a column.'),
    
    selectInput('show_vals2', 'Choose value:',
                       levels(tab$variable), selected = levels(tab$variable)[1]),
    
    #selectInput('show_countries2', 'Choose countries to show:',multiple=T,
    #                        levels(tab$cntry), selected = levels(tab$cntry)[c(1,5,10)])
    
    tags$html(strong('Choose countries to show:'), br()),
    
    actionLink("showNorth", "North"),"|",
    actionLink("showEast", "East"),"|",
    actionLink("showWest", "West"),"|",
    actionLink("showSouth", "South"),"|",
    
    actionLink("reset", "", icon = icon("remove-circle", lib = "glyphicon")),
    checkboxGroupInput('show_countries2', NA,
                levels(tab$cntry), selected = levels(tab$cntry)[c(2,19,22,31)])
    
    
  ),
  
  conditionalPanel(
    'input.dataset === "value map"',  
  sliderInput("round", "Year of survey", min(tab$essround),max(tab$essround), value=2006,
              step=2, round=TRUE, animate=T, sep=""),
  helpText("Put cursor over the points to see confidence intervals and click to see the change.")
  
  
  )),


  mainPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel('by country', 
                       
    plotOutput('plot1'),
    tags$html(br(),em("Created by", a(href="http://www.maksimrudnev.com", "Maksim Rudnev"), 
                 "with ShinyApps, using data from", a(href="http://www.europeansocialsurvey.org/data/", "European Social Survey."),
                 "Value indices are based on,", a("Schwartz theory of basic values.", href="https://pdfs.semanticscholar.org/dc49/e27d0ed890cd3ed2e80ca0b0107207f12a64.pdf"), "They were computed following ", a(href="http://essedunet.nsd.uib.no/cms/topics/1/", "ESS EduNet"),
                 "instructions, and weighted with design weight" #, a("pspweight.", href="http://www.europeansocialsurvey.org/methodology/ess_methodology/data_processing_archiving/weighting.html")
    )
    )
    #, tableOutput("tabl")
    ),
    
    tabPanel('by value',
             plotOutput('plot2'),
             tags$html(br(),em("Created by", a(href="http://www.maksimrudnev.com", "Maksim Rudnev"), 
                          "with ShinyApps, using data from", a(href="http://www.europeansocialsurvey.org/data/", "European Social Survey."),
                          "Value indices are based on", a("Schwartz theory of basic values.", href="https://pdfs.semanticscholar.org/dc49/e27d0ed890cd3ed2e80ca0b0107207f12a64.pdf"), "They were computed following ", a(href="http://essedunet.nsd.uib.no/cms/topics/1/", "ESS EduNet"),
                          "instructions, and weighted with  design weight" #", a("pspweight.", href="http://www.europeansocialsurvey.org/methodology/ess_methodology/data_processing_archiving/weighting.html")
                          )
             )),
    tabPanel('value map',
             plotOutput('plot3', height = "600px", click = clickOpts(id = "plot_click"),
                        hover=hoverOpts(id="plot_hover", delay =1, nullOutside=TRUE))
                        ,
             tags$html(br(),em("Created by", a(href="http://www.maksimrudnev.com", "Maksim Rudnev"), 
                          "with ShinyApps, using data from", a(href="http://www.europeansocialsurvey.org/data/", "European Social Survey."),
                          "Value indices are based on", a("Schwartz theory of basic values.", href="https://pdfs.semanticscholar.org/dc49/e27d0ed890cd3ed2e80ca0b0107207f12a64.pdf"), "They were computed following ", 
                          a(href="http://essedunet.nsd.uib.no/cms/topics/1/", "ESS EduNet"),
                          "instructions, and weighted with  design weight" #", 
                          #a("pspweight.", href="http://www.europeansocialsurvey.org/methodology/ess_methodology/data_processing_archiving/weighting.html")
                          )
             )
                        )
             
    
    
    
    )
  )
))
