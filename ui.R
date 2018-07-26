
#load("data/tab.pspweight.R")
#load("data/tab.pspweight_rus.R")
#load("data/tab.dweight.R")
#load("data/tb.Rdata")

library("markdown")

fluidPage(tags$head(
  #@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700'); #move it to style when needed
  tags$style(HTML("
                  
                  
                  h1 {
                  font-family: 'Tahoma', cursive;
                  font-weight: 900;
                  line-height: 1.1;
                  color: #525050;
                  padding: 0 0 0 0;
                  }

                  a.action-button {
                   font-weight: 500;
                   color: #000;
                  }

                div.shiny-input-checkboxgroup {
                    margin-top: 0px;
                    padding-top: 0px;
                }
              
                div.lang {
font-size: 9pt;
float: right;
padding: 0px 0;
height: 40px;
width: 100px;
                }
                  
    "))
  ),
  #headerPanel('Trends in basic values in European countries'),
  
  uiOutput("language.selector", class="lang"),
  headerPanel(textOutput("lang.general.title", inline=T)),
  
   sidebarLayout(
  
     #Sidebar ####
   sidebarPanel(

    #...Conditional tab 1 by country #####
    conditionalPanel(
      'input.dataset === "by country"',
      uiOutput("country.selector.tab.1"),
      
    actionLink("tenValues.link", label=textOutput("lang.val10.header")),
    uiOutput("ten.values.selector.tab.1"),

    actionLink("fourValues.link",  label=textOutput("lang.val4.header")),
    uiOutput("four.values.selector.tab.1"),
    
    # checkboxGroupInput('show_vals_b', NA,
    #                    choiceValues=c("Openness",     "Conserv",      "Self_Trans",   "Self_Enhance"),
    #                    choiceNames=list(textOutput("lang.Openness", inline=T),
    #                                     textOutput("lang.Conserv", inline=T),
    #                                     textOutput("lang.Self_Trans", inline=T),
    #                                     textOutput("lang.Self_Enhance", inline=T)
    #                                     ),
    #                   # levels(tab$variable)[11:14],
    #                    selected = NULL),

    actionLink("twoValues.link",  label=textOutput("lang.val2.header")),
    uiOutput("two.values.selector.tab.1")
    
    # checkboxGroupInput('show_vals_c', NA,
    #                    choiceValues=c("Conservation_Openness", "Self_Enhancement_Self_Transcendence"),
    #                    choiceNames=list(textOutput("lang.Conservation_Openness", inline=T),
    #                                     textOutput("lang.Self_Enhancement_Self_Transcendence", inline=T)),
    #                    #levels(tab$variable)[15:16],
    #                    selected = c("Conservation_Openness", "Self_Enhancement_Self_Transcendence")
    # )
  ), #conditionalPanel

  #...Conditional tab 2 by country #####
  conditionalPanel(  # TAB 2
    'input.dataset === "by value"',

uiOutput("value.selector.tab.2"),

    #selectInput('show_countries2', 'Choose countries to show:',multiple=T,
    #                        levels(tab$cntry), selected = levels(tab$cntry)[c(1,5,10)])

    #tags$html(strong('Choose countries to show:'), br()),
    tags$html(strong(textOutput("lang.multiple.countries.selector", inline=T)), br()),

    actionLink("showNorth", textOutput("lang.north", inline=T) #"North"
               ),"|",
    actionLink("showEast", textOutput("lang.east", inline=T)  #"East"
               ),"|",
    actionLink("showWest", textOutput("lang.west", inline=T) # "West"
               ),"|",
    actionLink("showSouth", textOutput("lang.south", inline=T) #"South"
               ),"|",

    actionLink("reset", "", icon = icon("remove-circle", lib = "glyphicon")),
    uiOutput("countries.selector.tab.2")


  ),

  #...Conditional tab 3 value map #####
  conditionalPanel(
    'input.dataset === "value map"',
  sliderInput("round", textOutput("lang.year.slider", inline=T), #"Year of survey",

              #min(tab$essround),max(tab$essround), 
              2002,2016,
              value=2006,
              step=2, round=TRUE, animate=T, sep=""),
  #helpText("Put cursor over the points to see confidence intervals and click to see the change.")
  helpText(textOutput("lang.hint.year.slider", inline=T))
  ),

#...Conditional tab 4 geo map Europe #####
conditionalPanel(
  'input.dataset === "geo map"',
  sliderInput("round.tab4", textOutput("lang.year.slider2", inline=T), 2002, 2016, value=2006, step=2, round=T, animate=T, sep="" ),
  uiOutput("values.selector.tab4")
  
)

 ),

# Main panel #####
  mainPanel(
    tabsetPanel(
      id = 'dataset',

      tabPanel(value='by country', title=textOutput('lang.tab1.name', inline=T),
        plotOutput('plot1')
        ),

      tabPanel(value='by value', title=textOutput('lang.tab2.name', inline=T),
         plotOutput('plot2')
         ),

      tabPanel(value='value map', title=textOutput('lang.tab3.name', inline=T),
         plotOutput('plot3', height = "600px", click = clickOpts(id = "plot_click"),
                        hover=hoverOpts(id="plot_hover", delay =1, nullOutside=TRUE)
                    )
         ),
      tabPanel(value='geo map', title=textOutput('lang.geomap.tab', inline=T),
          plotOutput('plot4')     
               ),
      
      
      tabPanel(value="credits", title="？",
               includeMarkdown("CREDITS.md"))
      )
  )
) #sidebarLayout

) #fluidPage
