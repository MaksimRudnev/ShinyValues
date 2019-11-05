# Basic values in Europe

A Shiny App that demontrates means of basic values measured with [Schwartz (2001)](http://journals.sagepub.com/doi/abs/10.1177/0022022101032005001) scale in [European Social Survey](https://europeansocialsurvey.org).

To run, type in the RStudio console: `shiny::runGitHub("ShinyValues", "maksimrudnev")`
You might need to install some of the following packages to enable the app properly running on your device: *devtools, shiny, ggplot2, ggrepel, reshape2, stringr, maps, sf, readr*.

This app was created with [ShinyApps](https://shinyapps.io/), using data from 
 [European Social Survey](http://www.europeansocialsurvey.org/data/)

Value indices are based on [Schwartz theory of basic values](https://pdfs.semanticscholar.org/dc49/e27d0ed890cd3ed2e80ca0b0107207f12a64.pdf). They were computed following [ESS EduNet](http://essedunet.nsd.uib.no/cms/topics/1/) instructions, and weighted with design weight (post-stratification weights are not available for round 9 so far). 

Data on Russia in round 7 are taken from ESS website and Russia round 9 from CESSI website. 


## Run

The app is published here: http://values.maksimrudnev.com

(Slow) mirror is here: https://rudnev.shinyapps.io/Basic_Values/

To run it locally, type in the RStudio console: `shiny::runGitHub("ShinyValues", "maksimrudnev")`


## Issues and problems

The tool is purely exploratory, don’t forget about cross-country comparability and measurement invariance problems. 

Report any issues https://github.com/MaksimRudnev/ShinyValues/issues, visit [my website](http://www.maksimrudnev.com), or send me messages to maksim dot rudnev at gmail.com.

## Replicability

R and Shiny codes are available at https://github.com/MaksimRudnev/ShinyValues

## Help with translation

If you are willing to participate in translating the app into your language, please, download the translation file [translation_elements.txt](https://github.com/MaksimRudnev/ShinyValues/blob/master/data/translation_elements.txt), edit it with a spreadsheet editor (like MS Excel) by adding a column with your language. Return to me by email maksim dot rudnev at gmail.com or at the [Issues section of its github repository](https://github.com/MaksimRudnev/ShinyValues/issues).


![](screenshot.png)