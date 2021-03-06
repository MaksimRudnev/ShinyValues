## This app

was created with [ShinyApps](https://shinyapps.io/), using data from 
 [European Social Survey](http://www.europeansocialsurvey.org/data/)

Value indices are based on [Schwartz theory of basic values](https://pdfs.semanticscholar.org/dc49/e27d0ed890cd3ed2e80ca0b0107207f12a64.pdf). They were computed following [ESS EduNet](http://essedunet.nsd.uib.no/cms/topics/1/) instructions, and weighted with post-stratification weight [pspweight](http://www.europeansocialsurvey.org/methodology/ess_methodology/data_processing_archiving/weighting.html).


Data on Russia in round 7 are taken from ESS website section "Related studies" and Russia round 9 from CESSI website. Instead of poststratification weight in all Russian respondents were assigned 1. 

## Run

Either of the ways is functional.

The app is published here: http://apps.maksimrudnev.com:3838/shiny_values/

(Slow) mirror is here: https://rudnev.shinyapps.io/Basic_Values/

To run it locally, type in the RStudio console: `shiny::runGitHub("ShinyValues", "maksimrudnev")`


## Issues and problems

The tool is purely exploratory, don’t forget about cross-country comparability and measurement invariance problems. 

Report any issues https://github.com/MaksimRudnev/ShinyValues/issues, visit [my website](http://www.maksimrudnev.com) and send me messages to maksim dot rudnev at gmail.com.

## Replicability

R and Shiny codes are available at https://github.com/MaksimRudnev/ShinyValues

## Help with translation

If you are willing to participate in translating of the app to your language, please, download the translation file [translation_elements.txt](https://github.com/MaksimRudnev/ShinyValues/blob/master/data/translation_elements.txt), edit it with a spreadsheet editor (like MS Excel) by adding a column with your language. Return it to me by email maksim dot rudnev at gmail.com or at the [Issues section of its github repository](https://github.com/MaksimRudnev/ShinyValues/issues).


## Packages used

The following R packages were used and required to run app:

- shiny
- ggplot2
- ggrepel
- reshape2

<!-- 
- stringr
- maps
- sf
- readr 
-->

