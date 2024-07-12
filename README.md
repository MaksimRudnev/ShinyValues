# Basic values in Europe

A Shiny App that demontrates means of basic values measured with [Schwartz (2001)](http://journals.sagepub.com/doi/abs/10.1177/0022022101032005001) scale in [European Social Survey](https://europeansocialsurvey.org).

This app was created with [ShinyApps](https://shinyapps.io/), using data from 
 [European Social Survey](http://www.europeansocialsurvey.org/data/)

Value indices are based on [Schwartz theory of basic values](https://pdfs.semanticscholar.org/dc49/e27d0ed890cd3ed2e80ca0b0107207f12a64.pdf). They were computed following [ESS EduNet](http://essedunet.nsd.uib.no/cms/topics/1/) instructions, and weighted with post-stratification weight. 

Data on Russia in round 7 are taken from the ESS website section "Related studies" and Russia rounds 9 and 10 from CESSI website. 

Round 10 (2021) includes only countries that continued to use face-to-face interviews.

## Run

 [The app is published here: http://values.maksimrudnev.com]: #

You are here: https://rudnev.shinyapps.io/Basic_Values/

To run it locally, type in the RStudio console: `shiny::runGitHub("ShinyValues", "maksimrudnev")`. (You might need to install some of the following packages to enable the app properly running on your device: *devtools, shiny, ggplot2, ggrepel, reshape2, stringr, maps, sf, readr*.)


## Issues and problems

The tool is purely exploratory. 

‼️ _Cross-country comparability and measurement invariance are not guaranteed!_ 

Report any issues https://github.com/MaksimRudnev/ShinyValues/issues, visit [my website](http://www.maksimrudnev.com), or send me a message maksim dot rudnev at gmail.com.

## Replicability

R and Shiny codes are available at https://github.com/MaksimRudnev/ShinyValues

