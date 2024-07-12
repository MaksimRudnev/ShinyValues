# Basic values in Europe

This app was created with [ShinyApps](https://shinyapps.io/), using  
 [European Social Survey](http://www.europeansocialsurvey.org/data/) data.

Value indices are based on [Schwartz theory of basic values](https://pdfs.semanticscholar.org/dc49/e27d0ed890cd3ed2e80ca0b0107207f12a64.pdf) [Schwartz (2001)](http://journals.sagepub.com/doi/abs/10.1177/0022022101032005001). They were computed following [ESS EduNet](http://essedunet.nsd.uib.no/cms/topics/1/) instructions, and weighted with design weights. 

Data on Russia in round 7 are taken from the ESS website section "Related studies" and Russia rounds 9 and 10 from CESSI website. 

Round 10 (2021) includes only countries that continued to use face-to-face interviews.

## Run

 [The app is published here: http://values.maksimrudnev.com]: #

You are here: https://rudnev.shinyapps.io/Basic_Values/

To run it locally, type in the RStudio console: `shiny::runGitHub("ShinyValues", "maksimrudnev")`. (You might need to install some of the following packages to enable the app properly running on your device: *devtools, shiny, ggplot2, ggrepel, reshape2, stringr, maps, sf, readr*.)


## Issues and problems

The tool is purely exploratory. 

> <span style="color:red;font-face:strong;">!! Cross-country comparability and measurement invariance are not guaranteed! </span>

Report any issues https://github.com/MaksimRudnev/ShinyValues/issues, visit [my website](http://www.maksimrudnev.com), or send me a message maksim dot rudnev at gmail.com.

## Replicability

R and Shiny codes are available at https://github.com/MaksimRudnev/ShinyValues


## References

European Social Survey European Research Infrastructure (ESS ERIC) (2024) ESS round 11 - 2023. Social inequalities in health, Gender in contemporary Europe. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess11-2023.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 10 - 2020. Democracy, Digital social contacts. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS10-2020.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 9 - 2018. Timing of life, Justice and fairness. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS9-2018.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 8 - 2016. Welfare attitudes, Attitudes to climate change. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS8-2016.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 7 - 2014. Immigration, Social inequalities in health. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi:10.21338/NSD-ESS7-2014.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 6 - 2012. Personal wellbeing, Democracy. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS6-2012.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 5 - 2010. Family work and wellbeing, Justice. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS5-2010.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 4 - 2008. Welfare attitudes, Ageism. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS4-2008.

European Social Survey European Research Infrastructure (ESS ERIC) (2018) ESS round 3 - 2006. Timing of life, Personal wellbeing. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS3-2006.

European Social Survey European Research Infrastructure (ESS ERIC) (2018) ESS round 2 - 2004. Health and care, Economic morality, Family work and wellbeing. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS2-2004.

European Social Survey European Research Infrastructure (ESS ERIC) (2023) ESS round 1 - 2002. Immigration, Citizen involvement. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/NSD-ESS1-2002.

### Russian data

Data of Russian social survey based on ESS program. The survey is conducted by CESSI (Institute for comparative social research) in ____ (dates are specified below) by face-to-face interviews in respondent's homes based on probability random sample of population of Russia 15 years old and over. In ___ particular round ____ interviews were conducted. The methodological specification and technical details of the survey are located in www.ess-ru.ru and www.europeansocialsurvey.org.

  - Round 7 (ESS), Round 5 of RSS – December 2014-Febrary 2015, sample size of 2445  interviews
  - Round 9 (ESS), Round 7 of RSS – November 2018-February 2019, sample size of 2416 interviews.
  - Round 10 (ESS), Round 8 of RSS – November 2021-February 2019, sample size of 2416 interviews.

    
    