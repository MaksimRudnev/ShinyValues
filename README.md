# Basic values in Europe

A Shiny App that demontrates means of basic values measured with [Schwartz (2001)](http://journals.sagepub.com/doi/abs/10.1177/0022022101032005001) scale in [European Social Survey](https://europeansocialsurvey.org).

To run, type in the RStudio console: `shiny::runGitHub("ShinyValues", "maksimrudnev")`
You might need to install some of the following packages to enable the app properly running on your device: *devtools, shiny, ggplot2, ggrepel, reshape2, stringr, maps, sf*.

The tool is purely exploratory, don’t forget about comparability and measurement invariance problems. 

Demo is published here: https://rudnev.shinyapps.io/Basic_Values/

There are three tabs to explore trends by country, which allows comparison of value trends within each country, by value – to compare countries, and value map to see all the countries as points in the space of two higher order value dimensions. Point you mouse at country point on the value map to see how they moved during the measurement period.


![](screenshot.png)