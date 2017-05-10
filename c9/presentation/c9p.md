Course 9 Project - Presentation Part
========================================================
author: Feng Li
date: April 7, 2017
autosize: true

Summary
========================================================

Course 9 project includes two parts: shiny application part and presentation part. This presentation part is supposed to describe the shiny application.

My shiny application is deployed to Rstudio shiny server. Here is the app link  <https://fengliplatform.shinyapps.io/c9papp/>. Source code link is <https://github.com/fengliplatform/course9/tree/master/shinyapp/c9pApp>


Shiny App - Air Quality Dataset Plots
========================================================
airquality dataset is a daily air quality measurements in New York from May to September. quality values include:
- Ozone
- Solar.R
- Wind
- Temp

The question that my shiny app tries to anser is how Ozone values change when Solar.R, Wind or Temp values change.

To do so, We'll plot Ozone data against the other three mesures.

Shiny App - UI instruction
========================================================

This app uses sidebar layout.

On left side sidebar panel, there are three sections:
- Brief readme: a brief readme message on how to use the app.
- Choose month: checkboxes to each month from May to September with which you can choose the month you are interested. By defaul, data from all the five months are selected.
- Show/Hide linear model to selected data. The app will show/hide a linear regression line to tell the data relation.

On right side main panel, there are three tabs to show plots: Ozone vs Solar.R, Ozone vs Wind and Ozone vs Temp.


Shiny App - Take aways
========================================================
From this app, we can tell:

- when Solar.R values increase, Ozone values increase, a positive relation.
- when Wind values increase, Ozone values decrease, a negative relation.
  - but in June, there is a positive relation observed.
- when Temp values increase, Ozone values increase, a positive relation.

Based on the plots, further analysis can be looking for the reason behind and how to protect our ENV.



