# library(arules) # for apriori
# library(arulesViz) # for viz
# library(forcats) # for categoricals
# library(dplyr) # for convenience
# library(plotly) # for interactive plots
# library(data.table) # for speed
# library(pander) # to pretty print some things

LabPackages = c(
  'arules',
  'arulesViz',
  'forcats',
  'dplyr',
  'plotly',
  'data.table',
  'pander',
  'knitr',
  'skimr',
  'lubridate'
)


package.check <- lapply(LabPackages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
