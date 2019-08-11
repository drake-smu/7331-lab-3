# library(arules) # for apriori
# library(arulesViz) # for viz
# library(forcats) # for categoricals
# library(dplyr) # for convenience
# library(plotly) # for interactive plots
# library(data.table) # for speed
# library(pander) # to pretty print some things
library(remotes)
Crans = c(
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

# GithubArray = as.array(
#   c('tswgewrapped','josephsdavid/tswgewrapped')
# )
  
GithubRepos <- data.frame(
  name= as.character(),
  repo= as.character(),
  stringsAsFactors = F
)

.installCrans <- function(){
  MissingCrans <- Crans[!(Crans %in% installed.packages()[,"Package"])]
  if(length(MissingCrans)) install.packages(MissingCrans)
}

.installGitHubs <- function(){
  gits = matrix(
    data = GithubArray,
    ncol = 2,
    dimnames = list(NULL,c('name','repo'))
  )
  GithubRepos = rbind.data.frame(GithubRepos,
                                 gits,stringsAsFactors = F
  )
  MissingGithubRepos <- GithubRepos[!(GithubRepos['name'] %in% installed.packages()[,"Package"]),]
  if(nrow(MissingGithubRepos)) install_github(MissingGithubRepos[['repo']])
}

.installCrans()
if(exists("GithubArray")) .installGitHubs()

LabPackages = c(Crans,GithubRepos[['name']])
loaded <- lapply(LabPackages, library, character.only=T)

