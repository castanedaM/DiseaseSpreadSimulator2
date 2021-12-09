# Disease Spread Simulator II

Important Notes for Developers:
  - inputs are displayed to users with a descriptive name, however they are referenced
     throughout the code by unique ids (alpha, beta, etc.). These names and ids can be found in CWD Fitting Results.pdf
  - The main code can be found in ./cwd_model/app.r
  - ./DiseaseRepo contains code and documents from Spring21 implementation

Important Libraries Used:

  The following libraries are used throughout our project. In order to best understand our code, it would be good
  to review and understand them.

  - library(shiny)
  - library(shinyjs)
  - library(shinythemes)
  - library(shinydashboard)


Improvements for the future:
  - allow for graph and table to be downloaded
  - create additional plots
  - instead of having plot update automatically, update after the user hits a button. This would allow users to compare completely different models as a whole
    rather than comparing plots changed by individual variables.
