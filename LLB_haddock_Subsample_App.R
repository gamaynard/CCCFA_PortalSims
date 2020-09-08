## ---------------------------
##
## Script name: LLB_haddock_Subsample_App.R
##
## Purpose of script: Runs an RShiny application to simulate discards of haddock
##    on longline fishing trips and generate estimates of discard weight from
##    different subsampling protocols on those trips
##
## Author: George A. Maynard
##
## Date Created: 2020-08-28
##
## Copyright (c) George Alphonse Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory
## If working on the development machine, set working directory automatically,
## else, working directory is set to the user's default
if(Sys.info()[[4]]=="HOOK-05"){
  setwd("C:/Users/George/Desktop/Autotask Workplace/Personal Documents/Development/ShinyApps/PortalSims/")
}
## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(shiny)
library(truncnorm)
## ---------------------------

## load up our functions into memory

## ---------------------------

## Read in and format necessary data
## "data" is a file of lengths of individual discarded haddock along with a trip
## and haul index value for each one (e.g. trip 1, haul 1) to allow for viewing
## the data at a more granular level
data=read.csv("https://raw.githubusercontent.com/gamaynard/CCCFA_PortalSims/master/LLB_haddock_lengths.csv")
data$GEAR=as.character(data$GEAR)
data$TRIP=as.numeric(as.character(data$TRIP))
data$HAUL=as.numeric(as.character(data$HAUL))
data$SPECIES=as.character(data$SPECIES)
data$QUANTITY=as.numeric(as.character(data$QUANTITY))
data$LENGTH=as.numeric(as.character(data$LENGTH))

## Create a vector of lengths to use for demonstration purposes
demo=sample(
  data$LENGTH,
  size=50,
  replace=FALSE
)

## "lw" contains the ln(alpha) and beta values necessary to estimate weights of 
## most groundfish species from lengths using the formula:
##    weight=exp(log(a)+b*log(length)) ## kg
##    weight=exp(log(a)+b*log(length))*2.204623 ## lbs
lw=read.csv("https://raw.githubusercontent.com/gamaynard/CCCFA_PortalSims/master/LengthWeightEquations.csv")
lw=subset(
  lw,
  lw$Species==as.character(
    unique(
      data$SPECIES
      )
    )
  )
## --------------------------
########## UI ###############
ui <- fluidPage(
  titlePanel(
    h2(
      "Longline Haddock Discard Subsampling Simulator",
      align="center",
      style="color:darkblue"),
    windowTitle="Haddock Subsample Sim"
    ),
  sidebarLayout(
    ## Create a sidebar with host information and
    ## user controls
    sidebarPanel(
      ## Host information
      h4(
        "Hosted by the CCCFA",
        style="color:darkblue"
      ),
      img(
        src="logo.jpg",
        height=181.5,
        width=206.25
        ),
      ## User controls
      h3(
        "Simulation Parameters"
      ),
      ## Select a number of simulations to run (each simulation is one trip)
      sliderInput(
        inputId="simulations",
        label="Trips to Simulate",
        min=10,
        max=10000,
        step=10,
        value=1000
      ),
      ## Select a number of subsamples to collect per trip
      sliderInput(
        inputId="samplesize",
        label="Individuals to Subsample per Trip",
        min=1,
        max=35,
        step=1,
        value=20
      ),
      ## Select how the discards will be distributed across the strings of
      ## fish
      selectInput(
        inputId="DPS",
        label="Distribution of Discards Across Strings",
        choices=c(
          "random",
          "even",
          "skewed to first",
          "skewed to mid",
          "skewed to last"
        )
      )
    ),
    mainPanel(
      ## The main panel will have a README tab and a results tab
      tabsetPanel(
        ## README
        tabPanel(
          title="README",
          helpText(
            "This is where the helpText goes."
          )
        ),
        ## Simulation visualizer
        ## This tab shows plots describing the scenario being run
        tabPanel(
          title="Simulation Descriptors",
          helpText(
            "This graphs below show the scenarios being simulated using the",
            "average values for number of discards and strings fished as well as",
            "the user specified parameters for subsampling and distribution of",
            "discards. All scenarios being simulated will use the single discard",
            "distribution selected (top graph) across strings (e.g., if 'random'",
            " is selected, each simulation will have discards randomly assigned",
            " to strings). All scenarios being simulated will test all four within",
            " string distributions (bottom graphs). The same fish are rearranged on",
            " the strings in each of the four distribution types before estimates",
            " are calculated."
          ),
          plotOutput(
            outputId='fishByString'
          ),
          fluidRow(
            splitLayout(
              cellWidths=c("23%","23%","23%","23%"),
              plotOutput("RDisc"),
              plotOutput("FDisc"),
              plotOutput("MDisc"),
              plotOutput("LDisc")
            )
          )
        ),
        ## Results
        tabPanel(
          title="Graphical Results",
          ## Plot outputs
          plotOutput("plot1")
        ),
        tabPanel(
          title="Text Results",
          tableOutput(
            "results"
            )
        )
      )
    )
  )
)
######## SERVER #############
server <- function(input,output){
  ## Create a blank dataframe to store simulation results
  newResults=reactive({
    results=data.frame(
      simNum=as.numeric(),
      stringsFished=as.numeric(),
      discards=as.numeric(),
      known=as.numeric(),
      RestR=as.numeric(),
      RestF=as.numeric(),
      RestM=as.numeric(),
      RestL=as.numeric(),
      FestR=as.numeric(),
      FestF=as.numeric(),
      FestM=as.numeric(),
      FestL=as.numeric()
    )
    ## Assign each simulated trip a number of strings to fish using a truncated 
    ## normal distribution bounded by 1 and 11 with a mean of 6.9 strings and a 
    ## standard deviation of 1.8 strings (based on the FY2017-2019 data available
    stringsFished=round(
      rtruncnorm(
        n=input$simulations,
        a=1,
        b=11,
        mean=6.9,
        sd=1.8
      ),
      0
    )
    ## In the FY2017-2019 data haddock discards per trip range from 8 to 809, with 
    ## a mean of 265 (+/- 120). This step assigns each simulated trip a number of
    ## discards using a truncated normal distribution bounded on the lower end at
    ## 1 and unbounded on the upper end. 
    discards=round(
      rtruncnorm(
        n=input$simulations,
        a=1,
        mean=265,
        sd=120
      ),
      0
    )
    ## For each simulation
    for(sim in 1:input$simulations){
      ## Read in the strings fished and discard values for the trip
      sf=stringsFished[sim]
      d=discards[sim]
      ## sample the vector of real lengths to create a vector of discard lengths 
      ## for the trip
      tripDisc=sample(
        x=data$LENGTH,
        size=d,
        replace=TRUE
      )
      ## assign each discard to a string based on input$DPS
      strings=list()
      ############### For evenly distributed discards
      if(input$DPS=='even'){
        for(i in seq(1,sf-1,1)){
          strings[[i]]=sample(
            x=subset(
              seq(1,d,1),
              seq(1,d,1)%in%unlist(strings)==FALSE
            ),
            size=round(d/sf,0),
            replace=FALSE
          )
        }
        ## The final string contains any remaining fish that have yet to be
        ## assigned
        strings[[sf]]=subset(
          seq(1,d,1),
          seq(1,d,1)%in%unlist(strings)==FALSE
        )
      }
      ############### For skewed discards
      if(input$DPS%in%c('skewed to first','skewed to last','skewed to mid')){
        for(i in seq(1,sf-1,1)){
          if(d>0){
            b=sample(
              d,
              1
            )
            strings[[i]]=sample(
              x=subset(
                seq(1,d,1),
                seq(1,d,1)%in%unlist(strings)==FALSE
              ),
              size=b,
              replace=FALSE
            )
            a=a-b
          } else {
            strings[[i]]=0
          }
        }
        if(a>0){
          strings[[input$sf]]=subset(
            seq(1,input$d,1),
            seq(1,input$d,1)%in%unlist(strings)==FALSE
          )
        } else {
          strings[[input$sf]]=0
        }
        if(input$DPS=='skewed to last'){
          strings=rev(strings)
        }
        if(input$DPS=='skewed to mid'){
          a=strings
          b=lengths(strings)
          strings=list()
          for(i in 1:length(a)){
            strings[[i]]=0
          }
          h=order(b)
          a=a[h]
          e=round(sf/2,0)
          strings[[e]]=a[[length(a)]]
          a=a[-length(a)]
          g=seq(1,sf,1)
          for(i in seq(
            1,
            round(
              length(
                subset(g,g!=e)
              )/2,0
            ),
            1)
          ){
            strings[[e-i]]=a[[length(a)]]
            a=a[-length(a)]
            strings[[e+i]]=a[[length(a)]]
            a=a[-length(a)]
          }
        }
      }
      ############### For random discards
      if(input$DPS=='random'){
        a=runif(sf,0,1)
        b=a/sum(a)
        b=round(b*d,0)
        while(sum(b)>d){
          x=sample(1:sf,1)
          if(b[x]!=0){
            b[x]=b[x]-1
          }
        }
        while(sum(b)<d){
          x=sample(1:sf,1)
          b[x]=b[x]+1
        }
        for(i in 1:sf){
          y=subset(
            seq(1,d,1),
            seq(1,d,1)%in%unlist(strings)==FALSE
          )
          if(is.null(length(y))==FALSE){
            strings[[i]]=sample(
              x=y,
              size=b[i],
              replace=FALSE
            )
          }
        }
        strings[sapply(strings, function(strings) length(strings)==0)]=0
      }
      for(i in 1:length(strings)){
        if(strings[[i]][1]==0){
          strings[[i]]=NA
        } else {
          strings[[i]]=tripDisc[strings[[i]]]
        }
      }
      ## Within each string, discards can be distributed randomly, or larger 
      ## discards could be biased to the middle, or biased towards one end. 
      ## The script tests all of these scenarios and displays the results
      ## Clear all strings with no discards
      strings=subset(strings,is.na(strings)==FALSE)
      stringsMid=strings
      stringsFirst=strings
      stringsLast=strings
      for(i in 1:length(strings)){
        stringsFirst[[i]]=rev(strings[[i]][order(strings[[i]])])
        stringsLast[[i]]=strings[[i]][order(strings[[i]])]
        a=stringsMid[[i]]
        b=which(a==max(a))[1]
        d1=a[b]
        e=round(length(stringsMid[[i]])/2,0)
        a=a[-b]
        a=a[order(a)]
        stringsMid[[i]][e]=d1
        if(length(stringsMid[[i]])>1){
          for(j in seq(1,e,1)){
            if(length(stringsMid[[i]])>=(e+j)){
              stringsMid[[i]][e+j]=a[length(a)]
              a=a[-length(a)]
              if((e-j)>0){
                stringsMid[[i]][e-j]=a[length(a)]
                a=a[-length(a)]
              }
            }
          }
        }
      }
      ## Create a vector to represent the trip under each of the the 4 conditions
      ## Randomly distributed lengths within strings
      strings=unlist(strings)
      ## Large fish towards the first part of the strings
      stringsFirst=unlist(stringsFirst)
      ## Large fish in the middle of the strings
      stringsMid=unlist(stringsMid)
      ## Large fish towards the end of the strings
      stringsLast=unlist(stringsLast)
      
      ## Randomly subsample n discards from each trip if the number of discards
      ## is greater than the subsample size
      if(d>input$samplesize){
        RestS=sample(
          strings,
          input$samplesize,
          replace=FALSE
        )
        RestF=sample(
          stringsFirst,
          input$samplesize,
          replace=FALSE
        )
        RestM=sample(
          stringsMid,
          input$samplesize,
          replace=FALSE
        )
        RestL=sample(
          stringsLast,
          input$samplesize,
          replace=FALSE
        )
      } 
      
      ## Subsample the first n discards from each trip if the number of discards
      ## is greater than the subsample size
      if(d>input$samplesize){
        FestS=strings[1:input$samplesize]
        FestF=stringsFirst[1:input$samplesize]
        FestM=stringsMid[1:input$samplesize]
        FestL=stringsLast[1:input$samplesize]
      } 
      
      ## Convert lengths to weights to calculate simulated true (known) values
      ## for each trip. Trips should have the same known weight, regardless of
      ## how the fish are allocated among the strings
      kstrings=sum(exp(lw$lnAlpha+lw$Beta*log(strings))*2.204623)
      
      ## If the number of discards is greater than the subsample, produce an
      ## estimate, otherwise all fish are measured
      if(d>input$samplesize){
        ## For all estimates, the total weight of all subsampled fish is divided
        ## by the number of subsampled fish to get an average weight of the fish 
        ## in the subsample. The average is multiplied by the number of discards
        ## from the trip to estimate the weight of discards. 
        RestS=sum(exp(lw$lnAlpha+lw$Beta*log(RestS))*2.204623)/input$samplesize*d
        RestF=sum(exp(lw$lnAlpha+lw$Beta*log(RestF))*2.204623)/input$samplesize*d
        RestM=sum(exp(lw$lnAlpha+lw$Beta*log(RestM))*2.204623)/input$samplesize*d
        RestL=sum(exp(lw$lnAlpha+lw$Beta*log(RestL))*2.204623)/input$samplesize*d
        FestS=sum(exp(lw$lnAlpha+lw$Beta*log(FestS))*2.204623)/input$samplesize*d
        FestF=sum(exp(lw$lnAlpha+lw$Beta*log(FestF))*2.204623)/input$samplesize*d
        FestM=sum(exp(lw$lnAlpha+lw$Beta*log(FestM))*2.204623)/input$samplesize*d
        FestL=sum(exp(lw$lnAlpha+lw$Beta*log(FestL))*2.204623)/input$samplesize*d
      } else {
        RestS=kstrings
        RestF=kstrings
        RestM=kstrings
        RestL=kstrings
        FestS=kstrings
        FestF=kstrings
        FestM=kstrings
        FestL=kstrings
      }
      
      ## Store the data in the output dataframe
      results[sim,]=NA
      results$simNum[sim]=sim
      results$stringsFished[sim]=sf
      results$discards[sim]=d
      results$known[sim]=kstrings
      results$RestR[sim]=RestS
      results$RestF[sim]=RestF
      results$RestM[sim]=RestM
      results$RestL[sim]=RestL
      results$FestR[sim]=FestS
      results$FestF[sim]=FestF
      results$FestM[sim]=FestM
      results$FestL[sim]=FestL
    }
    return(results)
  })
  ## Finalize the dataframe for display
  output$results=renderTable({
    newResults()
  })
  ## Ouput graphical representations of a simulation using the average values
  ## for discards and strings fished and the subsampling and distribution 
  ## parameters entered by the user. 
  output$fishByString=renderPlot({
    sf=6
    d=265
    a=d
    strings=list()
    if(input$DPS=='random'){
      a=runif(sf,0,1)
      b=a/sum(a)
      b=round(b*d,0)
      while(sum(b)>d){
        x=sample(1:sf,1)
        if(b[x]!=0){
          b[x]=b[x]-1
        }
      }
      while(sum(b)<d){
        x=sample(1:sf,1)
        b[x]=b[x]+1
      }
    barplot(
      b,
      xlab="<-- First Hauled | String Order | Last Hauled -->",
      ylab="Discards per String",
      main="Discards Distributed Randomly Among Strings"
      )
    }
    if(input$DPS=='even'){
      strings=list()
      for(i in seq(1,sf-1,1)){
        strings[[i]]=sample(
          x=subset(
            seq(1,d,1),
            seq(1,d,1)%in%unlist(strings)==FALSE
          ),
          size=round(d/sf,0),
          replace=FALSE
        )
      }
      ## The final string contains any remaining fish that have yet to be
      ## assigned
      strings[[sf]]=subset(
        seq(1,d,1),
        seq(1,d,1)%in%unlist(strings)==FALSE
      )
      barplot(
        lengths(strings),
        xlab="<-- First Hauled | String Order | Last Hauled -->",
        ylab="Discards per String",
        main="Discards Distributed Evenly Among Strings"
      )
    }
    if(input$DPS%in%c('skewed to first','skewed to last','skewed to mid')){
      for(i in seq(1,sf-1,1)){
        if(d>0){
          b=sample(
            a,
            1
          )
          strings[[i]]=sample(
            x=subset(
              seq(1,d,1),
              seq(1,d,1)%in%unlist(strings)==FALSE
            ),
            size=b,
            replace=FALSE
          )
          a=a-b
        } else {
          strings[[i]]=0
        }
      }
      if(a>0){
        strings[[sf]]=subset(
          seq(1,d,1),
          seq(1,d,1)%in%unlist(strings)==FALSE
        )
      } else {
        strings[[sf]]=0
      }
      if(input$DPS=='skewed to last'){
        strings=rev(strings)
      }
      if(input$DPS=='skewed to mid'){
        a=strings
        b=lengths(strings)
        strings=list()
        for(i in 1:length(a)){
          strings[[i]]=0
        }
        h=order(b)
        a=a[h]
        e=round(sf/2,0)
        strings[[e]]=a[[length(a)]]
        a=a[-length(a)]
        g=seq(1,sf,1)
        for(i in seq(
          1,
          round(
            length(
              subset(g,g!=e)
            )/2,0
          ),
          1)
        ){
          strings[[e-i]]=a[[length(a)]]
          a=a[-length(a)]
          strings[[e+i]]=a[[length(a)]]
          a=a[-length(a)]
        }
      }
      barplot(
        lengths(strings),
        xlab="<-- First Hauled | String Order | Last Hauled -->",
        ylab="Discards per String",
        main="Discards Skewed"
      )
    }
  })
  output$RDisc=renderPlot({
    barplot(
      demo,
      xlab="<-- First Hauled",
      ylab="Discard Length (cm)",
      main="Random Distribution Within String"
    )
  })
  output$FDisc=renderPlot({
    barplot(
      demo[order(
        demo,
        decreasing=TRUE
      )],
      xlab="| Haul "
    )
  })
  output$LDisc=renderPlot({
    barplot(
      demo[order(
        demo,
        decreasing=FALSE
      )],
      xlab="Last Hauled -->"
    )
  })
  output$MDisc=renderPlot({
    e=ceiling(length(demo)/2)
    a=demo[order(
      demo,
      decreasing=TRUE
      )]
    b=a
    for(j in seq(1,e,1)){
      b[j]=a[length(a)]
      a=a[-length(a)]
      if(length(a)>0){
        b[length(b)-j]=a[length(a)]
        a=a[-length(a)]
      }
    }
    barplot(
      b,
      xlab="Order |",
      main="Skewed Distributions Within Strings"
    )
  })
}
## Run application
shinyApp(ui=ui,server=server)