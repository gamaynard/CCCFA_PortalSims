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
        value=100
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
            "This application is designed to analyze the effect of different",
            "distributions of haddock discards within longline sets on the", 
            "estimates of weight generated from two styles of subsampling. To",
            "the left you will see user controls for the simulations. The first",
            "is a slider bar to control how many trips are simulated. Simulating",
            "more trips will result in longer load times for graphs and data. The",
            "second control is the number of individuals to subsample per trip.",
            "Currently, longliners subsample 20 individual haddock per haul, which",
            "can still mean measuring over 150 fish per trip. The final control",
            "allows the user to alter the distribution of discards across strings.",
            "Discards can be evenly spread (roughly the same number on each string),",
            "randomly spread, skewed to first (meaning more discards occur on the",
            "first string hauled, with few on the last string), skewed to mid",
            "(few discards on the first and last strings, increasing to the middle),",
            "or skewed to last (few discards on the first strings, increasing in",
            "number to the most on the last string or two). Graphical represenatations",
            "of each of these scenarios can be found on the 'Simulation Descriptors'",
            "tab of the application."
          ),
          helpText(
            "Once the user has determined which parameters to use,",
            "the application runs a series of simulations that proceed as follows:"
          ),
          helpText(
            "1) A number of strings fished and a number of total haddock discards are",
            "assigned to each trip based on trip sizes and discards recorded as part",
            "of the audit model EM program."
          ),
          helpText(
            "2) Discards are distributed among strings according to the user specification."
          ),
          helpText(
            "3) Each discard is assigned a length from a length distribution",
            "generated by longliners participating in the audit model program."
          ),
          helpText(
            "4) Discards are arranged each of four ways and subsampled. Discard",
            "arrangements are either random, skewed towards the first fish hauled",
            "(i.e., large fish are hauled first), skewed towards the last fish",
            "hauled (i.e., small fish are hauled first), or skewed towards the",
            "middle (i.e., smaller fish towards the end of the strings, larger",
            "fish towards the middle). Subsamples take both the first 'n' fish",
            "and a random 'n' fish (user specifies 'n') from the trip."
          ),
          helpText(
            "5) Lengths of subsampled fish are converted to weights using the",
            "equations of Wigley et al. 2003. An average weight is taken from the",
            "subsampled fish and multiplied by the total number of discards to",
            "get an estimated discard weight for the trip under each subsampling",
            "method and length distribution."
          ),
          helpText(
            "6) Results are made available as graphical outputs on a tab in the",
            "application or text outputs on a separate tab. The text outputs can",
            "be downloaded as a .csv file for further exploration."
          ),
          helpText(
            "Anecdotal information from the captains we've spoken with indicates",
            "that fish cluster by size class (e.g., a string might be predominantly",
            "scrod and another string might be predominantly market-size fish),",
            "but there is little clustering within size class (e.g., a 40 cm discard",
            "is no more likely to be in a particular location on a string of short",
            "fish than a 37 cm discard). Thus, the user controlled distribution",
            "of fish between strings is likely more representative of reality",
            "than the computer controlled distributions of lengths within strings,",
            "which the fishermen believe to be more or less random."
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
          helpText(
            "The four graphs below show estimated weights for each simulated trip.",
            "If weights are estimated using a 'Random n' subsampling method, the",
            "points are orange circles. If weights are estimated using a 'First n'",
            "subsampling method, the points are blue X's. The dashed black line",
            "is the 1:1, estimate:known (perfect match) line. Orange text in the figures",
            "(left hand column) gives summary statistics for the 'Random n'",
            "subsamples. Blue text in the figures (right hand column) gives summary",
            "statistics for the 'First n' subsamples. 'p' indicates the proportion",
            "of the estimates that fall above the known values (closer to 0.5 is better).",
            "Total error is the sum of the absolute (non-negative) errors across",
            "all simulated trips (e.g., three trips with errors of 5 lbs, -5 lbs,",
            "and 8 lbs would have a total error of 18 lbs). Average error is the",
            "total error divided by the number of simulated trips."
          ),
          ## Plot outputs
          fluidRow(
            splitLayout(
              cellWidths=c("48%","48%"),
              plotOutput("RandomPlot"),
              plotOutput("FirstPlot")
            )
          ),
          fluidRow(
            splitLayout(
              cellWidths=c("48%","48%"),
              plotOutput("MidPlot"),
              plotOutput("LastPlot")
            )
          )
        ),
        tabPanel(
          title="Text Results",
          downloadButton(
            outputId="textDownload",
            label="Download Simulation Outputs",
            col='gray'
          ),
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
        a=3,
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
    ## Threre must be at least as many discards as strings
    for(i in 1:length(discards)){
      if(discards[i]<stringsFished[i]){
        discards[i]=stringsFished[i]
      }
    }
    ## For each simulation
    for(sim in 1:input$simulations){
      ### Read in the strings fished and discard values for the trip
      sf=stringsFished[sim]
      discs=discards[sim]
      if(discs<sf){
        discs=sf
      }
      ## sample the vector of real lengths to create a vector of discard lengths 
      ## for the trip
      tripDisc=sample(
        x=data$LENGTH,
        size=discs,
        replace=TRUE
      )
      ## assign each discard to a string based on input$DPS
      strings=list()
      ############### For evenly distributed discards
      if(input$DPS=='even'){
        strings=split(
          x=tripDisc,
          f=ceiling(
            seq(
              from=1,
              to=sf
              )
            )
          )
        }
      ############### For skewed discards
      a=discs
      if(input$DPS%in%c('skewed to first','skewed to last','skewed to mid')){
          for(i in seq(1,sf-1,1)){
            if(a>0){
              b=sample(
                a,
                1
              )
              strings[[i]]=sample(
                x=subset(
                  seq(1,discs,1),
                  seq(1,discs,1)%in%unlist(strings)==FALSE
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
              seq(1,discs,1),
              seq(1,discs,1)%in%unlist(strings)==FALSE
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
            e=ceiling(sf/2)
            strings[[e]]=a[[length(a)]]
            a=a[-length(a)]
            g=seq(1,sf,1)
            for(i in seq(1,
              ceiling(
                length(
                  subset(g,g!=e)
                )/2
              ),
              1)
            ){
              if((e-i)!=0){
                strings[[e-i]]=a[[length(a)]]
                a=a[-length(a)]
                strings[[e+i]]=a[[length(a)]]
                a=a[-length(a)]
              }
            }
          }
          for(i in 1:length(strings)){
            if(strings[[i]][1]==0){
              strings[[i]]=NA
            } else {
              strings[[i]]=tripDisc[strings[[i]]]
            }
          }
        }
      ############### For random discards
      if(input$DPS=='random'){
          a=runif(sf,0,1)
          b=a/sum(a)
          b=round(b*discs,0)
          while(sum(b)>discs){
            x=sample(1:sf,1)
            if(b[x]!=0){
              b[x]=b[x]-1
            }
          }
          while(sum(b)<discs){
            x=sample(1:sf,1)
            b[x]=b[x]+1
          }
          for(i in 1:sf){
            y=subset(
              seq(1,discs,1),
              seq(1,discs,1)%in%unlist(strings)==FALSE
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
          for(i in 1:length(strings)){
            if(strings[[i]][1]==0){
              strings[[i]]=NA
            } else {
              strings[[i]]=tripDisc[strings[[i]]]
            }
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
      if(discs>input$samplesize){
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
      if(discs>input$samplesize){
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
      if(discs>input$samplesize){
        ## For all estimates, the total weight of all subsampled fish is divided
        ## by the number of subsampled fish to get an average weight of the fish 
        ## in the subsample. The average is multiplied by the number of discards
        ## from the trip to estimate the weight of discards. 
        RestS=sum(exp(lw$lnAlpha+lw$Beta*log(RestS))*2.204623)/input$samplesize*discs
        RestF=sum(exp(lw$lnAlpha+lw$Beta*log(RestF))*2.204623)/input$samplesize*discs
        RestM=sum(exp(lw$lnAlpha+lw$Beta*log(RestM))*2.204623)/input$samplesize*discs
        RestL=sum(exp(lw$lnAlpha+lw$Beta*log(RestL))*2.204623)/input$samplesize*discs
        FestS=sum(exp(lw$lnAlpha+lw$Beta*log(FestS))*2.204623)/input$samplesize*discs
        FestF=sum(exp(lw$lnAlpha+lw$Beta*log(FestF))*2.204623)/input$samplesize*discs
        FestM=sum(exp(lw$lnAlpha+lw$Beta*log(FestM))*2.204623)/input$samplesize*discs
        FestL=sum(exp(lw$lnAlpha+lw$Beta*log(FestL))*2.204623)/input$samplesize*discs
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
      results$discards[sim]=discs
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
        e=ceiling(sf/2)
        strings[[e]]=a[[length(a)]]
        a=a[-length(a)]
        g=seq(1,sf,1)
        for(i in seq(
          1,
          ceiling(
            length(
              subset(g,g!=e)
            )/2
          ),
          1)
        ){
          if((e-i)>0){
            strings[[e-i]]=a[[length(a)]]
            a=a[-length(a)]
          }
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
  output$textDownload=downloadHandler(
    filename=function(){
      paste0("Results_SampleSize",input$samplesize,"_DPS_",input$DPS,".csv")
    },
    content=function(file){
      write.csv(newResults(),file,row.names=FALSE)
    }
  )
  output$RandomPlot=renderPlot({
    x=newResults()
    plot(
      x$RestR~x$known,
      xlim=c(0,1200),
      ylim=c(0,1200),
      col='orange',
      pch=1,
      xlab="Known Weight (lbs)",
      ylab="Estimated Weight (lbs)",
      main="Random Lengths Within Strings"
      )
    points(
      x$FestR~x$known,
      pch=4,
      col='blue'
    )
    lines(
      seq(0,1200,1)~seq(0,1200,1),
      lty=2
      )
    legend(
      "topleft",
      legend=c("Random n","First n"),
      col=c('orange','blue'),
      pch=c(1,4)
    )
    yR=abs(x$RestR-x$known)
    yF=abs(x$FestR-x$known)
    text(
      x=rep(600,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$RestR>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yR),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yR),1)," lbs")
      ),
      col='orange'
    )
    text(
      x=rep(1000,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$FestR>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yF),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yF),1)," lbs")
      ),
      col='blue'
    )
  })
  output$FirstPlot=renderPlot({
    x=newResults()
    plot(
      x$RestF~x$known,
      xlim=c(0,1200),
      ylim=c(0,1200),
      col='orange',
      pch=1,
      xlab="Known Weight (lbs)",
      ylab="Estimated Weight (lbs)",
      main="Large Fish First"
    )
    points(
      x$FestF~x$known,
      pch=4,
      col='blue'
    )
    lines(
      seq(0,1200,1)~seq(0,1200,1),
      lty=2
    )
    legend(
      "topleft",
      legend=c("Random n","First n"),
      col=c('orange','blue'),
      pch=c(1,4)
    )
    yR=abs(x$RestF-x$known)
    yF=abs(x$FestF-x$known)
    text(
      x=rep(600,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$RestF>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yR),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yR),1)," lbs")
      ),
      col='orange'
    )
    text(
      x=rep(1000,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$FestF>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yF),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yF),1)," lbs")
      ),
      col='blue'
    )
  })
  output$MidPlot=renderPlot({
    x=newResults()
    plot(
      x$RestM~x$known,
      xlim=c(0,1200),
      ylim=c(0,1200),
      col='orange',
      pch=1,
      xlab="Known Weight (lbs)",
      ylab="Estimated Weight (lbs)",
      main="Large Fish Mid"
    )
    points(
      x$FestM~x$known,
      pch=4,
      col='blue'
    )
    lines(
      seq(0,1200,1)~seq(0,1200,1),
      lty=2
    )
    legend(
      "topleft",
      legend=c("Random n","First n"),
      col=c('orange','blue'),
      pch=c(1,4)
    )
    yR=abs(x$RestM-x$known)
    yF=abs(x$FestM-x$known)
    text(
      x=rep(600,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$RestM>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yR),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yR),1)," lbs")
      ),
      col='orange'
    )
    text(
      x=rep(1000,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$FestM>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yF),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yF),1)," lbs")
      ),
      col='blue'
    )
  })
  output$LastPlot=renderPlot({
    x=newResults()
    plot(
      x$RestL~x$known,
      xlim=c(0,1200),
      ylim=c(0,1200),
      col='orange',
      pch=1,
      xlab="Known Weight (lbs)",
      ylab="Estimated Weight (lbs)",
      main="Large Fish Last"
    )
    points(
      x$FestL~x$known,
      pch=4,
      col='blue'
    )
    lines(
      seq(0,1200,1)~seq(0,1200,1),
      lty=2
    )
    legend(
      "topleft",
      legend=c("Random n","First n"),
      col=c('orange','blue'),
      pch=c(1,4)
    )
    yR=abs(x$RestL-x$known)
    yF=abs(x$FestL-x$known)
    text(
      x=rep(600,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$RestL>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yR),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yR),1)," lbs")
      ),
      col='orange'
    )
    text(
      x=rep(1000,3),
      y=c(200,150,100),
      labels=c(
        paste0("p = ",round(nrow(subset(x,x$FestL>x$known))/nrow(x),2)),
        paste0("Total Error = ",round(sum(yF),1)," lbs"),
        paste0("Avg. Error = ",round(mean(yF),1)," lbs")
      ),
      col='blue'
    )
  })
}
## Run application
shinyApp(ui=ui,server=server)