# Example for Jags-Ymet-Xnom2fac-MrobustHet.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
# graphics.off() # This closes all of R's graphics windows.
# rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
#Load The data file 

fileNameRoot = "Assn3" 
graphFileType = "png" 
myDataFrame = read.csv("SocialSecLong.csv")
# Re-label and re-order the Pos factor:
myDataFrame$payment_type = factor(myDataFrame$payment_type, ordered=F)

myDataFrame$State = factor(myDataFrame$State, ordered=F)

# Specify the column names in the data file relevant to the analysis:
yName="amount" 
# x1 should be factor with fewer levels, to plot in single pane:
x1Name="payment_type" 
x2Name="State" 
# Specify desired contrasts.
# Each main-effect contrast is a list of 2 vectors of level names, 
# a comparison value (typically 0.0), and a ROPE (which could be NULL):

x1contrasts = list( 
  
  # family_tax_benefit_a
  list( c("family_tax_benefit_a") , c("family_tax_benefit_b") , compVal=0.0 , ROPE=c(-1000,1000) ) ,
  list( c("family_tax_benefit_a") , c("healthcare_card") , compVal=0.0 , ROPE=c(-1000,1000) ) ,
  
  # family_tax_benefit_b
  list( c("family_tax_benefit_b") , c("healthcare_card") , compVal=0.0 , ROPE=c(-1000,1000) )
)
# unique(myDataFrame$State)
x2contrasts = list( 
  
  # VIC
  list( c("Victoria") , c("New South Wales") , compVal=0.0 , ROPE=c(-1000,1000) ) ,
  list( c("Victoria") , c("Queensland") , compVal=0.0 , ROPE=c(-1000,1000) ) ,

  
  # NSW
  list( c("New South Wales") , c("Queensland") , compVal=0.0 , ROPE=c(-1000,1000) )

)

# Each interaction contrast is a list of 2 lists of 2 vectors of level names, 
# a comparison value (typically 0.0), and a ROPE (which could be NULL)::
x1x2contrasts = list( 
  list( list( c("family_tax_benefit_a") , c("healthcare_card") ) ,
        list( c("New South Wales") , c("Victoria") ) ,
        compVal=0.0 , ROPE=c(-1000,1000) ) ,
  list( list( c("family_tax_benefit_b") , c("healthcare_card") ) ,
        list( c("New South Wales") , c("South Australia") ) ,
        compVal=0.0 , ROPE=c(-1000,1000) ) ,
  list( list( c("family_tax_benefit_a") , c("family_tax_benefit_b","healthcare_card") ) ,
        list( c("Victoria") , c("New South Wales","Queensland") ) , 
        compVal=0.0 , ROPE=c(-1000,1000) )
) 

#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xnom2fac-MrobustHet.Assn3.R")

#------------------------------------------------------------------------------- 
# Generate the MCMC chain:

mcmcCoda = genMCMC( datFrm=myDataFrame , 
                    yName=yName , x1Name=x1Name , x2Name=x2Name ,
                    numSavedSteps=25000 , thinSteps=10 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) 
show( parameterNames ) # show all parameter names, for reference
for ( parName in c("b0","b1[1]","b2[1]","b1b2[1,1]","ySigma[1,1]","ySigma[1,7]","ySigma[5,7]","nu") ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        datFrm=myDataFrame , x1Name=x1Name , x2Name=x2Name ,
                        x1contrasts=x1contrasts , 
                        x2contrasts=x2contrasts , 
                        x1x2contrasts=x1x2contrasts ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , 
          datFrm=myDataFrame , yName=yName , x1Name=x1Name , x2Name=x2Name ,
          x1contrasts=x1contrasts , 
          x2contrasts=x2contrasts , 
          x1x2contrasts=x1x2contrasts ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
# Other specific comparisons of cells:
if ( fileNameRoot == "Assn3" ) {
  # THIS x1level minus THAT x1level at AT x2level:
  THISx1 = "family_tax_benefit_a"
  THATx1 = "healthcare_card"
  ATx2 = "New South Wales"
  THISidx = which(levels(myDataFrame[,x1Name])==THISx1)
  THATidx = which(levels(myDataFrame[,x1Name])==THATx1)
  ATidx   = which(levels(myDataFrame[,x2Name])==ATx2)
  openGraph(height=4,width=4)
  compInfo = plotPost( 
    as.matrix(mcmcCoda)[,paste("m[",THISidx,",",ATidx,"]",sep="")] -
      as.matrix(mcmcCoda)[,paste("m[",THATidx,",",ATidx,"]",sep="")] , 
    main=paste(THISx1,"-",THATx1,"@",ATx2) , 
    xlab=paste("Difference in",yName) , 
    compVal=0 ,ROPE=c(-1000,1000) )
  show(compInfo)
  saveGraph(file=paste(fileNameRoot,THISx1,"-",THATx1,"At",ATx2,sep=""),
            type=graphFileType)
  # THIS x1level minus THAT x1level at AT x2level:
  THISx1 = "family_tax_benefit_a"
  THATx1 = "healthcare_card"
  ATx2 = "Victoria"
  THISidx = which(levels(myDataFrame[,x1Name])==THISx1)
  THATidx = which(levels(myDataFrame[,x1Name])==THATx1)
  ATidx   = which(levels(myDataFrame[,x2Name])==ATx2)
  openGraph(height=4,width=4)
  compInfo = plotPost( 
    as.matrix(mcmcCoda)[,paste("m[",THISidx,",",ATidx,"]",sep="")] -
      as.matrix(mcmcCoda)[,paste("m[",THATidx,",",ATidx,"]",sep="")] , 
    main=paste(THISx1,"-",THATx1,"@",ATx2) , 
    xlab=paste("Difference in",yName) , 
    compVal=0 ,ROPE=c(-1000,1000) )
  show(compInfo)
  saveGraph(file=paste(fileNameRoot,THISx1,"-",THATx1,"At",ATx2,sep=""),
            type=graphFileType)
  # THIS x2level minus THAT x2level at AT x1level:
  THISx2 = "Queensland"
  THATx2 = "Victoria"
  ATx1 = "family_tax_benefit_a"
  THISidx = which(levels(myDataFrame[,x2Name])==THISx2)
  THATidx = which(levels(myDataFrame[,x2Name])==THATx2)
  ATidx   = which(levels(myDataFrame[,x1Name])==ATx1)
  openGraph(height=4,width=4)
  compInfo = plotPost( 
    as.matrix(mcmcCoda)[,paste("m[",ATidx,",",THISidx,"]",sep="")] -
      as.matrix(mcmcCoda)[,paste("m[",ATidx,",",THATidx,"]",sep="")] , 
    main=paste(THISx2,"-",THATx2,"@",ATx1) , 
    xlab=paste("Difference in",yName) , 
    compVal=0 ,ROPE=c(-1000,1000) )
  show(compInfo)
  saveGraph(file=paste(fileNameRoot,THISx2,"-",THATx2,"At",ATx1,sep=""),
            type=graphFileType)
}
#-------------------------------------------------------------------------------