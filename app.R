library(shiny)
library(lubridate)

GeoData <- read.table('GeoDAT.txt', header=TRUE)
RefCities <- read.table('RefCities.txt', header=TRUE)
CropData <- read.table('CROPWAT_Kc.txt', header=TRUE)
CropSL <- read.table('CropStageLength.txt', header=TRUE)
Stock_A <- read.table('Stock_A.txt', header=TRUE)
Stock_B <- read.table('Stock_B.txt', header=TRUE)

Kc = data.frame(ini=CropData[,2], dev=rowMeans(CropData[,2:3]), mid=CropData[,3], lat=CropData[,4])
rownames(Kc) = CropData[,1]

Files <- list.files(path = paste(getwd(),'/Datasets',sep=''), pattern='_eto.txt')

nFile <- length(Files)

FileList <- character(nFile)
LFiles <- list()

for(i in 1:nFile){
 FileList[i] <- substr(Files[i], 1, nchar(Files[i])-8)
 assign(FileList[i], read.table(paste('Datasets/', Files[i],sep=''), header=TRUE))
 LFiles[[i]] <- get(FileList[i])
 rownames(LFiles[[i]]) <- month.name
}

names(LFiles) <- FileList

KNN = 3

ui <- fluidPage(

 titlePanel('Ruyesh Irrigation Calculator'),

  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
  
     selectInput('province', label='Province', choices=unique(GeoData$Province), selected=unique(GeoData$Province)[1]),
     uiOutput('city'),
     radioButtons('type', 'Type:',c('Farm' = 'frm', 'Greenhouse' = 'grnhs')),
     uiOutput('Crops'),
     dateInput('pdate', 'Planting Date:'),
     numericInput(inputId = 'area',
                   label = 'Farm or Greenhouse Area (in hec):',value = 100),
     fileInput('file1', 'Image file of your farm or greenhouse (optional)',
                multiple = TRUE, accept = c('image/*')),
     helpText('Note: Please press button below to update the plot.'),
     downloadButton('downData', 'Download'),
     width = 3
   ),

 mainPanel(

      h3(textOutput('ruyesh', container = span)),

      tabsetPanel(type = 'tabs',
                  tabPanel('Climate and ET Plot', plotOutput('plot1')),
                  tabPanel('Irrigantion Calendar', verbatimTextOutput('IrrCal')),
                  tabPanel('Irrigantion Plot', plotOutput('plot2')),
                  tabPanel('Stock A Table', tableOutput('FertA')),
                  tabPanel('Stock B Table', tableOutput('FertB')),
                  tabPanel('Weather', htmlOutput('frame'))             
   )
  )
 )
)


server <- function(input, output){

 output$city <- renderUI({
  selectInput('cities', 'City', choices = GeoData$City[GeoData$Province == input$province],
               selected=GeoData$City[1])
  }
 )

 FGcoef <- reactive({switch(input$type, frm = 1, grnhs = 2, 1)
  })

 CropDB <- reactive({switch(input$type, frm = CropData$CROP, grnhs = intersect(Stock_A$CROP, CropData$CROP), CropData$CROP)
  })

 output$Crops <- renderUI({
  selectInput('crop', 'Crops', 
              choices = CropDB())
  }
 )

 cityLoc <- reactive({
  GeoData[which(GeoData$City == input$cities, arr.ind=TRUE), 3:4]
 })

 SrcStrng <- reactive({paste('https://forecast.io/embed/#lat=', cityLoc()[1],
                             '&lon=', cityLoc()[2], '&name=', input$cities, sep='')
 })

 output$frame <- renderUI({
    tags$iframe(seamless = 'seamless', 
                src = SrcStrng(), 
                height = 250, width = 700)
  })

 dist <- reactive({
  matrix(c(RefCities[,2]-as.numeric(cityLoc()[1]), RefCities[,3]-as.numeric(cityLoc()[2])),
                      nr=nrow(RefCities), nc=2)
 })

 KnearLoc <- reactive({
  order(rowSums(dist()^2))[1:KNN]
 })

 KIDW <- reactive({
  KSrtDst <- sort(rowSums(dist()^2))[1:KNN]
  invdist <- 1/KSrtDst
  W <- invdist/sum(invdist)
  if(KSrtDst[1] == 0){W[1:KNN] <- c(1, 0, 0)}
 return(W)
 })

 Temp <- reactive({KIDW()[1]*LFiles[[KnearLoc()[1]]]})
 ClimData <- reactive({
  for(i in 2:KNN){TempClim <- Temp() + KIDW()[i]*LFiles[[KnearLoc()[i]]]}
 return(round(TempClim, digits=2))
 })

 ETo <- reactive({ClimData()[,3]})

 CropNum <- reactive({which(rownames(Kc) == input$crop)})

 vSL <- reactive({CropSL[which(rownames(CropSL) == input$crop),]})
 vdate <- reactive({c(vSL()[1], dev=sum(vSL()[1:2]), mid=sum(vSL()[1:3]), lat=sum(vSL()[1:4]))})
 irrdate <- reactive({c(input$pdate, input$pdate + unlist(vdate()))})

 datnum <- reactive({
  d_tmp <- month(irrdate())
  return(d_tmp)
 })

 irrmnth <- reactive({
  mnth_tmp <- list()
  for(i in 1:4){
   if(datnum()[i] > datnum()[i+1]){mnth_tmp[[i]] <- c(datnum()[i]:12, 1:datnum()[i+1])}else{
     mnth_tmp[[i]] <- datnum()[i]:datnum()[i+1]}
  }
  return(mnth_tmp)})

 ETc <- reactive({
  ETc_tmp <- list()
  for(i in 1:4){
   ETc_tmp[[i]] <- ETo()[irrmnth()[[i]]]*Kc[CropNum(),i]
  }
  return(ETc_tmp)
 })

 P_eff <- reactive({ClimData()[,2]/days_in_month(1:12)})

 P_eff_s <- reactive({
  Peff_tmp <- list()
  for(i in 1:4){
   Peff_tmp[[i]] <- P_eff()[irrmnth()[[i]]]
  }
  return(Peff_tmp)
 })

 Irr_Net_v <- reactive({
  Irrnet_tmp <- list()
  Irr_vol <- list()
  for(i in 1:4){
   Irrnet_tmp[[i]] <- ifelse(ETc()[[i]] <= P_eff_s()[[i]], 0, ETc()[[i]] - P_eff_s()[[i]])
   Irr_vol[[i]] <- input$area * Irrnet_tmp[[i]] *0.1
  }
  Irr_v_vec <- round(unlist(Irr_vol), digits=3)
  return(Irr_v_vec)
 })


  output$plot1 <- renderPlot({
    barplot(t(ClimData()),beside=TRUE, col=c('red','blue','green'))
    legend('top', colnames(ClimData()), fill=c('red','blue','green'), horiz=TRUE)
  })

  output$IrrCal <- renderPrint({
   list(Dates = irrdate(), Irrigation = FGcoef()*Irr_Net_v())
  })

  output$plot2 <- renderPlot({
   plot(FGcoef()*Irr_Net_v(), type='b', xlab='month', ylab='Net Irrigation (cubic meter)', col='red', lwd=2, pch=16)
  })

  output$FertA <- renderTable({
   Stock_A[Stock_A$CROP == input$crop,]
  })

  output$FertB <- renderTable({
   Stock_B[Stock_A$CROP == input$crop,]
  })

  output$downData <- downloadHandler(
    filename = function() {
      paste(input$cities, '.csv', sep = '')
    },
    content = function(file) {
      write.csv(ClimData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui=ui, server=server)