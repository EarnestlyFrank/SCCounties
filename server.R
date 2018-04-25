require(leaflet)
require(RColorBrewer)
require(scales)
require(lattice)
require(dplyr)
require(rgdal)
require(rgeos)

function(input, output, session) {
  ##Data import
  inputs <- read.delim("Extracted DataNoError.txt")
  
  names <- c(
    "Number of self-employed adults",
    "Percent of working adults who are self-employed",
    "Number of children without health insurance",
    "Percent of children without health insurance",
    "Median family income",
    "Percent of adults with a high school education or higher",
    "Percent of adults with a bachelor degree or higher",
    "Number of households living in the same house as last year",
    "Percent of households living in the same house as last year",
    "Median value of owner occupied housing units",
    "Number of vacant housing units",
    "Percent of housing units vacant",
    "Number of housing units without a full kitchen",
    "Percent of housing units without a full kitchen",
    "Percent turnout in 2016 general election",
    "Median age",
    "Male life expectancy",
    "Female life expectancy"
  )
  
  colnames(inputs) <- names
  
  
  County <- readOGR("SC/SC_County.shp",
                    layer = "SC_County",
                    GDAL1_integer64_policy = TRUE)
  County <- spTransform(County, CRS("+proj=longlat +ellps=GRS80"))
  
  centers <- data.frame(gCentroid(County, byid = T))
  
  tagText <- function(cName, depVar, indepVar, cID)
    as.character(paste(
      tags$h4(cName),
      tags$br(),
      paste0("Independent: ", names[which(names(inputs) == indepVar)], ": ", inputs[cID, which(names(inputs) == indepVar)]),
      tags$br(),
      paste0("Dependent: ", names[which(names(inputs) == depVar)], ": ", inputs[cID, which(names(inputs) == depVar)])
    ))
  
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(
      data = County,
      options = leafletOptions(
        crs = leafletCRS(proj4def = County@proj4string),
        zoomControl = FALSE,
        minZoom = 8,
        maxZoom = 8,
        dragging = FALSE
      )
    ) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = -80.9,
              lat = 33.9,
              zoom = 8) %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.75,
        opacity = 1.0,
        fillOpacity = 0.5,
        fillColor = "blue",
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = T
        ),
        layerId = County@data$ID
      )
  })
  
  
  output$qqPlot <- renderPlot({
    qqnorm(resid(lm(inputs[, which(names(inputs) == input$dep)] ~ inputs[, which(names(inputs) == input$indep)],
                    data = inputs)))
  })
  
  output$histIndep <- renderPlot({
    hist(inputs[, which(names(inputs) == input$indep)],
         xlab = names[which(names(inputs) == input$indep)],
         main = "Independent Variable")
  })
  
  output$histDep <- renderPlot({
    hist(inputs[, which(names(inputs) == input$dep)],
         xlab = names[which(names(inputs) == input$dep)],
         main = "Dependent Variable"
    )
  })
  
  output$scatter <- renderPlot({
    plot(
      inputs[, which(names(inputs) == input$dep)] ~ inputs[, which(names(inputs) == input$indep)],
      data = inputs,
      xlim = range(inputs[, which(names(inputs) == input$indep)]),
      ylim = range(inputs[, which(names(inputs) == input$dep)]),
      xlab = names[which(names(inputs) == input$indep)],
      ylab = names[which(names(inputs) == input$dep)]
    )
    
    abline(lm(inputs[, which(names(inputs) == input$dep)] ~ inputs[, which(names(inputs) == input$indep)]))
  })
  
  output$analysis <- renderText({
    deps <- inputs[, which(names(inputs) == input$dep)]
    indeps <- inputs[, which(names(inputs) == input$indep)]
    model <- lm(deps ~ indeps, data = inputs)
    results <- anova(model)
    
    sumText <- paste(
      "<br>The independent variable has a median of ", median(indeps), ", a mean of ", signif(mean(indeps),4), ", and a standard deviation of ", signif(sd(indeps),4), "<br/>",
      "<br>The dependent variable has a median of ", median(deps), ", a mean of ", signif(mean(deps),4), ", and a standard deviation of ", signif(sd(deps),4), "<br/>", sep = ""
    )
    
    if (results$`Pr(>F)`[1] < 0.05) {
      conclude <- paste(
        "The linear regression has a p-value less than 0.05, meaning there is sufficient evidence to conclude that ",
        tolower(input$indep),
        " is connected to ",
        tolower(input$dep),
        ". The equation is as follows: <br/>y = ", signif(as.numeric(model$coefficients[1]), 4), " + ", signif(as.numeric(model$coefficients[2]), 4), "x.<br/>", 
        "This means that for every increase of 1 unit in ", tolower(input$indep), 
        " there is a change of ", signif(as.numeric(model$coefficients[2]), 4), " +/- ", signif(as.numeric(summary(model)$coefficients[4]), 4), " units in ", tolower(input$dep), ". ",
        sep = ""
      )
    }
    else{
      conclude <- paste(
        "There is not sufficient evidence to conclude that ",
        tolower(input$indep),
        " is connected to ",
        tolower(input$dep),
        ".",
        sep = ""
      )
    }
    
    
    if (input$numIn < max(inputs[, which(names(inputs) == input$indep)]) &&
        input$numIn > min(inputs[, which(names(inputs) == input$indep)])) {
      x.value <- data.frame(indeps = input$numIn)
      # x.indep[1:length(inputs[, 1])] <- input$numIn
      # x.indep <- as.data.frame(x.indep)
      
      confintValues <-
        predict(
          model,
          x.value,
          type = "response",
          interval = "prediction",
          level = 0.95
        )
      
      confintText <- paste(
        "With 95% confidence, the ",
        tolower(input$dep),
        " in a county with a ",
        tolower(input$indep),
        " equal to ",
        input$numIn,
        " is (",
        signif(confintValues[2], 4),
        ", ",
        signif(confintValues[3], 4),
        ").",
        sep = ""
      )
    }
    else{
      
      confintText <- "The input value is outside the scope of the model. It would be improper to extrapolate. "
      
    }
    
    
    
    corText <- paste(
      "The correlation (R<sup>2</sup>) between ",
      tolower(input$indep),
      " and ",
      tolower(input$dep),
      " is ",
      signif(cor(inputs[, which(names(inputs) == input$dep)], inputs[, which(names(inputs) == input$indep)], use = "c")^2, 4),
      ". This means that ", 100*signif(cor(inputs[, which(names(inputs) == input$dep)], inputs[, which(names(inputs) == input$indep)], use = "c")^2, 4),
      "% of the variation between the two variables can be explained by the regression model. ",
      "The R<sup>2</sup> value has a P-value of ",
      signif(cor.test(inputs[, which(names(inputs) == input$dep)], inputs[, which(names(inputs) == input$indep)], use = "c")$p.value, 4),
      sep = ""
    )
    
    HTML(paste0(sumText,
                '<br/>',
                conclude,
                '<br/><br/>',
                confintText,
                '<br/><br/>',
                corText))

  })
  
  
  
  showCountyPopup <- function(cID, lat, lng) {
    cName = County@data$GEO_id2[cID]
    content <- tagText(cName, input$dep, input$indep, cID)
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = cID)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showCountyPopup(event$id, event$lat, event$lng)
    })
  })
  
}