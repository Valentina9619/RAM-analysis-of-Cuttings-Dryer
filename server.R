# Solicitamos las librerias que necesitaremos
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(patchwork)
library(DT)
library(mathjaxr)
library(plotly)

#Data
dataRandF <- data.frame(
    Months = c(0, 1, 3, 6, 9, 12),
    Probability = c(0, 0.09940, 0.27402, 0.47303, 0.61752, 0.72242, 1, 0.90060, 0.72598, 0.52697, 0.38248, 0.27758),
    Indicator = c("Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability", "Non-Reliability",
                  "Reliability", "Reliability", "Reliability", "Reliability", "Reliability", "Reliability")
)

PrimerMes <- subset(Original, Original$One < 20)
SegundoMes <- subset(PrimerMes, PrimerMes$Two < 20)
TercerMes <- subset(SegundoMes, SegundoMes$Three < 20)
CuartoMes <- subset(TercerMes, TercerMes$Four < 20)
QuintoMes <- subset(CuartoMes, CuartoMes$Five < 20)
SextoMes <- subset(QuintoMes, QuintoMes$Six < 20)
NovenoMes <- subset(SextoMes, SextoMes$Nine < 20)
BD <- subset(NovenoMes, NovenoMes$Twelve < 20)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$FRawData <- DT::renderDataTable(
        DT::datatable({
            FailureRate
        },
        
        filter = "top",
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = c("Component","Function","Failure Rate [failure/hour]","Reference")
        )) 
    
    output$RRawData <- DT::renderDataTable(
        DT::datatable({
            RepairRate
        },
        
        filter = "top",
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = c("Component","Function","Min Repair Time [hour]","Max Repair Time [hour]","Reference")
        )) 
    
    output$RandF <- renderPlot({
        q = ggplot(dataRandF, aes(x=Months, y=Probability, col=Indicator)) + geom_line(size=1)
        q + labs(title = "Reliability down, Non-Reliability up") +
            labs(caption = "(based on FT quantification)") +
            theme_minimal()
        
    })
    
    output$Prueba <- renderPlotly({
        p <- ggplot(
            dataRandF,
            aes(x = Months, y = Probability, group = Indicator, color = Indicator)
        ) +
            geom_line(size=0.5) +
            scale_color_manual(values = c("#353436", "#1b98e0")) +
            labs(title = "Reliability down, Non-Reliability up",x = "Months", y = "Probability") +
            labs(caption = "(based on FT quantification)") +
            theme(legend.position = "top")
        ggplotly(p)
        
    })
    
    h = hist(BD$One)
    u = hist(BD$Two)
    g = hist(BD$Three)
    s = hist(BD$Four)
    l = hist(BD$Five)
    z = hist(BD$Six)
    e = hist(BD$Nine)
    t = hist(BD$Twelve)
    
    output$RepairTimesSimulated = renderPlot({
        if(input$plots == "All") {
            par(mfrow = c(2, 4))
        }
        if(is.element(input$plots, c("One Month (730 hours)", "All"))) {
            h$density = h$counts/sum(h$counts)*100
            plot(h,freq=FALSE, main = "One Month (97.3% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#1568c6")
            
        }
        if(is.element(input$plots, c("Two Months (1460 hours)", "All"))) {
            u$density = u$counts/sum(u$counts)*100
            plot(u,freq=FALSE, main = "Two Months (97.3% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#82b6f2")
            
        }
        if(is.element(input$plots, c("Three Months (2190 hours)", "All"))) {
            g$density = g$counts/sum(g$counts)*100
            plot(g,freq=FALSE, main = "Three Months (97.3% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#1568c6")
            
        }
        if(is.element(input$plots, c("Four Months (2920 hours)", "All"))) {
            s$density = s$counts/sum(s$counts)*100
            plot(s,freq=FALSE, main = "Four Months (97.1% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#82b6f2")
            
        }
        if(is.element(input$plots, c("Five Months (3650 hours)", "All"))) {
            l$density = l$counts/sum(l$counts)*100
            plot(l,freq=FALSE, main = "Five Months (97.1% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#1568c6")
            
        }
        if(is.element(input$plots, c("Six Months (4380 hours)", "All"))) {
            z$density = z$counts/sum(z$counts)*100
            plot(z,freq=FALSE, main = "Six Months (97.1% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#82b6f2")
            
        }
        if(is.element(input$plots, c("Nine Months (6570 hours)", "All"))) {
            e$density = e$counts/sum(e$counts)*100
            plot(e,freq=FALSE, main = "Nine Months (97.4% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#1568c6")
            
        }
        if(is.element(input$plots, c("Twelve Months (8760 hours)", "All"))) {
            t$density = t$counts/sum(t$counts)*100
            plot(t,freq=FALSE, main = "Twelve Months (97.3% of data)", xlab = "Time [hours]", ylab = "Probability", ylim = c(0, 35), col="#82b6f2")
            
        }
    })
    
    output$AvailabilityTime <- DT::renderDataTable(
        DT::datatable({
            AvailabilityT
        },
        
        filter = "top",
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = c("Time [months]","MTBF [hours]","MTTR [hours]","Availability", "5th Percentile", "95th Percentile")
        ))
    
    output$ImportanceM <- DT::renderDataTable(
        DT::datatable({
            ImportanceMeasures
        },
        
        filter = "top",
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = c("Component","Birnbaum","Criticality","RRW", "RAW", "FV")
        ))
    
})

