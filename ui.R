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


# Define UI for RAM analysis application 
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  
                  # Application title
                  titlePanel("Reliability, Availability and Maintainability Analysis for Drilling Rigs' Cuttings Dryer"),
                  navbarPage("Let's get started",
                             # Home
                             tabPanel("Home", icon = icon("home"),
                                      fluidRow(column(tags$img(src="Cuttings1.PNG", width="210px", height="200px"), width = 2),
                                               column(
                                                   br(),
                                                   p("Offshore drilling operations consist of a complex system carried out in extreme
                                       conditions. Their operational safety relies on a series of well barriers elements
                                       (WBE), that alone or combined can prevent unintentional flows of fluids or gases from the formation
                                       either into another formation or to the surface. During well construction and workover meaneuvers, 
                                       drilling fluid column is always employed as a primary barrier, a configuration that by itself shows 
                                       the drilling fluid relevance for the overall operational safety and reliability. Besides that, the fluid 
                                       column also transports the cuttings from the well. The cuttings and the fluid that reach the surface are
                                       important materials for geological research, providing information about the reservoirs formation (Petri,
                                       2017). 
                                       
                                       
                                       Before being pumped back on the wellbore, the drilling fluid must be cleaned out from the carried cuttings.
                                       This operation is done in stages, in a process that although seemingly simple, is responsible for several
                                       hours of downtime since its unavailability forces the drilling unit to stop operations.",
                                                     style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                   width = 8),
                                               column(
                                                   br(),
                                                   tags$img(src="LabRisco.PNG",width="200px",height="60px"),
                                                   br(),
                                                   br(),
                                                   p("For more information please check the",
                                                     em("Analysis, Evaluation and Risk Management Laboratory"), "page clicking",
                                                     br(),
                                                     a(href="https://www.labrisco.usp.br/home_br", "Here",target="_blank"),
                                                     style = "text-align:center;color:black"),
                                                   width = 2)
                                               
                                      ),
                                      fluidRow(column(tags$img(src="Cuttings2.PNG", width="210px", height="200px"), width = 2),
                                               column(
                                                 br(),
                                                 p("This app presents a particular configuration of a", strong("cuttings dryer system"), "employed in a drill ship operating
                                                 in the Brazilian Pre-Salt, consisting in a solid/liquid separation system that aims both a maximum fluid recovery 
                                                 together with the compliance to Brazilian environmental regulations regarding the disposal of solids from drilling 
                                                 operations. 
                                                 
                                                 
                                                 In the ", strong("App"), "you could find the functional analysis results, the most important findings of reliability analysis
                                                   as well as the critical compenents identified from Importance Measures. Furthermore, in the tab Maintainability Analysis, 
                                                   the repair times simulates are exposed and in Availability Analysis the reader could find the 90% confidence level of
                                                   availability of the system.",
                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 width = 8),
                                               hr(),
                                               p(em("Developed by"), br("Maria Valentina Clavijo Mesa"), style = "text-align:center; font-family: times")
                                              )
                                      
                                      
                             ), # Home closed
                             
                             
                             # Functional Analysis
                             tabPanel("Functional Analysis", icon = icon("bookmark"),
                                      fluidRow(column(width = 2),
                                               column(h4(p(strong("System Description"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 2, icon("hand-point-right","fa-5x"), align = "center"),
                                               column(
                                                   p("The cuttings dryer system is responsible for the treatment of the drilling fluid in the
                                                     topside of the rig, removing the rock cuttings originating from the drilling operations
                                                     and eventually transported to the surface along with fluid itself.
                                                     
                                                     Basically, the system is composed of three stages of separation, that take place on the",strong("(a) shakers,
                                                     (b) cuttings dryer"),"and", strong("(c) centrifuge."), "The cuttings that come out of the shakers
                                                     (first separation) are taken to the dryer by means of a conveyor screw. After the dryer (where 
                                                     the second separation takes place), the recovered fluid is taken to a fluid recovery tank (also
                                                     called Catch Tank), and through a centrifugal pump it enters the centrifuge, where the third separation
                                                     takes place. At this point, the fluid can either be safely discarded or returned to a recovery tank 
                                                     compartment. Finally, the centrifuged drilling fluid is transferred to the rig's fluid module for reuse
                                                     in operation.", style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                   br(),
                                                   tags$img(src="FunctionDiagram.PNG",width="700px",height="250px", 
                                                            style="display: block; margin-left: auto; margin-right: auto;"),
                                                   br(),
                                                   br(),
                                                   p("The system also contains a vacuum system used to suck the cuttings through the suction points and deposit it in the dryer.
                                                   Note that this description corresponds to the general arrangement of the Cuttings Dryer System.", 
                                                     style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                   width=8,style="background-color:lavender;border-radius: 10px")
                                               
                                               
                                      ),
                                      br(),
                                      fluidRow(column(width = 2),
                                               column(p("Let's do it. You are going to find the functional diagrams of the Vertical Cuttings Dryer analyzed, in order to identify 
                                                         technical and operational characteristics of system. Furthermore, Failure and Repair data used in the analysis will be 
                                                         presented.", style = "color:black;text-align:center"),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                               
                                      ),
                                      br(),
                                      fluidRow(column(width = 2),
                                               column(h4(p(strong("Functional Diagrams and Collected Data"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      hr(),
                                      
                                      tabsetPanel(
                                        tabPanel("Functional Diagrams",
                                                 
                                                 br(),
                                                 navlistPanel(widths = c(2,9), fluid = T, well = T,
                                                              tabPanel("System",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="SystemCD.JPG",width="750px",height="450px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=11,style="border:1px solid black")
                                                                         ), width = 11
                                                                       ))),
                                                              
                                                              tabPanel("Operational Characteristics",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("Some operational conditions that must be considered in the study are: "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("- Regarding flow line shakers, screw conveyors and Low Voltage (LV) switchboards (480V),
                                                                                         the analysis considers that they have an active redundancy, that is, the two units of each
                                                                                         equipment are functioning and connected, to obtain a system in parallel sharing charge. ", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- The boundary of the cuttings dryer system starts at the shakers and ends with the disposal of the
                                                                                    cuttings at sea or the return of the fluid to the rig's fluid module.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- It is assumed that the operator performs his duties in accordance with established operating procedures
                                                                                    (human reliability is not assessed).", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=10,style="border:1px solid black")
                                                                         ), width = 11
                                                                       ))),
                                                              
                                                              tabPanel("Technical Characteristics",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("Some technical conditions that must be considered in the study are: "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("- The fluid transported to the surface enters the system through the shakers, in this first stage, through
                                                                                    the vibratory movement of the shakers, the collected cuttings receive a first separation. The mud cleaner is
                                                                                    responsible for the recovery of particles, so the joint function of the shakers and the mud cleaner allows to
                                                                                    obtain a percentage of clean drilling fluid that is directly recovered. However, in this first stage it is not
                                                                                    possible to recover 100% of the fluid and for this reason the solid control equipment provides treated cuttings
                                                                                    that still needs to be sent to the cuttings dryer.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- The screw conveyors are located on the front of the solid control equipmennt, in order to collect and load the
                                                                                  residues from the shakers and transport them to the cuttings dryer to continue the drying process.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Once cuttings are introduced into the dryer's charge hopper, widely spaced, independently adjustable flights
                                                                                    continuously direct cuttings to the screen surface. Flights within the VERTI-G dryer create a rolling action that
                                                                                    promotes efficient separation and prevents screen plugging. Under high G forces created by the large cone diameter,
                                                                                    liquid-solids separation occurs instantly as cuttings make contact with the finer-mesh, high-capacity centrifuge screen.
                                                                                    The result is clean return fluid and dry solids discharge (GNSC, 2019). For disposal at sea, there is a sea water
                                                                                    manifold that helps in carrying the cuttings generated by the dryer.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- The recovered fluid is transferred by gravity to a catch tank compartment. The moyno transfer pump sucks the fluid 
                                                                                    from that compartment and transfers it to the centrifuge; the centrifuge reduces the amount of synthetic fluid in the
                                                                                    solid residues to less than 5% by mass, generating greater recovery of the fluid. Note that at this point the third
                                                                                    separation takes place, that is, a part of the fluid is discarded and another part is returned to a container box.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- From the container box, the centrifuged fluid is directed by gravity to the other compartment of the catch tank. In 
                                                                                    the compartment that stores the centrigued fluid there is a second moyno pump (circulation moyno pump) that stucks the
                                                                                    processed fluid, directing it to the fluid manifold from where, finally, the fluid is transferred to the rig's fluid module.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- There is a contingency plan in case of failure of the screw conveyors, this plan employs the belly funnel for the accumulation
                                                                                    of untreated cuttings, which will later be sucked with the vacuum system for processing by the cuttings dryer system.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=11,style="border:1px solid black")
                                                                         ), width = 11
                                                                       ))),
                                                              
                                                              tabPanel("Functional Tree",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="Funcional.jpg",width="880px",height="300px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"),
                                                                                  br(), width=12,style="border:1px solid black")
                                                                         ), width = 11
                                                                       )))
                                                              )
                                                 ),
                                        tabPanel("Data",
                                                 
                                                 br(),
                                                 navlistPanel(widths = c(2,9), fluid = T, well = T,
                                                              tabPanel("Failure Data",
                                                                      tags$style(".fa-database {color:#227fe8}"),
                                                                      h3(p(em("Failure Dataset "), icon("database",lib = "font-awesome"), style ="color:black;text-align:center")),
                                                                      fluidRow(column(br(),DT::dataTableOutput("FRawData"), width = 11)
                                                                       )),
                                                              
                                                              tabPanel("Repair Data",
                                                                       tags$style(".fa-database {color:#227fe8}"),
                                                                       h3(p(em("Repair Dataset "), icon("database",lib = "font-awesome"), style ="color:black;text-align:center")),
                                                                       fluidRow(column(br(),DT::dataTableOutput("RRawData"), width = 11)
                                                                       ))
                                                 )
                                                 
                                                 )
                                        
                                        
                                      ),
                                      br(),
                                      br(),
                                      fluidRow(),
                                      
                             ), # Functional Analysis closed
                             
                             
                             # Reliability Analysis
                             tabPanel("Reliability Analysis", icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Concept of Reliability"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3, tags$img(src="Rcurve.JPG",width="300px",height="250px", 
                                                                          style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                               column(
                                                 p("Reliability has many connotations. In general, it refers to the ability of an item (a product or a system) to
                                                 perform its", em("function")," under designed operating ", em("conditions")," for a designated", em("period of 
                                                 time or number of cycles"), " (Modarres, Kaminskiy, & Krivtsov, 2009). The ability of an item to perform its function is normally
                                                 designated through a", em("probability"), " (the probabilistic connotation). The probabilistic treatment of an item reliability, according
                                                 to the definition above, can be summarized by", style = "color:black;text-align:justify"),
                                                 withMathJax(),
                                                    p("$$R(~t~)=(T \\geq T'|C_1, C_2, ... , C_n)$$",style="color:black;border:1px solid black;background-color:white"),
                                                 p("Where", em("T'")," is the designated period of time or number of cycles for the item's operation (e.g., mission time) when time
                                                 or cycle of application is the aggregated agent of failure and is the strength, endurance limit, or performance requirements when 
                                                 stress-strength, damage-tolerance or performance-requirements models are used.", em("T")," is the time to failure or cycle to failure 
                                                 when time or application cycle is the agent of failure and is the stress, amount of damage, or performance of the item when stress-strength,
                                                 damage-tolerance, or performance-requirements models are used.", em("R(t)"), " is the reliability of the item at time or application cycle t
                                                   after which the mission is completed, and", em("C1, C2, ... , Cn"), "are the designated conditions, such as environmental conditions.",
                                                   style = "color:black;text-align:justify"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Reliability Techniques"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3, icon("hand-point-right","fa-5x"), align = "center"),
                                               column(
                                                 p("During reliability analysis, the analyst should select the analytical logic techniques to quantify the probability of system
                                                 will perform without failure under a given set of operating conditions for a mission time, given it was operating at the initial 
                                                 instant. Currently, there are multiple alternatives that might be applicable such as Fault Tree Analysis (FTA), Reliability Block 
                                                 Diagrams (RBD), Markov Chain or Bayesian Networks (BN). Additional, according to Distefano and Puliafito (2007), FTA and RBD are widely
                                                 used formalisms in system reliability modeling.", style = "color:black;text-align:justify"),
                                                 
                                                 p("The techniques previously mentioned will be briefly discussed below.", style = "color:black;text-align:justify"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                               
                                               
                                      ),
                                      br(),
                                      tabsetPanel(
                                        tabPanel("Fault Tree Analysis",
                                                 hr(),
                                                 h4(p(strong("FTA"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="FTA_Example.png",width="700px",height="350px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 2),
                                                 column(
                                                   p("FTA is a technique used in the analysis of complex systems that has been around for many years (Leitch, 1995). Bell Telephone Laboratories
                                                   developed the concept in 1962 for the US Air Force for use with the Minuteman system. It was later adopted and extensively applied by the Boeing Company (NASA, 2002).", 
                                                     style = "color:black;text-align:justify"),
                                                   p("Then, FTA are logic diagrams that display the state of systems (top event) in terms of the states of its basic events, that is, FTA is a graphical and logical 
                                                     combination of causes of a defined undesired event where Boolean algebra is used (Lewis, 1996).", 
                                                     style = "color:black;text-align:justify"),
                                                   p("In essence there are three type of symbols in the tree: events, gates and transfers. Basic events, undeveloped events, conditions events and external events are sometimes
                                                     referred to as primary events. The gates show the relationships of events needed for the occurrence of a 'higher' event. The 'higher' event is the output of the gate; the 
                                                     'lower' events are the 'inputs' to the gate. The transfers allows different branches of the tree to be connected in order to avoid duplicate branches and support the tree 
                                                     design.", style = "color:black;text-align:justify"),
                                                   p("It is worth noting that the FTA suppose that the fault events are independents, that is, whether some of the components are functioning or failed does not make any
                                                   more or any less likely that the remainders are in a similar state.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px")
                                                 
                                                 
                                        ),
                                        tabPanel("Reliability Block Diagrams",
                                                 hr(),
                                                 h4(p(strong("RBD"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("RBD is a technique used to perform the system reliability and availability analyses on large and complex systems using block diagrams to show network relationships.
                                                     The structure of the reliability block diagram defines the logical interaction of system components that are required to sustain system operation (Modarres, Kaminskiy,
                                                     & Krivtsov, 2009).", 
                                                     style = "color:black;text-align:justify"),
                                                   p("The simplest form of a system for reliability analysis is one where the elements are connected in series. In this type of system, if one or more of the elements are 
                                                     down then the system is down. On the other hand, there is redundancy in a system if not all of its constituent elements are required to be up for successful operation 
                                                     of the system, and this system behavior is modeled by a parallel configuration (Leitch, 1995).", style = "color:black;text-align:justify"),
                                                   p("Most practical systems are neither a parallel nor a series configuration, but exhibit some hybrid combination of the two. These systems are often referred to as 
                                                     parallel-series systems. A parallel-series system can be analyzed by dividing it into its basic parallel and series modules and then determining the reliability function 
                                                     for each module separately. The process can be continued until a reliability function for the whole system is determined (Kapur and Lamberson, 2009).", 
                                                     style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="RBD.JPG",width="600px",height="350px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center")
                                        ),
                                        tabPanel("Markov Chain",
                                                 hr(),
                                                 h4(p(strong("Markov Chain"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="MarkovChain.png",width="500px",height="300px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("Invented by Russian mathematician Andrey Markov, Markov chains are used across a broad range of applications to represent a 'memoryless' stochastic process. This 
                                                     process is made up of random variables that represent the evolution of the process through various states. The meaning of 'memoryless', also called the ", em("Markov property,"),
                                                     " is that the probability of being in a state during the next step is only dependent on the information present in the current step and not on any information from any steps prior
                                                     to the current step (Possan and De Oliviera, 2014).",style = "color:black;text-align:justify"),
                                                   p("When Markov chains are used in reliability analysis, the process usually represents the various stages (states) that a system can be in at any given time. The states are connected
                                                     via transitions that represent the probability, or rate, that the system will move from one state to another during a step, or a given time. When using probabiliities and steps the 
                                                     Markov chain is referred to as a discrete Markov chian, while a Markov Chain that uses rate and the time domain is referred to as a continuous Markov chain.", 
                                                     style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px")
                                                 
                                        ),
                                        tabPanel("Bayesian Networks",
                                                 hr(),
                                                 h4(p(strong("BN"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="BN.jpeg",width="500px",height="300px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("A Bayesian Network represents the causal probabilistic relationship among a set of random variables, their conditional dependences, and it provides a compact 
                                                   representation of a joint probability distribution (Murphy, 1998).",style = "color:black;text-align:justify"),
                                                   p("It consists of two major parts: a directed acyclic graph and a set of conditional probability distributions.", 
                                                     style = "color:black;text-align:justify"),
                                                   p("The directed acyclic graph is a set of random variables represented by nodes. If there exists a causal probabilistic dependence between two random variables 
                                                     in the graph, the corresponding two nodes are connected by a directed edge (Murphy, 1998), while the directed edge from a node A to a node B indicates that the 
                                                     random variable A causes the random variable B. Since the directed edges represent a static causal probabilistic dependence, cycles are not allowed in the graph.
                                                     A conditional probability distribution is defined for each node in the graph. In other words, the conditional probability distribution of a node (random variable)
                                                     is defined for every possible outcome of the preceding causal node(s).", 
                                                     style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px")
                                                 
                                                 
                                        )
                                        
                                      ),
                                      br(),
                                      fluidRow(),
                                      fluidRow(),
                                      h1(),
                                      h1(),
                                      h1(),
                                      fluidRow(column(width = 3),
                                               column(p("Particularly for the analysis of the Cuttings Dryer System, the RBD was used. The Block Diagram for the system is detailed below, as well as the reliability and 
                                                        non-reliability results estimated from the failure rates collected (see Data section of", em(" Functional Analysis)."), style = "color:black;text-align:center"),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px;text-align:center")
                                               
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Reliability Assessment for The Cuttings Dryer System"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      hr(),
                                      tabsetPanel(
                                        tabPanel("Qualitative Analysis",
                                                 
                                                 br(),
                                                 navlistPanel(widths = c(2,9), fluid = T, well = T,
                                                              tabPanel("Reliability Block Diagram",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),tags$img(src="Failure.png",width="970px",height="170px", 
                                                                                                style="display: block; margin-left: auto; margin-right: auto;"), 
                                                                                  br(), width=12,style="border:1px solid black")
                                                                         ), width = 12
                                                                       ))),
                                                              tabPanel("Description of RBD",
                                                                       fluidRow(column(width = 1), column(
                                                                         fluidRow(
                                                                           column(br(),p(strong("Note the general structure of the system corresponds to a series system. That is, if a component fails, the
                                                                                                system will lose its functionality. Thus, if any of the following events occur, the Cuttings Dryer System will fail: "), 
                                                                                         style = "color:black;text-align:center"),
                                                                                  p("- Loss of low voltage power supply,", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Failure of solid control equipment,", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Inability for the main screw conveyor to mobilize the cuttings from the shakers to the Verti-G and, simultaneously, to lose the
                                                                                    contingency plan (loss of the functionality of the Funnel Belly or the Vacuum System),", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Verti-G failure,", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Catch Tank failure,", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Failure of any of the moyno pumps (transfer or circulation),", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Centrifuge failure,", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  p("- Loss of fluid manifold.", 
                                                                                    style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                                  br(), width=11,style="border:1px solid black")
                                                                         ), width = 11
                                                                       )))
                                                 )
                                        ),
                                        tabPanel("Quantitative Analysis",
                                                 fluidRow(br(),
                                                          br(),
                                                          column(width = 1), 
                                                          column(
                                                            fluidRow(plotlyOutput("Prueba")), 
                                                            width = 8),
                                                          column(h3("Note that..."),
                                                                 p("Even considering the contingency plans previously exposed, the cuttings dryer system is considerably vulnerable to failure due
                                                                   to the fact that it has few units with redundancy, that is, the failure of some main component causes the system to fail.", 
                                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                 p("Furthermore, the figure shows that after six months of operation the non-reliability of system exceeds its reliability.", 
                                                                   style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                          br(), width=3,style="background-color:lavender;border-radius: 10px")
                                                 )
                                                 )
                                      ),
                                      br(),
                                      br(),
                                      fluidRow(),
                                     
                             ), # Reliability Analysis closed
                             
                             # Importance Measures
                             tabPanel("Importance Measures", icon = icon("bookmark"),
                                      fluidRow(column(width = 2),
                                               column(h4(p(strong("Importance Measures - IM"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(
                                        p("IM are numeric tools to rank critical equipment. The choice of IM depends not only on the objective of the analysis, but also on the available data. In the 
                                          literature the IM had been discussed and different contexts (Sudarmono, 2019), (Makajic-Nikolic et al., 2018), (Si et al., 2010) and (Salazar et al., 2016). 
                                          In this way Van Der Bost and Schoonakker (2001), shows a classification of different IM according with the data required and the knowledge of the asset.", 
                                          style = "color:black;text-align:justify"),
                                        p("Note that if only the structure of the system is known, Structural IM are used. Tha data about components reliabilities enable the usage of Reliability IM. 
                                          Lifetime IM includes lifetime of the components in the criticality analysis. Finally, if costs of components improvement or maintenance are significantly high,
                                          cost based IM should be applied. There are even IM that introduce uncertainty of data in components criticality analysis. The scope of this work includes the 
                                          Structural and Reliability IM.", style = "color:black;text-align:justify"),
                                        width=12,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      tabsetPanel(
                                        tabPanel("Structural IM",
                                                 hr(),
                                                 h4(p(strong("Birnbaum"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("The first Structural IM was introduced by (Van Der Bost and Schoonakker, 2001) in order to analyze criticality of i-th components and it is commonly called Birnbaum
                                                     IM. Birnbaum IM of some component i is structural IM because it depends only on structure of the system and the reliability of the remaining system's components and 
                                                     does not depend on the actual reliability of component", em("i"),  "(Birnbaum, 1968).", style = "color:black;text-align:justify"),
                                                   p("If for a given component", em("i,"),  " the Birnbaum measure is large, it means that a small change in the reliability of the i-th component will result in a large 
                                                     change in the system reliability.", style = "color:black;text-align:justify"),
                                                   p("According to Thoft-Christensen and Rausand (2004), if system components are assumed independent, the Birnbaum measure of importance can be represented as exposed in 
                                                     the figure right.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="Birnbaum.JPG",width="600px",height="250px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 hr(),
                                                 br(),
                                                 fluidRow(),
                                                 br(),
                                                 h4(p(strong("Risk Reduction Worth - RRW"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="RRW.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("The RRW importance is a measure of the change in unreliability when an input variable, such as the unavailability of component, is set to zero-that is, 
                                                     by assuming that a component is 'perfect' (or its failure probability is zero) and thus eliminating any possibility of failure. This IM shows how much better the system 
                                                     can become as its components are improved. RRW can be computed as shows in the figure left.", style = "color:black;text-align:justify"),
                                                   p("In practice, this measure is use to identify elements of system that are the best candidates for improving system reliability.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 hr(),
                                                 br(),
                                                 fluidRow(),
                                                 br(),
                                                 h4(p(strong("Risk Achievement Worth - RAW"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("The RAW importance is the inverse of the RRW measure. In this method, the input variable (e.g., component unavailability) is set to one, and the effect of this change on 
                                                     system unreliability (unavailability) is measured. By setting component failure probability to one, RAW measures the increases in system failure probability assuming worst 
                                                     case of failing component. Therefore, RAW measure is as exposed in the figure right.", style = "color:black;text-align:justify"),
                                                   p("The risk increase measure is useful for identifying elements of the system that are the most crucial for making the system unreliable. Thus, components with high RAW 
                                                     importance are the one that will have the most impact, should their failure probability unexpectedly rise.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="RAW.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center")
                                                 
                                                 
                                        ),
                                        tabPanel("Reliability IM",
                                                 hr(),
                                                 h4(p(strong("Fussell-Vesely"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="FV.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("Regarding the Reliability IM, Kiss (2011) proposes the Fussell-Vesely importance. This measure, introduced by (Fussell, 1975) is in the form as shown in the figure left.", 
                                                     style = "color:black;text-align:justify"),
                                                   p("In components with large Fussell-Vesely IM, it is important not to allow their long-term average probabilities to further increase. Accordingly, in an aging regime, 
                                                     Fussell-Vesely importance can be interpreted as the amount of allowed degradation of performance as a function of failure probability increases (Modarres et al., 1999).", 
                                                     style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 hr(),
                                                 br(),
                                                 fluidRow(),
                                                 br(),
                                                 h4(p(strong("Criticality - CR"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("Besides Fussell-Vesely IM based importance measures, CR importance measure is a reliability IM that is widely used in components criticality analysis. In other words, 
                                                     note that the Birnbaum measure does not consider the present of baseline performance (probability of success or failure of an element) so, it would be 
                                                     hard to use it for reliabiliity-informed decision making, since low-failure probability items are not necessarily the main candidates for any change 
                                                     (Makajic-Nikolic et al., 2018) and (Modarres et al., 1999). To remedy this shortcoming, an extended version of this measure may be used, called criticality importance 
                                                     and defined as shown in the figure right.", style = "color:black;text-align:justify"),
                                                   p("Note that the Birnbaum importance is correct for reliability of the individual components relative to the reliability of the whole system. Therefore, if the Birnbaum 
                                                     importance of a component is high, but the reliability of the component is low with respect to the reliability of the system, then criticality importance assigns a low 
                                                     importance to this component.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="CR.JPG",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center")                                        )
                                      ),
                                      fluidRow(width = 3),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Importance Measures Assessment for The Cuttings Dryer System"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(br(),DT::dataTableOutput("ImportanceM"), width = 11)
                                      )
                                      
                                      ), # Importance Measures closed
                             
                             # Maintainability Analysis
                             tabPanel("Maintainability Analysis", icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Concept of Maintainability"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3, tags$img(src="Mcurve.JPG",width="300px",height="250px", 
                                                                          style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                               column(
                                                 p("When a system fails to perform satisfactorily, repair is normally carried out to locate and correct the fault. The system is restored to
                                                 operational effectiveness by making an adjustment or by replacing a component.", style = "color:black;text-align:justify"),
                                                 p("Maintainability is defined as the probability that a failed system will be restored to specified conditions within a given period of time 
                                                   when maintenance is performed according to prescribed procedures and resources. In other words, maintainability is the probability of 
                                                   isolating and repairing a fault in a system within a given time (Pham, 2006).", style = "color:black;text-align:justify"),
                                                 p("To quantify repair times, let", em("T")," be the continuous random variable representing the time to repair a failed unit, having a 
                                                 probability density function (pdf) of repair", em("m(t).")," Then the cumulative distribution function is ",style = "color:black;text-align:justify"),
                                                 withMathJax(),
                                                 p("$$P_r (T \\leq t) = M(t) = \\int_0^t m(t') \\, dt'$$",style="color:black;border:1px solid black;background-color:white"),
                                                 p("This equation is the probability that a repair will accomplishes within time", em("t.")," Therefore, the mean time to repair is given by ",
                                                   style = "color:black;text-align:justify"),
                                                 withMathJax(),
                                                 p("$$MTTR = \\int_0^{\\infty} tm(t) \\, dt = \\int_0^{\\infty} [1-M(t)] \\, dt$$",style="color:black;border:1px solid black;background-color:white"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Maintainability Techniques"), style = "color:black;text-align:center")),
                                                      width = 8, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3, icon("hand-point-right","fa-5x"), align = "center"),
                                               column(
                                                 p("For evaluating the system operation and maintenance the analyst should to apply maintainability techniques. Some alternatives are Monte Carlo
                                                   Simulation (MCS), Markov Chains, Dynamic Fault Tree Analysis (DFTA), Dynamic Reliability Block Diagram (DRBD), Dynamic Bayesian Network (DBN)
                                                   or Petri Nets.", style = "color:black;text-align:justify"),
                                                 p("Note that traditional reliability techniques of analysis, such as FTA and RBD are unable to model dynamic behavior of systems (as transitions 
                                                   of unavailable state to available state due to process of failure-repair) (Kabir, 2017).", style = "color:black;text-align:justify"),
                                                 p("The maintainability techniques previously mentioned will be briefly discussed below.", style = "color:black;text-align:justify"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      tabsetPanel(
                                        tabPanel("Monte Carlo Simulation",
                                                 hr(),
                                                 h4(p(strong("MCS"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="MCS.png",width="300px",height="250px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("Monte Carlo Simulation is a widely used technique in the probabilistic analysis of engineering systems. It is a numerical experimentation technique to obtain 
                                                     the statistics of the output variables of a system computational model, given the statistics of the input variables. In each experiment, the values of the 
                                                     input random variables are sampled based on their distributions, and the output variables are calculated using the computational model. A number of experiments 
                                                     are carried out in this manner, and the results are used to compute the statistics of the output variables (Cruse, 1997).", 
                                                     style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px")
                                        ),
                                        tabPanel("Markov Chain",
                                                 hr(),
                                                 h4(p(strong("Markov Chain"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="MarkovChains.JPG",width="450px",height="300px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("Invented by Russian mathematician Andrey Markov, Markov chains are used across a broad range of applications to represent a 'memoryless' stochastic process. This 
                                                     process is made up of random variables that represent the evolution of the process through various states. The meaning of 'memoryless', also called the ", em("Markov property,"),
                                                     " is that the probability of being in a state during the next step is only dependent on the information present in the current step and not on any information from any steps prior
                                                     to the current step (Possan and De Oliviera, 2014).",style = "color:black;text-align:justify"),
                                                   p("There are discrete Markov Chain and continous Markov chain, the main difference between a discrete Markov chain and a continous Markov chain is that the transition between the 
                                                     states are no longer represented by a fixed probability per step, but instead with a transition rate (constant) per unit time.", style = "color:black;text-align:justify"),
                                                   p("Because the transitions between states are represented by transition rates, the probability of being in a given state at a given time is represented by a differential 
                                                     equation for each state.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px")
                                        ),
                                        tabPanel("Dynamic Fault Tree Analysis",
                                                 hr(),
                                                 h4(p(strong("DFTA"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("The DFTA is a fault tree, which is extended with the time requirements using the house events matrix and the time dependent probabilistic models for the basic events. It 
                                                     represents an extension of the classic fault tree with time (Cepin and Mavko, 2002). Therefore, DFTA introduces four basic (dynamic) gates: the priority AND (PAND), the sequence
                                                     enforcing (SEQ), the standby or spare (SPARE), and the functional dependency (FDEP).", style = "color:black;text-align:justify"),
                                                   p("The PAND gate reaches a failure state if all of its input components have failed in a pre-assigned order (from left to right in graphical notation). A SEQ gate forces its inputs 
                                                     to fail in a particular order: when a SEQ gate is found in a DFTA, it never happens that the failure sequence takes place in different orders. While the SEQ gate allows the events 
                                                     to occur only in a pre-assigned order and states that a different faillure sequence can never take place, the PAND gate does not force such a strong assumption: it simply detects 
                                                     the failure order and fails just in one case (in figure PAND: failure occurs if A fails before B, but B may fail before A without producing a failure in G).", 
                                                     style = "color:black;text-align:justify"),
                                                   p("SPARE gates are dynamic gates modeling one or more principal components that can be substituted by one or more backups (spares), with the same functionality. The SPARE gate fails 
                                                     when the number of operational powered spares and/or principal components is less than the minimum required. In the FDEP gate, there will be one trigger input (either a basic event 
                                                     or the output of another gate in the tree) and one or more dependent events. The dependent events are functionally dependent on the trigger event. When the trigger event occurs, the
                                                     dependent basic events are forced to occur (Durga Rao et al., 2009).", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="DFTA.JPG",width="450px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;text-align:center"), align = "center")
                                        ),
                                        tabPanel("Dynamic Reliability Block Diagram",
                                                 hr(),
                                                 h4(p(strong("DRBD"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(
                                                   p("DRBD, an extended and enhanced successor of a Reliability Block Diagram, is an effective and powerful tool for modelling dynamic behavior (Jia et al., 2019). DRBD constructs have 
                                                     recently been extended by an introduction of several key modelling constructs, such as state dependency (SDEP), spare part (SPARE), and load-sharing (LSH) blocks. To model the 
                                                     common cause failures in the system, a CCF block have been introduced.", style = "color:black;text-align:justify"),
                                                   p("SDEP blocks are used to model the state-based dependence relationships between components in a system. In such blocks, trigger events caused by a trigger component's state change 
                                                     can result in state changes of target components. Additionally, both trigger events and target events can be of three types: 'Activation (A)', 'Deactivation (D) and 'Failure (F)'.", 
                                                     style = "color:black;text-align:justify"),
                                                   p("SPARE blocks represent systems' redundant behaviours, whereby n components are used as redundant backups for the primary component. In SPARE blocks, primary components' deactivation
                                                     or failure leads to the first spare component's activation. Similarly, the first spare component's deactivation or failure leads to the second spare component's activation.", 
                                                     style = "color:black;text-align:justify"),
                                                   p("LSH blocks represent an intermediate state where a component is inactive yet at the same time has not failed, i.e., they can be used to model reliability conditions changing in 
                                                     relation to the variation of the load shared among various components. LSH blocks can operate well only if more than k components are in a good state, which is similar to 'k/n' gates. 
                                                     If more than n-k components enter the failed/deactivated state in an LSH block, other components will fail or be deactivated because of overloading.Whereas, CCF blocks represent that 
                                                     severaal components experience simultaneous failures if some common causes occur.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px"),
                                                 column(width = 1),
                                                 column(width = 5, tags$img(src="DRBD.JPG",width="600px",height="400px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center")
                                                 
                                        ),
                                        tabPanel("Dynamic Bayesian Networks",
                                                 hr(),
                                                 h4(p(strong("DBN"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="DBN.png",width="600px",height="200px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("DBNs were developed by Paul Dagum in the early 1990s at Stanford University's Section on Medical Informatics (Dagum, Galper and Horvitz, 1992). DBNs extend standard Bayesian Networks 
                                                     with the concept of time. This allows us to model time series or sequences. In fact they can model complex multivariate time series, which means we can model the relationships between 
                                                     multiple time series in the same model, and also different regimes of behavior, since time series often behave differently in different contexts.",
                                                     style = "color:black;text-align:justify"),
                                                   p("According to Seiver (1995), Dagum developed DBNs to unify and extend traditional linear state-space models such as Kalman filters, linear and normal forecasting models such as ARMA 
                                                     and simple dependency models such as hidden Markov models into a general probabilistic representation and inference mechanism for arbitrary nonlinear and non-normal time-dependent 
                                                     domains.", style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px")
                                        ),
                                        tabPanel("Petri Nets",
                                                 hr(),
                                                 h4(p(strong("Petri Nets"), style = "color:black;text-align:center")),
                                                 hr(),
                                                 column(width = 5, tags$img(src="PN.png",width="500px",height="300px", 
                                                                            style="display: block; margin-left: auto; margin-right: auto;"), align = "center"),
                                                 column(width = 1),
                                                 column(
                                                   p("Petri Nets is a directed bipartite graph with two types of nodes: place and transitions. Places and transitions in Petri Nets are represented by circles and rectangles, respectively.
                                                     Directed arcs connect places to transitions and transitions to places. The place that connects to a transition is called an input place of the transition.",
                                                     style = "color:black;text-align:justify"),
                                                   p("On the other hand, the place that be connected from a transition is called an output place of the transition. Tokens (represented by dots) are located at places in Petri Nets. When 
                                                     a transition fires, it removes a token from each input place of the transition, and puts a token to each output place of the transition. The firing of a transition occurs only when 
                                                     there is at least one token for each input place of the transition. Then the transition is said to be enable. A marking of a Petri Nets is given by a vector that represents the number 
                                                     of tokens for all the places. In the Petri Nets modeling, markings provide the state of a target system (Okamura and Dohi, 2017).", 
                                                     style = "color:black;text-align:justify"),
                                                   width=5,style="background-color:lavender;border-radius: 10px")
                                        )
                                      ),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      fluidRow(width = 3),
                                      br(),
                                      h1(),
                                      h1(),
                                      h1(),
                                      fluidRow(column(width = 1),
                                               column(p("Particularly for the analysis of the Cuttings Dryer System, the MCS was used. The maintainability results for different operational times are exposed to follow. Note that the 
                                                        inputs for the analysis are the failure and repair rates previously collected (see Data section of", em(" Functional Analysis)."), style = "color:black;text-align:center"),
                                                      width = 11, style = "background-color:lavender;border-radius: 10px;text-align:center")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Maintainability Assessment for The Cuttings Dryer System"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      sidebarLayout(
                                        sidebarPanel(column(width = 1),
                                          radioButtons("plots", "Select the operational time you want to view:", 
                                                       choices=list("One Month (730 hours)", 
                                                                    "Two Months (1460 hours)", 
                                                                    "Three Months (2190 hours)",
                                                                    "Four Months (2920 hours)",
                                                                    "Five Months (3650 hours)",
                                                                    "Six Months (4380 hours)",
                                                                    "Nine Months (6570 hours)",
                                                                    "Twelve Months (8760 hours)",
                                                                    "All"), 
                                                       selected="One Month (730 hours)"),
                                          column(width = 1)
                                        ),
                                        
                                        mainPanel(
                                          plotOutput(outputId ="RepairTimesSimulated")
                                        )
                                      ),
                                      br(),
                                      fluidRow(column(width = 1),
                                               column(p("From to the cuttings dryer repair time histograms, it was possible to conclude that for all the mission times under study, more than 95% of unavailability from maintenance 
                                                        procedures lasts less than 20 hours, with about 75% of system repair times being less than 4 hours.", style = "color:black;text-align:center"),
                                                      width = 11, style = "background-color:lavender;border-radius: 10px;text-align:center")
                                      ),
                                      br(),
                                      br(),
                                      fluidRow(),
                                      ), # Maintainability Analysis closed
                             
                             # Availability Analysis
                             tabPanel("Availability Analysis", icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Concept of Availability"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(
                                                 p("Reliability is a measure that requires system success for an entire mission time. No failures or repairs are allowed. Space missions and aircraft flights 
                                                   are examples of systems where failures or repairs are not allowed. Availability is a measure that allows for a system to repair when failure occurs (Pham, 
                                                   2006).", style = "color:black;text-align:justify"),
                                                 p("The availability of a system is defined as the probability that a system is performing its required function at a given point in time or over a stated 
                                                   period of time when operated and maintained in a prescribed manner (Komal, Sharma and Kumar, 2010). Mathematically,", style = "color:black;text-align:justify"),
                                                 withMathJax(),
                                                 p("$$A = \\frac{(System up time)}{(System up time + System down time)} = \\frac{(MTBF)}{(MTBF + MTTR)}$$",style="color:black;border:1px solid black;background-color:white"),
                                                 p("Where MTBF is Mean Time Between Failure and MTTR is Mean Time To Repair.", style = "color:black;text-align:justify"),
                                                 p("The implication of this formula is that a high availability can be obtained either by increasing the MTBF, and hence the reliability, or improving the maintainability
                                                   by decreasing the MTTR (Leitch, 1995).", style = "color:black;text-align:justify"),
                                                 p("Availability is a measure of success used primarily for repairable systems. For non-repairable systems, availability, ", em("A(t),")," equals reliability", em("R(t)."),
                                                   " In reparable systems,", em("A(t),")," will be equal to or greater than", em("R(t)."), style = "color:black;text-align:justify"),
                                                 width=12,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("Availability Assessment for The Cuttings Dryer System"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(br(),DT::dataTableOutput("AvailabilityTime"), width = 11)
                                      )
                                      ), # Availability Analysis closed
                             
                             # References 
                             tabPanel("References", icon = icon("bookmark"),
                                      fluidRow(column(width = 3),
                                               column(h4(p(strong("References used in this work"), style = "color:black;text-align:center")),
                                                      width = 12, style = "background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(
                                        p("- Adam Seiver. (1995).'Uncertain Reasoning and Forecasting'. International Journal of Forecasting. 11 (1): 73-87.doi:10.1016/0169-2070(94)02009-e", 
                                          style = "color:black;text-align:justify"),
                                        p("- Aji, A., & Soepangkat, P. (2013). Penentuan interval waktu pemeliharaanpencegahan berdasarkanalokasi dan optimasi kehandalanpadacontinues soap making(CSM). 
                                          Prosiding Seminar Nasional Manajemen Teknologi XVII, 29-37.", style = "color:black;text-align:justify"),
                                        p("- Ayele, Y. Z., Barabady, J., & Droguett, E. L. (2016). Dynamic Bayesian network- based risk assessment for arctic offshore drilling waste handling practices. 
                                          Journal of Offshore Mechanics and Arctic Engineering, 138(5). https://doi.org/10.1115/1.4033713.", style = "color:black;text-align:justify"),
                                        p("- Birnbaum, Z.W. (1968). On the importance of different components in a multicomponent system.", style = "color:black;text-align:justify"),
                                        p("- Cadwallader, L. (1998). Selected Component Failure Rate Values from Fusion Safety Assessment Tasks. Edited by Idaho: Idaho National Engineering and Environmental 
                                          Laboratory. Idaho", style = "color:black;text-align:justify"),
                                        p("- Cepin, M. and Mavko, B. (2002). A Dynamic Fault Tree. Reliability Engineering & System Safety, 75, 83-91.", style = "color:black;text-align:justify"),
                                        p("- Correia, L. (2014). Brief Analysis of the Environmental Impacts Resulting from Offshore Drilling of Block BM-CAL-13, in the Camamu-Almada. JUS.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Dagum, P. Galper, A. Horvitz, E. (1992). 'Dynamic Network Models for Forecasting'. Proceedings of the Eighth Conference on Uncertainty in Artificial Intelligence. 
                                          AUAI Press: 41-48", style = "color:black;text-align:justify"),
                                        p("- Distefano, S., & Puliafito, A. (2007). Dynamic Reliability Block Diagrams VS Dynamic Fault Trees. Reliability and Maintainability Symposium, 71-76.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Durga Rao, K., Gopika, V., Sanyasi Rao, V. V. S., Kushwaha, H. S., Verma, A. K., & Srividya, A. (2009). Dynamic fault tree analysis using Monte Carlo 
                                          simulation in probabilistic safety assessment. Reliability Engineering and System Safety, 94(4), 872-883.", style = "color:black;text-align:justify"),
                                        p("- Feng, J. (2017). Reliability Evaluation for a Subsea to Shore Production System. Edited by Universidade Federal do Rio de Janeiro - UFRJ. Rio de Janeiro.", 
                                          style = "color:black;text-align:justify"),
                                        p("- GNSC. (2019). Vertical Cuttings Dryer. 2020, de GN Solids Control Sitio web: https://gnsolidscontrol.com/vertical-cuttings-dryer", style = "color:black;text-align:justify"),
                                        p("- IEEE. (2007). Design of Reliable Industrial and Commercial Power Systems. IEEE Std 493-2007. Institute of Electrical and Electronics Engineers.", style = "color:black;text-align:justify"),
                                        p("- INL. (2012). Review of Maintenance and Repair Times for Components in Technological Facilities. Idaho National Laboratory.", style = "color:black;text-align:justify"),
                                        p("- Jaleel, J, and M Vijayan. (2013). 'Reliability Evaluation and Failure Rate Prediction of Ilmenite Fluidized Bed Dryer at IREL, Chavara'. International Journal on Information Technology 1: 
                                          319-24.", style = "color:black;text-align:justify"),
                                        p("- Jia, L., Ren, Y., Yang, D., Feng, Q., Sun, B., & Qian, C. (2019). Reliability analysis of dynamic reliability block diagram based on dynamic uncertain causality graph. Journal of Loss 
                                          Prevention in the Process Industries, 62(September), 103947.", style = "color:black;text-align:justify"),
                                        p("- Kabir, Sohag. (2017). 'An Overview of Fault Tree Analysis and Its Application in Model Based Dependability Analysis'. Expert Systems with Applications 77: 114-35.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Kapur, C., & Lamberson, L. (2009). Reliability in Engineering Design (W. I. P. Limited (ed.)).", style = "color:black;text-align:justify"),
                                        p("- Karklit, A. K., and A. N. Sokolov. (1984). 'Increasing the Resistance of Vacuum Treatment Unit Linings Abroard'. Refractories 25 (1-2): 9-13.", style = "color:black;text-align:justify"),
                                        p("- Komal, Sharma, S. P., & Kumar, D. (2010). RAM analysis of repairable industrial systems utilizing uncertain data. Applied Soft Computing Journal, 10(4), 1208-1221.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Leitch, R. (1995). Reliability Analysis for Engineers: An introduction (Oxford University Press (ed.)).", style = "color:black;text-align:justify"),
                                        p("- Lewis, E. (1996). Introduction to Reliability Engineering (I. John Wiley & Sons (ed.)).", style = "color:black;text-align:justify"),
                                        p("- Makajic-Nikolic, D., Vujosevic, M., Pavlovic, P., 2018. Importance measures in reliability and maintenance 7-16.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Miszta-Kruk, K. (2016). 'Reliability and Failure Rate Analysis of Pressure, Vacuum and Gravity Sewer Systems Based on Operating Data'. Engineering Failure Analysis 61: 37-45", 
                                          style = "color:black;text-align:justify"),
                                        p("- Modarres, M., Kaminskiy, M., & Krivtsov, V. (2009). Basic Reliability Mathematics: Review of Probability and Statistics. In CRC Press (Ed.), Reliability engineering and risk 
                                          analysis (pp. 15-70).", style = "color:black;text-align:justify"),
                                        p("- NASA. (2002). Fault Tree Handbook with Aerospace Applications. NASA Handbook, XXXIII(8), 218.", style = "color:black;text-align:justify"),
                                        p("- Neves, L. 2011. Lean Maintenance Management Applied to Solid Bulk Transport Equipment. Lisboa: Faculdade de Ciencias e Tecnologia, Universidade Nova de Lisboa.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Ng, G., Wong, Y., Sakolnukornkij, N., Phanpanich, W. and Pukpiboon, T. (2016). Groundwater Treatment Plant Project. Edited by University of New South Wales. New South Wales.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Okaro, A. 2017. 'An Integrated Model for Asset Reliability, Risk and Production Efficiency Management in Subsea Oil and Gas Operations'. Newcastle University, School of Marine 
                                          Science and Technology, 203.", style = "color:black;text-align:justify"),
                                        p("- OREDA. 2015. Offshore Reliability Data Handbook, 6th Edition. Edited by SINTEF. Oslo.", style = "color:black;text-align:justify"),
                                        p("- Petri, I. (2017). Decontamination of Drilling Cuttings Using a Continuous Semi-Industrial Microwave Dryer. Universidade Federal de Uberlandia, 176.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Pham, H. (2006). System Software Reliability (Springer (ed.)).", style = "color:black;text-align:justify"),
                                        p("- Possan, E., De Oliveira Andrade. (2014). Markov chains and reliability analysis for reinforced concrete structure service life. Materials Research, 17(3), 593-602", 
                                          style = "color:black;text-align:justify"),
                                        p("- Salazar, J.C., Nejjari, F., Sarrate, R., Weber, P., Theilliol, D. (2016). Reliability importance measures for a health-aware control of drinking water networks. Conf. Control 
                                          Fault-Tolerant Syst. SysTol 2016-Novem, 572- 578.", style = "color:black;text-align:justify"),
                                        p("- Sembiring, N., and Y. P. Batubara. (2019). 'The Spare Part Maintenance of Cake Breaker Conveyor with Reliability Centered Spares Method'. IOP Conference Series: Materials Science 
                                          and Engineering 523 (1). ", style = "color:black;text-align:justify"),
                                        p("- Setiawan, C., Wijaya G., Hartanti, L. (2015). 'Reliability Analysis Using Fuzzy FMEA To Design Sustainable Production'. In The 1ST UMM International Conference on Pure and Applied 
                                          Research, 32-40.", style = "color:black;text-align:justify"),
                                        p("- Si, S., Cai, Z., Sun, S., Zhang, S. (2010). Integrated importance measures of multi-state systems under uncertainty. Comput. Ind. Eng. 59, 921-928", style = "color:black;text-align:justify"),
                                        p("- Sudarmono, S.H., (2019). Application of Reliability, Availability and Maintainability analysis to Dynamic Positioning Systems used in offshore operations.", 
                                          style = "color:black;text-align:justify"),
                                        p("- Thoft-Christensen, P., Rausand, M. (2004). System reliability, Engineering Design Reliability Handbook.", style = "color:black;text-align:justify"),
                                        p("- U.S. Department of Energy. (1990). Availability Report Based On Rev. 00 P&IDs. Edited by Engineering Studies - U.S. Department of Energy. Washington.", style = "color:black;text-align:justify"),
                                        p("- Van Der Borst, M., Schoonakker, H. (2001). An overview of PSA importance measures. Reliab. Eng. Syst. Saf. 72, 241-245.", style = "color:black;text-align:justify"),
                                        p("- Vishnu, C.R., and  Regikumar, V. (2016). 'Reliability Based Maintenance Strategy Selection in Process Plants: A Case Study'. Procedia Technology 25 (Raerest): 1080-87.", 
                                          style = "color:black;text-align:justify"),
                                        width=12,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br()
                                      
                                      
                                      ) # References closed
                  ),
)
)

