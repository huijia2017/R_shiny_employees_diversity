
library(shiny)
library(ggplot2)
library(png)

#######=======================================part1

#lineplot-----------------------------------------------------------------
mydata1 <- read.csv("employment2008-18.csv")
employ_linegraph1 <- ggplot(mydata1, aes(x=Year, y=Employment))+geom_point()
mydata1$Year <- factor(mydata1$Year)
employ_linegraph1 <- employ_linegraph1+  geom_line(linetype="dotted")
employ_linegraph1 <- employ_linegraph1+ geom_line(colour= "black")
employ_linegraph1 <- employ_linegraph1+ ggtitle("The Changing of the numeber of Employees ")
employ_linegraph1 <- employ_linegraph1+ scale_x_continuous(breaks=seq(2008, 2018, 1))
employ_linegraph1



mydata1 <- read.csv("employment2008-18.csv")
employ_linegraph <- ggplot(mydata1, aes(x=Year, y=Employment))+geom_point()
mydata1$Year <- factor(mydata1$Year)
employ_linegraph <- employ_linegraph + geom_line(linetype="dotted")
employ_linegraph <- employ_linegraph  + geom_line(colour= "blue")
employ_linegraph <- employ_linegraph  + ggtitle("Employment Rate")
employ_linegraph <- employ_linegraph+ scale_x_continuous(breaks=seq(2008, 2018, 1))
employ_linegraph
#--------------------------------------------------------------
mydata2 <- read.csv("employment_new.csv")
mydata2 <- as.data.frame(mydata2)
mydata2$year <- as.factor(mydata2$year)
graph1 <- ggplot(mydata2, aes(x= Country, y= Employment, fill= year))+
  geom_bar(stat = "identity",position = "dodge")+
  theme(panel.background= element_rect(fill= "transparent"))+
  ggtitle("Top 10 Countries which Employees Come From ")+
  scale_x_discrete(limits= c("England", "China", "India", "New Zealand", 
                             "Philippines", "Vietnam", "South Africa", 
                             "Italy", "Malaysia", "Scotland"))+
  theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
graph1

#2008/2013/2018
year2008<-mydata2[which(mydata2$year=="2008"),]
year2013<-mydata2[which(mydata2$year=="2013"),]
year2018<-mydata2[which(mydata2$year=="2018"),]

#2008
country_graph2008 <- ggplot(year2008,aes(x = Country ,y= Employment))+
  geom_bar(stat = "identity",fill="steelblue")+ 
  ggtitle("Top 10 Countries which Employees Come From")+ 
  scale_x_discrete(limits=c("England","China","India",
                            "New Zealand", "Philippines","Vietnam",
                            "South Africa","Italy","Malaysia", "Scotland"))+ 
  theme(plot.title = element_text(color = "grey19", size = 15, face = "bold")
  )+theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
country_graph2008

#2013
country_graph2013 = ggplot(year2013,aes(x = Country,y= Employment))+ 
  geom_bar(stat = "identity",fill="forestgreen")+ 
  ggtitle("Top 10 Countries which Employees Come From")+ 
  scale_x_discrete(limits=c("England","China","India","New Zealand", 
                            "Philippines","Vietnam","South Africa",
                            "Italy","Malaysia", "Scotland"))+ 
  theme(plot.title = element_text(color = "grey19", size = 15, face = "bold")
  )+theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
country_graph2013

#2018
country_graph2018 = ggplot(year2018,aes(x = Country ,y= Employment))+ 
  geom_bar(stat = "identity",fill="orange")+ 
  ggtitle("Top 10 Countries which Employees Come From")+ 
  scale_x_discrete(limits=c("England","China", 
                            "India","New Zealand", "Philippines",
                            "Vietnam","South Africa",
                            "Italy","Malaysia", "Scotland"))+ 
  theme(plot.title = element_text(color = "grey19", size = 15, face = "bold")
  )+theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))
country_graph2018



#####==========================================part2

mydata3 <- read.csv("HRDataset_new.csv")
mydata3 = as.data.frame(mydata3)
#------------------------------------------------------------------------------
p1 <- ggplot(mydata3, aes(x=recruitmentsource, y=frequency(recruitmentsource),fill=maritaldesc)) +
  geom_bar(stat="identity",pos='fill', width= 0.6)+coord_flip()+ggtitle("       The Background of Emplyees")+ 
  theme(panel.background=element_rect(fill='transparent'), 
        axis.text.y = element_text(color = "black",size=10))+
  theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))
p1
#---------------------------------------------------------------------------
p2 <- ggplot(mydata3, aes(x=recruitmentsource, y=frequency(recruitmentsource),fill=racedesc)) +
  geom_bar(stat="identity",pos='fill', width= 0.6)+coord_flip()+ggtitle("       The Background of Emplyees")+ 
  theme(panel.background=element_rect(fill='transparent'), 
        axis.text.y = element_text(color = "black",size=10))+
  theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))
p2
#----------------------------------------------------------------------------
p3 <- ggplot(mydata3, aes(x=recruitmentsource, y=frequency(recruitmentsource),fill=sex)) +
  geom_bar(stat="identity",pos='fill', width= 0.6)+coord_flip()+ggtitle("       The Background of Emplyees")+ 
  theme(panel.background=element_rect(fill='transparent'), 
        axis.text.x = element_text(color = "black",size=9))+
  theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))
p3
#-------------------------------------------------------------
#recruitmentresoureswithyear
mydata0 <- read.csv("recruitmentresoureswithyear.csv")
mydata0 <- as.data.frame(mydata0)
mydata0$Year = as.factor(mydata0$Year)


year2008<-mydata0[which(mydata0$Year=="2009"),]

bp<- ggplot(year2008, aes(x= Year, y= Number, fill= RecruitmentResoures))+
  geom_bar(width = 1, stat = "identity") +coord_polar("y", start=0)
bp

#===================Part3
mydata4 <- read.csv("HRDataset_new.csv")

h1 <- ggplot(mydata4,aes(x=perfScoreID))+geom_histogram(binwidth =0.5)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))+
  ggtitle("Employees Satisfaction")
h1

b1 <- ggplot(mydata4, aes(x = factor(mydata4$empsatisfaction)), fill(mydata4$empsatisfaction))+
  geom_bar(width = 0.7,aes(color=factor(mydata4$empsatisfaction))) + coord_polar()+ 
  theme(legend.justification = c(1,1), legend.position= c(1,1))+ 
  theme(plot.margin = unit(c(2,1.8,2,1.8),"cm"))+
  scale_color_discrete(name="Score")+
  ggtitle("Employees Satisfaction")+
  theme(plot.background = element_rect("grey70"))+
  scale_x_discrete("")
b1

library(dplyr)
mydata5 <- read.csv("HRDataset_new.csv")
performscore = mydata5 [ , c("perfScoreID","Department")]
View(performscore)
sumframe <- group_by(performscore,Department )
sumframe <- summarise(sumframe,sum_score = sum(perfScoreID)) 
View(sumframe)
performgraph <- ggplot(sumframe,aes(x = Department ,y= sum_score ))
performgraph <- performgraph + geom_bar(stat = "identity",fill="grey50")
performgraph <- performgraph + ggtitle("The performance score of different department")+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(plot.margin = unit(c(1.2,1.2,2.2,1.2),"cm"))
performgraph









################

library(shiny)
ui <- shinyUI(
  fluidPage(
    navbarPage("WEWORK",
               #Page1
               #Page2
               tabPanel('general employment introduction',
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            style = "background: lightgrey",
                            
                            selectInput("Yearchoice","1. Please Select The Year:",
                                        choices = c("2008","2013","2018")),
                            helpText("Note:",
                                     br(),
                                     "1.Choose the year to see the changes of the Number of Employees Coming from Top 10 Countries"), 
                            
                          ),
                          
                          mainPanel(
                            plotOutput("countryplot"),
                            plotOutput("line")
                            
                          )
                          
                        )),
               #Page3
               tabPanel('employees background',
                        sidebarLayout(
                          sidebarPanel(
                            style = "background: lightgrey",
                            
                            radioButtons("EmployeesBackground", "1. Please Select the Catergory:", c("maritaldesc", 
                                                                                                     "racedesc", 
                                                                                                     "sex")),
                            sliderInput("YearRange","2. Please select Year:",
                                        min = 2008,max = 2017,value = 2013,step = 1)
                          ),
                          mainPanel(
                            plotOutput("backgroundplot"),
                            plotOutput("RecruitmentResource"))
                        )),
               #Page4
               tabPanel('employees performance & Satisfaction',
                        
                        mainPanel(
                          
                          
                          plotOutput(outputId = "distPlot"),
                          plotOutput("lastplot")
                          
                        ))
               
    ))
)
#server=====================================================================
library(shiny)
server = shinyServer(
  function(input,output,session){
    
    ######
    
    
    
    #countryplot
    output$countryplot <- renderPlot({
      
      Year <- input$Yearchoice
      
      if(Year == "2008"){
        
        country_graph2008
      }
      
      else if(Year == "2013") {
        country_graph2013
      }
      
      else if(Year == "2018"){
        country_graph2018
      }
    })
    #linegraph
    output$line <- renderPlot({
      employ_linegraph1
    })
    #page2 employeesbackground
    output$backgroundplot <- renderPlot({
      
      Category <- input$EmployeesBackground
      
      if(Category == "maritaldesc"){
        
        p1
      }
      
      else if(Category == "racedesc") {
        p2
      }
      
      else if(Category == "sex"){
        p3
      }
      
    })
    output$RecruitmentResource <- renderPlot({
      Yearrange <- input$YearRange
      
      buffer<-mydata0[which(mydata0$Year==Yearrange),]
      ppp<- ggplot(buffer, aes(x= Year, y= Number, fill= RecruitmentResoures))+
        geom_bar(width = 1, stat = "identity") +coord_polar("y", start=0)
      ppp
    })
    #Page3 histgram 
    output$distPlot <- renderPlot({
      performgraph
    })
    output$lastplot <- renderPlot({
      b1
    })
    
  })
shinyApp(ui = ui, server = server)


