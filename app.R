####import libraries

#go back and check if all of these are neccesary - can reduce here I think?

library(shiny)
library(ggplot2)
library(tidyverse)
library(httr)
library(partykit)
library(qvcalc)
library(psychotools)
library(PlackettLuce)
library(gosset)
library(stringr)
library(knitr)
library(formattable)
library(scales)

library(gtools)
library(plotly)


skiplist<-c("variety_a","variety_b","variety_c")
skippatter<-c("_best","_worst")


#load the other functions written so far
#future will add these to a package or elsewhere to give a better interface at this point
source("functions.R")

#set the shiny app as a function with a data input and sensible defaults so that initial 
#load of app will go to a sensible view
PL_Covar<-function(data){

#make sure data is a data frame rather than a tibble for compatibility with the code written  
data1<-data.frame(data,stringsAsFactors = FALSE,check.names = FALSE)

traits <- unique(gsub(paste(skippatter,collapse="|"),"",
     colnames(data1)[grep(paste(skippatter,collapse="|"),colnames(data1))]))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Plackett-Luce Models With Covariates"),
   
   # Sidebar with inputs for the "win","loss" columns plus the covariates
   sidebarLayout(
      sidebarPanel(
         selectizeInput("trait",
                     "Select Trait", choices = traits)
           ),
      
      # Set up tab layouts - 
      #1 Plots by Factor; #2 Re-grouping factor levels; #3 Categorising numeric variables; #4 model output
      mainPanel(tabsetPanel(
        
        # 1 Plots by Factor
        
        tabPanel("Single Variable Comparison", 
                 selectizeInput("graphfac","Select factor to plot",choices=""),
                 #% win plot by factor
                 plotlyOutput("winplot",height = "700px"),
                 #CI plot of estimates by factor
                 plotOutput("CIPlot")),
        
        # 2 Re-grouping factor levels
         tabPanel("Modify Factors",
                  
                  selectizeInput("char_modify","Select factor to modify",choices=""),
                  radioButtons("na_action",label="Select option for handling missing values",
                                   choices=c("Exclude"="exclude",
                                             "Create 'missing' category"="cat")),
                  
                  
  #set number of groups. keeping this in for future modification but for now fix it for 2 to make code easier   
             sliderInput("ngrps","Select Number of Groups in New Variable",min = 2,max=2,value = 2),
                conditionalPanel(condition="input.ngrps==2",
    #create one column for each group                                         
    fluidRow(textInput("name1","Group Name 1"),
             textInput("name2","Group Name 2"),
             #summary plot
             plotOutput("plot_summ"),
             uiOutput('group_allocation'))         
            ,
    
    #trigger for adding new column into data
    actionButton("go3",
                 "Confirm Updated Groups"),
    #message containing new variable name
    textOutput("message1"))
                                        
                  ),
    
    
    #3 modify variates
    tabPanel("Modify Variates",
             selectizeInput("number_modify","Select Variable to modify",choices=""),
             
             plotOutput("plot_summ2"),
   
  #select number of groups - again fixed at 2 for now, but keeping this in for later
   sliderInput("ngrps2","Select Number of Groups in New Variable",min = 2,max=2,value = 2), 
  
  #select break point. 
  #need to provide default min max values otherwise error is triggered but these are immediately overwritten later
   sliderInput("breaks","Select Break Point for Groups",min = 0,max=2,value = 1),
  
  #add a box-y layour to input group names and show frequencies
   column(width=3,textInput("name3","Group Name 1"),
          textInput("name4","Group Name 2")),
  #show frequencies
  tableOutput("sumtab"),
  #trigger to add variable into the data frame
  actionButton("go5",
               "Confirm Updated Groups"),
  #show message
  textOutput("message2")
  
         
             ),
   
  #4 Model Output
  tabPanel("Model Output",
          selectizeInput("covars",
                          "Select Possible Covariates",
                          choices = colnames(data1) [!colnames(data1)%in%
                            skiplist&grepl(paste(skippatter,collapse="|"),
                              colnames(data1))==FALSE],multiple=TRUE),
           sliderInput("minsize","Select Minimum Size For Groups",min = 20,
                       max=200,value = 50,step=10),
           sliderInput("alpha","Select Significance Level",min = 0.001,
                                      max=0.2,value = 0.05,step=0.001),
           actionButton("go",
                        "Fit Model"),
           plotOutput("TreePlot"),
          plotOutput("NodePlot"),
           tableOutput("ModelOut")
           )
        
      )
      )
   )
)
####End of UI
   



start<-0
# Define server logic required to draw a histogram
server <- function(input, output,session) {

###Section A: REACTIVE DATA UPDATING STUFF  
  
 
#convert data into a reactive dataframe so new variables can be added
  globals <- reactiveValues(
    mydf = data1
  )
  

#on loading of app immediately update the ui to give the selectable columns from the data
  observeEvent({input$go3;input$go5;start==0},
               
               {
                 start<-1

                 

    #identify numeric columns
    nums <- colnames(globals$mydf)[unlist(lapply(globals$mydf[,colnames(globals$mydf)],
                                     function(x){class(x)%in%c("numeric","integer","double","Date")})) ] 
    
    nums<-nums[!nums%in%skiplist]
    nums<-nums[grepl(paste(skippatter,collapse="|"),nums)==FALSE]
    
   #identify character or factor columns
     chars <- colnames(globals$mydf)[unlist(lapply(globals$mydf[,colnames(globals$mydf)],
                                        function(x){class(x)%in%c("character","factor")})) ] 
     chars<-chars[!chars%in%skiplist]
     chars<-chars[grepl(paste(skippatter,collapse="|"),chars)==FALSE]
     
  #TO DO: WHAT OTHER CLASS TYPES ARE MISSING AND IMPORTANT?
  
  
  updateSelectInput(session, "char_modify", choices = chars)
  updateSelectInput(session, "number_modify", choices = nums)
  
  #factor for graphs can only be factor with 2 levels (force this for now since
  #with >3 levels ns get small and trigger errors; but could easily extend)
  
 # chars_2lev <- chars[apply(globals$mydf[,chars],2,
  #                          function(x) nlevels(factor(x))==2)]
  
  chars_2lev<-chars
  
  updateSelectInput(session, "graphfac", choices = chars_2lev)
  })
  
########## 
 
#if recode factor trigger is pushed
  observe({
    if(input$char_modify!=""){
#make sure something is selected before entering loop
    #(probably need to explicitly reset other values at some point to prevent crashes?)

  
  
  #auto generate group names
  updateTextInput(session, "name1", value=paste(input$char_modify,1,sep=""))
  updateTextInput(session, "name2", value=paste(input$char_modify,2,sep=""))
  

    }
  }) 
    

  observeEvent( {input$na_action;input$char_modify},{  
    if(input$char_modify!=""){
  if(input$na_action=="cat"){
    globals$mydf[,input$char_modify][is.na(globals$mydf[,input$char_modify])]<-"Missing"

  }
  if(input$na_action=="exclude"){
    globals$mydf[,input$char_modify][globals$mydf[,input$char_modify]=="Missing"]<-NA

  }
    }
  })
  
  
output$group_allocation <- renderUI({
  
  

  
vals<-sort(unique(globals$mydf[,input$char_modify]))

      tagList(lapply(vals, function(i){ # must use `tagList` `
        column(3,
               radioButtons(
                 inputId = paste(input$char_modify,"grp",i,sep="_"), # Set the id to the column name
                 label = i, # Label is the value name
                 choices=c(input$name1,input$name2),
                 selected=input$name1# first value set to the first row of the column
               ))
      })
      )
    })    
    
 

###########  
#if new factor variable is confirmed  
observeEvent(input$go3,{
    
  all_inputs <- names(session$input)[grep(paste(input$char_modify,"grp",sep="_"),
                                          names(session$input))]
  
  input_grps <- plyr::ldply(all_inputs, function(i){
    data.frame(input_name = i, input_value = input[[i]],stringsAsFactors = FALSE,
               orig=gsub(paste(input$char_modify,"grp_",sep="_"),"",i))
  })

  input_grps1<-subset(input_grps,input_value!="")

  tmp<-data.frame(orig=globals$mydf[,input$char_modify])
  tmp<-merge(tmp,input_grps,by="orig",all.x=TRUE,all.y=FALSE)

  globals$mydf$newvar<-tmp$input_value
  
#add new variable to data and give temporary name
 
# if grouping has never been done before then give orignal name_grp
    if(!paste(input$char_modify,"grp",sep="_")%in%colnames(globals$mydf)){
      nm<-paste(input$char_modify,"grp",sep="_")
      
    }
  
  #if name has been used before then give sequential name
    else{
     # identify how many columns already exist
      i<-sum(grepl(paste(input$char_modify,"grp",sep="_"),colnames(globals$mydf)))
     #original is effectively the 0th column so adding i to column name will be fine
      nm<-paste(input$char_modify,"grp",i,sep="_")
      
    }     
#  give column the name
    colnames(globals$mydf)[ncol(globals$mydf)]<-nm
#create the message to push with the new variable name
    output$message1 <- renderText({
      paste("New Variable '",nm,"' created",sep="")
    })
    
#include new variable as a possible covariate
    #TO DO - how to update without erasing previous selection?    
    
    choice<-colnames(globals$mydf)[ !colnames(globals$mydf)%in%skiplist&grepl(paste(skippatter,collapse="|"),
                                                                      colnames(globals$mydf))==FALSE]
   updateSelectInput(session, "covars", choices =choice)
   
   chars <- colnames(globals$mydf)[unlist(lapply(globals$mydf[,colnames(globals$mydf)],
                                                 function(x){class(x)%in%c("character","factor")})) ] 
   chars<-chars[!chars%in%skiplist]
   chars<-chars[grepl(paste(skippatter,collapse="|"),chars)==FALSE]
     #TO DO: WHAT OTHER CLASS TYPES ARE MISSING AND IMPORTANT?
   
   updateSelectInput(session, "char_modify", choices = chars)
     chars_2lev<-chars
   
   updateSelectInput(session, "graphfac", choices = chars_2lev)
   
   
  })


#######################   

  
  
observe({  
##########  
#if something has been selected then go through  
if(input$number_modify!=""){ 
  
#update input for break value based on data values
  
if(is.numeric(data1[,input$number_modify])){
updateSliderInput(session, "breaks", min=floor(min(data1[,input$number_modify],na.rm=T)),
                     max=ceiling(max(data1[,input$number_modify],na.rm=T)),
                      value=median(data1[,input$number_modify],na.rm=T),
                  step=10**round(log10(diff(range(data1[,input$number_modify],na.rm=T))/100)))
}
else{
  updateSliderInput(session, "breaks", min=min(data1[,input$number_modify],na.rm=T),
                    max=max(data1[,input$number_modify],na.rm=T),
                    value=median(data1[,input$number_modify],na.rm=T))  
}  
  
#create group names for factor based on break value    
observeEvent(input$breaks,{
updateTextInput(session, "name3", value=paste(input$number_modify,"<",input$breaks,sep=""))
updateTextInput(session, "name4", value=paste(input$number_modify,">=",input$breaks,sep=""))
})
}
})
  
#if new variable is confirmed
observeEvent(input$go5,{
  #add new variable to data and give temporary name  
  globals$mydf$newvar<-ifelse((globals$mydf[,input$number_modify])<as.numeric(input$breaks),input$name3,input$name4)
  
  #if grouping has never been done before then give orignal name_grp
  if(!paste(input$number_modify,"grp",sep="_")%in%colnames(globals$mydf)){
      nm<-paste(input$number_modify,"grp",sep="_")
  }
  #if name has been used before then give sequential name
  else{
    #identify how many columns already exist
    i<-sum(grepl(paste(input$number_modify,"grp",sep="_"),colnames(globals$mydf)))
    #original is effectively the 0th column so adding i to column name will be fine
    nm<-paste(input$number_modify,"grp",i+1,sep="_")  
    
  }
  #give column the name
  colnames(globals$mydf)[ncol(globals$mydf)]<-nm
  
#create the message to push with the new variable name  
  output$message2 <- renderText({
    paste("New Variable '",nm,"' created",sep="")
  })

  
  choice<-colnames(globals$mydf)[ !colnames(globals$mydf)%in%skiplist&grepl(paste(skippatter,collapse="|"),
                                                                            colnames(globals$mydf))==FALSE]
#include new variable as a possible covariate
  #TO DO - how to update without erasing previous selection?
updateSelectInput(session, "covars", choices = choice)

chars <- colnames(globals$mydf)[unlist(lapply(globals$mydf[,colnames(globals$mydf)],
                                              function(x){class(x)%in%c("character","factor")})) ] 
chars<-chars[!chars%in%skiplist]
chars<-chars[grepl(paste(skippatter,collapse="|"),chars)==FALSE]
#TO DO: WHAT OTHER CLASS TYPES ARE MISSING AND IMPORTANT?

updateSelectInput(session, "char_modify", choices = chars)
chars_2lev<-chars

updateSelectInput(session, "graphfac", choices = chars_2lev)


#####
###################  
})
############
  
####SECTION B - Outputs  

# 1 - factor plots
##Outputs: 
  #winning plots
  #confidence interval plots
    
output$CIPlot <- renderPlot({
  
  validate(need(length(input$graphfac)>0,"Please select a factor to plot"))
  validate(need(input$graphfac%in%colnames(globals$mydf),"Data Loading. Please Wait"))
  
  
R<-to_rankings(globals$mydf,
               items = c(which(colnames(globals$mydf)=="variety_a"),which(colnames(globals$mydf)=="variety_b"),
                         which(colnames(globals$mydf)=="variety_c")),
               rankings = c(which(colnames(globals$mydf)==paste(input$trait,"best",sep="_")),which(colnames(globals$mydf)==paste(input$trait,"worst",sep="_"))),
               type = "tricot")

mod1<-tryCatch( {PlackettLuce(R)},
          error=function(x) {return(NULL)})

validate(need(length(mod1)>0,"Plackett Luce Model Unable to Converge") )

split=factor(globals$mydf[,input$graphfac])

out<-NULL
for(i in 1:nlevels(split)){
  mod_t<-tryCatch( {update(mod1,rankings=R[split==levels(split)[i],])},
    error=function(x) {return(NULL)})
  
  validate(need(length(mod_t)>0,"Plackett-Luce model unable to converge for given factor"))
  
  qv1<-  tryCatch( {qvcalc(mod_t)$qvframe},
                   error=function(x) {return(NULL)})
  
  validate(need(length(qv1)>0,"Plackett-Luce model unable to converge for given factor"))
  
    tmp<-data.frame(var=rownames(qv1),
                    split=levels(split)[i],qv1)
  tmp$estimate_adj<-tmp$estimate-mean(tmp$estimate)
  
  
  out<-rbind(out,tmp)
}

out$var<-reorder(out$var,out$estimate_adj,mean)

p1<-ggplot(data=out,aes(y=estimate_adj,x=var,ymax=estimate_adj+qnorm(0.92)*quasiSE,
                    ymin=estimate_adj-qnorm(0.92)*quasiSE,col=split))+
  geom_errorbar(width=0.2,position = position_dodge(width=0.25))+
  geom_point(position = position_dodge(width=0.25))+coord_flip()+ylab("Estimate")+xlab("Variety")+labs(col=input$graphfac)

plot(p1)

})


output$winplot <- renderPlotly({
  
  validate(need(input$graphfac%in%colnames(globals$mydf),"Data Loading. Please Wait"))
  
#selection is forced to be  group max so can hard code this in now to cheat
  #but maybe something to come back to make a bit more generalisable
  tmp<-globals$mydf
tmp$split=factor(tmp[,input$graphfac])
  
varorder<-favourability(a = tmp$variety_a,
                               b = tmp$variety_b,
                               c = tmp$variety_c,
                               best =tmp[,paste(input$trait,"best",sep="_")] ,
                               worst =tmp[,paste(input$trait,"worst",sep="_")])


if(length(levels(tmp$split))>1){


fav<-  purrr::map_df(1:nlevels(tmp$split),function(x){
    tmp1<-tmp[tmp$split==levels(tmp$split)[x]&is.na(tmp$split)==FALSE,]
    data.frame(favourability(a = tmp1$variety_a,
                             b = tmp1$variety_b,
                             c = tmp1$variety_c,
                             best =tmp1[,paste(input$trait,"best",sep="_")] ,
                             worst =tmp1[,paste(input$trait,"worst",sep="_")]),factor=levels(tmp$split)[x])
  })
  
}
if(length(levels(tmp$split))==1){
fav<-varorder
fav$factor<-levels(tmp$split)[1]
}
#get overall ordering
#maybe overkill doing it this way but could be useful in case of massive interaction

fav$var<-factor(fav$var,levels=levels(varorder$var))


p1<-ggplot(data=fav,aes(y=wins,x=var,fill=var))+
  facet_wrap(~factor)+
  geom_bar(stat="identity",position="dodge",show.legend = TRUE)+
    coord_flip()+
      scale_y_continuous(labels=percent)+
        xlab("Variety")+
          ylab("% of 'Head-to-Head' Contests Won")+
            ggtitle("% of Head-To-Head Wins",
                    subtitle = paste(input$trait,"by",input$graphfac))

plotly::ggplotly(p1)

})

  output$plot_summ <- renderPlot({
  
    if(is.character(globals$mydf[,input$char_modify])|
       is.factor(globals$mydf[,input$char_modify])){
      
      
      
      all_inputs <- names(session$input)[grep(paste(input$char_modify,"grp",sep="_"),names(session$input))]
      
      input_grps <- plyr::ldply(all_inputs, function(i){
        data.frame(input_name = i, input_value = input[[i]],stringsAsFactors = FALSE,
                   orig=gsub(paste(input$char_modify,"grp_",sep="_"),"",i))
      })
    
      validate(need(length(input_grps$input_value)>0,"Plot Loading. Please Wait"))
      
      input_grps1<-subset(input_grps,input_value!="")
 
      tmp<-data.frame(orig=globals$mydf[,input$char_modify])
      tmp<-merge(tmp,input_grps,by="orig",all.x=TRUE,all.y=FALSE)
      zz1<-data.frame(table(tmp$orig,tmp$input_value))
     zz1<-subset(zz1,Freq>0)
     zz1$Var1<-factor(zz1$Var1,levels=rev(sort(unique(zz1$Var1))))
   p1<-ggplot(data=zz1,aes(x=Var1,fill=Var2,y=Freq))+
        geom_bar(aes(fill=Var2),show.legend = TRUE,stat="identity")+ 
     geom_label(aes(label=paste("n=",Freq,sep=""))) +
     coord_flip()+
     xlab(input$char_modify)+
     ylab("Frequency")+
     labs(fill="New Group")
    }
    else{
      p1<-ggplot(data=globals$mydf,aes_string(x=input$char_modify))+
        geom_histogram()
    }
    p1
  })
  
  output$sumtab <- renderTable({
    tmpfac<-(globals$mydf[,input$number_modify])>=as.numeric(input$breaks)
    t1<-table(tmpfac) %>%
      data.frame() 
    colnames(t1)<-c("Group","N")
    t1$Group<-c(input$name3,input$name4)
    t1
  })
  
  
  
  output$plot_summ2 <- renderPlot({
    
    if(is.character(globals$mydf[,input$number_modify])|
       is.factor(globals$mydf[,input$number_modify])){
      
      n1=data.frame(table(globals$mydf[,input$number_modify]))
      
      
      p1<-ggplot(data=n1,aes(x=Var1,y=Freq))+
        geom_bar(aes(fill=Var1),show.legend = FALSE,stat="identity")+ 
        geom_label(aes(label=paste("n=",Freq,sep=""))) +
        coord_flip()+
        xlab(input$number_modify)+
        ylab("Frequency")
    }
    else{
     
        
      tmpfac<-(globals$mydf[,input$number_modify])>=as.numeric(input$breaks)
      
      tmpfac<-factor(tmpfac,levels=c(FALSE,TRUE),
                     labels=c(paste(input$number_modify,c("<",">="),input$breaks)))
      
      p1<-ggplot(data=globals$mydf,aes_string(x=input$number_modify))+
        geom_histogram(aes(fill=tmpfac),position = "dodge")+
        geom_vline(xintercept=as.numeric(input$breaks))+
        labs(fill="New Group")
      }
     
      
      
      
      
  
    
    p1
  })
  observeEvent(input$go,{
    
    validate(need(length(input$covars)>0,
                  "Please select one or more covariates"))
    
    R<-to_rankings(globals$mydf,
                   items = c(which(colnames(globals$mydf)=="variety_a"),which(colnames(globals$mydf)=="variety_b"),
                             which(colnames(globals$mydf)=="variety_c")),
                   rankings = c(which(colnames(globals$mydf)==paste(input$trait,"best",sep="_")),which(colnames(globals$mydf)==paste(input$trait,"worst",sep="_"))),
                   type = "tricot")
    dt1<-data.frame(Rg=grouped_rankings(R, index = seq_len(nrow(R))),
                    globals$mydf[,input$covars])
    
    colnames(dt1)[-1]<-input$covars
    
    alpha1<<-as.numeric(input$alpha)
    minsize1<<-as.numeric(input$minsize)
    
    for(i in 2:ncol(dt1)){
      if(is.character(dt1[,i])){
        dt1[,i]<-as.factor(dt1[,i])
      }
    }
    
    
    mod_t<-tryCatch( {pltree(Rg~.,data=dt1,alpha=alpha1,minsize=minsize1)},
                    error=function(x) {return(NULL)})
    
    

    output$NodePlot <- renderPlot({
      
      validate(need(length(mod_t)>0,"Plackett-Luce model unable to converge. Try increasing minimum group size."))
      
      coefs<-map_df(nodeids(mod_t,terminal = TRUE),
                    function(x)data.frame(node=x,
                                          rule=partykit:::.list.rules.party(mod_t, x),
                                          multcompPL(mod_t[[ x ]]$node$info$object)))
      
      coefs$Label<-paste("Node",coefs$node,":",coefs$rule)
      coefs<-coefs %>% group_by(node) %>% mutate(m=mean(estimate),ctd=estimate-m) %>%data.frame()
      
      ggplot(data=coefs,aes(x=term,y=ctd,ymax=ctd+1.96*quasiSE,ymin=ctd-1.96*quasiSE,col=Label))+
        geom_point(position = position_dodge(width=0.3))+
        geom_errorbar(position = position_dodge(width=0.3),width=0)+
        coord_flip()+
        geom_text(aes(label=.group),size=3,nudge_x=-0.2)+
        ylab("")+
        xlab("Variety")+
        ggtitle(paste("Terminal node parameter estimates for",input$trait,"rankings."),
                subtitle = paste("Covariates considered:",paste(input$covars,collapse=", ")))
      
      
    })
    
    
    
  
  output$TreePlot <- renderPlot({
    
    validate(need(length(mod_t)>0,"Plackett-Luce model unable to converge. Try increasing minimum group size."))
    
      plot(mod_t)
    
    })
    
    output$ModelOut <- renderTable({
validate(need(length(mod_t)>0,"Plackett-Luce model unable to converge. Try increasing minimum group size."))
      
      n1<-length(names(mod_t))
      xxx<-NULL
      for(i in 1:n1){
        tmp<- nodeapply(mod_t,i,info_node)[[1]]$test
        
        if(length(tmp)>0){
          
          tmp1<-paste(format.pval(tmp[2,],digits = 3),stars.pval(tmp[2,]),sep="")
          
          out<- data.frame(Node=paste("Node",i),
                           Variable=colnames(tmp),Statistic=tmp[1,],pvalue=tmp1)
          xxx<-rbind(xxx, out  )
        }
        
        if(length(tmp)==0){
          xxx<-rbind(xxx,  data.frame(Node=paste("Node",i),Variable="No further splits possible",Statistic=NA,pvalue=NA) ) 
        }
      }
      xxx
      
    })
    
    
  })

}
  
# Run the application 
shinyApp(ui = ui, server = server)
}

data("breadwheat")
PL_Covar(breadwheat)




