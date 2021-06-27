# AA,2019.09.19.
# Warehouse Stock Monitoring


library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(shinybusy)
library(RMariaDB)
library(DT)
library(openxlsx)
library(rpivotTable)
library(dplyr)
library(ggplot2)
library(forcats)
library(shinyjs)
library(lubridate)



pw <- {"password"}
u <- {"user"}
db <- {"db_name"}
db2 <- {"db2_name"}
h <- {"host_ip"}
p <- {0000}
tz="Europe/Prague"



shinyServer(function(input, output, session) {
    
    ## Show szerver info
    # realtime_szerver_info <- reactivePoll(5000,
    #                                       
    #                                       session,
    #                                       
    #                                       checkFunc = function(){
    #                                           
    #                                           con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
    #                                           update_time <- dbGetQuery(con2, "SELECT...")
    #                                           dbDisconnect(con2)
    #                                           
    #                                           update_time
    #                                       },
    #                                       
    #                                       valueFunc = function() {
    #                                           
    #                                           con<-dbConnect(MariaDB(),user=u,password=pw,dbname=db,host=h,port=p)
    #                                           info <- dbGetQuery(con, "show slave status;")
    #                                           dbDisconnect(con)
    #                                           
    #                                           info
    #                                       })
    
    output$text <- renderUI({
        
        #info <- realtime_szerver_info()
        info <- data.frame(Seconds_Behind_Master=0, Slave_SQL_Running="Yes")
        
        str1 <- paste("Server: Name (IP)")
        str2 <- paste("Synchron: ", ifelse(info$Slave_SQL_Running=="Yes", "Working", "Not Working"))
        
        library(lubridate)
        td <- seconds_to_period(info$Seconds_Behind_Master)
        str3 <- paste("Behind Master: ", sprintf('%02d %02d:%02d:%02d', day(td), td@hour, minute(td), second(td)))
        str4 <- ifelse(str3 == "Cancellation:  NA NA:NA:NA", "Cancellation: ?", str3)
        
        HTML(paste(tags$li(str1), tags$li(str2), tags$li(str4), sep = ''))
        
    })
    
    
    
    
    ## Get Update date
    # realtime_data1 <- reactivePoll(10000,
    #                                
    #                                session,
    #                                
    #                                checkFunc = function(){
    #                                    
    #                                    con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
    #                                    update_time_dbok <- dbGetQuery(con2, "SELECT...")
    #                                    dbDisconnect(con2)
    #                                    
    #                                    update_time_dbok
    #                                },
    #                                
    #                                valueFunc = function() {
    #                                    
    #                                    con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
    #                                    fr <- dbGetQuery(con2, sprintf("SELECT..."))
    #                                    dbDisconnect(con2)
    #                                    
    #                                    fr
    #                                })
    
    output$title <- renderText({
        #fr <- realtime_data1()
        fr <- data.frame(UPDATE_TIME=ymd_hms("2021-01-31 20:11:59"))
        sprintf("Warehouse Stock Monitoring (Update: %s)", format(lubridate::with_tz(fr$UPDATE_TIME, tz), format = "%Y.%m.%d. %H:%M"))
    })
    
    
    

    ## Update Data
    observeEvent(input$friss, {

        # con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
        # update_time <- dbGetQuery(con2, "SELECT...")
        # dbDisconnect(con2)

        update_time <- data.frame(UPDATE_TIME=ymd_hms("2021-01-31 20:11:59"))

        update_time_plus <- update_time$UPDATE_TIME + 600

        if (Sys.time() <= update_time_plus) {
            shinyalert("The update has been completed.", type = "success", animation=TRUE)
        } else {


            show_modal_spinner(
                spin = "circle",
                color = "#224f77",
                text = "Waiting..."
            )


            # con<-dbConnect(MariaDB(),user=u,password=pw,dbname=db,host=h,port=p)
            # con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
            #
            # ## Based on warehouse bills
            # dbSendQuery(con2,"drop table...")
            # dbSendQuery(con,"CREATE TABLE...")
            #
            # dbDisconnect(con2)
            # dbDisconnect(con)
            #

            Sys.sleep(5)

            remove_modal_spinner()

            shinyalert("The update has been completed.", type = "success", animation=TRUE)

        }

    })


    
    
    
    
    # 1. Movement of devices
    
    
    
    ## Group by Type
    
        ### Create a Pivot table
        # realtime_pivot_df_type <- reactivePoll(10000,
        #                                       session,
        #                                       checkFunc = function(){
        #                                           con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
        #                                           update_time<- dbGetQuery(con2, "SELECT...")
        #                                           dbDisconnect(con2)
        #                                           update_time
        #                                       },
        #                                       
        #                                       valueFunc = function() {
        #                                           con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
        #                                           df <- dbGetQuery(con2, "SELECT...")
        #                                           dbDisconnect(con2)
        #                                           df
        #                                       })
        
        
        output$pivot_device_type <- renderRpivotTable({
            #alcs_df <-  realtime_pivot_df_type()
            alcs_df <-  read.xlsx("C:/Users/Admin/Desktop/Github_demo/warehouse_equipments.xlsx")
                
            alcs_df2 <- alcs_df %>% 
                select(year, month, equipment_type, movement, total)
            colnames(alcs_df2) <- c("Year", "Month", "Type", "Movement", "Piece")
            
            rpivotTable(alcs_df2,rows=c("Type", "Movement"), cols=c("Year","Month"), vals = "Piece", rendererName = "Table With Subtotal Row Heatmap", aggregatorName = "Integer Sum" ,subtotals=TRUE, width="100%", unusedAttrsVertical=F)
            
        })
        
        ## Download xlsx file
        date = format(as.POSIXct(Sys.Date(), tz=tz), format="%y%m%d")
        
        output$dl2 <- downloadHandler(
          
          filename = function() { paste0("equipment_movement_group_type_",date,".xlsx")},
          content = function(file) {
            
            #fr <- realtime_data1()
            #y <- realtime_pivot_df_type()
              
            fr <- data.frame(UPDATE_TIME=ymd_hms("2021-01-31 20:11:59"))
            y <- read.xlsx("C:/Users/Admin/Desktop/Github_demo/warehouse_equipments.xlsx")
              
            y<- y %>% 
              group_by(equipment_type,year, month, movement) %>% 
              summarize(db=sum(total)) %>% 
              arrange(equipment_type,year, month, movement)
            
            colnames(y) <- c("Type", "Year", "Month", "Movement", "Piece")
            
            wb <- createWorkbook()
            sheet1 <- addWorksheet(wb, "Pivot")
            setColWidths(wb,sheet1,cols= c(1:nrow(y)),widths = "23")
            writeDataTable(wb, sheet1, startRow = 3, y)
            
            text <- "Some information about the datatable..."
            writeData(wb, sheet1, text, startRow = 1, startCol = 1, rowNames = F)
            addText1 <- createStyle(fontSize = 14, fontColour = "#0e1111", textDecoration = "bold", halign = "left",fgFill = "white")
            addStyle(wb, sheet1, addText1, rows = 1, cols = 1, gridExpand = F)
            
            text2 <- sprintf("Update: %s", format(lubridate::with_tz(fr$UPDATE_TIME, tz), format = "%Y.%m.%d. %H:%M"))
            writeData(wb, sheet1, text2, startRow = 2, startCol = 1, rowNames = F)
            addText2 <- createStyle(fontSize = 12, fontColour = "#0e1111", textDecoration = "bold", halign = "left",fgFill = "white")
            addStyle(wb, sheet1, addText2, rows = 2, cols = 1, gridExpand = F)
            
            saveWorkbook(wb, file)
          }
        )
        
        
        
        
        ### Create barplots (group by type)
        
        for (i in 1:8) {
          
          local({
            local_i <- i
            
            nev <- paste0("plot_type", local_i)
            
            output[[nev]] <- renderPlot({
              
              # con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
              # df <- dbGetQuery(con2, "select...")
              # dbDisconnect(con2)
              
              df <- read.xlsx("C:/Users/Admin/Desktop/Github_demo/warehouse_equipments.xlsx")
              
              # group by type, year, month, movement (without grid!)
              y <- df %>% 
                mutate(year_month=paste0(year, "_",month)) %>% 
                group_by(equipment_type,year_month, year, month, movement) %>% 
                summarize(pc_label=sum(total)) %>% 
                mutate(pc_value=ifelse(movement=="At Costumers", -1*pc_label, pc_label))
              
              y <- y %>% 
                mutate(month_name = case_when(
                  month==1 ~ "January",
                  month==2 ~ "February",
                  month==3 ~ "March",
                  month==4 ~ "April",
                  month==5 ~ "May",
                  month==6 ~ "June",
                  month==7 ~ "July",
                  month==8 ~ "August",
                  month==9 ~ "September",
                  month==10 ~ "October",
                  month==11 ~ "November",
                  month==12 ~ "December"
                ),
                month_name = factor(month_name ,ordered = TRUE, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
                )
              
              # Plot function
              plot_type <- function(type) {
                plot <- y %>% 
                  filter(equipment_type==type) %>% 
                  mutate(movement=ifelse(movement=="At Costumers", "At Costumers", "In the Warehouse"))
                
                a <- ifelse(plot$equipment_type=="C", -500, ifelse(plot$equipment_type=="E", -50, ifelse(plot$equipment_type=="H", -300, ifelse(plot$equipment_type=="valami", -100, ifelse(plot$equipment_type=="F", -300, -200)))))
                b <- ifelse(plot$equipment_type=="C", 500, ifelse(plot$equipment_type=="E", 50, ifelse(plot$equipment_type=="H", 300, ifelse(plot$equipment_type=="F", 300, 200))))

                p <- plot %>% 
                  arrange(year, month_name) %>% 
                  ggplot(aes(month_name, pc_value, fill=movement))+
                  geom_col(position = "stack")+
                  scale_fill_manual("Movement",values = c("#CFCECA", "#dac38b")) +
                  scale_x_discrete(name="", limits = rev(levels(y$month_name))) +
                  geom_text(aes(y=ifelse(movement=="At Costumers", a, b), label = pc_value), color="black", size = 4, fontface = "bold") +
                  xlab("") + ylab("") +
                  coord_flip()+
                  facet_grid(year~.) +
                  geom_hline(yintercept = 0)+
                  scale_y_continuous(breaks = scales::breaks_extended(n = 15))
                
                p + theme_bw() + 
                  theme(panel.spacing = unit(1, "lines")
                        ,plot.title = element_text(size = 25, face = "bold",hjust = 0.5)
                        ,axis.text.x = element_text(angle = 0, hjust = 1)
                        ,text = element_text(size = 15)
                        #,plot.background = element_rect(fill = "#d9d9d9", color = NA) # bg of the plot
                        ,legend.position = "bottom"
                        ,plot.margin=unit(c(1,1,1.5,1.2),"cm")
                        ,strip.background=element_rect(color="black", fill="white", size=0.5)
                        ,strip.text.y=element_text(color="black", face="bold", angle=270, size=15, hjust=0.5, vjust=0.5, margin = margin(0,0.2,0,0.2, "cm"))) +
                  labs(title = paste0(plot$equipment_type[1]), subtitle="Movement of devices between customers and the warehouse")
              }
              
              if (local_i==1) {
                plot_type("A")
              } else if (local_i==2) {
                plot_type("B")
              } else if (local_i==3) {
                plot_type("C")
              } else if (local_i==4) {
                plot_type("D")
              } else if (local_i==5) {
                plot_type("E")
              } else if (local_i==6) {
                plot_type("F")
              } else if (local_i==7) {
                plot_type("G")
              } else {
                plot_type("H")
              }
              
              
            })
          })
        }
        
        
        
        
        
        
        
     
    ## Group by device number
        
        ### Create a Pivot table
        # realtime_pivot_df_type <- reactivePoll(10000,
        #                                       session,
        #                                       checkFunc = function(){
        #                                           con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
        #                                           update_time<- dbGetQuery(con2, "SELECT...")
        #                                           dbDisconnect(con2)
        #                                           update_time
        #                                       },
        #                                       
        #                                       valueFunc = function() {
        #                                           con2<-dbConnect(MariaDB(),user=u,password=pw,dbname=db2,host=h,port=p)
        #                                           df <- dbGetQuery(con2, "SELECT...")
        #                                           dbDisconnect(con2)
        #                                           df
        #                                       })
        
        
        output$pivot_device_type <- renderRpivotTable({
            #alcs_df <-  realtime_pivot_df_type()
            alcs_df <-  read.xlsx("C:/Users/Admin/Desktop/Github_demo/warehouse_equipments.xlsx")
                
            alcs_df2 <- alcs_df %>% 
                select(year, month, equipment_type, movement, total)
            colnames(alcs_df2) <- c("Year", "Month", "Type", "Movement", "Piece")
            
            rpivotTable(alcs_df2,rows=c("Type", "Movement"), cols=c("Year","Month"), vals = "Piece", rendererName = "Table With Subtotal Row Heatmap", aggregatorName = "Integer Sum" ,subtotals=TRUE, width="100%", unusedAttrsVertical=F)
            
        })
        
        ## Download xlsx file
        date = format(as.POSIXct(Sys.Date(), tz=tz), format="%y%m%d")
        
        output$dl2 <- downloadHandler(
          
          filename = function() { paste0("equipment_movement_group_type_",date,".xlsx")},
          content = function(file) {
            
            #fr <- realtime_data1()
            #y <- realtime_pivot_df_type()
              
            fr <- data.frame(UPDATE_TIME=ymd_hms("2021-01-31 20:11:59"))
            y <- read.xlsx("C:/Users/Admin/Desktop/Github_demo/warehouse_equipments.xlsx")
              
            y<- y %>% 
              group_by(equipment_type,year, month, movement) %>% 
              summarize(db=sum(total)) %>% 
              arrange(equipment_type,year, month, movement)
            
            colnames(y) <- c("Type", "Year", "Month", "Movement", "Piece")
            
            wb <- createWorkbook()
            sheet1 <- addWorksheet(wb, "Pivot")
            setColWidths(wb,sheet1,cols= c(1:nrow(y)),widths = "23")
            writeDataTable(wb, sheet1, startRow = 3, y)
            
            text <- "Some information about the datatable..."
            writeData(wb, sheet1, text, startRow = 1, startCol = 1, rowNames = F)
            addText1 <- createStyle(fontSize = 14, fontColour = "#0e1111", textDecoration = "bold", halign = "left",fgFill = "white")
            addStyle(wb, sheet1, addText1, rows = 1, cols = 1, gridExpand = F)
            
            text2 <- sprintf("Update: %s", format(lubridate::with_tz(fr$UPDATE_TIME, tz), format = "%Y.%m.%d. %H:%M"))
            writeData(wb, sheet1, text2, startRow = 2, startCol = 1, rowNames = F)
            addText2 <- createStyle(fontSize = 12, fontColour = "#0e1111", textDecoration = "bold", halign = "left",fgFill = "white")
            addStyle(wb, sheet1, addText2, rows = 2, cols = 1, gridExpand = F)
            
            saveWorkbook(wb, file)
          }
        )
        
    
    
    
    
    
    
    # 2. Current stock monitoring
    
    
    
    
    
    
    
    ### Info icon
    observeEvent(input$openModal, {
        showModal(
            modalDialog(
                title = HTML("<center> Warehouse Stock Monitoring app </center>"),
                HTML("The purpose of the application is to show a solution how we can monitoring movements of devices (uniqualy iditified) at a fictive telecommunication company.</br>
                      Click on the icon in the left corner next to the headline to update the data from 01/01/2019.</b> </br></br>
             
             <b>1. Movement of devices:</b>  You can see the movement of uniquely identified devices by types (A, B, C, e.t.) and all items (AA Equipment1001...).</br>
              Click on the icon next to the subheadings to download the data in xlsx format.</br>
              Within the Pivot Tables, it is possible to filter or choose a different display.</br>
              Pivot table colours are interpreted row by row, darker red indicates higher values.</br></br>
             - Type of device: It contains a 'Plots' and a 'Pivot' tab where you can read data from same dataset.</br></br>
             - Devices: </br></br>
             
            <b>2. Current stock monitoring:</b> "),
                easyClose = TRUE,
                size="l",
                footer = tagList(actionButton("close", "Close"))
            ))
    })
    observeEvent(input$close, {
        removeModal()
    }) 
    

    
})



