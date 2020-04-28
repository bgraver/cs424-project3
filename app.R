library(shiny)
library(dplyr)
library(stringr)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(tidyr)
library(DT)
library(lubridate)


# just load the file regularly
data <- read.csv("combined_data.csv")


# cleaner <- data[! ((data$kind == "videogame") | (data$certificates_rating == "USA:X") | (data$certificates_rating == "USA:NC-17")), ]
# removes videogames, tv, videos, ratings
# cleaning everythign minus the keywords and genres
cleaner <- data[! (grepl("(V)", data$id, fixed=TRUE)) | (grepl("(TV)", data$id, fixed=TRUE)) | (grepl("(VG)", data$id, fixed=TRUE)) | (data$certificates_rating == "USA:X") | (data$certificates_rating == "USA:NC-17") | (data$certificates_country != "USA") | (data$certificates_rating == "TV-G") | (data$certificates_rating == "TV-14") | (data$certificates_rating == "TV-Y") | (data$certificates_rating == "TV-PG") | (data$certificates_rating == "TV-MA") | (data$certificates_rating == "E") | (data$certificates_rating == "E10+") | (data$certificates_rating == "T") | (data$certificates_rating == "C") | (data$certificates_rating == "M") | (data$certificates_rating == "Not Rated") | (data$running_times_secs > 3600), ]

# cleaner <- cleaner[! (data$certificates_country != "USA") | (data$certificates_rating == "TV-G") | (data$certificates_rating == "TV-14") | (data$certificates_rating == "TV-Y") | (data$certificates_rating == "TV-PG") | (data$certificates_rating == "TV-MA") | (data$certificates_rating == "E") | (data$certificates_rating == "E10+") | (data$certificates_rating == "T") | (data$certificates_rating == "C") | (data$certificates_rating == "M") | (data$certificates_rating == "Not Rated") | (data$running_times_secs > 3600), ]

# cleaning the genres
genres <- as.character(unique(cleaner$genres))
indiv.genres <- c()
# 1) tokenize the string into pieces based on the comma
# 2) check each piece of the string to see if its a new type of genre (check the indiv.genres$Genre)

for(genre in genres) {
    tokens <- strsplit(genre, ",")[[1]]
    # append(tokens, indiv.genres)
    indiv.genres <- c(indiv.genres, tokens)
}

# plot just for genres
df.genre <- as.data.frame(table(indiv.genres))

# Just for sorting
# df.genre[order(df.genre$Freq, decreasing=TRUE), ]
unwanted_genres <- c("Short", "Adult", "Reality-TV", "Talk-Show", "Game-Show", "News", "Film-Noir", "Sci-fi")
for (genre in unwanted_genres) {
    cleaner <- cleaner[! ( grepl(genre, cleaner$genres, fixed=TRUE) ), ]
}

df.genre <- df.genre[! (df.genre$Freq < 100), ]

# cleaner$month <- month(as.POSIXct(cleaner$release_dates_date, format="%d %m %Y")), 

# setting the months and year up
# cleaner$release_dates_date <- as.character(cleaner$release_dates_date)
cleaner$date <- dmy_h(cleaner$release_dates_date)
cleaner$month <- month(cleaner$date, label=TRUE)
cleaner$year <- year(cleaner$date)

# ggplot months
month <- cleaner$month
month <- month[!is.na(month)]
df.month <- as.data.frame(table(month))
###########################################################################################################



# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Project 3: Saturday Night at the Movies"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
                     sidebarMenu(menuItem("Movie Graph", tabName = 'movieGraph'),
                                 menuItem("Movie Tables", tabName = 'movieTab'),
                                 radioButtons("radio", label = "Search by: ",
                                              choices = list("Drama" = 1, "Crime" = 2, "Action" = 3,
                                                             "Adventure" = 4, "Fantasy" = 5, "Biography" = 6,
                                                             "History" = 7, "Sci-Fi" = 8, "Romance" = 9,
                                                             "Western" = 10, "Animation" = 11, "Family" = 12,
                                                             "War" = 13, "Comedy" = 14, "Mystery" = 15,
                                                             "Thriller" = 16, "Music" =17, "Horror" = 18,
                                                             "all Genres" = 19), 
                                              selected = 1),
                                 menuItem("Info", tabName = 'Info')
                     )
                     
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'movieGraph',
                    fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"), plotOutput("genrePlot"), plotOutput("monthPlot"))
                    )
                   # fluidRow(
                   #     column(1, 
                   #            box(title="Movie Graph", solidHeader=TRUE, status="primary", width=12,
                   #                plotOutput("genrePlot"))
                   #     )
                   # ),
                   # fluidRow(
                   #     column(6, 
                   #            box(title="Movie Graph", solidHeader=TRUE, status="primary", width=12,
                   #                plotOutput("monthPlot"))
                   #     )
                   # )
            ),
            tabItem(tabName = 'movieTab',
                    fluidRow(
                        splitLayout(cellWidths = c("50%", "50%"), dataTableOutput("DT.genre"), dataTableOutput("DT.month"))
                    ))
            ,
            tabItem(tabName = 'Info',
                    fillPage(
                        title = 'Movie Info',
                        box(title="About", solidHeader=TRUE,  status="primary", width=12,
                            htmlOutput("about", height=200))
                    )
            )
            
        )
    )
)

server <- function(input, output) {
    
    output$genrePlot <- renderPlot({
        # plot just for genres
        # df.genre <- as.data.frame(table(indiv.genres))
        # 
        # # Just for sorting
        # # df.genre[order(df.genre$Freq, decreasing=TRUE), ]
        # unwanted_genres <- c("Short", "Adult", "Reality-TV", "Talk-Show", "Game-Show", "News", "Film-Noir", "Sci-fi")
        # for (genre in unwanted_genres) {
        #     data <- data[! ( grepl(genre, data$genres, fixed=TRUE) ), ]
        # }
        # 
        # df.genre <- df.genre[! (df.genre$Freq < 100), ]
        # 
        g <- ggplot(data=df.genre, aes(indiv.genres, y=Freq)) + geom_bar(stat="identity")  + labs(x="Genres", y="Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        g
    })
    
    output$monthPlot <- renderPlot({
        # ggplot - number of graphs released each month of the database
        g <- ggplot(data=df.month, aes(month, y=Freq)) + geom_bar(stat="identity")
        g
        
    })
    
    output$DT.month <- DT::renderDataTable(
        DT::datatable(
            {
                df.month
            },
            options = list(searching=FALSE, pageLength=10, lengthChange=FALSE,  rownames=FALSE)
        )
    )
    
    output$DT.genre <- DT::renderDataTable(
        DT::datatable(
            {
                df.genre
            },
            options = list(searching=FALSE, pageLength=10, lengthChange=FALSE,  rownames=FALSE)
        )
    )
    
    
    output$value <- renderPrint({ input$radio })
    
    output$about <- renderUI({
        str0 <- paste("- Dashboard made by Brandon Graver, Nicholas Abassi, and Ho Chon.")
        str1 <- paste("- Libraries used: Shiny, Shinydashboard, dplyr, ggplot2, stringr, tidyr, DT")
        str2 <- paste("- Data comes from ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/")
        HTML(paste(str0, str1, str2, sep='<br>'))
    })
    
}

shinyApp(ui = ui, server = server)