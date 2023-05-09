#Libraries
library(shiny)
library(ggplot2)
library(reshape2)

default_row <- 10
default_col <- 10

ui <- fluidPage(

    titlePanel("Picross!"),
    sidebarLayout(
        sidebarPanel(
            # Game information
            tags$div(class="header", checked=NA,
                     tags$p("Picross is a puzzle game where the goal is to use the clues along the edge of the grid to fill in the grid squares"),
                     tags$p("Single click to fill in a square, double click to block off a square, single click again to clear squares"),
                     tags$a(href="https://en.wikipedia.org/wiki/Nonogram", "Click here for description and rules"),
                     tags$p("Clues are read left-to-right on the Y axis, and bottom-to-top on the X axis"),
                     tags$a(href="https://github.com/hbwddl/rshiny-picross", "Github repository")
                     ),
            # Input for number of rows and columns
            sliderInput("n_row",
                        "Number of Rows:",
                        min = 5,
                        max = 20,
                        value = default_row),
            sliderInput("n_col",
                        "Number of Columns:",
                        min = 5,
                        max = 20,
                        value = default_col),
            # Reset the board or update the number of rows and columns
            actionButton("update", "Reset/Update"),
            
            # Display the correct answer
            radioButtons("show_answer",label="Show Answer",
                         choices=list("Yes" = 1,"No"=0),
                         selected=0)
        ), 

        # Show the puzzle grid
        mainPanel(
           plotOutput("picross",click = "plot.click",dblclick = "plot.dblclick")
        )
    )
)

server <- function(input, output) {
    
    board_nrow <- reactiveVal(default_row)
    board_ncol <- reactiveVal(default_col)
    
    answer_board <- reactive(makePicross(row_n = board_nrow(),col_n = board_ncol()))
    answer_plot_board <- reactive(melt(answer_board(),factorsAsStrings=F))
    
    play.board <- reactive({
        matrix(data=0,nrow=board_nrow(),ncol=board_ncol(),dimnames = list(rownames(answer_board()),colnames(answer_board())))
    })
    
    play_plot_board <- reactive(melt(play.board(),factorsAsStrings=F))
    
    click_data <- reactiveVal( rep(0, isolate(board_nrow())* isolate(board_ncol())) )
    
    play_plot_board2 <- reactive(cbind(play_plot_board(),click_data()))
    
    r_show_answer <- reactive(input$show_answer)
    
    # Store whether or not the board has been clicked on, in order to clear it with a single click
    v <- reactiveValues(
        click1 = NULL,  # Represents the first mouse click, if any
        click2 = NULL
    )
    
    # Update the board if button is pressed
    observeEvent(input$update, {
        board_nrow(input$n_row)
        board_ncol(input$n_col)
        
        click_data(rep(0, board_nrow()* board_ncol()) )
        
        answer_board <- reactive(makePicross(row_n = board_nrow(),col_n = board_ncol()))
        answer_plot_board <- reactive(melt(answer_board(),factorsAsStrings=F))
        
        play_plot_board2 <- reactive(cbind(play_plot_board(),click_data()))
    })
    
    # Fill in with a blue tile if the grid is clicked once
    observeEvent(input$plot.click,{
        if(is.null(v$click1)){
            v$click1 <- input$plot.click
        }
        
        click.row <- reactive(floor(input$plot.click$y + 0.5))
        click.col <- reactive(floor(input$plot.click$x + 0.5))

        newClick <- click_data()
        
        if(newClick[((click.col()-1)*board_nrow()) + click.row()] == 1){
            newClick[((click.col()-1)*board_nrow()) + click.row()] <- 0
        }
        else if(newClick[((click.col()-1)*board_nrow()) + click.row()] == -1){
            newClick[((click.col()-1)*board_nrow()) + click.row()] <- 0
        }
        else{
            newClick[((click.col()-1)*board_nrow()) + click.row()] <- 1
        }
        
        click_data(newClick)
    })
    
    # Fill in the board with a red square if user double clicks
    observeEvent(input$plot.dblclick,{
        dbl.row <- reactive(floor(input$plot.dblclick$y + 0.5))
        dbl.col <- reactive(floor(input$plot.dblclick$x + 0.5))
        
        newClick <- click_data()
        
        newClick[((dbl.col()-1)*board_nrow()) + dbl.row()] <- -1
        
        click_data(newClick)
    })

    # The board is colored in by graphing a tiled heatmap using the values -1 (red), 0 (white/null), and blue (1)
    play_color_blank <- "white"
    play_color_fill <- "steelblue"
    play_color_block <- "firebrick"
    
    output$picross <- renderPlot({
        input$update
        if(r_show_answer() == 1){ # Display correct answer if correct answer is filled in
            ggplot(data = answer_plot_board(),aes(x=Var2,y=Var1,fill=value)) +
                geom_tile(data=answer_plot_board(),color="black") +
                scale_fill_gradient(low=play_color_block,high=play_color_fill) +
                scale_x_discrete(position = "top") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_blank(),axis.title.y=element_blank(),legend.position = "none", text = element_text(size=18))
            }
            else{
                if(is.null(v$click1)){
                    ggplot(data = play_plot_board(),aes(x=Var2,y=Var1,fill=value)) +
                        geom_tile(data=play_plot_board(),color="black") +
                        scale_fill_gradient2(low=play_color_block,high=play_color_fill,mid=play_color_blank) +
                        scale_x_discrete(position = "top") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_blank(),axis.title.y=element_blank(),legend.position = "none", text = element_text(size=18))
                }
                else{
                    ggplot(data = play_plot_board2(),aes(x=Var2,y=Var1,fill=click_data())) +
                        geom_tile(data=play_plot_board2(),color="black") +
                        scale_fill_gradient2(low=play_color_block,high=play_color_fill,mid=play_color_blank) +
                        scale_x_discrete(position = "top") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_blank(),axis.title.y=element_blank(),legend.position = "none", text = element_text(size=18))
                }
            }
        }, height=600,width=600)
}

#Picross Functions
#Construct a Picross board. A higher value of prob_filled will result in a more densely-colored playing grid
makePicross <- function(row_n=10,col_n=10,prob_filled = 0.6){
    if((row_n > 20) | (col_n > 20)){
        break
    }
    if((row_n < 5) | (col_n < 5)){
      break
    }
  
    board <- matrix(data=NA,nrow=row_n,ncol=col_n)
    row.clues <- rep(NA,row_n)
    for(i in 1:row_n){
        new.row <- makeRow(col_n,prob_fill = prob_filled)
        board[i,] <- new.row
        row.clue <- makeClue(new.row)
        row.clue.name <- paste0(row.clue,collapse=" ")
        row.clues[i] <- row.clue.name
    }
    
    col.clues <- rep(NA,col_n)
    for(j in 1:col_n){
        col.clue <- makeClue(board[,j])
        col.clue.name <- paste0(col.clue,collapse=" ")
        col.clues[j] <- col.clue.name
    }
    
    unique.row <- make.unique(row.clues)
    unique.col <- make.unique(col.clues)
    
    which.dup.row <- grep("\\.",unique.row)
    n.dup <- 1
    
    for(k in which.dup.row){
        a.clue <- unique.row[k]
        dot.string <- paste(rep(".",n.dup),collapse="")
        sp.string <- paste(rep(" ",n.dup),collapse="")
        clue.sub <- substr(a.clue,1,nchar(a.clue)-2)
        
        unique.sp <- paste(sp.string,clue.sub,collapse="")
        
        unique.row[k] <- unique.sp
        
        n.dup <- n.dup + 1
    }
    
    which.dup.col <- grep("\\.",unique.col)
    n.dup <- 1
    for(k in which.dup.col){
        a.clue <- unique.col[k]
        dot.string <- paste(rep(".",n.dup),collapse="")
        sp.string <- paste(rep(" ",n.dup),collapse="")
        clue.sub <- substr(a.clue,1,nchar(a.clue)-2)
        
        unique.sp <- paste(sp.string,clue.sub,collapse="")
        
        unique.col[k] <- unique.sp
        
        n.dup <- n.dup + 1
    }
    
    rownames(board) <- unique.row
    colnames(board) <- unique.col
    
    return(board)
}

# Creates clue when given a vector of 1's and 0's. 
makeClue <- function(picr_vec){
    clue <- c()
    if(sum(picr_vec) == 0){
        clue <- c(0)
    }
    chunk <- 0
    for(i in 1:length(picr_vec)){
        if(picr_vec[i] == 0){
            if(chunk > 0){
                clue <- append(clue,chunk)
            }
            chunk <- 0
        }
        else{
            chunk <- chunk+1
        }
        if((i == length(picr_vec)) & (chunk > 0)){
            clue <- append(clue,chunk)
        }
    }
    return(clue)
}

makeRow <- function(row_length,prob_fill = 0.6){
    a.row <- sample(c(0,1),size=row_length,replace=T,prob=c(1-prob_fill,prob_fill))
    return(a.row)
}

# Run the application 
shinyApp(ui = ui, server = server)