#Libraries
library(shiny)
library(ggplot2)
library(reshape2)

default.row <- 10
default.col <- 10

ui <- fluidPage(

    # Application title
    titlePanel("Picross!"),
    # Sidebar with: 
    #a slider input for number of rows and columns
    #A button to reset/update the board
    #Button to show answer
    sidebarLayout(
        sidebarPanel(
            tags$div(class="header", checked=NA,
                     tags$p("Single click to fill in, double click to block off, single click to clear"),
                     tags$a(href="https://en.wikipedia.org/wiki/Nonogram", "Click Here for Rules")
            ),
            sliderInput("n.row",
                        "Number of Rows:",
                        min = 5,
                        max = 20,
                        value = default.row),
            sliderInput("n.col",
                        "Number of Columns:",
                        min = 5,
                        max = 20,
                        value = default.col),
            actionButton("update", "Reset/Update"),
            radioButtons("show.answer",label="Show Answer",
                         choices=list("Yes" = 1,"No"=0),
                         selected=0)
        ), 

        # Show a plot of the play board
        mainPanel(
           plotOutput("picross",click = "plot.click",dblclick = "plot.dblclick")
        )
    )
)

server <- function(input, output) {
    board.nrow <- reactiveVal(default.row)
    board.ncol <- reactiveVal(default.col)
    
    #answer.board <- reactive(makePicross(row.n = input$n.row,col.n = input$n.col))
    answer.board <- reactive(makePicross(row.n = board.nrow(),col.n = board.ncol()))
    answer.plot.board <- reactive(melt(answer.board(),factorsAsStrings=F))
    
    play.board <- reactive({
        matrix(data=0,nrow=board.nrow(),ncol=board.ncol(),dimnames = list(rownames(answer.board()),colnames(answer.board())))
    })
    
    play.plot.board <- reactive(melt(play.board(),factorsAsStrings=F))
    
    click.data <- reactiveVal( rep(0, isolate(board.nrow())* isolate(board.ncol())) )
    
    play.plot.board2 <- reactive(cbind(play.plot.board(),click.data()))
    

    r.show.answer <- reactive(input$show.answer)
    
    v <- reactiveValues(
        click1 = NULL,  # Represents the first mouse click, if any
        click2 = NULL
    )
    
    observeEvent(input$update, {
        board.nrow(input$n.row)
        board.ncol(input$n.col)
        
        click.data(rep(0, board.nrow()* board.ncol()) )
        
        answer.board <- reactive(makePicross(row.n = board.nrow(),col.n = board.ncol()))
        answer.plot.board <- reactive(melt(answer.board(),factorsAsStrings=F))
        
        play.plot.board2 <- reactive(cbind(play.plot.board(),click.data()))
    })
    
    observeEvent(input$plot.click,{
        if(is.null(v$click1)){
            v$click1 <- input$plot.click
        }
        
        click.row <- reactive(floor(input$plot.click$y + 0.5))
        click.col <- reactive(floor(input$plot.click$x + 0.5))

        newClick <- click.data()
        
        if(newClick[((click.col()-1)*board.nrow()) + click.row()] == 1){
            newClick[((click.col()-1)*board.nrow()) + click.row()] <- 0
        }
        else if(newClick[((click.col()-1)*board.nrow()) + click.row()] == -1){
            newClick[((click.col()-1)*board.nrow()) + click.row()] <- 0
        }
        else{
            newClick[((click.col()-1)*board.nrow()) + click.row()] <- 1
        }
        
        click.data(newClick)
    })
    
    observeEvent(input$plot.dblclick,{
        dbl.row <- reactive(floor(input$plot.dblclick$y + 0.5))
        dbl.col <- reactive(floor(input$plot.dblclick$x + 0.5))
        
        newClick <- click.data()
        
        newClick[((dbl.col()-1)*board.nrow()) + dbl.row()] <- -1
        
        click.data(newClick)
    })

    play.colors <- c("white","steelblue","red")
    
    output$picross <- renderPlot({
        input$update
        if(r.show.answer() == 1){
            ggplot(data = answer.plot.board(),aes(x=Var2,y=Var1,fill=value)) +
                geom_tile(data=answer.plot.board(),color="black") +
                scale_fill_gradient(low="white",high="steelblue") +
                scale_x_discrete(position = "top") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_blank(),axis.title.y=element_blank(),legend.position = "none", text = element_text(size=18))
            }
            else{
                if(is.null(v$click1)){
                    ggplot(data = play.plot.board(),aes(x=Var2,y=Var1,fill=value)) +
                        geom_tile(data=play.plot.board(),color="black") +
                        scale_fill_gradient2(low="firebrick",high="steelblue",mid="white") +
                        scale_x_discrete(position = "top") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_blank(),axis.title.y=element_blank(),legend.position = "none", text = element_text(size=18))
                }
                else{
                    ggplot(data = play.plot.board2(),aes(x=Var2,y=Var1,fill=click.data())) +
                        geom_tile(data=play.plot.board2(),color="black") +
                        scale_fill_gradient2(low="firebrick",high="steelblue",mid="white") +
                        scale_x_discrete(position = "top") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x = element_blank(),axis.title.y=element_blank(),legend.position = "none", text = element_text(size=18))
                }
            }
        }, height=600,width=600)
}

#Important Picross Functions!
makePicross <- function(row.n=10,col.n=10,prob.filled = 0.6){
    if((row.n > 20) | (col.n > 20)){
        break
    }
    board <- matrix(data=NA,nrow=row.n,ncol=col.n)
    row.clues <- rep(NA,row.n)
    for(i in 1:row.n){
        new.row <- makeRow(col.n,prob.1 = prob.filled)
        board[i,] <- new.row
        row.clue <- makeClue(new.row)
        row.clue.name <- paste0(row.clue,collapse=" ")
        row.clues[i] <- row.clue.name
    }
    
    col.clues <- rep(NA,col.n)
    for(j in 1:col.n){
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

makeClue <- function(pi.vec){
    clue <- c()
    if(sum(pi.vec) == 0){
        clue <- c(0)
    }
    chunk <- 0
    for(i in 1:length(pi.vec)){
        if(pi.vec[i] == 0){
            if(chunk > 0){
                clue <- append(clue,chunk)
            }
            chunk <- 0
        }
        else{
            chunk <- chunk+1
        }
        if((i == length(pi.vec)) & (chunk > 0)){
            clue <- append(clue,chunk)
        }
    }
    return(clue)
}

makeRow <- function(row.length,prob.1 = 0.6){
    a.row <- sample(c(0,1),size=row.length,replace=T,prob=c(1-prob.1,prob.1))
    return(a.row)
}

# Run the application 
shinyApp(ui = ui, server = server)