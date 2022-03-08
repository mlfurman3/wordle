#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)

source('wordlist.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(
    tags$style(
      HTML("
      
        .container-fluid {
          max-width: 500px;
        }
        
        #keyboard {
          display: grid;
          grid-template-columns: repeat(19, 20px);
          gap: 1px;
          padding: 1px;
          justify-content: center;
          background-color: #eeeeee;
        }
        
        .unused {
          width: 18px;
          height: 20px;
          font-size: 14px;
          background-color: lightgray;
           display: grid;
          place-content: center
        }
        
        .blank-key {
          border-radius: 5px;
          width: 18px;
          height: 20px;
          background-color: white;
        }
        
        #result {
          display: grid;
          grid-template-columns: repeat(5, 50px);
          gap: 5px;
          padding: 15px;
          justify-content: center;
        }
        
        #result > .guess-letter {
          width: 50px;
          height: 50px;
          font-size: 20px;
          display: grid;
          place-content: center;
        }
        
        .guess-letter {
          padding: 4px;
          border-radius: 5px;
          color: white;
          font-weight: bold;
        }
        
        .in-word {
          background-color: #c8b458;
        }
        
        .not-in-word {
          background-color: #787c7e;
        }
        
        .correct {
          background-color: #6aaa64;
        }
        
        .input-wrapper {
          display: flex;
          align-items: flex-end;
          justify-content: center;
          padding-bottom: 10px;
        }
        
        .input-wrapper > div {
          margin-bottom: 0;
        }
        
        .input-wrapper > button {
          margin-left: 10px;
        }
      ")
    )
   ),
   uiOutput('result'),
  div(
    textInput('guess', '', placeholder = 'Enter guess here'),
    actionButton('go', 'Go'),
    class = 'input-wrapper'
  ),
  #verbatimTextOutput('keyboard', placeholder = TRUE),
  uiOutput('keyboard'),
  
  p(span('X' ,class = 'guess-letter correct'), ' means that the letter is correct.'),
  p(span('X', class='guess-letter in-word'), ' means that the letter is in the word but in the wrong place.'),
  p(span('X', class='guess-letter unused'), ' test letter')
  
)

set.seed(as.integer(Sys.Date()))
target <- sample(words_common, 1)

# Define server logic required to draw a histogram
server <- function(input, output) {

  all_guesses <- reactiveVal(character())
  
  
  output$result <- renderUI({

    if(!(input$guess %in% words_all)){ 
      req(FALSE, cancelOutput = TRUE)
    }

      ## would need isolate() without the bindEvent to prevent infinite reactiveVal loop
      all_guesses_new <- c(all_guesses(), input$guess)
      all_guesses(all_guesses_new)
    
      # out_str <- vapply(all_guesses(), function(guess){
      #   result <- check_words(target, guess)
      #   format_result(result)
      # }, character(1))
      
      out_str <- lapply(all_guesses(), function(guess){
        result <- check_words(target, guess)
        #div(format_result(result), style = 'outline: 1px solid salmon;')
        format_result(result)
      })
      
      out_str
      #paste(out_str, collapse='\n')
      
      #out_str
      # result <- check_words(target, input$guess)
      # format_result(result)
      
  })  |> 
    bindEvent(input$go)
  
  #output$keyboard <- renderText({
  output$keyboard <- renderUI({
    # keys <- paste(
    #   ' q w e r t y u i o p ',
    #   '  a s d f g h j k l ',
    #   '   z x c v b n m ',
    #   sep = '\n'
    # )
    
    keys <- str_split(paste(
      'q w e r t y u i o p',
      'a s d f g h j k l ',
      ' z x c v b n m    '), '', simplify = T)
    
    used_letters <- paste(all_guesses(), collapse='')
    used_letters <- str_split(used_letters, '')[[1]]
    used_letters <- unique(used_letters)
    
    for(l in used_letters){
      keys <- sub(l, ' ', keys)#str_replace_all()
    }
    
    #keys
    format_keys(keys)
  })
}

format_result <- function(r){
  
  #out_str <- ''
  out_divs <- tagList()
  
  for(i in seq_along(r$letters)){
    # letterText <- if(r$result[i] == 'correct'){
    #   #out_divs[[i]] <- div('[',r$letters[i],']')
    #   #out_str <- paste0(out_str, '[',r$letters[i],']')
    #   paste0('[',r$letters[i],']')
    # } else if (r$result[i] == 'in-word'){
    #   #out_divs[[i]] <- div('(',r$letters[i],')')
    #   #out_str <- paste0(out_str, '(',r$letters[i], ')')
    #   paste0('(',r$letters[i],')')
    # } else {
    #   #out_divs[[i]] <- div( ' ', r$letters[i], ' ')
    #   #out_str <- paste0(out_str, ' ', r$letters[i], ' ')
    #   paste0(' ',r$letters[i],' ')
    # }
    
    # out_divs[[i]] <- div(letterText, 
    #                      class=paste('guess-letter',r$result[i])
    #                      )
    out_divs[[i]] <- div(r$letters[i], 
                         class=paste('guess-letter',r$result[i])
    )
  }
  
  return(out_divs)
  #return(out_str)
}

format_keys <- function(k){
  
  out_divs <- tagList()
  
  for(i in seq_along(k)){
    if(k[i] %in% letters) {
      out_divs[[i]] <- div(k[i], class = 'guess-letter unused')
      
    } else {
      out_divs[[i]] <- div(k[i], class = 'blank-letter')
      
    }
    
  }
  
  return(out_divs)
}

compare_words <- function(target_str, guess_str){
 
  if(nchar(target_str) != nchar(guess_str)){
    stop('Target and guess string must be the same length!')
  }
  
  target <- str_split(target_str, '', simplify = T) |> as.vector()
  guess <- str_split(guess_str, '', simplify = T) |> as.vector()
  
  remaining <- character(0)
  
  #result <- character(length = nchar(guess_str))
  result <- rep('not-in-word', nchar(guess_str))
  
  for(i in seq_along(guess)){
    if(guess[i] == target[i]){
      result[i] <- 'correct'
    } else {
      remaining <- c(remaining, target[i])
    }
    
  for(i in seq_along(guess)){
    
    if(guess[i] != target[i] && guess[i] %in% remaining){
      result[i] <- 'in-word'
      
      remaining <- remaining[-match(guess[i], remaining)]
    }
    # } else {
    #   result[i] <- 'not-in-word'
    # }
  }
  
  }
  return(result)
}

check_words <- function(target_str, guess_str){
  
  compare_result <- compare_words(target_str, guess_str)
  
  correct <- FALSE
  if(all(compare_result == 'correct')){
    correct <- TRUE
  }
  
  list(
    word = guess_str,
    letters = str_split(guess_str, '', simplify=T) |> as.vector(),
    result = compare_result,
    correct = correct
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
