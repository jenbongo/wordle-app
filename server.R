
#-----------------------------------------------------
# Server - functions and inputs
shinyServer <- function(input, output) {
  
  # Function for switching pages
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "inTabset", selected = paste0("page_", i))
  }
  
  # Apply function when action button clicked on relevant page
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_34, switch_page(4))
  observeEvent(input$page_45, switch_page(5))
  
  # Where user chooses "my guess" as selectInput (drop-down option) create a text box
  # Where they click another option, remove the text box
  observeEvent(input$strategy, {
    
    if (input$strategy==strategy_choices[6]) {
      insertUI(
        selector = "#description",
        where = "afterEnd",
        ui = textInput(inputId = "guess", label = "", value = "")
      )
    } else {
      removeUI(
        selector = "div:has(> #guess)")      
    }
  })
  
  # Code for first panel "Play Wordle"------------------
  guesses <- reactiveVal(character()) # All guesses so far stored as reactive values - "guesses"
  remaining_words <- reactiveVal(character()) # Also store list of remaining feasible words consistent with hints so far
  hidden_word <- reactiveVal(sample(answers,1)) # Start with a random word from list of allowable answers
  
  # For each drop-down option under "Choose a strategy", different information is displayed
  output$description <- renderUI({
    # There are five strategy choices - descriptions and related links in inputs.R 
    
    strategy = paste(input$strategy)
    
    if (strategy != strategy_choices[6]) {
      
      for (i in 1:5) {
        if (strategy == strategy_choices[i]) {
          description = paste(strategy_descriptions[i])
          strategy_link = paste(strategy_links[i])
        }
      }
      withTags(
        h4(i(strategy),description,a("See here for more.", href = strategy_link))
      ) 
    } else if (strategy == strategy_choices[6]) {
      withTags(h4("Type your guess into the box below."))
    }
  })
  
  # Output depends on selected strategy
  output$result <- renderUI({
    
    if (length(guesses())==6) {
      cancelOutput = TRUE # max of six guesses allowed
    } else if (input$strategy==strategy_choices[6]) {
      #req(input$guess %in% test_words, cancelOutput = TRUE) # at the moment, any typed word accepted - could limit this
      guesses_new <<- c(guesses(), input$guess)
    } else if (input$strategy==strategy_choices[1]) { # random
      guesses_new <<- c(guesses(),sample(test_words,1))
    } else if (input$strategy==strategy_choices[5]) { # tree-search
      optimal_guesses <- games %>% filter(strategy==strategy_choices[5] & answer == hidden_word())
      current_guess <- optimal_guesses$guess[length(guesses())+1]   
      guesses_new <<- c(guesses(),current_guess)
    } else {
      # remaining code for average-minimise, information-maximise and greedy-min-max - see "functions.R" for details
      if (length(guesses())==0) {
        current_guess <- starting_words[match(input$strategy, strategy_choices)] # use published "best" first word for each strategy 
        remaining_words_new <- test_words
      } else if (length(guesses())==1) {
        last_guess <- guesses()
        remaining_words_new <- get_remaining_words(last_guess, return_match_string(last_guess,hidden_word()),test_words)
        current_guess <- suggest_next_guess(remaining_words_new, input$strategy)
      } else {
        last_guess <- tail(guesses(),1)
        remaining_words_new <- get_remaining_words(last_guess, return_match_string(last_guess,hidden_word()),remaining_words())
        current_guess <- suggest_next_guess(remaining_words_new, input$strategy)
      }
      
      guesses_new <<- c(guesses(),current_guess)
      remaining_words(remaining_words_new)
    }
    
    guesses(guesses_new)
    out_str <- lapply(guesses(), function(guess) format_result(guess))
    
    out_str
    
  }) %>% bindEvent(input$enter) 
  
  # Make sure Wordle guesses are shown when enter pressed
  observeEvent(input$enter, {

    show("result")

  } )
  
  # formatting for output$result - using divs that map to css styling file 
  format_result <- function(guess) {
    
    match_data <- return_match_data(guess, hidden_word())
    result_divs <- tagList()
    
    for (i in 1:5) {
      result_divs[[i]] <- div(class = paste("letter",match_data$match[i]), toupper(match_data$letters[i]))
    }
    
    result_divs
  }
  
  # clear guesses and hide inputs and results
  clear_guesses <- function() {
    
    # clear guesses and remaining words
    guesses(character())
    remaining_words(character())
    hide("result")
    
    # either remove or clear guess box
    if (input$strategy != strategy_choices[6]) {
      removeUI(
        selector = "div:has(> #guess)")
    } else {
      reset("guess") # text is cleared from guess box
    }
    
  }
  
  # Call function to clear guesses when action button pressed 
  observeEvent(input$clear_guesses, {
    
   clear_guesses()
    
  })
  
  # Call function to clear guesses and also get new hidden word
  observeEvent(input$new_hidden_word, {
    
    clear_guesses()
    hidden_word(character())
    hidden_word(sample(answers,1))
    
  })
  
  # Code for second panel "Play lots of games!"---------------
  
  # updated when action button "take_sample" pressed (line 173)
  sample_words <- reactiveVal(character()) 
  
  # select games for selected strategy and sample of answers (hidden words) from data set "games" (see inputs.R)
  sample_data <- reactive({ 
    req(sample_words())
    req(input$sample_strategy)
    games %>% filter(answer %in% sample_words() & strategy==input$sample_strategy) 
  })
  
  output$sample <- renderUI({
    
    n = input$sample_size
    req(n < length(answers), cancelOutput = TRUE)
    req(input$sample_strategy)
    
    sample_words_new <- sample(answers,n, replace = F)
    sample_words(sample_words_new) # update reactive value
    
    sample <- games %>% filter(answer %in% sample_words_new & strategy==input$sample_strategy)
    
    game_divs <- tagList()
    
    for (i in 1:n) {
      
      game <- sample %>% filter(answer == sample_words_new[i])
      game_divs[[i]] <- div(class = paste("game"), lapply(1:nrow(game), function(j) format_sample(game[j,])))
    }
    
    game_divs
    
    
  }) %>% bindEvent(input$take_sample) 
  
  # formatting - using divs that map to css styling file - app_style.css
  format_sample <- function(row) {
    
    data <- row
    match <- str_split(data$match,"")[[1]]
    letter <- str_split(data$guess,"")[[1]]
    result_divs <- tagList()
    
    for (i in 1:5) {
      result_divs[[i]] <- div(class = paste("letter",switch(match[i], 
                                                            `B` = "not-in-word",
                                                            `G` = "correct",
                                                            `Y` = "in-word",
                                                            `F` = "null")), toupper(letter[i]))
    }
    
    result_divs
    
  }
  
  observeEvent(input$take_sample, { 
    show("sample")
    show("sample_scores")
    show("guess_distribution")
  } )
  
  # text output of summary stats
  output$sample_scores <- renderUI({
    
    n <- input$sample_size
    sample <- req(sample_data())
    scores <- as.numeric(c(sample %>% filter(turn==1))$score)
    
    mean_score <- round(mean(scores),3)
    max_score <- max(scores)
    in_three <- length(scores[scores < 4])
    win <- length(scores[scores < 7])
    
    percent_in_three <- 100*round(in_three/n,3)
    percent_win <- 100*round(win/n,3)
    
    withTags(
      h5(b(n),"Played,", b(paste0(percent_win,"%")),"Won.", br(),
         b(paste0(percent_in_three,"%")), "in 3 or fewer guesses.", br(),
         b(mean_score), "guesses, on average.")
    )
  }) %>% bindEvent(input$take_sample) 
  
  # histogram  - full guess distribution in D3
  output$guess_distribution <- renderD3({
    
    sample <- req(sample_data())
    
    DF <- sample %>%
      filter(turn == 1) %>%
      mutate(label = score) %>%
      group_by(label) %>%
      tally() %>%
      arrange(label) %>%
      mutate(
        y = n,
        fill = "#E69F00",
        ylabel = prettyNum(n, big.mark = ","),
        mouseover = "#0072B2"
      )
    
    # See barchart.js for javascript
    r2d3(DF, script = "data/barchart.js")
    
  }) %>% bindEvent(input$take_sample) 
  
  
  # Inputs for third panel - compare strategies
  
  output$comparison_plot <- renderD3({
    
    n <- input$comparison_sample_size
    req(n < length(answers), cancelOutput = TRUE)
    req(input$comparison_statistic)
    
    sample_words <- sample(answers,n, replace = F)
    DF <- data.frame(x = seq(1,4,1), 
                     y = rep(0, 4), 
                     ylabel = rep("0", 4), 
                     ll = rep(0, 4), 
                     ul = rep(0,4), 
                     label = c("Ave-min","Info-max","Greedy","Tree-search"))
    
    for (i in 1:4) {
      
      scores <- as.numeric(c(games %>% filter(strategy==strategy_choices[i+1] & answer %in% sample_words & turn==1))$score)
      
      # 500 bootrap replicates to estimate 95% confidence intervals 
      scores_boot <- vapply(1:500, function(j) get_summary(sample(scores,n, replace = T),input$comparison_statistic, n),1)
      
      DF$ll[i] <- rescale(quantile(scores_boot,0.025), input$comparison_statistic) # ;pwer limit of 95% CI (2.5th percentile of bootstrapped distribution)
      DF$ul[i] <- rescale(quantile(scores_boot,0.975), input$comparison_statistic) # upper limit of 95% CI (97.5th percentile)
      DF$y[i] <-  rescale(mean(scores_boot), input$comparison_statistic)
      DF$ylabel[i] <- paste(round(mean(scores_boot),2))
    }
    
    # D3 graph see "error_bars.js" for javascript 
    r2d3(DF, script = "data/error_bars.js")
    
  }) %>% bindEvent(input$compare_strategies)
  
  
  observeEvent(input$compare_strategies, {
    show("comparison_plot")
  })
  
  # Clear guesses and hide output
  observeEvent(input$clear_comparisons, {
    hide("comparison_plot")
  })
}

