

# 1. Basic function - Start by getting the specific match for a word - for a given hidden words
return_match_data <- function(guess, hidden_word) {
  
  # Return the match data for a selected five-letter word, given the hidden five_letter word
  # For each letter in the correct position - this is correct and returns a value of 2 (green)
  # For remaining letters that are contained in the hidden word, but not in the right position, returns value of 1 (yellow in the game)
  # letters in guess word but not hidden word returns a value of zero (grey in the game)
  # The function takes two strings as arguments (guess and hidden word) and list with elements:
  # $guess - string length five (guess) 
  # $letters  vector with five elements - each containing letter of guessed word
  # $match - vector with five elements - each containing match status of letter e.g. "correct", "in-word", or "not-in-word"
  # $match_str - string length five with numeric match string (as above)
  
  guess <- str_to_lower(guess)
  guess_letters <- str_split(guess,"")[[1]]
  hidden_letters <- str_split(hidden_word,"")[[1]]
  match <- rep("not-in-word",5)
  match_val <- rep("B",5)
  
  remaining_letters <- hidden_letters
  
  
  for (i in 1:5) {
    if (guess_letters[i] == hidden_letters[i]) {
      match_val[i] <- "G"
      match[i] <- "correct"
      remaining_letters <- remaining_letters[-(match(guess_letters[i],remaining_letters))]
    } 
  }
  

  for (i in 1:5) {
    if (match_val[i] == "B" & str_detect(hidden_word, guess_letters[i])== TRUE & (guess_letters[i] %in% remaining_letters)) {
      match_val[i] <- "Y"
      match[i] <- "in-word"
      remaining_letters <- remaining_letters[-(match(guess_letters[i],remaining_letters))]
    }
  }
  
  match_str <- paste(match_val, collapse="")
  
  return(list(guess = guess, letters = guess_letters, match = match, match_str = match_str))
}

# Start by getting the specific match for a word - for a given hidden words
return_match_string <- function(guess, hidden_word) {
  
  # Return the match pattern for a selected five-letter word, given the hidden five_letter word
  # For each letter in the correct position - this is correct and returns a value of 2 (green)
  # For remaining letters that are contained in the hidden word, but not in the right position, returns value of 1 (yellow in the game)
  # letters in guess word but not hidden word returns a value of zero (grey in the game)
  # The function takes two strings as arguments (guess and hidden word) and returns a string length 5
  
  guess <- str_to_lower(guess)
  guess_letters <- str_split(guess,"")[[1]]
  hidden_letters <- str_split(hidden_word,"")[[1]]
  match_val <- rep("B",5)
  remaining_letters <- hidden_letters

  for (i in 1:5) {
    if (guess_letters[i] == hidden_letters[i]) {
      match_val[i] <- "G"
      remaining_letters <- remaining_letters[-(match(guess_letters[i],remaining_letters))]

    } 
  }
  
  for (i in 1:5) {
    if (match_val[i] == "B" & str_detect(hidden_word, guess_letters[i])== TRUE & (guess_letters[i] %in% remaining_letters)) {
      match_val[i] <- "Y"
      remaining_letters <- remaining_letters[-(match(guess_letters[i],remaining_letters))]

    }
  }

  match_str <- paste(match_val, collapse="")
  
  return(match_str)
}

# 2. Return matches 
# uses table of precalculated matches - used to generate dataset with played games
return_matches <- function(guess, word_list) {

  # Return all match patterns for a given guess across a given list of words
  # inputs: guess is string length five, word list is a character vector containing list of five-letter words
  # output: character vector of strings containing match patterns
  # For example, for input "crate" and word list c("crane", "pears", "cynic")
  # returns list of strings "22202", "01201", "20000"

  index_guess <- match(guess, test_words)
  index_word_list <- match(word_list, test_words)
  list_matches <- match_table[index_guess,index_word_list]

  return(list_matches)
}

# Original function used to populate matches look-up table
# return_matches <- function(guess, word_list) {
# 
#   return(
#     sapply(1:length(word_list), function(i) return_match_string(guess,word_list[i]))
#     )
# 
# }

# Remaining
get_remaining_words <- function(guess, return_match, word_list) {
  
  list_matches <- return_matches(guess, word_list)
  index_matches <- which(list_matches %in% return_match)
  remaining_words <- word_list[index_matches]
  
  return(remaining_words)
}

# Strategy 1: Minimize the expected number of remaining possible answers for next step, averaged over all possible answers

calculate_stats <- function(guess, word_list,strategy) {
  
  # function to calculate the expected number of words in the remaining set, for a given guess
  # inputs: guess is a single word as a string length 5; remaining_words is a character vector listing
  # all the words left in the set that are consistent with any previous guesses
  # For the first guess - the remaining set includes all the words selected to be in the set (sgb words)
  # Returns the expected number of remaining words after making guess, as a numeric value 
  
  n <- length(word_list)
  list_matches <- return_matches(guess, word_list)
  counts_per_match <- table(list_matches)
  p <- counts_per_match/n
  
  # Average-minimise
  if (strategy==strategy_choices[2]) {
    return(sum(p*counts_per_match)) # average number of words per match group
    
  # Information-maximise
  } else if (strategy==strategy_choices[3]) {
    return(sum(p*log2(1/p))) # Information
  
  # Greedy-min-max
  } else if (strategy==strategy_choices[4]) {
    return(max(counts_per_match)) # max number of words per match group
  }

}

suggest_next_guess <- function(remaining_words, strategy) {
  
  # function to suggest a next guess word from within the set of 1,488 common words
  # inputs are the set of remaining words (character vector) consistent with previous guesses and returned matches
  # and the chosen strategy: either "Average-minimise" or "Information-maximise"
  # in the "end game" when number of remaining words is less than or equal to 3, then restrict pick to these
  
  if (length(remaining_words) <= 2) {
    
    next_guess <- sample(remaining_words,1)
    
  } else {
    
    stats <- t(sapply(test_words, function(guess) calculate_stats(guess, remaining_words,strategy)))
    
    # Ave minimise
    if(strategy == strategy_choices[2]) {
      index_guesses <- which(stats %in% min(stats))
    # Info maximise
    } else if (strategy == strategy_choices[3]) {
      index_guesses <- which(stats %in% max(stats))
    # Greedy min max
    } else if (strategy == strategy_choices[4]) {
      index_guesses <- which(stats %in% min(stats))
    }
    
    if (length(index_guesses) > 1) {
      index_next_guess <- sample(index_guesses,1)
    } else {
      index_next_guess <- index_guesses
    }
    
    next_guess <- test_words[index_next_guess]
  }
  
  return(next_guess)
}


play_game <- function(game_number, first_guess, answer, strategy) {
  
  first_match <- return_match_string(first_guess,answer)
  remaining_words <- get_remaining_words(first_guess, first_match,test_words)
  
  game <- data.frame(matrix(NA, nrow = 6, ncol = 7))
  names(game) <- c("game", "strategy", "answer","score","turn","guess","match")
  game$strategy <- rep(strategy,6)
  game$game <- rep(game_number,6)
  game$answer <- rep(answer,6)
  
  game$guess <- c(first_guess,rep(NA,5))
  game$match <- c(first_match,rep(NA,5))
  game$turn <- c(1:6)
  
  if (first_guess == answer) { 
    game$score <- rep(1,6) 
    return(game)
  }
  
  
  for (i in 2:6) {
    
    game$guess[i] <- suggest_next_guess(remaining_words, strategy)
    game$match[i] <- return_match_string(game$guess[i],answer)
    
    if (game$guess[i]==answer) {
      game$score <- rep(i,6)
      break
    } else {
      remaining_words <- get_remaining_words(game$guess[i], game$match[i],remaining_words)
    }
  }
  
  game <- game %>% 
          mutate(guess = case_when(turn > score ~ answer, TRUE ~ guess), 
                 match = case_when(turn > score ~ "FFFFF", TRUE ~ match))
  return(game)
  
}

# For final panel of app - can choose different stat to view to compare strategies
get_summary <- function(x, stat, n) {
  
  if (stat == comparison_statistics[1]) {
    return(mean(x))
  } else if (stat == comparison_statistics[2]) {
    return(100*length(x[x<4])/n)
  } else if (stat == comparison_statistics[3]) {
    return(100*length(x[x<5])/n)
  } else if (stat == comparison_statistics[4]) {
    return(100*length(x[x<6])/n)
  }

    
}

rescale <- function(x, stat) {
  
  if (stat==comparison_statistics[1]) {
    return(100*x/6)
  } else {
    return(x)
  }
  
}

