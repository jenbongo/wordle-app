
# load libraries
#library(rstudioapi)


app_pages <- c("Intro","Play Wordle","Play lots of games!", "Compare strategies", "Review ideas")

# Lists and descriptions
# Strategies
strategy_choices <- c("Random", "Average-minimise", "Information-maximise","Greedy-min-max", "Tree-search", "Guess myself")
strategy_descriptions <- c("is not a good strategy! It's just here for fun. Each guess is sampled at random from a set of 5,757 five-letter words in the Stanford Graph Base.",
                           "looks one move ahead - guessing a word that minimises the average number of remaining possible answers consistent with the returned hint, averaged over all candidate hidden words.",
                           "looks one move ahead - maximising the average information gain or entropy i.e. how many times, on average, does the guessed word halve the set of remaining possible words consistent with the returned hint?",
                           "looks one move ahead - guessing a word the minimises the number of remaining possible answers consistent with the returned hint, targeting the longest list (max) across all candidate hidden words.",
                           "is the best strategy - using a tree search approach, including an exhaustive search for the best starting word.")
strategy_links <- c("https://www-cs-faculty.stanford.edu/~knuth/sgb.html",
                    "https://www.youtube.com/watch?v=B2AVF3_qdHY",
                    "https://www.youtube.com/watch?v=v68zYyaEmEA",
                    "https://towardsdatascience.com/automatic-wordle-solving-a305954b746e",
                    "http://sonorouschocolate.com/notes/index.php?title=The_best_strategies_for_Wordle")

starting_words <- c("crane","tares","tares","aloes","salet")



# Summary statisstics
comparison_statistics <- c("Mean number of guesses", "% in 3 or fewer guesses", "% in 4 or fewer guesses", "% in 5 or fewer guesses")

     
# # Datasets
# app_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(app_wd)

# Wordle test words
test_words <- read.csv("data/sgb-words.csv", header = F)[[1]]

# Match look-up table (otherwise live-playing becomes too slow)
match_table <- readRDS("data/match_table")

# Pre-calculated answers for other strategies
games <- readRDS("data/all_games")

# New Yorker removed six words from original word list - now 2309 words
answers <- read.csv("data/wordlist_nyt20220316_hidden.csv", header = F)[[1]]
answers <- answers[answers %in% test_words]



