#' Created on 09 February, 2019
#' auuthor: Harjyot Kaur
#' Implementation of text_grams function in the RySyntext package.


#' This function returns a DataFrame with k top ngrams.
#' ngrams is a combination of n words occuring together with
#' highest frequency in the text. The function will return multiple
#' values in cases of frequency conflict.


#' Takes in a string and returns a data.frame
#' Number of rows are dependent on the input n of the user
#' Size of the list is dependent on the input k of the user

#'

#' @param text string
#' @param k top ngrams
#' @param n n combination of words
#' @param stopwords_remove Boolean
#' @param remove_punctuation Boolean
#' @param remove_numbers Boolean
#' @param case_sensitive Boolean

#'

#' @return data.frame

#' @export

#'

#' @examples

#' text <-  "Today is a sunny day. We should go to a beach on this sunny day"

#'

#' grams<- text_grams(text)

text_grams <- function(text, k = 5, n = c(2,3), stop_remove = TRUE, remove_punctuation = TRUE,
                       remove_number = TRUE, case_sensitive = FALSE) {

  ngrams_list <- c()
  ngrams_dfs <- c()
  lbls <- c()

  #clean_text <- clean(text, remove_punctuation, remove_number)
  #clean_text <- pre_processing(clean_text, case_sensitive, stop_remove)

  clean_text <- text

  split_sentences <- unlist(strsplit(clean_text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))

  for (i in 1:length(n)){

    for (sentence in split_sentences){
      ngrams_list <- c(ngrams_list, create_grams(sentence, n[i])[1:k])
    }
  }

  return (ngrams_list)

}

# THIS SHIT DOESN'T WORK WHEN n is greater than the number of words in a sentence!!!!
create_grams <- function(sentence, n){
  list_of_words <- c()
  split_words <- unlist(strsplit(sentence, " "))

  for (i in 1:(length(split_words) - (n-1))){
    #print(split_words[i:(i+n-1)])
    list_of_words <- c(list_of_words, paste(split_words[i:(i+n-1)], collapse = " "))
  }
  sort(decreasing = T, table(list_of_words))
}

ex <- "This is an example sentence blah blah blah blah. I wonder will this work? Please work oh god."
text_grams(ex, k = 2, n = c(2,3,4))
