#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'`create_game()` generates a new game that consists of two doors 
#'with goats behind them, and one with a car.
#'
#' @details
#'The game setup replicates the game on the TV show "Let's
#'Make a Deal" where there are three doors for a contestant
#'to choose from, one of which has a car behind it and two 
#'have goats. The contestant selects a door, then the host
#'opens a door to reveal a goat, and then the contestant is
#'given an opportunity to stay with their original selection
#'or switch to the other unopened door. There was a famous 
#'debate about whether it was optimal to stay or switch when
#'given the option to switch, so this simulation was created
#'to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#' 
#'create_game()
#'
#' @export
create_game <- function( )
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 

#' @title
#'Monty Python Door Selection
#'
#' @description
#' 
#'`select_door()` randomly generates a door selection.
#'
#' @details
#'This function replicates the participant of the game show making their
#'initial door selection.
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number between 1 and 3 which represents the 
#'    door placement within the game array
#'
#' @examples
#' 
#'select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'Monty Hall Goat Door Reveal
#'
#' @description
#'`open_goat_door()` represents the stage of the game where the game show host 
#'reveals one of the doors the participant did not choose. This door is 
#'guaranteed to not hold the grand prize.
#'
#' @details
#'Randomly opens a door provided that the 
#'door doesn't hold the car or was initially selected in select_door().
#'
#' @param ... Requires the game (array) from create_game() and the initial 
#'    door selection (number, a.pick) from select_door() to be included.
#' 
#' @return The function returns A number between 1 and 3 representing the 
#'    placement in the game array that the opened door object was stored.
#'
#' @examples
#' 
#' game <- create_game()
#' a.pick <- select_door()
#' open_goat_door(game, a.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}

#' @title
#' Monty Hall Door Switch Opportunity
#' @description
#' This function represents the point in the show where the game show host
#' offers the participant an opportunity to change their choice to the one 
#' remaining door.
#'  
#' @details
#' It gives the user a choice to switch doors.  If stay is false, the
#' return is the only remaining door, if true, the return is the initial
#' chosen door.
#'    
#' @param ... The default stay value is true. It also requires the return
#'    from open_goat_door(game, a.pick) and select_door().
#'    
#' @return This function returns a value between 1 and 3 representing the 
#'    position of the participant's final choice.
#'    
#' @examples
#' 
#' game <- create_game()
#' a.pick <- select_door()
#' opened.door <- open_goat_door(game, a.pick)
#' change_door( TRUE, opened.door, a.pick ) 
#' change_door( FALSE, opened.door, a.pick )
#' change_door( opened.door = opened.door, a.pick = a.pick )
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}

#' @title
#' Monty Hall Prize Reveal
#' @description
#' This function represents the point in the game where the host reveals
#' what the participant has won.
#' 
#' @details
#' This function reveals what is stored in the final.pick position of the
#' game array.
#' 
#' @param ... It requires the game array from create_game() and the final.pick
#'    from change_door(stay = T, opened.door, a.pick). 
#'    
#' @return The return is the text stored in game[final.pick].
#'    
#' @examples
#' 
#' game <- create_game()
#' a.pick <- select_door()
#' opened.door <- open_goat_door(game, a.pick)
#' final.pick <- change_door( TRUE, opened.door, a.pick ) 
#' determine_winner( final.pick, game )
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}

#' @title
#'Monty Hall Master Function
#' @description
#'This function plays the entire game from start to finish.
#' 
#' @details
#'This function runs all functions needed to play the Monty Hall game.
#' 
#' @param ... This function does not require any inputs.
#' 
#' @return This function returns a 2 object array containing the strategy 
#'    (whether the participant switched during that run or stayed) and the outcome
#'    (what prize the participant won).
#'    
#' @examples
#' 
#'play_game()
#'     
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

#' @title
#' Monty Hall Game Loop
#' @description
#' This function loops through the game n amount of times in order to
#' determine the probability of winning or loosing based on whether or not
#' the participant stays with their original choice.
#' 
#' @details
#' For every iteration of n, the function calls play_game() and stores the
#' results in an array using rbind. The function then displays the results
#' in a 2x2 table.
#' 
#' @param ... The default value for n is 100, however, you can call the function with 
#'    your own value.
#'    
#' @return It returns the results data frame.
#'    
#' @examples
#' 
#'play_n_games()
#'play_n_games(200)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()# collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
