#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select a door.
#'
#' @description
#'   `select_door()` generates a numeric scalar that consists of an integer between 1 and 3.
#'
#' @details
#'   The function creates an object that serves as the contestants selection.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 1 numeric vector
#'   indicating the contestants selection.
#' @examples
#'   select_door( )
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a door.
#'   
#' @description
#'    `open_goat_door` opens the door that is a goat and was not selected by the contestant.
#'    
#' @details
#'   The function returns an object that is a number between 1 and 3 that represents the door that was opened. 
#'   
#' @param 
#'   The function requires objects `game` and `a.pick`.
#'   
#' @return 
#'   The function returns an object that is the door which is opened.
#'   
#' @examples
#'   game <- create_game( )
#'   a.pick <- select_door( )
#'   open_goat_door( game, a.pick )
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
#' Change doors.
#' 
#' @description
#' Function to determine whether the contestant changes doors.
#' 
#' @details
#' The function returns a numeric scalar that represents the door that is the contestants final pick.
#' 
#' @param 
#' The function requires three arguments. First `stay=` is `TRUE` by default and keeps the value of `a.pick`, which is the contestants initial selection. Second,
#' the function requires `opened.door` which is the door that is opened. Finally, the function requires `a.pick` which is the contestants pick. If `stay=` 
#' is set to `FALSE`, then the contestant switches doors and the value of `a.pick` changes.
#' 
#' @return
#'   The function returns an object that is the contestants final pick.
#'   
#' @examples
#' game <- create_game( )
#' a.pick <- select_door( )
#' opened.door <- open_goat_door( game, a.pick )
#' change_door( stay=T, opened.door, a.pick ) # if the contestant stays.
#' change_door( stay=F, opened.door, a.pick ) # if the contestant switches.
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
#' Determine whether the contestant wins.
#' 
#' @description
#' If the contestant has selected the door with a "car", it is a "WIN". Otherwise a "LOSE".
#' 
#' @details
#' The function returns the outcome of the game.
#' 
#' @param 
#' The function requires the object `final.pick` which is the contestants pick and `game` which is the arrangement of the doors.
#' 
#' @return
#' The function prints the outcome of the game.
#' 
#' @examples
#' game <- create_game( )
#' a.pick <- select_door( )
#' opened.door <- open_goat_door( game, a.pick )
#' final.pick.stay <- change_door( stay=T, opened.door, a.pick ) # if the contestant stays.
#' final.pick.switch <- change_door( stay=F, opened.door, a.pick ) # if the contestant switches.
#' determine_winner ( final.pick.stay, game )
#' determine_winner ( final.pick.switch, game )
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
#' Play the game.
#' 
#' @description
#' Wrapper function that executes all of the functions used to play the game.
#' 
#' @details
#' Executes each of the functions to play the game.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return
#' The function prints a table with the outcome under each strategy (i.e. "stay" or "switch").
#' 
#' @examples
#' play_game( )
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
#' Play the game n times.
#' 
#' @description
#' Wrapper function that executes all of the functions used to play the game. `play_n_games` differs from `play_game` in that 
#' the user specifies how many iterations of the game to play.
#' 
#' @details
#' Plays the game n times.
#' 
#' @param 
#' Requires the user to specify how many games are played with `n=`.
#' 
#' @return
#' Returns a table of the proportion of games won and lost for each strategy.
#' 
#' @examples
#' play_n_games( ) # the default is n = 100.
#' play_n_games( n=1000 ) # play the game 1,000 times.
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
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
