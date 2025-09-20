#' @title Create a new Monty Hall Problem game
#' @description `create_game()` generates a new game with two goats and one car.
#' @details The game mirrors "Let's Make a Deal": three doors, one with a car and two with goats.
#' @return A character vector of length 3 with entries `"goat"` or `"car"`.
#' @examples
#' create_game()
#' @export
create_game <- function()
{
  a.game <- sample(x = c("goat","goat","car"), size = 3, replace = FALSE)
  return(a.game)
}

#' @title Randomly select a door
#' @description Chooses one of the three doors at random.
#' @details Returns an integer 1, 2, or 3 representing the selected door.
#' @return An integer (1–3).
#' @examples
#' select_door()
#' @export
select_door <- function()
{
  doors <- c(1,2,3)
  a.pick <- sample(doors, size = 1)
  return(a.pick)
}

#' @title Open a goat door
#' @description Given a game layout and the contestant's first pick, open a door with a goat.
#' @details If the first pick is the car, randomly open one of the two goat doors;
#' if the first pick is a goat, open the *other* goat door.
#' @param game Character vector of length 3 with entries `"goat"` or `"car"`.
#' @param a.pick Integer (1–3) for the contestant's initial door pick.
#' @return An integer (1–3) giving the opened goat door (not equal to `a.pick`).
#' @examples
#' g <- create_game()
#' p <- select_door()
#' open_goat_door(g, p)
#' @export
open_goat_door <- function(game, a.pick)
{
  doors <- c(1,2,3)
  if (game[a.pick] == "car") {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(goat.doors, size = 1)
  }
  if (game[a.pick] == "goat") {
    opened.door <- doors[game != "car" & doors != a.pick]
  }
  return(opened.door)
}

#' @title Stay or switch to choose a final door
#' @description Apply the stay/switch strategy to select the final door.
#' @details If `stay = TRUE`, return `a.pick`; otherwise return the unopened door.
#' @param stay Logical; `TRUE` = keep original pick, `FALSE` = switch.
#' @param opened.door Integer (1–3) that the host opened (must be a goat).
#' @param a.pick Integer (1–3) original pick.
#' @return An integer (1–3) for the final chosen door.
#' @examples
#' change_door(stay = TRUE, opened.door = 3, a.pick = 1)
#' change_door(stay = FALSE, opened.door = 3, a.pick = 1)
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick)
{
  doors <- c(1,2,3)
  if (stay) {
    final.pick <- a.pick
  } else {
    final.pick <- doors[doors != opened.door & doors != a.pick]
  }
  return(final.pick)
}

#' @title Determine whether the player won
#' @description Given the final pick and the game layout, return `"WIN"` or `"LOSE"`.
#' @param final.pick Integer (1–3) final chosen door.
#' @param game Character vector of length 3 with entries `"goat"` or `"car"`.
#' @return A character scalar: `"WIN"` if the final door has the car, otherwise `"LOSE"`.
#' @examples
#' determine_winner(final.pick = 1, game = c("car","goat","goat"))
#' @export
determine_winner <- function(final.pick, game)
{
  if (game[final.pick] == "car") {
    return("WIN")
  }
  if (game[final.pick] == "goat") {
    return("LOSE")
  }
}

#' @title Simulate one Monty Hall game under both strategies
#' @description Plays one game and reports outcomes for `stay` and `switch`.
#' @details Internally calls the helper functions and returns a 2-row data frame.
#' @return A data frame with columns `strategy` and `outcome` (rows: stay, switch).
#' @examples
#' play_game()
#' @export
play_game <- function()
{
  new.game    <- create_game()
  first.pick  <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)

  final.pick.stay    <- change_door(stay = TRUE,  opened.door, first.pick)
  final.pick.switch  <- change_door(stay = FALSE, opened.door, first.pick)

  outcome.stay   <- determine_winner(final.pick.stay,   new.game)
  outcome.switch <- determine_winner(final.pick.switch, new.game)

  strategy <- c("stay","switch")
  outcome  <- c(outcome.stay, outcome.switch)
  game.results <- data.frame(strategy, outcome, stringsAsFactors = FALSE)
  return(game.results)
}

#' @title Simulate many Monty Hall games
#' @description Run `n` independent games and summarize win rates by strategy.
#' @param n Integer number of games to simulate (default 100).
#' @return A data frame of stacked game outcomes (2 rows per game).
#' @examples
#' # run a small simulation for speed in examples:
#' res <- play_n_games(10)
#' head(res)
#' @export
play_n_games <- function(n = 100)
{
  # NOTE: The package uses dplyr; the class handout has you place dplyr in
  # the DESCRIPTION under Depends so %>% and friends are available on load.
  # (We still qualify bind_rows explicitly for clarity.)
  results.list <- vector("list", n)
  for (i in seq_len(n)) {
    results.list[[i]] <- play_game()
  }

  results.df <- dplyr::bind_rows(results.list)

  # Print a quick 2x2 proportion table by strategy (rows)
  table(results.df) %>%
    prop.table(margin = 1) %>%
    round(2) %>%
    print()

  return(results.df)
}

