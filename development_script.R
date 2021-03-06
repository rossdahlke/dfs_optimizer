library(lpSolveAPI)
library(dplyr)
library(stringr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

position <- readr::read_csv("/Users/rossdahlke/Downloads/DKSalaries (2).csv") %>% 
  janitor::clean_names() %>% 
  rename(dk_positions = position) %>% 
  select(-id)

train <- dplyr::filter(readr::read_csv("/Users/rossdahlke/Downloads/DFF_NBA_cheatsheet_2020-08-04.csv"), is.na(injury_status)) %>%
  mutate(id = row_number(),
         name = paste0(first_name," ",last_name)) %>% 
  left_join(position) %>% 
  mutate(position = dk_positions)

high_scoring <- T
close_game <- T
blow_out <- F

optimize_nba <- function(train,
                         cap,
                         league, 
                         setplayers = NULL, 
                         removeteams = NULL){
  
  ## set constraints to use
  pg <- ifelse(str_detect(train$position, "PG"), 1, 0)
  sg <- ifelse(str_detect(train$position, "SG"), 1, 0)
  sf <- ifelse(str_detect(train$position, "SF"), 1, 0)
  pf <- ifelse(str_detect(train$position, "PF"), 1, 0)
  c <- ifelse(str_detect(train$position, "C"), 1, 0)
  g <- ifelse(str_detect(train$position, "PG") | str_detect(train$position, "SG"), 1, 0)
  f <- ifelse(str_detect(train$position, "SF") | str_detect(train$position, "PF"), 1, 0)
  util <- ifelse(!is.na(train$position), 1, 0)
  
  # also going to add on position flags as columns to make outputting easier
  train <- train %>% 
    cbind(pg = pg,
          sg = sg,
          sf = sf,
          pf = pf,
          c = c,
          g = g,
          f = f,
          util = util)
  
  ## number of decision variables is equal to the number of fantasy players/teams
  dfs_fantasy <- make.lp(0, nrow(train))
  
  ## Set objective function with the expected number of points
  set.objfn(dfs_fantasy, train$ppg_projection)
  
  ## Make sure the decision variables are binary
  set.type(dfs_fantasy, seq(1, nrow(train), by=1), type = c("binary"))
  
  ## Add some contraints
  ## Depends on what fantasy league you are playing in, currently for FanDuel and DraftKings
  if(league == "fanduel") {
    add.constraint(dfs_fantasy, pg, "=", 2)
    add.constraint(dfs_fantasy, sg, "=", 2)
    add.constraint(dfs_fantasy, sf, "=", 2)
    add.constraint(dfs_fantasy, pf, "=", 2)
    add.constraint(dfs_fantasy, c, "=", 1)
  }
  
  if(league == "draft_kings") {
    add.constraint(dfs_fantasy, pg, ">=", 1)
    add.constraint(dfs_fantasy, sg, ">=", 1)
    add.constraint(dfs_fantasy, sf, ">=", 1)
    add.constraint(dfs_fantasy, pf, ">=", 1)
    add.constraint(dfs_fantasy, c, ">=", 1)
    add.constraint(dfs_fantasy, c, "<=", 2)
    add.constraint(dfs_fantasy, g, ">=", 3)
    add.constraint(dfs_fantasy, f, ">=", 3)
    add.constraint(dfs_fantasy, util, "=", 8)
  }
  
  if(!is.null(setplayers)) {
    if(league == "FanDuel") {
      if((sum(setplayers$Position == "WR") > 3) || (sum(setplayers$Position == "RB") > 2) || (sum(setplayers$Position == "QB") > 1) ||
         (sum(setplayers$Position == "TE") > 1) || (sum(setplayers$Position == "K") > 1) || (sum(setplayers$Position == "D") > 1))
        stop("One of your positions has too many players")
    }
    if(league == "draft_kings") {
      if((sum(setplayers$Position == "PG") > 3) || (sum(setplayers$Position == "SG") > 3) || (sum(setplayers$Position == "SF") > 3) ||
         (sum(setplayers$Position == "PF") > 3) || (sum(setplayers$Position == "C") > 2))
        stop("One of your positions has too many players")
    }
    ## Set constraints that each player here must be in lineup
    for(k in 1:nrow(setplayers)) {
      add.constraint(lpfantasy, ifelse(setplayers$id[k] == train$id, 1, 0), "=", 1)
    }
  }
  
  if(!is.null(removeteams)) {
    if(nrow(removeteams) != nrow(train))
      stop("Your team restrictions do not match the number of players included in the 'train' file")
    for(m in 1:ncol(removeteams)) {
      add.constraint(dfs_fantasy, removeteams[, m], "<=", 7)
    }
  }
  
  ## Add monetary constraint, max salary for the team
  add.constraint(dfs_fantasy, train$salary, "<=", cap)
  
  ## Set objective direction
  lp.control(dfs_fantasy, sense='max')
  
  team <- data.frame(matrix(0, 1, ncol(train) + 2))
  colnames(team) <- c(colnames(train), "team_salary", "projected_team_points")
  
  ## Solve the model, if this returns 0 an optimal solution is found
  solve(dfs_fantasy)
  if(solve(dfs_fantasy) != 0){
    stop("Optimization failed at some step")
  }
  
  ## Get the players on the team
  team <- train %>% 
    cbind(tibble(selected_player = get.variables(dfs_fantasy))) %>% 
    filter(selected_player == 1) %>% 
    select(-selected_player) %>% 
    mutate(team_salary = sum(salary),
           team_points = sum(ppg_projection)) %>% 
    {if ("over_under" %in% colnames(.)) mutate(., avg_over_under = mean(over_under)) else .} %>% 
    {if ("spread" %in% colnames(.)) mutate(., avg_abs_spread = mean(abs(spread))) else .} %>% 
    get_positions()
  
  
  return(team)
}


top_nba_teams <- function(train, cap, n_top, league, setplayers = NULL) {
  result <- optimize_nba(train, cap, league = league, setplayers = setplayers)
  restrict <- as.matrix(rep(0, nrow(train)))
  restrict[match(result$id, train$id), 1] <- 1
  j <- 1
  
  while(j < n_top) {
    resultnew <- optimize_nba(train, cap, league = league, setplayers = setplayers, removeteams = restrict)
    restrict <- cbind(restrict, rep(0, nrow(restrict)))
    restrict[match(resultnew$id, train$id), j] <- 1
    result <- rbind(result, resultnew)
    j <- j + 1
  }
  
  TeamNumber <- rep(1:n_top, each = 8)
  result <- cbind.data.frame(result, TeamNumber)
  result
}


team <- optimize_nba(train, cap = 50000, league = "draft_kings")

## Generate the top 10 teams with no constraints (this may be a bit slow with other constraints)
top_100 <- top_nba_teams(train, cap = 50000, n_top = 100, league = "draft_kings")

ggplot(top_100, aes(avg_over_under, team_points)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(top_100, aes(avg_abs_spread, team_points)) +
  geom_point() +
  geom_smooth(method = "lm")

minimize_spread <- function(top_teams){
  min_spread <- top_teams %>% 
    filter(avg_abs_spread == min(avg_abs_spread)) %>% 
    filter(team_points == max(team_points))
  
  return(min_spread)
}

min_spread <- top_100 %>% 
  minimize_spread()

maximize_over_under <- function(top_teams){
  min_spread <- top_teams %>% 
    filter(avg_over_under == max(avg_over_under)) %>% 
    filter(team_points == max(team_points))
  
  return(min_spread)
}

max_over_under <- top_100 %>% 
  maximize_over_under()

top_players <- function(top_teams){
  top_players <- top_teams %>% 
    count(position, first_name, last_name) %>% 
    arrange(desc(n))
  
  return(top_players)
}

top_100 %>% 
  top_players()
