library(lpSolveAPI)
library(dplyr)

train <- dplyr::filter(readr::read_csv("test_data/20200731_dfs_300_3.csv"), is.na(injury_status))
train$id <- seq(1, nrow(train),1)

high_scoring <- T
close_game <- T
blow_out <- F

optimize_nba <- function(train,
                         cap,
                         league, 
                         setplayers = NULL, 
                         removeteams = NULL){
  ## set constraints to use
  pg <- ifelse(train$position == "PG", 1, 0)
  sg <- ifelse(train$position == "SG", 1, 0)
  sf <- ifelse(train$position == "SF", 1, 0)
  pf <- ifelse(train$position == "PF", 1, 0)
  c <- ifelse(train$position == "C", 1, 0)
  g <- ifelse(train$position == "PG" | train$position == "SG", 1, 0)
  f <- ifelse(train$position == "SF" | train$position == "PF", 1, 0)
  util <- ifelse(!is.na(train$position), 1, 0)
  
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
  team <- tibble(train, get.variables(dfs_fantasy)) %>% 
    filter(`get.variables(dfs_fantasy)` == 1) %>% 
    select(-`get.variables(dfs_fantasy)`) %>% 
    mutate(team_salary = sum(salary),
           team_points = sum(ppg_projection)) %>% 
    {if ("over_under" %in% colnames(.)) mutate(., avg_over_under = mean(over_under)) else .} %>% 
    {if ("spread" %in% colnames(.)) mutate(., avg_abs_spread = mean(abs(spread))) else .} %>% 
    group_by(position) %>% 
    mutate(position = if_else(position == "PG" & rank(-ppg_projection) == 3, "UTIL",
                              if_else(position == "PG" & rank(-ppg_projection) == 2, "G",
                                      if_else(position == "SG" & rank(-ppg_projection) == 3, "UTIL",
                                              if_else(position == "SG" & rank(-ppg_projection) == 2, "G", 
                                                      if_else(position == "SF" & rank(-ppg_projection) == 3, "UTIL",
                                                              if_else(position == "SF" & rank(-ppg_projection) == 2, "F",
                                                                      if_else(position == "PF" & rank(-ppg_projection) == 3, "UTIL",
                                                                              if_else(position == "PF" & rank(-ppg_projection) == 2, "F",
                                                                                      if_else(position == "C" & rank(-ppg_projection) == 2, "UTIL", position))))))))),
           position = factor(position, levels = c("PG", "SG", "SF", "PF", "C", "G", "F", "UTIL"))) %>% 
    arrange(position) 
  
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


team <- optimize_nba(train, cap = 50000, league = "draft_kings", removeteams = restrict)

## Generate the top 10 teams with no constraints (this may be a bit slow with other constraints)
top_1000 <- top_nba_teams(train, cap = 50000, n_top = 100, league = "draft_kings")

ggplot(top_1000, aes(avg_over_under, team_points)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(top_1000, aes(avg_abs_spread, team_points)) +
  geom_point() +
  geom_smooth(method = "lm")

minimize_spread <- function(top_teams){
  min_spread <- top_teams %>% 
    filter(avg_abs_spread == min(avg_abs_spread)) %>% 
    filter(team_points == max(team_points))
  
  return(min_spread)
}

min_spread <- top_1000 %>% 
  minimize_spread()

maximize_over_under <- function(top_teams){
  min_spread <- top_teams %>% 
    filter(avg_over_under == max(avg_over_under)) %>% 
    filter(team_points == max(team_points))
  
  return(min_spread)
}

max_over_under <- top_1000 %>% 
  maximize_over_under()

top_players <- function(top_teams){
  top_players <- top_teams %>% 
    count(position, first_name, last_name)
}