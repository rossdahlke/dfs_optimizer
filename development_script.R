library(lpSolveAPI)

train <- dplyr::filter(readr::read_csv("test_data/draftking_test.csv"), is.na(injury))
train$id <- seq(1, nrow(train),1)

high_scoring <- T
close_game <- T
blow_out <- F

### function

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
set.objfn(dfs_fantasy, train$projection)
  
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

for(k in 1:nrow(setplayers)) {
  add.constraint(lpfantasy, ifelse(setplayers$Id[k] == train$Id, 1, 0), "=", 1)
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
team <- subset(data.frame(train, get.variables(dfs_fantasy)), get.variables.dfs_fantasy. == 1)
team$get.variables.lpfantasy. <- NULL
team$team_salary <- sum(team_select$salary)
team$team_points <- sum(team_select$projection)
team <- subset(team, select = -c(get.variables.dfs_fantasy.))
team$position <- factor(team$position, levels = c("PG", "SG", "SF", "PF", "C"))
team <- dplyr::arrange(team, position, salary)

