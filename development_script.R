library(lpSolveAPI)



find_teams <- function(train, cap, constraint = c("none", "all_diff", "no_opp"), 
                       league = c("FanDuel", "DraftKings"), setplayers = NULL, removeteams = NULL, strategy = c("base", "risky", "safe")) {
  
  if(strategy == "base"){
    train <- train %>%
      select(Id, Position, FirstName, LastName, points, Salary, Team, Opponent) %>%
      rename(ExpectedPoints = points)
  }
  
  if(strategy == "risky"){
    train <- train %>%
      select(Id, Position, FirstName, LastName, ceiling, Salary, Team, Opponent) %>%
      rename(ExpectedPoints = ceiling)
  }
  
  if(strategy == "safe"){
    train <- train %>%
      select(Id, Position, FirstName, LastName, floor, Salary, Team, Opponent) %>%
      rename(ExpectedPoints = floor)
  }
  
  colnames(train) <- c("Id", "Position", "FirstName", "LastName", "ExpectedPoints", "Salary", "Team", "Opponent")
  
  ## set constraints to use
  pg <- ifelse(train$Position == "PG", 1, 0)
  sg <- ifelse(train$Position == "SG", 1, 0)
  sf <- ifelse(train$Position == "SF", 1, 0)
  pf <- ifelse(train$Position == "PF", 1, 0)
  c <- ifelse(train$Position == "C", 1, 0)
  
  ## number of decision variables is equal to the number of fantasy players/teams
  lpfantasy <- make.lp(0, nrow(train))
  
  ## Set objective function with the expected number of points
  set.objfn(lpfantasy, train$ExpectedPoints)
  
  ## Make sure the decision variables are binary
  set.type(lpfantasy, seq(1, nrow(train), by=1), type = c("binary"))
  
  ## Add some contraints
  ## Only select one defense, exactly 3 wide receivers, etc.
  ## Depends on what fantasy league you are playing in, currently for FanDuel and DraftKings
  if(league == "FanDuel") {
    add.constraint(lpfantasy, pg, "=", 2)
    add.constraint(lpfantasy, sg, "=", 2)
    add.constraint(lpfantasy, sf, "=", 2)
    add.constraint(lpfantasy, pf, "=", 2)
    add.constraint(lpfantasy, c, "=", 1)
  }
  # if(league == "DraftKings") {
  #   dk_total <- defense + qb + wr + rb + te + k
  #   add.constraint(lpfantasy, defense, "=", 1)
  #   add.constraint(lpfantasy, qb, "=", 1)
  #   add.constraint(lpfantasy, wr, "<=", 4)
  #   add.constraint(lpfantasy, rb, "<=", 3)
  #   add.constraint(lpfantasy, te, "<=", 2)
  #   add.constraint(lpfantasy, k, "=", 0)
  #   add.constraint(lpfantasy, dk_total, "=", 9)
  # }
  
  
  ## Add monetary constraint, max salary for the team
  add.constraint(lpfantasy, train$Salary, "<=", cap)
  
  ## Set objective direction
  lp.control(lpfantasy, sense='max')
  
  team_names <- levels(factor(train$Team))
  constraint <- match.arg(constraint)
  
  
  if(!is.null(setplayers)) {
    if(league == "FanDuel") {
      if((sum(setplayers$Position == "PG") > 2) || (sum(setplayers$Position == "SG") > 2) || (sum(setplayers$Position == "SF") > 2) ||
         (sum(setplayers$Position == "PF") > 2) || (sum(setplayers$Position == "C") > 1))
        stop("One of your positions has too many players")
    }
    # if(league == "DraftKings") {
    #   if((sum(setplayers$Position == "WR") > 4) || (sum(setplayers$Position == "RB") > 3) || (sum(setplayers$Position == "QB") > 1) ||
    #      (sum(setplayers$Position == "TE") > 2) || (sum(setplayers$Position == "K") > 0) || (sum(setplayers$Position == "D") > 1))
    #     stop("One of your positions has too many players")
    # }
    ## Set constraints that each player here must be in lineup
    for(k in 1:nrow(setplayers)) {
      add.constraint(lpfantasy, ifelse(setplayers$Id[k] == train$Id, 1, 0), "=", 1)
    }
  }
  
  if(!is.null(removeteams)) {
    if(nrow(removeteams) != nrow(train))
      stop("Your team restrictions do not match the number of players included in the 'train' file")
    for(m in 1:ncol(removeteams)) {
      add.constraint(lpfantasy, removeteams[, m], "<=", 8)
    }
  }
  
  team <- data.frame(matrix(0, 1, ncol(train) + 2))
  colnames(team) <- c(colnames(train), "TeamSalary", "TotalPoints")
  
  ## Solve the model, if this returns 0 an optimal solution is found
  solve(lpfantasy)
  if(solve(lpfantasy) != 0)
    stop("Optimization failed at some step")
  
  ## Get the players on the team
  team_select <- subset(data.frame(train, get.variables(lpfantasy)), get.variables.lpfantasy. == 1)
  team_select$get.variables.lpfantasy. <- NULL
  team_select$TeamSalary <- sum(team_select$Salary)
  team_select$TotalPoints <- sum(team_select$ExpectedPoints)
  team <- rbind(team, team_select)
  team <- team[-1,]
  rownames(team) <- NULL
  team
}


train <- gs_title("fd_prices") %>%
  gs_read() %>%
  janitor::clean_names() %>%
  left_join(gs_title("rotogrinders_bball") %>%
              gs_read() %>%
              janitor::clean_names(), by = c("nickname" = "player")) %>%
  filter(!is.na(salary_rg)) %>%
  mutate(player = nickname) %>%
  filter(is.na(injury_indicator)) %>%
  select(id, position.x, player, first_name, last_name, points, floor, ceiling, salary, team.x, opponent.x) %>%
  rename(Id = id, Position = position.x, FirstName = first_name, LastName = last_name, Salary = salary, Team = team.x, Opponent = opponent.x) %>%
  distinct(FirstName, LastName, .keep_all = T)



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # Application title
                titlePanel("NBA DFS Optimizer"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    actionButton(
                      inputId = "submit_loc",
                      label = "Optimize Lineup"),
                    
                    selectInput(inputId = "strategy",
                                label = "Choose your strategy/ game type",
                                choices = c("Balanced/ overall", "High-ceiling/ GPP", "Low-floor/ cash")),
                    
                    selectInput(inputId = "pg_1",
                                label = "Custom PG 1",
                                choices = train %>% filter(Position == "PG") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "pg_2",
                                label = "Custom PG 2",
                                choices = train %>% filter(Position == "PG") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "sg_1",
                                label = "Custom SG 1",
                                choices = train %>% filter(Position == "SG") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "sg_2",
                                label = "Custom SG 2",
                                choices = train %>% filter(Position == "SG") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "sf_1",
                                label = "Custom SF 1",
                                choices = train %>% filter(Position == "SF") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "sf_2",
                                label = "Custom SF 2",
                                choices = train %>% filter(Position == "SF") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "pf_1",
                                label = "Custom PF 1",
                                choices = train %>% filter(Position == "PF") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "pf_2",
                                label = "Custom PF 2",
                                choices = train %>% filter(Position == "PF") %>% pull(player) %>% 
                                  R.utils::insert(1, NA)),
                    
                    selectInput(inputId = "c_1",
                                label = "Custom C 1",
                                choices = train %>% filter(Position == "C") %>% pull(player) %>% 
                                  R.utils::insert(1, NA))
                    
                    
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("lineup")
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  strategy_type <- reactive({
    switch(input$strategy,
           "Balanced/ overall" = "base",
           "High-ceiling/ GPP" = "risky",
           "Low-floor/ cash" = "safe")
  })
  
  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {
      
      if(input$pg_1 == "NA" & input$pg_2 == "NA" & input$sg_1 == "NA" & input$sg_2 == "NA" & input$sf_1 == "NA" & input$sf_2 == "NA" & input$pf_1 == "NA" & input$pf_2 == "NA" & input$c_1 == "NA"){
        
        opt_lineup <- find_teams(train, 60000, constraint = "none", league = "FanDuel", setplayers = NULL, removeteams = NULL, strategy = strategy_type())
      }
      else{
        
        setplayers <- train %>% 
          filter(player %in% c(input$pg_1, input$pg_2, input$sg_1, input$sg_2, input$sf_1, input$sf_2, input$pf_1, input$pf_2, input$c_1)) %>% pull(Id)
        
        setplayers <- train %>% filter(Id %in% setplayers)
        opt_lineup <- find_teams(train, 60000, constraint = "none", league = "FanDuel", setplayers = setplayers, removeteams = NULL, strategy = strategy_type())
      }
      
      opt_lineup <- opt_lineup %>%
        mutate(Position = factor(Position, levels = c("PG", "SG", "SF", "PF", "C"))) %>%
        arrange(Position, Salary) %>%
        select(Position, FirstName, LastName, Salary, Team, Opponent, TeamSalary) %>%
        mutate(Salary = formattable::currency(Salary, digits = 0L),
               TeamSalary = formattable::currency(TeamSalary, digits = 0L))
      
      output$lineup <- renderPlot({
        
        
        colnames(opt_lineup) <- c("Position", "First Name", "Last Name", "Player Salary", "Team", "Opponent", "Team Salary")
        
        tt <- ttheme_minimal()
        
        grid.table(opt_lineup, theme=tt)
        
      }
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

