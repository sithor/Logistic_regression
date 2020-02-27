# setup libraries.
library(rio)
library(shiny)
library(magrittr)
library(regplot)
library(shinythemes)
library(plotly)
library(reshape2)
library(rms)

two_dp <- function(x){
    formatC(x, digits = 2, format = "f")
}

zero_dp <- function(x){
    formatC(x, digits = 0, format = "f")
}


##----  plot regression surface.----

## Import data ----
url <- "./data/simple_sids_epiinfo2.xlsx"

missing_strings <- c("", NA, " ", "  ")

for_shiny <- rio::import(url, na = missing_strings, which = 2) %>% data.frame




three_d_plot_with_regression_surface <- function(df = for_shiny, reg_model = "Logistic with linear terms",
                                                 covars = c("Mother_age", "Birth_wt"), 
                                                 outcome = "Case_bin", adjusted_or_crude = "crude",
                                                 dimension = c(10, 10), opacity_surface = 0.75, get_model = FALSE){
    
    
    run_model <- function(df1 = df, reg_model1 = reg_model, covars1 = covars, outcome1 = outcome, adjust = adjusted_or_crude){
        
        #print(head(df1))
        
       
   
        
        if (reg_model1 == "Logistic with linear terms" & adjust == "adjusted"){
            model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2])), 
                          family =  binomial(link = "logit"),
                          data = df1, na.action = na.exclude)
        }
        
        #browser()
        
        if (reg_model1 == "Logistic with linear terms" & adjust == "crude (birth weight \u2192 cot death)"){
            model3 <- glm(as.formula(paste0(outcome1, "~",  covars1[2])), 
                          family =  binomial(link = "logit"),
                          data = df1, na.action = na.exclude)
        }
        
        if (reg_model1 == "Logistic with linear terms" & adjust == "crude (maternal age \u2192 cot death)"){
            model3 <- glm(as.formula(paste0(outcome1, "~",  covars1[1])), 
                          family =  binomial(link = "logit"),
                          data = df1, na.action = na.exclude)
        }
        
        
        if (reg_model1 == "Logistic with quadratic terms" & adjust == "crude (birth weight \u2192 cot death)"){
            model3 <- glm(as.formula(paste0(outcome1, "~", 'I(', covars1[2], '^ 2)')), 
                          family =  binomial(link = "logit"),
                          data = df1, na.action = na.exclude)
        }
        if (reg_model1 == "Logistic with quadratic terms" & adjust == "adjusted"){
            model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2],' +', 
                                            'I(', covars1[1], '^ 2)', '+', 'I(', covars1[2], '^ 2)')), 
                          family = binomial(link = "logit"),
                          data = df1, na.action = na.exclude)
        }
        
        if (reg_model1 == "Logistic with quadratic terms" & adjust == "crude (maternal age \u2192 cot death)"){
            model3 <- glm(as.formula(paste0(outcome1, "~", 'I(', covars1[1], '^ 2)')), 
                          family = binomial(link = "logit"),
                          data = df1, na.action = na.exclude)
        }
        
        if (reg_model1 == "Logistic with restricted cubic spline" & adjust == "crude (birth weight \u2192 cot death)"){
            model3 <- rms::Glm(as.formula(paste0(outcome1, "~", "rcs(", covars1[2],")")),  family = binomial,
                          data = df1, na.action = na.exclude)
        }
        if (reg_model1 == "Logistic with restricted cubic spline" & adjust == "crude (maternal age \u2192 cot death)"){
            model3 <- rms::Glm(as.formula(paste0(outcome1, "~",  "rcs(", covars1[1],")")), family = binomial,
                          data = df1, na.action = na.exclude)
        }
        
        if (reg_model1 == "Logistic with restricted cubic spline" & adjust =="adjusted" ){
            model3 <- rms::Glm(as.formula(paste0(outcome1, "~", "rcs(", covars1[2],') + rcs(', 
                                            covars1[1], ')')), family = binomial,
                                               data = df1, na.action = na.exclude)
            
        }
        
        
        
        return(model3)
    }
    
    
    
    
    model <- run_model()
    
    if (get_model == TRUE){
      return(model)
    }
    
    # model %>% print
    stopifnot(df %>%  is.data.frame)
    ## Calculate various variables required for plotting.
    if (reg_model == "Logistic with restricted cubic spline"){
        df$predicted_outcome <- predict(model, newdata = df, type = "lp") %>% exp 
    } else {
        df$predicted_outcome <- predict(model, newdata = df) %>% as.numeric
    }
    #df %>% str %>% print
    df <- na.exclude(df)
    
    df$predicted_minus_observed_outcome <- df$predicted_outcome - df[, outcome] 
    
    df$predicted_minus_observed_outcome %>% str
    
    df$opacity <- (df[, outcome] - df$predicted_outcome )^2/ max((df[, outcome] - df$predicted_outcome )^2 %>% abs)
    
    df$predicted_outcome %>% zero_dp
    
    
    df %<>% data.frame
    
    
    n_points <- 25
    graph_reso_first_covar <- (max(df[, covars[1], drop = FALSE]) - min(df[, covars[1], drop = FALSE]))/n_points
    #print("graph_reso_mileage is:")
    #print(graph_reso_mileage)
    
    graph_reso_second_covar <- (max(df[, covars[2], drop = FALSE]) - min(df[, covars[2], drop = FALSE]))/n_points
    #print("graph_reso_year is:")
    #print(graph_reso_year)
    #Setup Axis
    axis_x <- seq(min(df[, covars[1], drop = FALSE]), max(df[, covars[1], drop = FALSE]), 
                  by = graph_reso_first_covar)
    axis_y <- seq(min(df[, covars[2], drop = FALSE]), max(df[, covars[2], drop = FALSE]), 
                  by = graph_reso_second_covar)
    model_surface <- expand.grid(x = axis_x, y = axis_y, KEEP.OUT.ATTRS = F)
    names(model_surface) <- covars
    
    if (reg_model == "Logistic with linear terms" | reg_model == "Logistic with quadratic terms"){
        model_surface$outcome <- predict.glm(model, newdata = model_surface, type = "response")
    }
    
    if (reg_model == "Logistic with restricted cubic spline"){
        model_surface$outcome <- 1/((-(predict(model, newdata = model_surface, type = "lp")) %>% exp) + 1)  
    }
    
 
    model_surface <- acast(model_surface, as.formula(paste0(covars[2], "~", covars[1])),
                           value.var = "outcome") #y ~ x
    surf <- cbind(axis_x, axis_y, model_surface) %>% data.frame
    
    # Create lists for axis properties
    f1 <- list(
        family = "Arial, sans-serif",
        size = 18,
        color = "black")
    
    f2 <- list(
        family = "Old Standard TT, serif",
        size = 14,
        color = "#ff9999")
    
    xaxis <- list(
        titlefont = f1,
        tickfont = f2,
        showgrid = TRUE,
        title = covars[1]
    )
    
    yaxis <- list(
        titlefont = f1,
        tickfont = f2,
        showgrid = TRUE,
        title =  covars[2]
    )
    
    
    zaxis <- list(
        titlefont = f1,
        tickfont = f2,
        showgrid = TRUE,
        title = outcome
    )
    
    
    scene = list(
        xaxis = xaxis,
        yaxis = yaxis,
        zaxis = zaxis
    )
    
 
    
    #- Plot 
    plot_3d <- plot_ly( data = df, 
                        x = ~df[, covars[1]], 
                        y = ~df[, covars[2]],
                        z = ~df[, outcome],
                        type = "scatter3d",
                        name = "Cot death, maternal age and birth weight",
                        marker = list(size = 5, 
                                      color = df$opacity, 
                                      line = list(
                                          color = 'black',
                                          width = 1)),
                        color = 'rgba(255, 182, 193, .9)',
                        width = (0.6*as.numeric(dimension[1])), 
                        height = 0.7*as.numeric(dimension[2]),
                        hoverinfo = 'text',
                        hoverlabel = list(bgcolor = "black", font = list(color = "white")),
                        text = ~paste('Maternal age', df[, covars[1]] %>% two_dp, 
                                      '<br>Birth weight', df[, covars[2]], 
                                      '<br>Cot death:', df[, outcome ])) %>% 
        layout(title = "", scene = list(
            xaxis = list(title = "Maternal age (years)"),
            yaxis = list(title = "Birth weight (grams)"),
            zaxis = list(title = "Cot death (1 = case; 0 = control)"))) %>% suppressWarnings
    
    plot_3d <- add_trace(p = plot_3d,
                         data = surf,
                         z = ~model_surface,
                         x = ~axis_x,
                         y = ~axis_y,
                         opacity = opacity_surface,
                         type = "surface",
                         name = "regression plane",
                         showlegend = F,
                         hoverinfo = 'all',
                         text = ~paste('Maternal age:', df[, covars[1]] %>% two_dp, 
                                       '<br>Birth weight:', df[, covars[2]], 
                                       '<br>Cot death:', df[, outcome]
                         )
    ) %>% suppressWarnings
    plot_3d
}





#---- Define UI for app  ----
ui <- fluidPage(theme = shinytheme("cyborg"), ## provides theme for shiny app...
                # this provides dimensions for plotly output.
                
                #--- This styles validation error messages... --- 
                
                tags$head(
                    tags$style(HTML("
                                  .shiny-output-error-validation {
                                  color: green;
                                  }
                                  "))),
                
                tags$head(
                    tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                        
                        h1 {
                        font-family: 'Lobster', cursive;
                        font-weight: 500;
                        line-height: 1.1;
                        color: #33ccff;
                        }
                        
                        "))),
                
                tags$head(
                    tags$style(HTML("
                        @import url('https://fonts.googleapis.com/css?family=Frank+Ruhl+Libre');
                        
                        body {
                        font-family: 'Frank Ruhl Libre', serif;
                        font-weight:  50;
                        line-height: 1.1;
                        color: #FFCBB8;
                        }
                        
                        "))),
                
                tags$head(tags$script('
                        var dimension = [0, 0];
                                      $(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                      });
                                      $(window).resize(function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                      });
                                      ')),
                tabsetPanel(
                    tabPanel("Visualise logistic regression", 
                             # App title ----
                             headerPanel("Is maternal age or birth weight associated with cot death?"),
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput('reg_model', 'Regression Model', c("Logistic with linear terms", "Logistic with quadratic terms",
                                                                                    "Logistic with restricted cubic spline")),
                                     selectInput('adjust', 'Crude or adjusted model?', c("crude (maternal age \u2192 cot death)",
                                                                                         "crude (birth weight \u2192 cot death)", "adjusted"))),
                                 # Sidebar layout with input and output definitions ----
                                 # 3d plot tab----
                                 mainPanel("This plot illustrates the relationship between cot death (case or controls), 
                                            maternal age and birth weight. Select the type of model and 
                                           whether or not you want a crude or adjusted model. A nomogram is another way of 
                                           visualising a regression model. Click on ", tags$b(HTML("<font color = 'white' >", "Regression nomogram", "</font>")),
                                           " at the top of the page to see 
                                           the corresponding nomogram of the model.",
                                           br(),
                                           br(),
                                           plotlyOutput("three_d_plot", width = "100%", height = "100%"))
                                 
                             )),
                    tabPanel("Regression nomogram", 
                             headerPanel("Visualise a regression model as a nomogram"),
                             mainPanel("This is a nomogram of the logistic regression model, from Roger Marshall's ", tags$i("regplot"), " package.",
                                       "\n The independent variables (maternal age and birth weight) contribute points that correspond to a probability
                                       of the outcome (case or control status). Density plots indicate the distribution of each independent variable. 
                                       Asterisks indicate statistical significance.",
                                       br(),
                                       br(),
                                       plotOutput("regplot_logistic", width = "100%", height = "800px")))
                    
                    ))






# Define server logic ----
server <- function(input, output) {
    
    
    #--- Render 3d plot ----
    
    output$three_d_plot <- renderPlotly({
        
        three_d_plot_with_regression_surface(df = for_shiny, reg_model = input$reg_model,
                                             covars = c("Mother_age", "Birth_wt"), 
                                             outcome = "Case_bin", adjusted_or_crude = input$adjust,
                                             dimension = input$dimension)
        
    })
    
    
    output$regplot_logistic <- renderPlot({
     model <- three_d_plot_with_regression_surface(df = for_shiny, reg_model = input$reg_model,
                                           covars = c("Mother_age", "Birth_wt"), 
                                           outcome = "Case_bin", adjusted_or_crude = input$adjust,
                                           dimension = input$dimension, get_model = TRUE)
      regplot(model, clickable = FALSE)
      
    })
    

}

shinyApp(ui = ui, server = server)



