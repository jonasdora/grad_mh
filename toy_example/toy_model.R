# Base modeling script - starting off with simple analysis
BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/shackman/grad_mh/project_config.R'

# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop(paste('ERROR: Missing project config file.', BASE_FILE))
}

source(BASE_FILE)
sapply(list.files(R_DIR, full.names = TRUE), source)

# Generate mock data if it doesn't exist in the working directory
if(!file.exists('/toy_data.csv')){
    library(simr)
    x <- 1:10
    x2 <- x^2
    g <- letters[1:10]
    n_obs <- 100
    X <- data.frame()
    for(n in 1:n_obs){
        df <- data.frame(
            group = rep(g, each=length(x)),
            time = rep(x, times=length(g)),
            time2 = rep(x2, times=length(g))
        )
        X <- rbind(X, df)
    }
    
    b <- c(0.2, .2, -0.01) # fixed intercept and slope
    V1 <- 0.5 # random intercept variance
    V2 <- matrix(
        c(
            0.25, -.25, -.50,
            -.25, 0.10, -.50,
            -.50, -.50, 0.001
            ), 3) # random intercept and slope variance-covariance matrix
    s <- .5 # residual standard deviation
    model_object <- makeGlmer(y ~ 1 + time + time2 + (1 + time + time2|group), 
                              family=binomial, fixef=b, VarCorr=V2, data=X)
    
    getData(model_object) %>% write_csv('toy_data.csv')
}

df <- read.csv('toy_data.csv')
id_var <- "group"
y_var <- "y"
lv1_vars <- c('time', 'time2')
lv2_int_vars <- NULL

# Simple tests to ensure required variables are present
testthat::expect_true(
    id_var %in% names(df)
)

testthat::expect_true(
    y_var %in% names(df)
)

testthat::expect_true(
    all(lv1_vars %in% names(df))
)

testthat::expect_true(
    all(lv2_int_vars %in% names(df))
)

res <- logistic_model_wrapper(
    data = df, 
    prior_config = PRIOR_CONFIG,
    y_var = y_var, 
    id_var = id_var, 
    lv1_vars = lv1_vars, 
    lv1_ran_vars = 'time', 
    lv2_int_vars = lv2_int_vars, 
    output_folder = POSTERIOR_OUTPUTS, 
    warmup = 1000, 
    iter = 2500, 
    chains = 3, 
    control_list = list(adapt_delta = .95), 
    model_save_name = "toy_example2",
    chain_hyperthreading = F
)
