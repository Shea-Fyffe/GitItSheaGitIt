#' @title Working Longitudinal Measurement Invariance Project
#' @author Shea Fyffe
import_data <- function(filename, n_items, time_points, ...) {
    if (!is.character(filename) || missing(filename)) {
        stop("Please specify filename as a string")
    }
    if (!is.numeric(n_items) || !is.numeric(time_points)) {
        stop("Verify items and time points are numeric")
    }
    if (!file.exists(filename)) {
        stop("Please verify file is in the appropriate directory")
    }
    .x <- read.table(filename, ...)
    if (ncol(.x) %% n_items != 0) {
        stop("Dataframe not divisible by number of items, remove any access columns")
    }
    if (!is.null(names(.x))) {
        names(.x) <- NULL
    }
    names(.x) <- as.vector(sapply(seq(time_points), function(X) paste("Time", X, "Item", 
        seq(n_items), sep = "_")))
    return(.x)
}
get_variables <- function(x, pattern) {
    x <- x[grepl(names(x), pattern = pattern)]
    x
}
calc_df <- function(n_items, time_points) {
    if (!is.numeric(n_items) || !is.numeric(time_points)) {
        stop("Verify items and time points are numeric")
    }
    if (n_items <= 1 || time_points <= 1) {
        stop("Time points and items need to be greater than 1")
    }
    p <- n_items * time_points
    p <- (p * (p + 1))
    p <- p/2
    return(p)
}
make_labels <- function(n_items, time_points) {
    if (!is.numeric(n_items) || !is.numeric(time_points)) {
        stop("Verify items and time points are numeric")
    }
    .out <- sapply(seq(time_points), function(X) paste(sprintf("Time %s", X), sprintf("Item %s", 
        seq(n_items)), sep = "\n"))
    return(as.vector(.out))
}
# ' Formula Building functions
latent_formula <- function(n_items, time_points, type) {
    .out <- sapply(seq(time_points), function(X) paste("Time", X, "Item", seq(n_items), 
        sep = "_"))
    if (type != "config") {
        .out <- rbind(apply(.out, 2, function(X) sprintf(".con%s*%s", seq(n_items), X)))
    }
    .out <- apply(.out, 2, function(X) paste(X, collapse = " + "))
    .out <- sprintf("Latent%s =~ %s", seq(time_points), .out)
    .out <- paste(.out, collapse = "\n")
    .out <- sprintf("# Define the latent factors.\n%s", .out)
    return(.out)
}
intercept_formula <- function(n_items, time_points, type) {
    .out <- sapply(seq(time_points), function(X) paste("Time", X, "Item", seq(n_items), 
        sep = "_"))
    if (type %in% c("config", "weak")) {
        .int <- ifelse(grepl("Item_1", .out), "~ 0*1", "~ 1")
        .out <- paste(.out, .int)
    } else {
        .const <- c("0*1", sprintf("i%s*1", seq(n_items - 1)))
        .out <- apply(.out, 2, function(X) sprintf("%s ~ %s", X, .const))
    }
    .out <- paste(.out, collapse = "\n")
    .out <- sprintf("# Intercepts.\n%s", .out)
    return(.out)
}
uniquevar_formula <- function(n_items, time_points, type, unique = FALSE) {
    loop <- function(x) {
        .x <- x[-length(x)]
        for (i in seq(.x)) {
            .x[i] <- paste(x[(i + 1):length(x)], collapse = " + ")
            .x[i] <- paste(x[i], .x[i], sep = " ~~ ")
        }
        return(.x)
    }
    .out <- sapply(seq(time_points), function(X) paste("Time", X, "Item", seq(n_items), 
        sep = "_"))
    if (type == "strict" & !unique) {
        ..out <- apply(.out, 2, function(X) sprintf("f%s*%s", seq(n_items), X))
        ..out <- paste(.out, " ~~ ", ..out)
        ..out <- paste(..out, collapse = "\n")
        .out <- apply(.out, 1, loop)
        .out <- paste(.out, collapse = "\n")
        .out <- c(..out, .out)
    } else if (type == "strict" & unique) {
        ..out <- apply(.out, 2, function(X) sprintf("f%s*%s", seq(n_items), X))
        ..out <- paste(.out, " ~~ ", ..out)
        .out <- paste(..out, collapse = "\n")
    } else {
        if (unique) {
            .out <- paste(.out, "~~", .out)
            .out <- paste(.out, collapse = "\n")
        } else {
            .out <- apply(.out, 1, loop)
            .out <- paste(.out, collapse = "\n")
        }
    }
    .out <- sprintf("# Unique Variances and Covariances.\n%s", .out)
    return(.out)
}
latentmeans_formula <- function(time_points) {
    # .out <- c('0*1', rep(1,(time_points - 1))) .out <- sprintf('Latent%s ~ %s',
    # seq(time_points), .out)
    .out <- sprintf("Latent%s ~ %s", seq(time_points), rep(1, (time_points)))
    .out <- paste(.out, collapse = "\n")
    .out <- sprintf("# Latent Variable Mean Structure.\n%s", .out)
    return(.out)
}
latentvar_formula <- function(time_points) {
    permutations <- function(time_points, type = "config") {
        if (time_points == 1) {
            return(matrix(1))
        } else {
            sp <- permutations(time_points - 1)
            p <- nrow(sp)
            A <- matrix(nrow = time_points * p, ncol = time_points)
            for (i in 1:time_points) {
                A[(i - 1) * p + 1:p, ] <- cbind(i, sp + (sp >= i))
            }
            return(A)
        }
    }
    if (!is.numeric(time_points)) {
        stop("Verify time points is numeric")
    }
    if (time_points <= 1) {
        stop("Time points needs to be greater than 1")
    }
    .out <- sapply(seq(time_points), function(X) sprintf("Latent%s ~~ Latent%s", X, seq(time_points)))
    .out <- .out[lower.tri(.out, diag = TRUE)]
    .out[1] <- sub("~~ L", "~~ 1*L", .out[1])
    .out <- paste(.out, collapse = "\n")
    .out <- sprintf("# Latent Variable Variances and Covariance.\n%s", .out)
    return(.out)
}
build_formula <- function(n_items, time_points, type, ...) {
    if (!is.character(type) || !type %in% c("config", "weak", "strong", "strict")) {
        stop("Please verify that type is a character specifying 'config', 'weak', 'strong', or 'strict'...")
    }
    if (!is.numeric(n_items) || !is.numeric(time_points)) {
        stop("Verify items and time points are numeric")
    }
    if (n_items <= 1 || time_points <= 1) {
        stop("Time points and items need to be greater than 1")
    }
    .a <- latent_formula(n_items, time_points, type)
    .b <- intercept_formula(n_items, time_points, type)
    .c <- uniquevar_formula(n_items, time_points, type, ...)
    .d <- latentmeans_formula(time_points)
    # .e <- latentvar_formula(time_points)
    .out <- paste(c(.a, .b, .c, .d), collapse = "\n\n")
    return(.out)
}

# ' Extract Model functions
print_summary <- function(x) {
    x <- summary(x, standardized = TRUE, fit.measures = TRUE)
    print(x)
}
extract_fit <- function(x) {
    .out <- round(lavaan::inspect(x, "fit.measures"), 3)
    return(.out)
}

# ' MAKE A MODEL COMPARISONS FUNCTION lavaan::anova()
compare_models <- function(a, b) {
    out <- lavaan::anova(a, b)
    return(out)
}

# ' Tests Correlation between items ' Step 1 in (Schmitt & Kuljanin, 2008)
# TODO(shea.fyffe) Deal with missing values for correlations
test_cor <- function(df, n_items, time_points, ...) {
    if (!inherits(df, "data.frame") && !inherits(df, "matrix")) {
        stop("Verify df is a matrix or data.frame")
    }
    .mode <- sapply(df, is.numeric)
    if (!all(.mode)) {
        stop("Verify all columns in df are numeric")
    }
    # figure out what to do with missing values
    .cor <- stats::cor(df, use = "pairwise.complete.obs")
    .cor <- corrplot::corrplot(.cor, method = "square", diag = FALSE, order = "hclust", 
        addrect = time_points, addCoef.col = "black", tl.col = "red", tl.cex = 0.75, number.cex = 0.5, 
        tl.srt = 45, ...)
    return(.cor)
}
