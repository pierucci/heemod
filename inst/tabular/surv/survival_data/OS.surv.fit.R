OS2.fit <-
structure(list(structure(list(call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
    data = this_data, dist = conditions[i, "dist"])), dlist = structure(list(
    name = "exp", pars = "rate", location = "rate", transforms = list(
        .Primitive("log")), inv.transforms = list(.Primitive("exp")), 
    inits = function (t, mf, mml, aux) 
    {
        if (aux$counting) {
            1/mean(t)
        }
        else {
            aux$formula <- aux$forms[[1]]
            aux$forms <- NULL
            aux$dist <- "exponential"
            sr <- do.call(survreg, aux)
            sr2fsexp(sr)
        }
    }), .Names = c("name", "pars", "location", "transforms", 
"inv.transforms", "inits")), aux = NULL, ncovs = 0L, ncoveffs = 0L, 
    mx = structure(list(rate = integer(0)), .Names = "rate"), 
    basepars = 1L, covpars = NULL, AIC = 345.292856186918, data = structure(list(
        Y = structure(c(3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
        22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf), .Dim = c(50L, 6L), .Dimnames = list(c("1", "2", 
        "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
        "13", "14", "15", "16", "17", "18", "19", "20", "21", 
        "22", "23", "24", "25", "26", "27", "28", "29", "30", 
        "31", "32", "33", "34", "35", "36", "37", "38", "39", 
        "40", "41", "42", "43", "44", "45", "46", "47", "48", 
        "49", "50"), c("time", "status", "start", "stop", "time1", 
        "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
        116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
        47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(50L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "(weights)"), terms = quote(survival::Surv(time, status) ~ 
            1), row.names = c(NA, 50L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
        mml = structure(list(rate = structure(c(1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 1L), .Dimnames = list(
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
            "11", "12", "13", "14", "15", "16", "17", "18", "19", 
            "20", "21", "22", "23", "24", "25", "26", "27", "28", 
            "29", "30", "31", "32", "33", "34", "35", "36", "37", 
            "38", "39", "40", "41", "42", "43", "44", "45", "46", 
            "47", "48", "49", "50"), "(Intercept)"), assign = 0L)), .Names = "rate")), .Names = c("Y", 
    "m", "mml")), datameans = numeric(0), N = 50L, events = 24L, 
    trisk = 11271, concat.formula = quote(survival::Surv(time, 
        status) ~ 1), all.formulae = structure(list(rate = quote(survival::Surv(time, 
        status) ~ 1)), .Names = "rate"), dfns = structure(list(
        p = function (q, rate = 1, lower.tail = TRUE, log.p = FALSE) 
        .Call(C_pexp, q, 1/rate, lower.tail, log.p), d = function (x, 
            rate = 1, log = FALSE) 
        .Call(C_dexp, x, 1/rate, log), h = function (x, rate = 1, 
            log = FALSE) 
        {
            h <- dbase("exp", log = log, x = x, rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- if (log) 
                log(rate)
            else rate
            ret
        }, H = function (x, rate = 1, log = FALSE) 
        {
            h <- dbase("exp", log = log, x = x, rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- if (log) {
                log(rate) + log(x)
            }
            else rate * x
            ret
        }, r = function (n, rate = 1) 
        .Call(C_rexp, n, 1/rate), DLd = function (t, rate) 
        {
            res <- matrix(nrow = length(t), ncol = 1)
            ts <- 1 - t * rate
            res[, 1] <- ts
            res
        }, DLS = function (t, rate) 
        {
            res <- matrix(nrow = length(t), ncol = 1)
            res[, 1] <- -t * rate
            res
        }, deriv = TRUE), .Names = c("p", "d", "h", "H", "r", 
    "DLd", "DLS", "deriv")), res = structure(c(0.00212935853074259, 
    0.00142724331982307, 0.00317687088772524, 0.000434653453759071
    ), .Dim = c(1L, 4L), .Dimnames = list("rate", c("est", "L95%", 
    "U95%", "se"))), res.t = structure(c(-6.15193450389414, -6.55201044358413, 
    -5.75185856420416, 0.204124128221606), .Dim = c(1L, 4L), .Dimnames = list(
        "rate", c("est", "L95%", "U95%", "se"))), cov = structure(0.0416666597222305, .Dim = c(1L, 
    1L), .Dimnames = list("rate", "rate")), coefficients = -6.15193450389414, 
    npars = 1L, fixedpars = NULL, optpars = 1L, loglik = -171.646428093459, 
    logliki = structure(c(-6.15832257948637, -6.39894009346028, 
    -6.33292997900726, -6.30737767663835, -6.16045193801711, 
    -6.37551714962212, -6.74389617544058, -6.35422356431469, 
    -6.64594568302642, -6.33505933753801, -6.17748680626305, 
    -6.21368590128568, -6.19878039157048, -6.26053178896202, 
    -6.25201435483905, -6.53308968089707, -6.3797758666836, -6.17535744773231, 
    -6.26266114749276, -6.35848228137617, -6.35635292284543, 
    -6.30311895957687, -6.81416500695509, -6.17109873067083, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907, -0.745275485759907, 
    -0.745275485759907, -0.745275485759907), .Names = c("1", 
    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
    "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", 
    "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
    "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
    "43", "44", "45", "46", "47", "48", "49", "50")), cl = 0.95, 
    opt = structure(list(par = structure(-6.15193450389414, .Names = "rate"), 
        value = 171.646428093459, counts = structure(c(2L, 1L
        ), .Names = c("function", "gradient")), convergence = 0L, 
        message = NULL, hessian = structure(24.0000039999959, .Dim = c(1L, 
        1L), .Dimnames = list("rate", "rate"))), .Names = c("par", 
    "value", "counts", "convergence", "message", "hessian")), 
    BIC = 347.204879192346, m2LL = 343.292856186918), .Names = c("call", 
"dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
"AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
"all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
"npars", "fixedpars", "optpars", "loglik", "logliki", "cl", "opt", 
"BIC", "m2LL"), class = "flexsurvreg"), structure(list(call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
    data = this_data, dist = conditions[i, "dist"])), dlist = structure(list(
    name = "weibull.quiet", pars = c("shape", "scale"), location = "scale", 
    transforms = list(.Primitive("log"), .Primitive("log")), 
    inv.transforms = list(.Primitive("exp"), .Primitive("exp")), 
    inits = function (t, mf, mml, aux) 
    {
        if (aux$counting) {
            lt <- log(t[t > 0])
            c(1.64/var(lt), exp(mean(lt) + 0.572))
        }
        else {
            aux$formula <- aux$forms[[1]]
            aux$forms <- NULL
            aux$dist <- "weibull"
            sr <- do.call(survreg, aux)
            sr2fswei(sr)
        }
    }), .Names = c("name", "pars", "location", "transforms", 
"inv.transforms", "inits")), aux = NULL, ncovs = 0L, ncoveffs = 0L, 
    mx = structure(list(scale = integer(0), shape = NULL), .Names = c("scale", 
    "shape")), basepars = 1:2, covpars = NULL, AIC = 340.068039261249, 
    data = structure(list(Y = structure(c(3, 116, 85, 73, 4, 
    105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 
    52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 116, 85, 
    73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
    11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 3, 116, 85, 73, 4, 
    105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 
    52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 
    6L), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
    "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
    "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
    "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", 
    "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", 
    "48", "49", "50"), c("time", "status", "start", "stop", "time1", 
    "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
    116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 
    179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
        NULL, c("time", "status")), type = "right", class = "Surv"), 
        `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1)), .Names = c("survival::Surv(time, status)", "(weights)"
    ), terms = quote(survival::Surv(time, status) ~ 1), row.names = c(NA, 
    50L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
        mml = structure(list(scale = structure(c(1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 1L), .Dimnames = list(
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
            "11", "12", "13", "14", "15", "16", "17", "18", "19", 
            "20", "21", "22", "23", "24", "25", "26", "27", "28", 
            "29", "30", "31", "32", "33", "34", "35", "36", "37", 
            "38", "39", "40", "41", "42", "43", "44", "45", "46", 
            "47", "48", "49", "50"), "(Intercept)"), assign = 0L), 
            shape = NULL), .Names = c("scale", "shape"))), .Names = c("Y", 
    "m", "mml")), datameans = numeric(0), N = 50L, events = 24L, 
    trisk = 11271, concat.formula = quote(survival::Surv(time, 
        status) ~ 1), all.formulae = structure(list(scale = quote(survival::Surv(time, 
        status) ~ 1)), .Names = "scale"), dfns = structure(list(
        p = function (q, shape, scale = 1, lower.tail = TRUE, 
            log.p = FALSE) 
        {
            ret <- suppressWarnings(pweibull(q = q, shape = shape, 
                scale = scale, lower.tail = lower.tail, log.p = log.p))
            ret
        }, d = function (x, shape, scale = 1, log = FALSE) 
        {
            ret <- suppressWarnings(dweibull(x = x, shape = shape, 
                scale = scale, log = log))
            ret
        }, h = function (x, ...) 
        {
            d(x, ...)/(1 - p(x, ...))
        }, H = function (x, ...) 
        {
            -log(1 - p(x, ...))
        }, r = function (n, shape, scale = 1) 
        .Call(C_rweibull, n, shape, scale), DLd = function (t, 
            shape, scale) 
        {
            res <- matrix(nrow = length(t), ncol = 2)
            tss <- (t/scale)^shape
            res[, 1] <- 1 + shape * (log(t/scale) - log(t/scale) * 
                tss)
            res[, 2] <- -1 - (shape - 1) + shape * tss
            res
        }, DLS = function (t, shape, scale) 
        {
            res <- matrix(nrow = length(t), ncol = 2)
            tss <- (t/scale)^shape
            res[, 1] <- ifelse(t == 0, 0, -shape * log(t/scale) * 
                tss)
            res[, 2] <- tss * shape
            res
        }, deriv = TRUE), .Names = c("p", "d", "h", "H", "r", 
    "DLd", "DLS", "deriv")), res = structure(c(0.631220652728093, 
    643.292024431365, 0.437494544219856, 313.503316937411, 0.910730242684468, 
    1320.00079852304, 0.118063378761429, 235.91817590617), .Dim = c(2L, 
    4L), .Dimnames = list(c("shape", "scale"), c("est", "L95%", 
    "U95%", "se"))), res.t = structure(c(-0.460099790201068, 
    6.46659878044078, -0.826691043616838, 5.74780994035962, -0.0935085367852975, 
    7.18538762052193, 0.187039790683602, 0.366735738896668), .Dim = c(2L, 
    4L), .Dimnames = list(c("shape", "scale"), c("est", "L95%", 
    "U95%", "se"))), cov = structure(c(0.0349838832989657, -0.0323532744516338, 
    -0.0323532744516338, 0.134495102184085), .Dim = c(2L, 2L), .Dimnames = list(
        c("shape", "scale"), c("shape", "scale"))), coefficients = structure(c(-0.460099790201068, 
    6.46659878044078), .Names = c("shape", "scale")), npars = 2L, 
    fixedpars = NULL, optpars = 1:2, loglik = -168.034019630625, 
    logliki = structure(c(-4.98085921232229, -6.63413533205962, 
    -6.45902576680751, -6.37737104423943, -5.09367346472609, 
    -6.57672157384766, -7.20615315209091, -6.52031492295956, 
    -7.07590954227843, -6.46540434785483, -5.5393311109881, -5.92511398897467, 
    -5.80061477036836, -6.19382286737405, -6.15355633660859, 
    -6.90093816749675, -6.58749579216707, -5.50291437098142, 
    -6.20347374694177, -6.53195604416826, -6.52615929334519, 
    -6.36272555754935, -7.29072509805567, -5.41978951522015, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264, -0.680993638892264, 
    -0.680993638892264, -0.680993638892264), .Names = c("1", 
    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
    "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", 
    "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
    "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
    "43", "44", "45", "46", "47", "48", "49", "50")), cl = 0.95, 
    opt = structure(list(par = structure(c(-0.460099790201068, 
    6.46659878044078), .Names = c("shape", "scale")), value = 168.034019630625, 
        counts = structure(c(2L, 1L), .Names = c("function", 
        "gradient")), convergence = 0L, message = NULL, hessian = structure(c(36.7630998807433, 
        8.84349423004682, 8.84349423004682, 9.56254893338659), .Dim = c(2L, 
        2L), .Dimnames = list(c("shape", "scale"), c("shape", 
        "scale")))), .Names = c("par", "value", "counts", "convergence", 
    "message", "hessian")), BIC = 343.892085272105, m2LL = 336.068039261249), .Names = c("call", 
"dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
"AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
"all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
"npars", "fixedpars", "optpars", "loglik", "logliki", "cl", "opt", 
"BIC", "m2LL"), class = "flexsurvreg"), structure(list(call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
    data = this_data, dist = conditions[i, "dist"])), dlist = structure(list(
    name = "lnorm", pars = c("meanlog", "sdlog"), location = "meanlog", 
    transforms = list(function (x) 
    x, .Primitive("log")), inv.transforms = list(function (x) 
    x, .Primitive("exp")), inits = function (t, mf, mml, aux) 
    {
        if (aux$counting) {
            lt <- log(t[t > 0])
            c(mean(lt), sd(lt))
        }
        else {
            aux$formula <- aux$forms[[1]]
            aux$forms <- NULL
            aux$dist <- "lognormal"
            sr <- do.call(survreg, aux)
            sr2fsln(sr)
        }
    }), .Names = c("name", "pars", "location", "transforms", 
"inv.transforms", "inits")), aux = NULL, ncovs = 0L, ncoveffs = 0L, 
    mx = structure(list(meanlog = integer(0), sdlog = NULL), .Names = c("meanlog", 
    "sdlog")), basepars = 1:2, covpars = NULL, AIC = 337.397815856574, 
    data = structure(list(Y = structure(c(3, 116, 85, 73, 4, 
    105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 
    52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 116, 85, 
    73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
    11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 3, 116, 85, 73, 4, 
    105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 
    52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 
    6L), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
    "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
    "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
    "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", 
    "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", 
    "48", "49", "50"), c("time", "status", "start", "stop", "time1", 
    "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
    116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 
    179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
    350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
        NULL, c("time", "status")), type = "right", class = "Surv"), 
        `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1)), .Names = c("survival::Surv(time, status)", "(weights)"
    ), terms = quote(survival::Surv(time, status) ~ 1), row.names = c(NA, 
    50L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
        mml = structure(list(meanlog = structure(c(1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 1L), .Dimnames = list(
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
            "11", "12", "13", "14", "15", "16", "17", "18", "19", 
            "20", "21", "22", "23", "24", "25", "26", "27", "28", 
            "29", "30", "31", "32", "33", "34", "35", "36", "37", 
            "38", "39", "40", "41", "42", "43", "44", "45", "46", 
            "47", "48", "49", "50"), "(Intercept)"), assign = 0L), 
            sdlog = NULL), .Names = c("meanlog", "sdlog"))), .Names = c("Y", 
    "m", "mml")), datameans = numeric(0), N = 50L, events = 24L, 
    trisk = 11271, concat.formula = quote(survival::Surv(time, 
        status) ~ 1), all.formulae = structure(list(meanlog = quote(survival::Surv(time, 
        status) ~ 1)), .Names = "meanlog"), dfns = structure(list(
        p = function (q, meanlog = 0, sdlog = 1, lower.tail = TRUE, 
            log.p = FALSE) 
        .Call(C_plnorm, q, meanlog, sdlog, lower.tail, log.p), 
        d = function (x, meanlog = 0, sdlog = 1, log = FALSE) 
        .Call(C_dlnorm, x, meanlog, sdlog, log), h = function (x, 
            meanlog = 0, sdlog = 1, log = FALSE) 
        {
            h <- dbase("lnorm", log = log, x = x, meanlog = meanlog, 
                sdlog = sdlog)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- dlnorm(x, meanlog, sdlog)/plnorm(x, meanlog, 
                sdlog, lower.tail = FALSE)
            ret
        }, H = function (x, meanlog = 0, sdlog = 1, log = FALSE) 
        {
            h <- dbase("lnorm", log = log, x = x, meanlog = meanlog, 
                sdlog = sdlog)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- -plnorm(x, meanlog, sdlog, lower.tail = FALSE, 
                log.p = TRUE)
            ret
        }, r = function (n, meanlog = 0, sdlog = 1) 
        .Call(C_rlnorm, n, meanlog, sdlog), DLd = NULL, DLS = NULL, 
        deriv = FALSE), .Names = c("p", "d", "h", "H", "r", "DLd", 
    "DLS", "deriv")), res = structure(c(5.90279644147043, 2.28612762695883, 
    5.10509082987875, 1.66184749679068, 6.7005020530621, 3.14492126193134, 
    0.407000137698383, 0.372003493888733), .Dim = c(2L, 4L), .Dimnames = list(
        c("meanlog", "sdlog"), c("est", "L95%", "U95%", "se"))), 
    res.t = structure(c(5.90279644147043, 0.826859393629952, 
    5.10509082987875, 0.507929933372782, 6.7005020530621, 1.14578885388712, 
    0.407000137698383, 0.162722102432925), .Dim = c(2L, 4L), .Dimnames = list(
        c("meanlog", "sdlog"), c("est", "L95%", "U95%", "se"))), 
    cov = structure(c(0.165649112086503, 0.0310506043856805, 
    0.0310506043856805, 0.0264784826201912), .Dim = c(2L, 2L), .Dimnames = list(
        c("meanlog", "sdlog"), c("meanlog", "sdlog"))), coefficients = structure(c(5.90279644147043, 
    0.826859393629952), .Names = c("meanlog", "sdlog")), npars = 2L, 
    fixedpars = NULL, optpars = 1:2, loglik = -166.698907928287, 
    logliki = structure(c(-5.052457416814, -6.62573514259904, 
    -6.39241676263205, -6.28496018312538, -5.08361445903166, 
    -6.54896209724525, -7.38066320071601, -6.47375172413155, 
    -7.21243338225913, -6.40085825163339, -5.34830143007615, 
    -5.72812403905934, -5.59319192806734, -6.04926954403543, 
    -5.99903214307478, -6.98214806591534, -6.56335604344576, 
    -5.31891709404187, -6.06140075966774, -6.48925009490515, 
    -6.48153088942058, -6.26582443658311, -7.48813276602331, 
    -5.25667211285287, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022, 
    -0.677611690805022, -0.677611690805022, -0.677611690805022
    ), .Names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
    "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", 
    "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
    "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", 
    "50")), cl = 0.95, opt = structure(list(par = structure(c(5.90279644147043, 
    0.826859393629952), .Names = c("meanlog", "sdlog")), value = 166.698907928287, 
        counts = structure(c(7L, 1L), .Names = c("function", 
        "gradient")), convergence = 0L, message = NULL, hessian = structure(c(7.73772794815386, 
        -9.07382544568236, -9.07382544568236, 48.4071456270385
        ), .Dim = c(2L, 2L), .Dimnames = list(c("meanlog", "sdlog"
        ), c("meanlog", "sdlog")))), .Names = c("par", "value", 
    "counts", "convergence", "message", "hessian")), BIC = 341.22186186743, 
    m2LL = 333.397815856574), .Names = c("call", "dlist", "aux", 
"ncovs", "ncoveffs", "mx", "basepars", "covpars", "AIC", "data", 
"datameans", "N", "events", "trisk", "concat.formula", "all.formulae", 
"dfns", "res", "res.t", "cov", "coefficients", "npars", "fixedpars", 
"optpars", "loglik", "logliki", "cl", "opt", "BIC", "m2LL"), class = "flexsurvreg"), 
    structure(list(call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
        data = this_data, dist = conditions[i, "dist"])), dlist = structure(list(
        name = "gamma", pars = c("shape", "rate"), location = "rate", 
        transforms = list(.Primitive("log"), .Primitive("log")), 
        inv.transforms = list(.Primitive("exp"), .Primitive("exp")), 
        inits = function (t, mf, mml, aux) 
        {
            m = mean(t)
            v = var(t)
            c(m^2/v, m/v)
        }), .Names = c("name", "pars", "location", "transforms", 
    "inv.transforms", "inits")), aux = NULL, ncovs = 0L, ncoveffs = 0L, 
        mx = structure(list(rate = integer(0), shape = NULL), .Names = c("rate", 
        "shape")), basepars = 1:2, covpars = NULL, AIC = 340.953574828623, 
        data = structure(list(Y = structure(c(3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 116, 85, 73, 4, 105, 278, 
        95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 
        96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 3, 116, 85, 73, 4, 
        105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
            "11", "12", "13", "14", "15", "16", "17", "18", "19", 
            "20", "21", "22", "23", "24", "25", "26", "27", "28", 
            "29", "30", "31", "32", "33", "34", "35", "36", "37", 
            "38", "39", "40", "41", "42", "43", "44", "45", "46", 
            "47", "48", "49", "50"), c("time", "status", "start", 
            "stop", "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
        116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
        47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(50L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "(weights)"), terms = quote(survival::Surv(time, status) ~ 
            1), row.names = c(NA, 50L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49", "50"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 24L, trisk = 11271, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(rate = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "rate"), dfns = structure(list(
            p = function (q, shape, rate = 1, scale = 1/rate, 
                lower.tail = TRUE, log.p = FALSE) 
            {
                if (!missing(rate) && !missing(scale)) {
                  if (abs(rate * scale - 1) < 1e-15) 
                    warning("specify 'rate' or 'scale' but not both")
                  else stop("specify 'rate' or 'scale' but not both")
                }
                .Call(C_pgamma, q, shape, scale, lower.tail, 
                  log.p)
            }, d = function (x, shape, rate = 1, scale = 1/rate, 
                log = FALSE) 
            {
                if (!missing(rate) && !missing(scale)) {
                  if (abs(rate * scale - 1) < 1e-15) 
                    warning("specify 'rate' or 'scale' but not both")
                  else stop("specify 'rate' or 'scale' but not both")
                }
                .Call(C_dgamma, x, shape, scale, log)
            }, h = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gamma", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- dgamma(x, shape, rate)/pgamma(x, 
                  shape, rate, lower.tail = FALSE)
                ret
            }, H = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gamma", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- -pgamma(x, shape, rate, lower.tail = FALSE, 
                  log.p = TRUE)
                ret
            }, r = function (n, shape, rate = 1, scale = 1/rate) 
            {
                if (!missing(rate) && !missing(scale)) {
                  if (abs(rate * scale - 1) < 1e-15) 
                    warning("specify 'rate' or 'scale' but not both")
                  else stop("specify 'rate' or 'scale' but not both")
                }
                .Call(C_rgamma, n, shape, scale)
            }, DLd = NULL, DLS = NULL, deriv = FALSE), .Names = c("p", 
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(0.58495591850489, 
        0.00083665978864999, 0.374766094406023, 0.000295147948719606, 
        0.913031973012979, 0.00237169055377327, 0.132881269407198, 
        0.000444778553133094), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.536218787572553, -7.08609303527741, 
        -0.981453195874202, -8.12803380626071, -0.0909843792709042, 
        -6.04415226429411, 0.227164586601387, 0.53161220267413
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0516037494057792, 
        0.0997407438793042, 0.0997407438793042, 0.282611534032041
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(-0.536218787572553, 
        -7.08609303527741), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -168.476787414311, 
        logliki = structure(c(-5.02536286510366, -6.63688236762351, 
        -6.48189254965658, -6.40868631998312, -5.14560026641567, 
        -6.58632833412225, -7.13518261484712, -6.53642268910611, 
        -7.02162153800334, -6.48758358152587, -5.60826607299436, 
        -5.98871969621811, -5.868205748735, -6.24143096502236, 
        -6.20418434250711, -6.8696362367825, -6.59583290643687, 
        -5.57131585616342, -6.25032698641996, -6.54674307314579, 
        -6.54160539993024, -6.39548325673666, -7.20934862894593, 
        -5.48635535210505, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492, 
        -0.678221914068492, -0.678221914068492, -0.678221914068492
        ), .Names = c("1", "2", "3", "4", "5", "6", "7", "8", 
        "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50")), cl = 0.95, opt = structure(list(
            par = structure(c(-0.536218787572553, -7.08609303527741
            ), .Names = c("shape", "rate")), value = 168.476787414311, 
            counts = structure(c(32L, 16L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(60.9654861705167, 
            -21.5162589256579, -21.5162589256579, 11.1320568763063
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "rate"), c("shape", "rate")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 344.777620839479, m2LL = 336.953574828623), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gompertz", pars = c("shape", 
        "rate"), location = "rate", transforms = list(function (x) 
        x, .Primitive("log")), inv.transforms = list(function (x) 
        x, .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            c(0.001, 1/mean(t))
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(rate = integer(0), 
            shape = NULL), .Names = c("rate", "shape")), basepars = 1:2, 
        covpars = NULL, AIC = 336.175785244992, data = structure(list(
            Y = structure(c(3, 116, 85, 73, 4, 105, 278, 95, 
            232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 
            96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 116, 85, 
            73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 
            179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
            86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 
            71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 
            6L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49", "50"
            ), c("time", "status", "start", "stop", "time1", 
            "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
            116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 
            51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = c(NA, 50L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49", "50"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 24L, trisk = 11271, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(rate = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "rate"), dfns = structure(list(
            p = function (q, shape, rate = 1, lower.tail = TRUE, 
                log.p = FALSE) 
            {
                d <- dbase("gompertz", lower.tail = lower.tail, 
                  log = log.p, q = q, shape = shape, rate = rate)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                prob <- numeric(length(q))
                prob[shape == 0] <- pexp(q[shape == 0], rate = rate[shape == 
                  0])
                sn0 <- shape != 0
                if (any(sn0)) {
                  q <- q[sn0]
                  shape <- shape[sn0]
                  rate <- rate[sn0]
                  prob[sn0] <- 1 - exp(-rate/shape * (exp(shape * 
                    q) - 1))
                }
                prob[q == Inf] <- 1
                if (!lower.tail) 
                  prob <- 1 - prob
                if (log.p) 
                  prob <- log(prob)
                ret[ind] <- prob
                ret
            }, d = function (x, shape, rate = 1, log = FALSE) 
            {
                d <- dbase("gompertz", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                logdens <- numeric(length(x))
                logdens[shape == 0] <- dexp(x[shape == 0], rate = rate[shape == 
                  0], log = TRUE)
                sn0 <- shape != 0
                if (any(sn0)) {
                  x <- x[sn0]
                  shape <- shape[sn0]
                  rate <- rate[sn0]
                  logdens[sn0] <- log(rate) + shape * x - rate/shape * 
                    (exp(shape * x) - 1)
                }
                ret[ind] <- if (log) 
                  logdens
                else exp(logdens)
                ret
            }, h = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gompertz", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                if (log) 
                  ret[ind] <- log(rate) + (shape * x)
                else ret[ind] <- rate * exp(shape * x)
                ret
            }, H = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gompertz", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- ifelse(shape == 0, rate * x, rate/shape * 
                  expm1(shape * x))
                if (log) 
                  ret[ind] <- log(ret[ind])
                ret
            }, r = function (n, shape = 1, rate = 1) 
            {
                r <- rbase("gompertz", n = n, shape = shape, 
                  rate = rate)
                for (i in seq_along(r)) assign(names(r)[i], r[[i]])
                ret[ind] <- qgompertz(p = runif(sum(ind)), shape = shape, 
                  rate = rate)
                ret
            }, DLd = function (t, shape, rate) 
            {
                res <- matrix(nrow = length(t), ncol = 2)
                rs <- rate/shape * exp(shape * t)
                res[shape == 0, 1] <- 0
                res[shape == 0, 2] <- 1 - rate[shape == 0] * 
                  t[shape == 0]
                sn0 <- (shape != 0)
                t <- t[sn0]
                rs <- rs[sn0]
                rate <- rate[sn0]
                shape <- shape[sn0]
                res[shape != 0, 1] <- t + rs * (1/shape - t) - 
                  rate/shape^2
                res[shape != 0, 2] <- 1 - rs + rate/shape
                res
            }, DLS = function (t, shape, rate) 
            {
                res <- matrix(nrow = length(t), ncol = 2)
                rs <- rate/shape * exp(shape * t)
                res[shape == 0, 1] <- 0
                res[shape == 0, 2] <- -rate[shape == 0] * t[shape == 
                  0]
                sn0 <- (shape != 0)
                t <- t[sn0]
                rs <- rs[sn0]
                rate <- rate[sn0]
                shape <- shape[sn0]
                res[shape != 0, 1] <- rs * (1/shape - t) - rate/shape^2
                res[shape != 0, 2] <- -rs + rate/shape
                res
            }, deriv = TRUE), .Names = c("p", "d", "h", "H", 
        "r", "DLd", "DLS", "deriv")), res = structure(c(-0.00753366154609757, 
        0.0053063747775873, -0.0124215719239411, 0.00292023747468172, 
        -0.00264575116825404, 0.00964223407320104, 0.00249387765101744, 
        0.00161696859565345), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.00753366154609757, -5.2388463929703, 
        -0.0124215719239411, -5.83609033906301, -0.00264575116825404, 
        -4.64160244687758, 0.00249387765101744, 0.304721898363284
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(6.21942573824426e-06, 
        -0.000564238237206288, -0.000564238237206288, 0.0928554353421237
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(-0.00753366154609757, 
        -5.2388463929703), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -166.087892622496, 
        logliki = c(-5.27718795563611, -6.52316572013805, -6.21229586971357, 
        -6.08676399250976, -5.28988991522396, -6.41489870649912, 
        -7.9508187862985, -6.31457487597014, -7.56834420016657, 
        -6.22261602251182, -5.39013333700689, -5.59555751207336, 
        -5.51216593832854, -5.84776388649883, -5.80295502830053, 
        -7.10886043803104, -6.43474152348592, -5.37773366622168, 
        -5.85889752585374, -6.33479136123989, -6.32469281657042, 
        -6.06552702074797, -8.2185227190348, -5.35282365448042, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082, -0.653929621152082, 
        -0.653929621152082, -0.653929621152082), cl = 0.95, opt = structure(list(
            par = structure(c(-0.00753366154609757, -5.2388463929703
            ), .Names = c("shape", "rate")), value = 166.087892622496, 
            counts = structure(c(57L, 14L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(358317.779015056, 
            2177.32641332424, 2177.32641332424, 24.0000039746278
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "rate"), c("shape", "rate")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 339.999831255848, m2LL = 332.175785244992), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gengamma", pars = c("mu", 
        "sigma", "Q"), location = "mu", transforms = list(function (x) 
        x, .Primitive("log"), function (x) 
        x), inv.transforms = list(function (x) 
        x, .Primitive("exp"), function (x) 
        x), inits = function (t, mf, mml, aux) 
        {
            lt <- log(t[t > 0])
            c(mean(lt), sd(lt), 0)
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(mu = integer(0), sigma = NULL, 
            Q = NULL), .Names = c("mu", "sigma", "Q")), basepars = 1:3, 
        covpars = NULL, AIC = 338.601162245681, data = structure(list(
            Y = structure(c(3, 116, 85, 73, 4, 105, 278, 95, 
            232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 
            96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 116, 85, 
            73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 
            179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
            86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 
            71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 
            6L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49", "50"
            ), c("time", "status", "start", "stop", "time1", 
            "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
            116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 
            51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = c(NA, 50L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(mu = structure(c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49", "50"
            ), "(Intercept)"), assign = 0L), sigma = NULL, Q = NULL), .Names = c("mu", 
            "sigma", "Q"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 24L, trisk = 11271, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(mu = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "mu"), dfns = structure(list(
            p = function (q, mu = 0, sigma = 1, Q, lower.tail = TRUE, 
                log.p = FALSE) 
            {
                d <- dbase("gengamma", lower.tail = lower.tail, 
                  log = log.p, q = q, mu = mu, sigma = sigma, 
                  Q = Q)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                prob <- numeric(length(q))
                prob[Q == 0] <- plnorm(q[Q == 0], mu[Q == 0], 
                  sigma[Q == 0])
                qn0 <- Q != 0
                if (any(qn0)) {
                  q <- q[qn0]
                  mu <- mu[qn0]
                  sigma <- sigma[qn0]
                  Q <- Q[qn0]
                  y <- log(q)
                  w <- ((y - mu)/sigma)
                  expnu <- exp(Q * w) * Q^-2
                  prob[qn0] <- ifelse(Q > 0, pgamma(expnu, Q^-2), 
                    1 - pgamma(expnu, Q^-2))
                }
                if (!lower.tail) 
                  prob <- 1 - prob
                if (log.p) 
                  prob <- log(prob)
                ret[ind] <- prob
                ret
            }, d = function (x, mu = 0, sigma = 1, Q, log = FALSE) 
            {
                d <- dbase("gengamma", log = log, x = x, mu = mu, 
                  sigma = sigma, Q = Q)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                logdens <- numeric(length(x))
                logdens[Q == 0] <- dlnorm(x[Q == 0], mu[Q == 
                  0], sigma[Q == 0], log = TRUE)
                qn0 <- Q != 0
                if (any(qn0)) {
                  x <- x[qn0]
                  mu <- mu[qn0]
                  sigma <- sigma[qn0]
                  Q <- Q[qn0]
                  y <- log(x)
                  w <- ((y - mu)/sigma)
                  logdens[qn0] <- -log(sigma * x) + log(abs(Q)) + 
                    (Q^-2) * log(Q^-2) + Q^-2 * (Q * w - exp(Q * 
                    w)) - lgamma(Q^-2)
                }
                ret[ind] <- if (log) 
                  logdens
                else exp(logdens)
                ret
            }, h = function (x, mu = 0, sigma = 1, Q) 
            {
                dgengamma(x = x, mu = mu, sigma = sigma, Q = Q)/pgengamma(q = x, 
                  mu = mu, sigma = sigma, Q = Q, lower.tail = FALSE)
            }, H = function (x, mu = 0, sigma = 1, Q) 
            {
                -log(pgengamma(q = x, mu = mu, sigma = sigma, 
                  Q = Q, lower.tail = FALSE))
            }, r = function (n, mu = 0, sigma = 1, Q) 
            {
                r <- rbase("gengamma", n = n, mu = mu, sigma = sigma, 
                  Q = Q)
                for (i in seq_along(r)) assign(names(r)[i], r[[i]])
                ret[ind][Q == 0] <- rlnorm(n, mu, sigma)
                qn0 <- Q != 0
                if (any(qn0)) {
                  mu <- mu[qn0]
                  sigma <- sigma[qn0]
                  Q <- Q[qn0]
                  w <- log(Q^2 * rgamma(n, 1/Q^2, 1))/Q
                  ret[ind][qn0] <- exp(mu + sigma * w)
                }
                ret
            }, DLd = NULL, DLS = NULL, deriv = FALSE), .Names = c("p", 
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(5.09466396241894, 
        2.58529675330872, -0.956574964960567, 2.95758482306936, 
        1.89976844026815, -3.02874844045996, 7.23174310176851, 
        3.51819682914894, 1.11559851053882, 1.09036653540911, 
        0.406411229668328, 1.05725079228212), .Dim = 3:4, .Dimnames = list(
            c("mu", "sigma", "Q"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(5.09466396241894, 0.949840299556778, 
        -0.956574964960567, 2.95758482306936, 0.64173200520213, 
        -3.02874844045996, 7.23174310176851, 1.25794859391143, 
        1.11559851053882, 1.09036653540911, 0.157200997969844, 
        1.05725079228212), .Dim = 3:4, .Dimnames = list(c("mu", 
        "sigma", "Q"), c("est", "L95%", "U95%", "se"))), cov = structure(c(1.18889918154007, 
        3.05803908055489e-05, 1.06038639761258, 3.05803908054539e-05, 
        0.0247121537627148, -0.0346369671704767, 1.06038639761258, 
        -0.0346369671704766, 1.11777923778117), .Dim = c(3L, 
        3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
        "sigma", "Q"))), coefficients = structure(c(5.09466396241894, 
        0.949840299556778, -0.956574964960567), .Names = c("mu", 
        "sigma", "Q")), npars = 3L, fixedpars = NULL, optpars = 1:3, 
        loglik = -166.30058112284, logliki = c(-5.12710245509206, 
        -6.70591634638883, -6.42041887967494, -6.28726511928401, 
        -5.04704479249932, -6.61255825962581, -7.5907850659138, 
        -6.52053550329669, -7.39886608106712, -6.43083755942852, 
        -5.15031489903687, -5.58999219149788, -5.42489457617611, 
        -5.9924390413203, -5.92929352193863, -7.13126735157711, 
        -6.63010671896232, -5.12203165022943, -6.00767877985716, 
        -6.53954296982477, -6.53007885850018, -6.26345657906875, 
        -7.71183688603027, -5.06868518839892, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025, -0.656447378775025, -0.656447378775025, 
        -0.656447378775025), cl = 0.95, opt = structure(list(
            par = structure(c(5.09466396241894, 0.949840299556778, 
            -0.956574964960567), .Names = c("mu", "sigma", "Q"
            )), value = 166.30058112284, counts = structure(c(29L, 
            13L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(7.28864841192944, 
            -10.1408104917766, -7.22864612257013, -10.1408104917766, 
            56.4123150539331, 11.368192019745, -7.22864612257013, 
            11.368192019745, 8.10438896081678), .Dim = c(3L, 
            3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
            "sigma", "Q")))), .Names = c("par", "value", "counts", 
        "convergence", "message", "hessian")), BIC = 344.337231261965, 
        m2LL = 332.601162245681), .Names = c("call", "dlist", 
    "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "exp", pars = "rate", location = "rate", 
            transforms = list(.Primitive("log")), inv.transforms = list(
                .Primitive("exp")), inits = function (t, mf, 
                mml, aux) 
            {
                if (aux$counting) {
                  1/mean(t)
                }
                else {
                  aux$formula <- aux$forms[[1]]
                  aux$forms <- NULL
                  aux$dist <- "exponential"
                  sr <- do.call(survreg, aux)
                  sr2fsexp(sr)
                }
            }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(rate = integer(0)), .Names = "rate"), 
        basepars = 1L, covpars = NULL, AIC = 473.789686788846, 
        data = structure(list(Y = structure(c(126, 119, 70, 10, 
        24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
        350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
        33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 126, 119, 70, 10, 24, 59, 106, 15, 
        52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 
        50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 
        24, 104, 21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 
        6L), .Dimnames = list(c("51", "52", "53", "54", "55", 
        "56", "57", "58", "59", "60", "61", "62", "63", "64", 
        "65", "66", "67", "68", "69", "70", "71", "72", "73", 
        "74", "75", "76", "77", "78", "79", "80", "81", "82", 
        "83", "84", "85", "86", "87", "88", "89", "90", "91", 
        "92", "93", "94", "95", "96", "97", "98", "99", "100"
        ), c("time", "status", "start", "stop", "time1", "time2"
        ))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
        119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 
        82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
        31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(50L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "(weights)"), terms = quote(survival::Surv(time, status) ~ 
            1), row.names = 51:100, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("51", "52", "53", "54", "55", 
            "56", "57", "58", "59", "60", "61", "62", "63", "64", 
            "65", "66", "67", "68", "69", "70", "71", "72", "73", 
            "74", "75", "76", "77", "78", "79", "80", "81", "82", 
            "83", "84", "85", "86", "87", "88", "89", "90", "91", 
            "92", "93", "94", "95", "96", "97", "98", "99", "100"
            ), "(Intercept)"), assign = 0L)), .Names = "rate")), .Names = c("Y", 
        "m", "mml")), datameans = numeric(0), N = 50L, events = 38L, 
        trisk = 6942, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(rate = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "rate"), dfns = structure(list(
            p = function (q, rate = 1, lower.tail = TRUE, log.p = FALSE) 
            .Call(C_pexp, q, 1/rate, lower.tail, log.p), d = function (x, 
                rate = 1, log = FALSE) 
            .Call(C_dexp, x, 1/rate, log), h = function (x, rate = 1, 
                log = FALSE) 
            {
                h <- dbase("exp", log = log, x = x, rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- if (log) 
                  log(rate)
                else rate
                ret
            }, H = function (x, rate = 1, log = FALSE) 
            {
                h <- dbase("exp", log = log, x = x, rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- if (log) {
                  log(rate) + log(x)
                }
                else rate * x
                ret
            }, r = function (n, rate = 1) 
            .Call(C_rexp, n, 1/rate), DLd = function (t, rate) 
            {
                res <- matrix(nrow = length(t), ncol = 1)
                ts <- 1 - t * rate
                res[, 1] <- ts
                res
            }, DLS = function (t, rate) 
            {
                res <- matrix(nrow = length(t), ncol = 1)
                res[, 1] <- -t * rate
                res
            }, deriv = TRUE), .Names = c("p", "d", "h", "H", 
        "r", "DLd", "DLS", "deriv")), res = structure(c(0.00547392682224137, 
        0.00398305106894237, 0.00752284475810398, 0.00088798811427082
        ), .Dim = c(1L, 4L), .Dimnames = list("rate", c("est", 
        "L95%", "U95%", "se"))), res.t = structure(c(-5.20775903669536, 
        -5.52570715313689, -4.88981092025382, 0.162221407612318
        ), .Dim = c(1L, 4L), .Dimnames = list("rate", c("est", 
        "L95%", "U95%", "se"))), cov = structure(0.0263157850877219, .Dim = c(1L, 
        1L), .Dimnames = list("rate", "rate")), coefficients = -5.20775903669536, 
        npars = 1L, fixedpars = NULL, optpars = 1L, loglik = -235.894843394423, 
        logliki = structure(c(-5.89747381629777, -5.85915632854208, 
        -5.59093391425225, -5.26249830491777, -5.33913328042915, 
        -5.5307207192076, -5.78799527985294, -5.28986793902898, 
        -5.49240323145191, -5.24607652445105, -5.47050752416294, 
        -5.48145537780742, -5.63472532883018, -5.4650335973407, 
        -5.65662103611915, -5.79346920667518, -6.34633581572156, 
        -7.12363342447984, -5.87010418218656, -5.27892008538449, 
        -5.41029432911829, -5.48145537780742, -5.27344615856225, 
        -6.0069523527426, -5.36102898771811, -5.77157349938622, 
        -5.37745076818484, -5.70588637751932, -6.42844471805518, 
        -5.38839862182932, -5.49787715827415, -5.27344615856225, 
        -5.25702437809553, -5.45955967051846, -5.33913328042915, 
        -5.77704742620846, -5.32271149996243, -5.35555506089587, 
        -1.91587438778448, -1.91587438778448, -1.91587438778448, 
        -1.91587438778448, -1.91587438778448, -1.91587438778448, 
        -1.91587438778448, -1.91587438778448, -1.91587438778448, 
        -1.91587438778448, -1.91587438778448, -1.91587438778448
        ), .Names = c("51", "52", "53", "54", "55", "56", "57", 
        "58", "59", "60", "61", "62", "63", "64", "65", "66", 
        "67", "68", "69", "70", "71", "72", "73", "74", "75", 
        "76", "77", "78", "79", "80", "81", "82", "83", "84", 
        "85", "86", "87", "88", "89", "90", "91", "92", "93", 
        "94", "95", "96", "97", "98", "99", "100")), cl = 0.95, 
        opt = structure(list(par = structure(-5.20775903669536, .Names = "rate"), 
            value = 235.894843394423, counts = structure(c(2L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(38.0000063333306, .Dim = c(1L, 
            1L), .Dimnames = list("rate", "rate"))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 475.701709794274, m2LL = 471.789686788846), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "weibull.quiet", pars = c("shape", 
        "scale"), location = "scale", transforms = list(.Primitive("log"), 
            .Primitive("log")), inv.transforms = list(.Primitive("exp"), 
            .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            if (aux$counting) {
                lt <- log(t[t > 0])
                c(1.64/var(lt), exp(mean(lt) + 0.572))
            }
            else {
                aux$formula <- aux$forms[[1]]
                aux$forms <- NULL
                aux$dist <- "weibull"
                sr <- do.call(survreg, aux)
                sr2fswei(sr)
            }
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(scale = integer(0), 
            shape = NULL), .Names = c("scale", "shape")), basepars = 1:2, 
        covpars = NULL, AIC = 470.392267796711, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 
            59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
            350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
            33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("51", "52", "53", "54", "55", "56", "57", "58", 
                "59", "60", "61", "62", "63", "64", "65", "66", 
                "67", "68", "69", "70", "71", "72", "73", "74", 
                "75", "76", "77", "78", "79", "80", "81", "82", 
                "83", "84", "85", "86", "87", "88", "89", "90", 
                "91", "92", "93", "94", "95", "96", "97", "98", 
                "99", "100"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 51:100, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(scale = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("51", "52", "53", "54", "55", 
            "56", "57", "58", "59", "60", "61", "62", "63", "64", 
            "65", "66", "67", "68", "69", "70", "71", "72", "73", 
            "74", "75", "76", "77", "78", "79", "80", "81", "82", 
            "83", "84", "85", "86", "87", "88", "89", "90", "91", 
            "92", "93", "94", "95", "96", "97", "98", "99", "100"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("scale", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 38L, trisk = 6942, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(scale = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "scale"), dfns = structure(list(
            p = function (q, shape, scale = 1, lower.tail = TRUE, 
                log.p = FALSE) 
            {
                ret <- suppressWarnings(pweibull(q = q, shape = shape, 
                  scale = scale, lower.tail = lower.tail, log.p = log.p))
                ret
            }, d = function (x, shape, scale = 1, log = FALSE) 
            {
                ret <- suppressWarnings(dweibull(x = x, shape = shape, 
                  scale = scale, log = log))
                ret
            }, h = function (x, ...) 
            {
                d(x, ...)/(1 - p(x, ...))
            }, H = function (x, ...) 
            {
                -log(1 - p(x, ...))
            }, r = function (n, shape, scale = 1) 
            .Call(C_rweibull, n, shape, scale), DLd = function (t, 
                shape, scale) 
            {
                res <- matrix(nrow = length(t), ncol = 2)
                tss <- (t/scale)^shape
                res[, 1] <- 1 + shape * (log(t/scale) - log(t/scale) * 
                  tss)
                res[, 2] <- -1 - (shape - 1) + shape * tss
                res
            }, DLS = function (t, shape, scale) 
            {
                res <- matrix(nrow = length(t), ncol = 2)
                tss <- (t/scale)^shape
                res[, 1] <- ifelse(t == 0, 0, -shape * log(t/scale) * 
                  tss)
                res[, 2] <- tss * shape
                res
            }, deriv = TRUE), .Names = c("p", "d", "h", "H", 
        "r", "DLd", "DLS", "deriv")), res = structure(c(0.74662905592844, 
        177.307401708128, 0.57367107713219, 115.81499766854, 
        0.971732704293441, 271.449426528177, 0.100382421371819, 
        38.5280584156681), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "scale"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.292186795594259, 5.17788495901154, 
        -0.555699083265723, 4.75199407029799, -0.0286745079227959, 
        5.60377584772509, 0.134447515234981, 0.217295262603253
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "scale"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0180761343528605, 
        0.000429618427513486, 0.000429618427513486, 0.0472172311498167
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "scale"
        ), c("shape", "scale"))), coefficients = structure(c(-0.292186795594259, 
        5.17788495901154), .Names = c("shape", "scale")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -233.196133898356, 
        logliki = structure(c(-6.15839662770313, -6.11154127781632, 
        -5.73421045660127, -4.85841387102798, -5.18804053996657, 
        -5.63102455287664, -6.02078928064327, -5.00246284431215, 
        -5.55945444127318, -4.74072177063478, -5.5159590230257, 
        -5.53796842828281, -5.80367111031718, -5.50474551520875, 
        -5.83694986497806, -6.02795984154543, -6.63712030009122, 
        -7.3039143121188, -6.12506175375469, -4.95017678082348, 
        -5.38343242816031, -5.53796842828281, -4.9216501246857, 
        -6.28582298042876, -5.25450242743177, -5.99907120080152, 
        -5.30019384149233, -5.90879931127211, -6.71488518082644, 
        -5.32903132817084, -5.57001266856638, -4.9216501246857, 
        -4.82287807316933, -5.49338549981163, -5.18804053996657, 
        -6.00634553666814, -5.13288890609761, -5.23853545429079, 
        -1.66153810421212, -1.66153810421212, -1.66153810421212, 
        -1.66153810421212, -1.66153810421212, -1.66153810421212, 
        -1.66153810421212, -1.66153810421212, -1.66153810421212, 
        -1.66153810421212, -1.66153810421212, -1.66153810421212
        ), .Names = c("51", "52", "53", "54", "55", "56", "57", 
        "58", "59", "60", "61", "62", "63", "64", "65", "66", 
        "67", "68", "69", "70", "71", "72", "73", "74", "75", 
        "76", "77", "78", "79", "80", "81", "82", "83", "84", 
        "85", "86", "87", "88", "89", "90", "91", "92", "93", 
        "94", "95", "96", "97", "98", "99", "100")), cl = 0.95, 
        opt = structure(list(par = structure(c(-0.292186795594259, 
        5.17788495901154), .Names = c("shape", "scale")), value = 233.196133898356, 
            counts = structure(c(2L, 1L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(55.3335286763863, 
            -0.503466700605415, -0.503466700605415, 21.1832899603666
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "scale"), c("shape", "scale")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 474.216313807568, m2LL = 466.392267796711), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "lnorm", pars = c("meanlog", 
        "sdlog"), location = "meanlog", transforms = list(function (x) 
        x, .Primitive("log")), inv.transforms = list(function (x) 
        x, .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            if (aux$counting) {
                lt <- log(t[t > 0])
                c(mean(lt), sd(lt))
            }
            else {
                aux$formula <- aux$forms[[1]]
                aux$forms <- NULL
                aux$dist <- "lognormal"
                sr <- do.call(survreg, aux)
                sr2fsln(sr)
            }
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(meanlog = integer(0), 
            sdlog = NULL), .Names = c("meanlog", "sdlog")), basepars = 1:2, 
        covpars = NULL, AIC = 461.129803068957, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 
            59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
            350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
            33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("51", "52", "53", "54", "55", "56", "57", "58", 
                "59", "60", "61", "62", "63", "64", "65", "66", 
                "67", "68", "69", "70", "71", "72", "73", "74", 
                "75", "76", "77", "78", "79", "80", "81", "82", 
                "83", "84", "85", "86", "87", "88", "89", "90", 
                "91", "92", "93", "94", "95", "96", "97", "98", 
                "99", "100"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 51:100, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(meanlog = structure(c(1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("51", "52", "53", "54", "55", 
            "56", "57", "58", "59", "60", "61", "62", "63", "64", 
            "65", "66", "67", "68", "69", "70", "71", "72", "73", 
            "74", "75", "76", "77", "78", "79", "80", "81", "82", 
            "83", "84", "85", "86", "87", "88", "89", "90", "91", 
            "92", "93", "94", "95", "96", "97", "98", "99", "100"
            ), "(Intercept)"), assign = 0L), sdlog = NULL), .Names = c("meanlog", 
            "sdlog"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 38L, trisk = 6942, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(meanlog = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "meanlog"), dfns = structure(list(
            p = function (q, meanlog = 0, sdlog = 1, lower.tail = TRUE, 
                log.p = FALSE) 
            .Call(C_plnorm, q, meanlog, sdlog, lower.tail, log.p), 
            d = function (x, meanlog = 0, sdlog = 1, log = FALSE) 
            .Call(C_dlnorm, x, meanlog, sdlog, log), h = function (x, 
                meanlog = 0, sdlog = 1, log = FALSE) 
            {
                h <- dbase("lnorm", log = log, x = x, meanlog = meanlog, 
                  sdlog = sdlog)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- dlnorm(x, meanlog, sdlog)/plnorm(x, 
                  meanlog, sdlog, lower.tail = FALSE)
                ret
            }, H = function (x, meanlog = 0, sdlog = 1, log = FALSE) 
            {
                h <- dbase("lnorm", log = log, x = x, meanlog = meanlog, 
                  sdlog = sdlog)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- -plnorm(x, meanlog, sdlog, lower.tail = FALSE, 
                  log.p = TRUE)
                ret
            }, r = function (n, meanlog = 0, sdlog = 1) 
            .Call(C_rlnorm, n, meanlog, sdlog), DLd = NULL, DLS = NULL, 
            deriv = FALSE), .Names = c("p", "d", "h", "H", "r", 
        "DLd", "DLS", "deriv")), res = structure(c(4.5397796402642, 
        1.50266947580279, 4.10685635613176, 1.18179025897402, 
        4.97270292439665, 1.91067368880645, 0.22088328538039, 
        0.18416681340016), .Dim = c(2L, 4L), .Dimnames = list(
            c("meanlog", "sdlog"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(4.5397796402642, 0.407243176941648, 
        4.10685635613176, 0.16703045736206, 4.97270292439665, 
        0.647455896521237, 0.22088328538039, 0.122559762053974
        ), .Dim = c(2L, 4L), .Dimnames = list(c("meanlog", "sdlog"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0487894257604348, 
        0.00421592643022102, 0.00421592643022102, 0.0150208952747268
        ), .Dim = c(2L, 2L), .Dimnames = list(c("meanlog", "sdlog"
        ), c("meanlog", "sdlog"))), coefficients = structure(c(4.5397796402642, 
        0.407243176941648), .Names = c("meanlog", "sdlog")), 
        npars = 2L, fixedpars = NULL, optpars = 1:2, loglik = -228.564901534479, 
        logliki = structure(c(-6.18193062077146, -6.11799012028749, 
        -5.59346482115245, -4.73704956818963, -4.9148388196679, 
        -5.45103234632882, -5.99300688822539, -4.77719241324286, 
        -5.35412430942692, -4.76193078590152, -5.29636285937461, 
        -5.32546682321766, -5.69031185922911, -5.28164132971675, 
        -5.73682145037569, -6.00293037767783, -6.80464409098985, 
        -7.56886170225632, -6.13648534967355, -4.75471117887262, 
        -5.1281488181108, -5.32546682321766, -4.74609144271335, 
        -6.35340680674772, -4.98128871152515, -5.96290700408454, 
        -5.03093253030403, -5.83722641780908, -6.89995311111104, 
        -5.0637010371667, -5.36828806943413, -4.74609144271335, 
        -4.73853614217303, -5.2668058609113, -4.9148388196679, 
        -5.97299586883231, -4.86578357685667, -4.96466316582772, 
        -1.65974818089115, -1.65974818089115, -1.65974818089115, 
        -1.65974818089115, -1.65974818089115, -1.65974818089115, 
        -1.65974818089115, -1.65974818089115, -1.65974818089115, 
        -1.65974818089115, -1.65974818089115, -1.65974818089115
        ), .Names = c("51", "52", "53", "54", "55", "56", "57", 
        "58", "59", "60", "61", "62", "63", "64", "65", "66", 
        "67", "68", "69", "70", "71", "72", "73", "74", "75", 
        "76", "77", "78", "79", "80", "81", "82", "83", "84", 
        "85", "86", "87", "88", "89", "90", "91", "92", "93", 
        "94", "95", "96", "97", "98", "99", "100")), cl = 0.95, 
        opt = structure(list(par = structure(c(4.5397796402642, 
        0.407243176941648), .Names = c("meanlog", "sdlog")), 
            value = 228.564901534479, counts = structure(c(7L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(21.0056945135761, 
            -5.89568471554003, -5.89568471554003, 68.2286744080329
            ), .Dim = c(2L, 2L), .Dimnames = list(c("meanlog", 
            "sdlog"), c("meanlog", "sdlog")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 464.953849079814, m2LL = 457.129803068957), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gamma", pars = c("shape", 
        "rate"), location = "rate", transforms = list(.Primitive("log"), 
            .Primitive("log")), inv.transforms = list(.Primitive("exp"), 
            .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            m = mean(t)
            v = var(t)
            c(m^2/v, m/v)
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(rate = integer(0), 
            shape = NULL), .Names = c("rate", "shape")), basepars = 1:2, 
        covpars = NULL, AIC = 472.614745591727, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 
            59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
            350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
            33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("51", "52", "53", "54", "55", "56", "57", "58", 
                "59", "60", "61", "62", "63", "64", "65", "66", 
                "67", "68", "69", "70", "71", "72", "73", "74", 
                "75", "76", "77", "78", "79", "80", "81", "82", 
                "83", "84", "85", "86", "87", "88", "89", "90", 
                "91", "92", "93", "94", "95", "96", "97", "98", 
                "99", "100"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 51:100, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("51", "52", "53", "54", "55", 
            "56", "57", "58", "59", "60", "61", "62", "63", "64", 
            "65", "66", "67", "68", "69", "70", "71", "72", "73", 
            "74", "75", "76", "77", "78", "79", "80", "81", "82", 
            "83", "84", "85", "86", "87", "88", "89", "90", "91", 
            "92", "93", "94", "95", "96", "97", "98", "99", "100"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 38L, trisk = 6942, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(rate = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "rate"), dfns = structure(list(
            p = function (q, shape, rate = 1, scale = 1/rate, 
                lower.tail = TRUE, log.p = FALSE) 
            {
                if (!missing(rate) && !missing(scale)) {
                  if (abs(rate * scale - 1) < 1e-15) 
                    warning("specify 'rate' or 'scale' but not both")
                  else stop("specify 'rate' or 'scale' but not both")
                }
                .Call(C_pgamma, q, shape, scale, lower.tail, 
                  log.p)
            }, d = function (x, shape, rate = 1, scale = 1/rate, 
                log = FALSE) 
            {
                if (!missing(rate) && !missing(scale)) {
                  if (abs(rate * scale - 1) < 1e-15) 
                    warning("specify 'rate' or 'scale' but not both")
                  else stop("specify 'rate' or 'scale' but not both")
                }
                .Call(C_dgamma, x, shape, scale, log)
            }, h = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gamma", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- dgamma(x, shape, rate)/pgamma(x, 
                  shape, rate, lower.tail = FALSE)
                ret
            }, H = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gamma", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- -pgamma(x, shape, rate, lower.tail = FALSE, 
                  log.p = TRUE)
                ret
            }, r = function (n, shape, rate = 1, scale = 1/rate) 
            {
                if (!missing(rate) && !missing(scale)) {
                  if (abs(rate * scale - 1) < 1e-15) 
                    warning("specify 'rate' or 'scale' but not both")
                  else stop("specify 'rate' or 'scale' but not both")
                }
                .Call(C_rgamma, n, shape, scale)
            }, DLd = NULL, DLS = NULL, deriv = FALSE), .Names = c("p", 
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(0.717499421490668, 
        0.00360463165652532, 0.490287690767893, 0.00192596605513978, 
        1.05000682157276, 0.0067464166071616, 0.139395053841227, 
        0.001152752778432), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.331983137633469, -5.62553568918665, 
        -0.712762936151229, -6.25232759029954, 0.0487966608842906, 
        -4.99874378807376, 0.194278977328819, 0.319797662639181
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.037744321031932, 
        0.0488450491743155, 0.0488450491743155, 0.102270545029483
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(-0.331983137633469, 
        -5.62553568918665), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -234.307372795864, 
        logliki = structure(c(-6.0967004548866, -6.05532074831446, 
        -5.72879100924626, -4.96279236701944, -5.2605776349801, 
        -5.64084438414822, -5.97577962713778, -5.09535965290797, 
        -5.57993391208633, -4.85121759404739, -5.54290327423701, 
        -5.56164477461793, -5.7881984627619, -5.5333510423025, 
        -5.81674496213184, -5.98203686587519, -6.53388540950488, 
        -7.19275501441084, -6.06723846359663, -5.04772431848125, 
        -5.42972220017954, -5.56164477461793, -5.0215075756015, 
        -6.21041240516464, -5.31854381783501, -5.95685511064735, 
        -5.35811138282938, -5.87860633052554, -6.60762644255836, 
        -5.38298268315827, -5.58891966984164, -5.0215075756015, 
        -4.92942332873753, -5.52367089522951, -5.2605776349801, 
        -5.96318923772595, -5.21204104434494, -5.30466530566119, 
        -1.70529711732742, -1.70529711732742, -1.70529711732742, 
        -1.70529711732742, -1.70529711732742, -1.70529711732742, 
        -1.70529711732742, -1.70529711732742, -1.70529711732742, 
        -1.70529711732742, -1.70529711732742, -1.70529711732742
        ), .Names = c("51", "52", "53", "54", "55", "56", "57", 
        "58", "59", "60", "61", "62", "63", "64", "65", "66", 
        "67", "68", "69", "70", "71", "72", "73", "74", "75", 
        "76", "77", "78", "79", "80", "81", "82", "83", "84", 
        "85", "86", "87", "88", "89", "90", "91", "92", "93", 
        "94", "95", "96", "97", "98", "99", "100")), cl = 0.95, 
        opt = structure(list(par = structure(c(-0.331983137633469, 
        -5.62553568918665), .Names = c("shape", "rate")), value = 234.307372795864, 
            counts = structure(c(31L, 9L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(69.3691915216732, 
            -33.1311578527504, -33.1311578527504, 25.6016337232268
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "rate"), c("shape", "rate")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 476.438791602583, m2LL = 468.614745591727), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gompertz", pars = c("shape", 
        "rate"), location = "rate", transforms = list(function (x) 
        x, .Primitive("log")), inv.transforms = list(function (x) 
        x, .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            c(0.001, 1/mean(t))
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(rate = integer(0), 
            shape = NULL), .Names = c("rate", "shape")), basepars = 1:2, 
        covpars = NULL, AIC = 460.282100878163, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 
            59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
            350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
            33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("51", "52", "53", "54", "55", "56", "57", "58", 
                "59", "60", "61", "62", "63", "64", "65", "66", 
                "67", "68", "69", "70", "71", "72", "73", "74", 
                "75", "76", "77", "78", "79", "80", "81", "82", 
                "83", "84", "85", "86", "87", "88", "89", "90", 
                "91", "92", "93", "94", "95", "96", "97", "98", 
                "99", "100"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 51:100, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("51", "52", "53", "54", "55", 
            "56", "57", "58", "59", "60", "61", "62", "63", "64", 
            "65", "66", "67", "68", "69", "70", "71", "72", "73", 
            "74", "75", "76", "77", "78", "79", "80", "81", "82", 
            "83", "84", "85", "86", "87", "88", "89", "90", "91", 
            "92", "93", "94", "95", "96", "97", "98", "99", "100"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 38L, trisk = 6942, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(rate = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "rate"), dfns = structure(list(
            p = function (q, shape, rate = 1, lower.tail = TRUE, 
                log.p = FALSE) 
            {
                d <- dbase("gompertz", lower.tail = lower.tail, 
                  log = log.p, q = q, shape = shape, rate = rate)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                prob <- numeric(length(q))
                prob[shape == 0] <- pexp(q[shape == 0], rate = rate[shape == 
                  0])
                sn0 <- shape != 0
                if (any(sn0)) {
                  q <- q[sn0]
                  shape <- shape[sn0]
                  rate <- rate[sn0]
                  prob[sn0] <- 1 - exp(-rate/shape * (exp(shape * 
                    q) - 1))
                }
                prob[q == Inf] <- 1
                if (!lower.tail) 
                  prob <- 1 - prob
                if (log.p) 
                  prob <- log(prob)
                ret[ind] <- prob
                ret
            }, d = function (x, shape, rate = 1, log = FALSE) 
            {
                d <- dbase("gompertz", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                logdens <- numeric(length(x))
                logdens[shape == 0] <- dexp(x[shape == 0], rate = rate[shape == 
                  0], log = TRUE)
                sn0 <- shape != 0
                if (any(sn0)) {
                  x <- x[sn0]
                  shape <- shape[sn0]
                  rate <- rate[sn0]
                  logdens[sn0] <- log(rate) + shape * x - rate/shape * 
                    (exp(shape * x) - 1)
                }
                ret[ind] <- if (log) 
                  logdens
                else exp(logdens)
                ret
            }, h = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gompertz", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                if (log) 
                  ret[ind] <- log(rate) + (shape * x)
                else ret[ind] <- rate * exp(shape * x)
                ret
            }, H = function (x, shape, rate = 1, log = FALSE) 
            {
                h <- dbase("gompertz", log = log, x = x, shape = shape, 
                  rate = rate)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- ifelse(shape == 0, rate * x, rate/shape * 
                  expm1(shape * x))
                if (log) 
                  ret[ind] <- log(ret[ind])
                ret
            }, r = function (n, shape = 1, rate = 1) 
            {
                r <- rbase("gompertz", n = n, shape = shape, 
                  rate = rate)
                for (i in seq_along(r)) assign(names(r)[i], r[[i]])
                ret[ind] <- qgompertz(p = runif(sum(ind)), shape = shape, 
                  rate = rate)
                ret
            }, DLd = function (t, shape, rate) 
            {
                res <- matrix(nrow = length(t), ncol = 2)
                rs <- rate/shape * exp(shape * t)
                res[shape == 0, 1] <- 0
                res[shape == 0, 2] <- 1 - rate[shape == 0] * 
                  t[shape == 0]
                sn0 <- (shape != 0)
                t <- t[sn0]
                rs <- rs[sn0]
                rate <- rate[sn0]
                shape <- shape[sn0]
                res[shape != 0, 1] <- t + rs * (1/shape - t) - 
                  rate/shape^2
                res[shape != 0, 2] <- 1 - rs + rate/shape
                res
            }, DLS = function (t, shape, rate) 
            {
                res <- matrix(nrow = length(t), ncol = 2)
                rs <- rate/shape * exp(shape * t)
                res[shape == 0, 1] <- 0
                res[shape == 0, 2] <- -rate[shape == 0] * t[shape == 
                  0]
                sn0 <- (shape != 0)
                t <- t[sn0]
                rs <- rs[sn0]
                rate <- rate[sn0]
                shape <- shape[sn0]
                res[shape != 0, 1] <- rs * (1/shape - t) - rate/shape^2
                res[shape != 0, 2] <- -rs + rate/shape
                res
            }, deriv = TRUE), .Names = c("p", "d", "h", "H", 
        "r", "DLd", "DLS", "deriv")), res = structure(c(-0.00754279397637771, 
        0.0115688102698746, -0.0118395064414649, 0.00741628749161452, 
        -0.00324608151129053, 0.0180464108506694, 0.00219224052022338, 
        0.00262448072254058), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.00754279397637771, -4.4594425719424, 
        -0.0118395064414649, -4.90407668507104, -0.00324608151129053, 
        -4.01480845881376, 0.00219224052022338, 0.226858307925991
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(4.80591849850929e-06, 
        -0.000347653902095399, -0.000347653902095399, 0.0514646918750437
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(-0.00754279397637771, 
        -4.4594425719424), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -228.141050439082, 
        logliki = c(-6.35065713420673, -6.26570968860074, -5.61660664551351, 
        -4.64630321782871, -4.89444011930337, -5.45537998319064, 
        -6.10325435198122, -4.7366597498335, -5.34929227718045, 
        -4.59112303865653, -5.28738339970363, -5.31845753164423, 
        -5.729919711945, -5.27175542890067, -5.78540144225606, 
        -6.11597819482136, -7.24265884479791, -8.52372385355786, 
        -6.29015423580011, -4.70075507396382, -5.11203122910487, 
        -5.31845753164423, -4.6826843665449, -6.5845406109461, 
        -4.96264726759359, -6.06484626916199, -5.01305891584101, 
        -5.90751893915845, -7.38997335358232, -5.04632046598934, 
        -5.36462100220948, -4.6826843665449, -4.62799156494158, 
        -5.25606624269731, -4.89444011930337, -6.07768868727833, 
        -4.8425221091949, -4.9457027950599, -1.42430338988326, 
        -1.42430338988326, -1.42430338988326, -1.42430338988326, 
        -1.42430338988326, -1.42430338988326, -1.42430338988326, 
        -1.42430338988326, -1.42430338988326, -1.42430338988326, 
        -1.42430338988326, -1.42430338988326), cl = 0.95, opt = structure(list(
            par = structure(c(-0.00754279397637771, -4.4594425719424
            ), .Names = c("shape", "rate")), value = 228.141050439082, 
            counts = structure(c(48L, 9L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(406925.968503626, 
            2748.86326255917, 2748.86326255917, 37.9998979553537
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "rate"), c("shape", "rate")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 464.10614688902, m2LL = 456.282100878163), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gengamma", pars = c("mu", 
        "sigma", "Q"), location = "mu", transforms = list(function (x) 
        x, .Primitive("log"), function (x) 
        x), inv.transforms = list(function (x) 
        x, .Primitive("exp"), function (x) 
        x), inits = function (t, mf, mml, aux) 
        {
            lt <- log(t[t > 0])
            c(mean(lt), sd(lt), 0)
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 0L, 
        ncoveffs = 0L, mx = structure(list(mu = integer(0), sigma = NULL, 
            Q = NULL), .Names = c("mu", "sigma", "Q")), basepars = 1:3, 
        covpars = NULL, AIC = 457.303386999027, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 
            59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
            350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
            33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("51", "52", "53", "54", "55", "56", "57", "58", 
                "59", "60", "61", "62", "63", "64", "65", "66", 
                "67", "68", "69", "70", "71", "72", "73", "74", 
                "75", "76", "77", "78", "79", "80", "81", "82", 
                "83", "84", "85", "86", "87", "88", "89", "90", 
                "91", "92", "93", "94", "95", "96", "97", "98", 
                "99", "100"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0), .Dim = c(50L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 51:100, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(mu = structure(c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("51", "52", "53", "54", "55", 
            "56", "57", "58", "59", "60", "61", "62", "63", "64", 
            "65", "66", "67", "68", "69", "70", "71", "72", "73", 
            "74", "75", "76", "77", "78", "79", "80", "81", "82", 
            "83", "84", "85", "86", "87", "88", "89", "90", "91", 
            "92", "93", "94", "95", "96", "97", "98", "99", "100"
            ), "(Intercept)"), assign = 0L), sigma = NULL, Q = NULL), .Names = c("mu", 
            "sigma", "Q"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 38L, trisk = 6942, concat.formula = quote(survival::Surv(time, 
            status) ~ 1), all.formulae = structure(list(mu = quote(survival::Surv(time, 
            status) ~ 1)), .Names = "mu"), dfns = structure(list(
            p = function (q, mu = 0, sigma = 1, Q, lower.tail = TRUE, 
                log.p = FALSE) 
            {
                d <- dbase("gengamma", lower.tail = lower.tail, 
                  log = log.p, q = q, mu = mu, sigma = sigma, 
                  Q = Q)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                prob <- numeric(length(q))
                prob[Q == 0] <- plnorm(q[Q == 0], mu[Q == 0], 
                  sigma[Q == 0])
                qn0 <- Q != 0
                if (any(qn0)) {
                  q <- q[qn0]
                  mu <- mu[qn0]
                  sigma <- sigma[qn0]
                  Q <- Q[qn0]
                  y <- log(q)
                  w <- ((y - mu)/sigma)
                  expnu <- exp(Q * w) * Q^-2
                  prob[qn0] <- ifelse(Q > 0, pgamma(expnu, Q^-2), 
                    1 - pgamma(expnu, Q^-2))
                }
                if (!lower.tail) 
                  prob <- 1 - prob
                if (log.p) 
                  prob <- log(prob)
                ret[ind] <- prob
                ret
            }, d = function (x, mu = 0, sigma = 1, Q, log = FALSE) 
            {
                d <- dbase("gengamma", log = log, x = x, mu = mu, 
                  sigma = sigma, Q = Q)
                for (i in seq_along(d)) assign(names(d)[i], d[[i]])
                logdens <- numeric(length(x))
                logdens[Q == 0] <- dlnorm(x[Q == 0], mu[Q == 
                  0], sigma[Q == 0], log = TRUE)
                qn0 <- Q != 0
                if (any(qn0)) {
                  x <- x[qn0]
                  mu <- mu[qn0]
                  sigma <- sigma[qn0]
                  Q <- Q[qn0]
                  y <- log(x)
                  w <- ((y - mu)/sigma)
                  logdens[qn0] <- -log(sigma * x) + log(abs(Q)) + 
                    (Q^-2) * log(Q^-2) + Q^-2 * (Q * w - exp(Q * 
                    w)) - lgamma(Q^-2)
                }
                ret[ind] <- if (log) 
                  logdens
                else exp(logdens)
                ret
            }, h = function (x, mu = 0, sigma = 1, Q) 
            {
                dgengamma(x = x, mu = mu, sigma = sigma, Q = Q)/pgengamma(q = x, 
                  mu = mu, sigma = sigma, Q = Q, lower.tail = FALSE)
            }, H = function (x, mu = 0, sigma = 1, Q) 
            {
                -log(pgengamma(q = x, mu = mu, sigma = sigma, 
                  Q = Q, lower.tail = FALSE))
            }, r = function (n, mu = 0, sigma = 1, Q) 
            {
                r <- rbase("gengamma", n = n, mu = mu, sigma = sigma, 
                  Q = Q)
                for (i in seq_along(r)) assign(names(r)[i], r[[i]])
                ret[ind][Q == 0] <- rlnorm(n, mu, sigma)
                qn0 <- Q != 0
                if (any(qn0)) {
                  mu <- mu[qn0]
                  sigma <- sigma[qn0]
                  Q <- Q[qn0]
                  w <- log(Q^2 * rgamma(n, 1/Q^2, 1))/Q
                  ret[ind][qn0] <- exp(mu + sigma * w)
                }
                ret
            }, DLd = NULL, DLS = NULL, deriv = FALSE), .Names = c("p", 
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(3.58341206119473, 
        1.27571918919786, -1.47672917789706, 2.76876277874759, 
        0.905112550794432, -2.71404611819489, 4.39806134364187, 
        1.79807411604136, -0.239412237599228, 0.415645026578545, 
        0.223389087260375, 0.631295753420792), .Dim = 3:4, .Dimnames = list(
            c("mu", "sigma", "Q"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(3.58341206119473, 0.243510089541833, 
        -1.47672917789706, 2.76876277874759, -0.0996959774962518, 
        -2.71404611819489, 4.39806134364187, 0.586716156579918, 
        -0.239412237599228, 0.415645026578545, 0.175108353901016, 
        0.631295753420792), .Dim = 3:4, .Dimnames = list(c("mu", 
        "sigma", "Q"), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.17276078811948, 
        0.0540519576845168, 0.229831978553774, 0.0540519576845168, 
        0.0306629356059234, 0.0740313487417578, 0.229831978553774, 
        0.0740313487417578, 0.398534328287125), .Dim = c(3L, 
        3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
        "sigma", "Q"))), coefficients = structure(c(3.58341206119473, 
        0.243510089541833, -1.47672917789706), .Names = c("mu", 
        "sigma", "Q")), npars = 3L, fixedpars = NULL, optpars = 1:3, 
        loglik = -225.651693499514, logliki = c(-6.47824063764775, 
        -6.39809729411359, -5.68325863957126, -4.51184956861264, 
        -4.56539484389961, -5.46802235269293, -6.23746874075084, 
        -4.37595123395592, -5.31543251697667, -4.9982618355835, 
        -5.22198376155557, -5.26930635318271, -5.82391285281813, 
        -5.19786243450475, -5.88993206837459, -6.25042272205193, 
        -7.19823103013351, -7.96762224069639, -6.42141612117353, 
        -4.38446210040133, -4.939090425188, -5.26930635318271, 
        -4.40665760754106, -6.68690736817963, -4.68155888566508, 
        -6.19795758626157, -4.76920264878775, -6.02927219157914, 
        -7.30016579781091, -4.82685018550804, -5.33805897988892, 
        -4.40665760754106, -4.61254787165891, -5.17342887940287, 
        -4.56539484389961, -6.2112379131942, -4.48353133619039, 
        -4.65225736861014, -1.45370669172721, -1.45370669172721, 
        -1.45370669172721, -1.45370669172721, -1.45370669172721, 
        -1.45370669172721, -1.45370669172721, -1.45370669172721, 
        -1.45370669172721, -1.45370669172721, -1.45370669172721, 
        -1.45370669172721), cl = 0.95, opt = structure(list(par = structure(c(3.58341206119473, 
        0.243510089541833, -1.47672917789706), .Names = c("mu", 
        "sigma", "Q")), value = 225.651693499514, counts = structure(c(18L, 
        8L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(30.6851595013313, 
            -20.610335845106, -13.8673624014984, -20.610335845106, 
            72.9766708360557, -1.6702378076161, -13.8673624014984, 
            -1.6702378076161, 10.8166674479548), .Dim = c(3L, 
            3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
            "sigma", "Q")))), .Names = c("par", "value", "counts", 
        "convergence", "message", "hessian")), BIC = 463.039456015312, 
        m2LL = 451.303386999027), .Names = c("call", "dlist", 
    "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "exp", pars = "rate", location = "rate", 
            transforms = list(.Primitive("log")), inv.transforms = list(
                .Primitive("exp")), inits = function (t, mf, 
                mml, aux) 
            {
                if (aux$counting) {
                  1/mean(t)
                }
                else {
                  aux$formula <- aux$forms[[1]]
                  aux$forms <- NULL
                  aux$dist <- "exponential"
                  sr <- do.call(survreg, aux)
                  sr2fsexp(sr)
                }
            }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 1L, 
        ncoveffs = 1L, mx = structure(list(rate = 1L), .Names = "rate"), 
        basepars = 1L, covpars = 2L, AIC = 819.082542975765, 
        data = structure(list(Y = structure(c(3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
        119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 
        82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
        31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
        22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 
        232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 
        71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 126, 119, 70, 10, 24, 59, 
        106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
        13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
        9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(100L, 6L
        ), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
        "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50", "51", "52", "53", 
        "54", "55", "56", "57", "58", "59", "60", "61", "62", 
        "63", "64", "65", "66", "67", "68", "69", "70", "71", 
        "72", "73", "74", "75", "76", "77", "78", "79", "80", 
        "81", "82", "83", "84", "85", "86", "87", "88", "89", 
        "90", "91", "92", "93", "94", "95", "96", "97", "98", 
        "99", "100"), c("time", "status", "start", "stop", "time1", 
        "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
        116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
        47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 
        78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(100L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 100L), covnames = structure("treatment", .Names = "rate"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(100L, 2L), .Dimnames = list(
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                "10", "11", "12", "13", "14", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", 
                "34", "35", "36", "37", "38", "39", "40", "41", 
                "42", "43", "44", "45", "46", "47", "48", "49", 
                "50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99", "100"), c("(Intercept)", "treatmentB"
                )), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment"))), .Names = "rate")), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.5, .Names = "treatmentB"), 
        N = 100L, events = 62L, trisk = 18213, concat.formula = quote(survival::Surv(time, 
            status) ~ treatment), all.formulae = structure(list(
            rate = quote(survival::Surv(time, status) ~ treatment)), .Names = "rate"), 
        dfns = structure(list(p = function (q, rate = 1, lower.tail = TRUE, 
            log.p = FALSE) 
        .Call(C_pexp, q, 1/rate, lower.tail, log.p), d = function (x, 
            rate = 1, log = FALSE) 
        .Call(C_dexp, x, 1/rate, log), h = function (x, rate = 1, 
            log = FALSE) 
        {
            h <- dbase("exp", log = log, x = x, rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- if (log) 
                log(rate)
            else rate
            ret
        }, H = function (x, rate = 1, log = FALSE) 
        {
            h <- dbase("exp", log = log, x = x, rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- if (log) {
                log(rate) + log(x)
            }
            else rate * x
            ret
        }, r = function (n, rate = 1) 
        .Call(C_rexp, n, 1/rate), DLd = function (t, rate) 
        {
            res <- matrix(nrow = length(t), ncol = 1)
            ts <- 1 - t * rate
            res[, 1] <- ts
            res
        }, DLS = function (t, rate) 
        {
            res <- matrix(nrow = length(t), ncol = 1)
            res[, 1] <- -t * rate
            res
        }, deriv = TRUE), .Names = c("p", "d", "h", "H", "r", 
        "DLd", "DLS", "deriv")), res = structure(c(0.00212935853074258, 
        0.944175467198776, 0.00142724331982305, 0.433145368019229, 
        0.00317687088772528, 1.45520556637832, 0.000434653453759085, 
        0.260734433494945), .Dim = c(2L, 4L), .Dimnames = list(
            c("rate", "treatmentB"), c("est", "L95%", "U95%", 
            "se"))), res.t = structure(c(-6.15193450389415, 0.944175467198776, 
        -6.55201044358414, 0.433145368019229, -5.75185856420415, 
        1.45520556637832, 0.204124128221613, 0.260734433494945
        ), .Dim = c(2L, 4L), .Dimnames = list(c("rate", "treatmentB"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0416666597222336, 
        -0.0416666597222258, -0.0416666597222258, 0.0679824448099301
        ), .Dim = c(2L, 2L), .Dimnames = list(c("rate", "treatmentB"
        ), c("rate", "treatmentB"))), coefficients = structure(c(-6.15193450389415, 
        0.944175467198776), .Names = c("rate", "treatmentB")), 
        npars = 2L, fixedpars = NULL, optpars = 1:2, loglik = -407.541271487882, 
        logliki = structure(c(-6.15832257948637, -6.39894009346029, 
        -6.33292997900727, -6.30737767663835, -6.16045193801712, 
        -6.37551714962212, -6.74389617544059, -6.35422356431469, 
        -6.64594568302643, -6.33505933753801, -6.17748680626306, 
        -6.21368590128568, -6.19878039157048, -6.26053178896202, 
        -6.25201435483905, -6.53308968089707, -6.3797758666836, 
        -6.17535744773232, -6.26266114749276, -6.35848228137618, 
        -6.35635292284543, -6.30311895957687, -6.81416500695509, 
        -6.17109873067083, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -0.745275485759904, -0.745275485759904, -0.745275485759904, 
        -5.89747381629777, -5.85915632854208, -5.59093391425226, 
        -5.26249830491778, -5.33913328042916, -5.53072071920761, 
        -5.78799527985295, -5.28986793902899, -5.49240323145192, 
        -5.24607652445106, -5.47050752416295, -5.48145537780744, 
        -5.63472532883019, -5.46503359734071, -5.65662103611916, 
        -5.79346920667519, -6.34633581572156, -7.12363342447982, 
        -5.87010418218657, -5.27892008538451, -5.4102943291183, 
        -5.48145537780744, -5.27344615856227, -6.0069523527426, 
        -5.36102898771813, -5.77157349938622, -5.37745076818485, 
        -5.70588637751933, -6.42844471805518, -5.38839862182933, 
        -5.49787715827416, -5.27344615856227, -5.25702437809554, 
        -5.45955967051847, -5.33913328042916, -5.77704742620847, 
        -5.32271149996244, -5.35555506089589, -1.91587438778445, 
        -1.91587438778445, -1.91587438778445, -1.91587438778445, 
        -1.91587438778445, -1.91587438778445, -1.91587438778445, 
        -1.91587438778445, -1.91587438778445, -1.91587438778445, 
        -1.91587438778445, -1.91587438778445), .Names = c("1", 
        "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
        "13", "14", "15", "16", "17", "18", "19", "20", "21", 
        "22", "23", "24", "25", "26", "27", "28", "29", "30", 
        "31", "32", "33", "34", "35", "36", "37", "38", "39", 
        "40", "41", "42", "43", "44", "45", "46", "47", "48", 
        "49", "50", "51", "52", "53", "54", "55", "56", "57", 
        "58", "59", "60", "61", "62", "63", "64", "65", "66", 
        "67", "68", "69", "70", "71", "72", "73", "74", "75", 
        "76", "77", "78", "79", "80", "81", "82", "83", "84", 
        "85", "86", "87", "88", "89", "90", "91", "92", "93", 
        "94", "95", "96", "97", "98", "99", "100")), cl = 0.95, 
        opt = structure(list(par = structure(c(-6.15193450389415, 
        0.944175467198776), .Names = c("rate", "treatmentB")), 
            value = 407.541271487882, counts = structure(c(2L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(62.0000103333247, 
            38.0000063333377, 38.0000063333377, 38.0000063333448
            ), .Dim = c(2L, 2L), .Dimnames = list(c("rate", "treatmentB"
            ), c("rate", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 824.292883347741, m2LL = 815.082542975765), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "weibull.quiet", pars = c("shape", 
        "scale"), location = "scale", transforms = list(.Primitive("log"), 
            .Primitive("log")), inv.transforms = list(.Primitive("exp"), 
            .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            if (aux$counting) {
                lt <- log(t[t > 0])
                c(1.64/var(lt), exp(mean(lt) + 0.572))
            }
            else {
                aux$formula <- aux$forms[[1]]
                aux$forms <- NULL
                aux$dist <- "weibull"
                sr <- do.call(survreg, aux)
                sr2fswei(sr)
            }
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 1L, 
        ncoveffs = 1L, mx = structure(list(scale = 1L, shape = NULL), .Names = c("scale", 
        "shape")), basepars = 1:2, covpars = 3L, AIC = 809.000787322249, 
        data = structure(list(Y = structure(c(3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
        119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 
        82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
        31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
        22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 
        232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 
        71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 126, 119, 70, 10, 24, 59, 
        106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
        13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
        9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(100L, 6L
        ), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
        "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50", "51", "52", "53", 
        "54", "55", "56", "57", "58", "59", "60", "61", "62", 
        "63", "64", "65", "66", "67", "68", "69", "70", "71", 
        "72", "73", "74", "75", "76", "77", "78", "79", "80", 
        "81", "82", "83", "84", "85", "86", "87", "88", "89", 
        "90", "91", "92", "93", "94", "95", "96", "97", "98", 
        "99", "100"), c("time", "status", "start", "stop", "time1", 
        "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
        116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
        47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 
        78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(100L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 100L), covnames = structure("treatment", .Names = "scale"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(scale = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(100L, 2L), .Dimnames = list(
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                "10", "11", "12", "13", "14", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", 
                "34", "35", "36", "37", "38", "39", "40", "41", 
                "42", "43", "44", "45", "46", "47", "48", "49", 
                "50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99", "100"), c("(Intercept)", "treatmentB"
                )), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                shape = NULL), .Names = c("scale", "shape"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.5, .Names = "treatmentB"), 
        N = 100L, events = 62L, trisk = 18213, concat.formula = quote(survival::Surv(time, 
            status) ~ treatment), all.formulae = structure(list(
            scale = quote(survival::Surv(time, status) ~ treatment)), .Names = "scale"), 
        dfns = structure(list(p = function (q, shape, scale = 1, 
            lower.tail = TRUE, log.p = FALSE) 
        {
            ret <- suppressWarnings(pweibull(q = q, shape = shape, 
                scale = scale, lower.tail = lower.tail, log.p = log.p))
            ret
        }, d = function (x, shape, scale = 1, log = FALSE) 
        {
            ret <- suppressWarnings(dweibull(x = x, shape = shape, 
                scale = scale, log = log))
            ret
        }, h = function (x, ...) 
        {
            d(x, ...)/(1 - p(x, ...))
        }, H = function (x, ...) 
        {
            -log(1 - p(x, ...))
        }, r = function (n, shape, scale = 1) 
        .Call(C_rweibull, n, shape, scale), DLd = function (t, 
            shape, scale) 
        {
            res <- matrix(nrow = length(t), ncol = 2)
            tss <- (t/scale)^shape
            res[, 1] <- 1 + shape * (log(t/scale) - log(t/scale) * 
                tss)
            res[, 2] <- -1 - (shape - 1) + shape * tss
            res
        }, DLS = function (t, shape, scale) 
        {
            res <- matrix(nrow = length(t), ncol = 2)
            tss <- (t/scale)^shape
            res[, 1] <- ifelse(t == 0, 0, -shape * log(t/scale) * 
                tss)
            res[, 2] <- tss * shape
            res
        }, deriv = TRUE), .Names = c("p", "d", "h", "H", "r", 
        "DLd", "DLS", "deriv")), res = structure(c(0.702360101626767, 
        586.740237326268, -1.19703545098942, 0.566460964608533, 
        323.598211312614, -1.94408882858238, 0.870862677533441, 
        1063.86282143292, -0.449982073396465, 0.0770596597896485, 
        178.144655872195, 0.381156686288941), .Dim = 3:4, .Dimnames = list(
            c("shape", "scale", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(-0.353309041197745, 
        6.37458219603501, -1.19703545098942, -0.568347107139707, 
        5.77950265770495, -1.94408882858238, -0.138270975255782, 
        6.96966173436507, -0.449982073396465, 0.109715314994641, 
        0.303617588396506, 0.381156686288941), .Dim = 3:4, .Dimnames = list(
            c("shape", "scale", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(0.0120374503443732, 
        -0.00964006441565666, 0.00948359060609521, -0.00964006441565666, 
        0.09218363998371, -0.0920583295931866, 0.00948359060609521, 
        -0.0920583295931866, 0.145280419502766), .Dim = c(3L, 
        3L), .Dimnames = list(c("shape", "scale", "treatmentB"
        ), c("shape", "scale", "treatmentB"))), coefficients = structure(c(-0.353309041197745, 
        6.37458219603501, -1.19703545098942), .Names = c("shape", 
        "scale", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -401.500393661124, logliki = structure(c(-5.18213679560322, 
        -6.5657120110155, -6.41032628226453, -6.33892752337074, 
        -5.2732673930477, -6.51441159128044, -7.09733674650072, 
        -6.46435041235615, -6.97289645649909, -6.41593114878568, 
        -5.6352606493683, -5.95377461201393, -5.85021524296293, 
        -6.18066815666827, -6.14633098456353, -6.80891256266268, 
        -6.52401174475565, -5.60550371622932, -6.18891728438377, 
        -6.47465486857916, -6.4695219699672, -6.32618895406891, 
        -7.17923199975206, -5.53772632097797, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -0.695674310167738, -0.695674310167738, 
        -0.695674310167738, -6.21615287351359, -6.16817637987346, 
        -5.77506033437468, -4.80790637672144, -5.18125006636819, 
        -5.66525870632976, -6.07475401507431, -4.97232777169269, 
        -5.58846873691964, -4.67232747335312, -5.54154172302722, 
        -5.56531231692783, -5.84838974161995, -5.5294113183827, 
        -5.88336062851388, -6.08216011972299, -6.6974040898416, 
        -7.34601186261587, -6.18203801232571, -4.91285859431667, 
        -5.39732326984582, -5.56531231692783, -4.88030884504138, 
        -6.34578575688042, -5.25520648707555, -6.05229626644645, 
        -5.30577617263416, -5.95851721633916, -6.77421464426306, 
        -5.33757675405549, -5.59983019482025, -4.88030884504138, 
        -4.76707764777995, -5.51710888673067, -5.18125006636819, 
        -6.05982276505744, -5.11952571981526, -5.2374819570928, 
        -1.61264593461292, -1.61264593461292, -1.61264593461292, 
        -1.61264593461292, -1.61264593461292, -1.61264593461292, 
        -1.61264593461292, -1.61264593461292, -1.61264593461292, 
        -1.61264593461292, -1.61264593461292, -1.61264593461292
        ), .Names = c("1", "2", "3", "4", "5", "6", "7", "8", 
        "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50", "51", "52", "53", 
        "54", "55", "56", "57", "58", "59", "60", "61", "62", 
        "63", "64", "65", "66", "67", "68", "69", "70", "71", 
        "72", "73", "74", "75", "76", "77", "78", "79", "80", 
        "81", "82", "83", "84", "85", "86", "87", "88", "89", 
        "90", "91", "92", "93", "94", "95", "96", "97", "98", 
        "99", "100")), cl = 0.95, opt = structure(list(par = structure(c(-0.353309041197745, 
        6.37458219603501, -1.19703545098942), .Names = c("shape", 
        "scale", "treatmentB")), value = 401.500393661124, counts = structure(c(2L, 
        1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(90.6703849572428, 
            9.72515988612055, 0.243674703256924, 9.72515988612055, 
            30.5852046808059, 18.7457706108156, 0.243674703256924, 
            18.7457706108156, 18.7457706108161), .Dim = c(3L, 
            3L), .Dimnames = list(c("shape", "scale", "treatmentB"
            ), c("shape", "scale", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 816.816297880213, m2LL = 803.000787322249), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "lnorm", pars = c("meanlog", 
        "sdlog"), location = "meanlog", transforms = list(function (x) 
        x, .Primitive("log")), inv.transforms = list(function (x) 
        x, .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            if (aux$counting) {
                lt <- log(t[t > 0])
                c(mean(lt), sd(lt))
            }
            else {
                aux$formula <- aux$forms[[1]]
                aux$forms <- NULL
                aux$dist <- "lognormal"
                sr <- do.call(survreg, aux)
                sr2fsln(sr)
            }
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 1L, 
        ncoveffs = 1L, mx = structure(list(meanlog = 1L, sdlog = NULL), .Names = c("meanlog", 
        "sdlog")), basepars = 1:2, covpars = 3L, AIC = 800.957841237647, 
        data = structure(list(Y = structure(c(3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
        119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 
        82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
        31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
        22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 
        232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 
        71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 126, 119, 70, 10, 24, 59, 
        106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
        13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
        9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(100L, 6L
        ), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
        "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50", "51", "52", "53", 
        "54", "55", "56", "57", "58", "59", "60", "61", "62", 
        "63", "64", "65", "66", "67", "68", "69", "70", "71", 
        "72", "73", "74", "75", "76", "77", "78", "79", "80", 
        "81", "82", "83", "84", "85", "86", "87", "88", "89", 
        "90", "91", "92", "93", "94", "95", "96", "97", "98", 
        "99", "100"), c("time", "status", "start", "stop", "time1", 
        "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
        116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
        47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 
        78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(100L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 100L), covnames = structure("treatment", .Names = "meanlog"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(meanlog = structure(c(1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(100L, 
            2L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49", "50", 
            "51", "52", "53", "54", "55", "56", "57", "58", "59", 
            "60", "61", "62", "63", "64", "65", "66", "67", "68", 
            "69", "70", "71", "72", "73", "74", "75", "76", "77", 
            "78", "79", "80", "81", "82", "83", "84", "85", "86", 
            "87", "88", "89", "90", "91", "92", "93", "94", "95", 
            "96", "97", "98", "99", "100"), c("(Intercept)", 
            "treatmentB")), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                sdlog = NULL), .Names = c("meanlog", "sdlog"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.5, .Names = "treatmentB"), 
        N = 100L, events = 62L, trisk = 18213, concat.formula = quote(survival::Surv(time, 
            status) ~ treatment), all.formulae = structure(list(
            meanlog = quote(survival::Surv(time, status) ~ treatment)), .Names = "meanlog"), 
        dfns = structure(list(p = function (q, meanlog = 0, sdlog = 1, 
            lower.tail = TRUE, log.p = FALSE) 
        .Call(C_plnorm, q, meanlog, sdlog, lower.tail, log.p), 
            d = function (x, meanlog = 0, sdlog = 1, log = FALSE) 
            .Call(C_dlnorm, x, meanlog, sdlog, log), h = function (x, 
                meanlog = 0, sdlog = 1, log = FALSE) 
            {
                h <- dbase("lnorm", log = log, x = x, meanlog = meanlog, 
                  sdlog = sdlog)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- dlnorm(x, meanlog, sdlog)/plnorm(x, 
                  meanlog, sdlog, lower.tail = FALSE)
                ret
            }, H = function (x, meanlog = 0, sdlog = 1, log = FALSE) 
            {
                h <- dbase("lnorm", log = log, x = x, meanlog = meanlog, 
                  sdlog = sdlog)
                for (i in seq_along(h)) assign(names(h)[i], h[[i]])
                ret[ind] <- -plnorm(x, meanlog, sdlog, lower.tail = FALSE, 
                  log.p = TRUE)
                ret
            }, r = function (n, meanlog = 0, sdlog = 1) 
            .Call(C_rlnorm, n, meanlog, sdlog), DLd = NULL, DLS = NULL, 
            deriv = FALSE), .Names = c("p", "d", "h", "H", "r", 
        "DLd", "DLS", "deriv")), res = structure(c(5.66467943823768, 
        1.81802545288532, -1.06495339666289, 5.08175573056953, 
        1.49919537989268, -1.83299718334886, 6.24760314590583, 
        2.20466030756811, -0.296909609976917, 0.297415520012704, 
        0.178858446543434, 0.391866275474552), .Dim = 3:4, .Dimnames = list(
            c("meanlog", "sdlog", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(5.66467943823768, 
        0.597750996144257, -1.06495339666289, 5.08175573056953, 
        0.404928550782143, -1.83299718334886, 6.24760314590583, 
        0.790573441506371, -0.296909609976917, 0.297415520012704, 
        0.0983806064208695, 0.391866275474552), .Dim = 3:4, .Dimnames = list(
            c("meanlog", "sdlog", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(0.088455991544427, 
        0.00886025002025895, -0.0853477637114001, 0.00886025002025896, 
        0.00967874371973803, -0.00546488978215761, -0.0853477637114001, 
        -0.00546488978215762, 0.153559177854298), .Dim = c(3L, 
        3L), .Dimnames = list(c("meanlog", "sdlog", "treatmentB"
        ), c("meanlog", "sdlog", "treatmentB"))), coefficients = structure(c(5.66467943823768, 
        0.597750996144257, -1.06495339666289), .Names = c("meanlog", 
        "sdlog", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -397.478920618824, logliki = structure(c(-5.76925087336176, 
        -6.39585146505197, -6.18524926348304, -6.09283079699074, 
        -5.67202770438197, -6.32518636151579, -7.14451839333347, 
        -6.25722322239399, -6.97061232195531, -6.19264165435404, 
        -5.53114119401737, -5.68241586575877, -5.60972492856609, 
        -5.9027641149444, -5.86491732317578, -6.73853740571727, 
        -6.33833881265593, -5.52898413585469, -5.91205875731002, 
        -6.2711211443371, -6.26419196128444, -6.07671796758775, 
        -7.25733594596377, -5.53274247090676, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -0.781601007204253, -0.781601007204253, 
        -0.781601007204253, -6.3614366422321, -6.30068160597941, 
        -5.78384664616026, -4.61753647025351, -5.00049516857637, 
        -5.63547705893438, -6.18074270623069, -4.76607150617227, 
        -5.53154924966759, -4.5279971630349, -5.46818009355939, 
        -5.50025632913241, -5.88233231934845, -5.45183426051521, 
        -5.92904405377528, -6.19032678562698, -6.93657722152007, 
        -7.6141055008128, -6.31829532020824, -4.70796943049972, 
        -5.2755162857611, -5.50025632913241, -4.67817228176451, 
        -6.52258883562015, -5.09193572976605, -6.15160386190578, 
        -5.15625283101797, -6.02874370207601, -7.02248851590559, 
        -5.1973137738558, -5.54691508353326, -4.67817228176451, 
        -4.58708129603552, -5.43527533967161, -5.00049516857637, 
        -6.16138221650825, -4.92709695747935, -5.06971483638576, 
        -1.4087624573009, -1.4087624573009, -1.4087624573009, 
        -1.4087624573009, -1.4087624573009, -1.4087624573009, 
        -1.4087624573009, -1.4087624573009, -1.4087624573009, 
        -1.4087624573009, -1.4087624573009, -1.4087624573009), .Names = c("1", 
        "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
        "13", "14", "15", "16", "17", "18", "19", "20", "21", 
        "22", "23", "24", "25", "26", "27", "28", "29", "30", 
        "31", "32", "33", "34", "35", "36", "37", "38", "39", 
        "40", "41", "42", "43", "44", "45", "46", "47", "48", 
        "49", "50", "51", "52", "53", "54", "55", "56", "57", 
        "58", "59", "60", "61", "62", "63", "64", "65", "66", 
        "67", "68", "69", "70", "71", "72", "73", "74", "75", 
        "76", "77", "78", "79", "80", "81", "82", "83", "84", 
        "85", "86", "87", "88", "89", "90", "91", "92", "93", 
        "94", "95", "96", "97", "98", "99", "100")), cl = 0.95, 
        opt = structure(list(par = structure(c(5.66467943823768, 
        0.597750996144257, -1.06495339666289), .Names = c("meanlog", 
        "sdlog", "treatmentB")), value = 397.478920618824, counts = structure(c(7L, 
        1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(26.7058541965071, 
            -16.3961129686641, 14.2595318237682, -16.3961129686641, 
            115.504297184543, -5.00232765432429, 14.2595318237682, 
            -5.00232765432429, 14.2595318237682), .Dim = c(3L, 
            3L), .Dimnames = list(c("meanlog", "sdlog", "treatmentB"
            ), c("meanlog", "sdlog", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 808.773351795612, m2LL = 794.957841237647), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gamma", pars = c("shape", 
        "rate"), location = "rate", transforms = list(.Primitive("log"), 
            .Primitive("log")), inv.transforms = list(.Primitive("exp"), 
            .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            m = mean(t)
            v = var(t)
            c(m^2/v, m/v)
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 1L, 
        ncoveffs = 1L, mx = structure(list(rate = 1L, shape = NULL), .Names = c("rate", 
        "shape")), basepars = 1:2, covpars = 3L, AIC = 812.03779496154, 
        data = structure(list(Y = structure(c(3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
        119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 
        82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
        31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
        22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 
        232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 
        71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 126, 119, 70, 10, 24, 59, 
        106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
        13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
        9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(100L, 6L
        ), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
        "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50", "51", "52", "53", 
        "54", "55", "56", "57", "58", "59", "60", "61", "62", 
        "63", "64", "65", "66", "67", "68", "69", "70", "71", 
        "72", "73", "74", "75", "76", "77", "78", "79", "80", 
        "81", "82", "83", "84", "85", "86", "87", "88", "89", 
        "90", "91", "92", "93", "94", "95", "96", "97", "98", 
        "99", "100"), c("time", "status", "start", "stop", "time1", 
        "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
        116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
        47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 
        78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(100L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 100L), covnames = structure("treatment", .Names = "rate"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(100L, 2L), .Dimnames = list(
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                "10", "11", "12", "13", "14", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", 
                "34", "35", "36", "37", "38", "39", "40", "41", 
                "42", "43", "44", "45", "46", "47", "48", "49", 
                "50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99", "100"), c("(Intercept)", "treatmentB"
                )), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                shape = NULL), .Names = c("rate", "shape"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.5, .Names = "treatmentB"), 
        N = 100L, events = 62L, trisk = 18213, concat.formula = quote(survival::Surv(time, 
            status) ~ treatment), all.formulae = structure(list(
            rate = quote(survival::Surv(time, status) ~ treatment)), .Names = "rate"), 
        dfns = structure(list(p = function (q, shape, rate = 1, 
            scale = 1/rate, lower.tail = TRUE, log.p = FALSE) 
        {
            if (!missing(rate) && !missing(scale)) {
                if (abs(rate * scale - 1) < 1e-15) 
                  warning("specify 'rate' or 'scale' but not both")
                else stop("specify 'rate' or 'scale' but not both")
            }
            .Call(C_pgamma, q, shape, scale, lower.tail, log.p)
        }, d = function (x, shape, rate = 1, scale = 1/rate, 
            log = FALSE) 
        {
            if (!missing(rate) && !missing(scale)) {
                if (abs(rate * scale - 1) < 1e-15) 
                  warning("specify 'rate' or 'scale' but not both")
                else stop("specify 'rate' or 'scale' but not both")
            }
            .Call(C_dgamma, x, shape, scale, log)
        }, h = function (x, shape, rate = 1, log = FALSE) 
        {
            h <- dbase("gamma", log = log, x = x, shape = shape, 
                rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- dgamma(x, shape, rate)/pgamma(x, shape, 
                rate, lower.tail = FALSE)
            ret
        }, H = function (x, shape, rate = 1, log = FALSE) 
        {
            h <- dbase("gamma", log = log, x = x, shape = shape, 
                rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- -pgamma(x, shape, rate, lower.tail = FALSE, 
                log.p = TRUE)
            ret
        }, r = function (n, shape, rate = 1, scale = 1/rate) 
        {
            if (!missing(rate) && !missing(scale)) {
                if (abs(rate * scale - 1) < 1e-15) 
                  warning("specify 'rate' or 'scale' but not both")
                else stop("specify 'rate' or 'scale' but not both")
            }
            .Call(C_rgamma, n, shape, scale)
        }, DLd = NULL, DLS = NULL, deriv = FALSE), .Names = c("p", 
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(0.655858144616729, 
        0.0010381655139146, 1.12754186259411, 0.490977896675616, 
        0.000486283164704138, 0.433605034919068, 0.876108494440626, 
        0.00221637867092791, 1.82147869026916, 0.0968899013961151, 
        0.000401724154463291, 0.354055907735412), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(-0.421810756384231, 
        -6.87030005231163, 1.12754186259411, -0.711356169154849, 
        -7.62871946031041, 0.433605034919068, -0.132265343613613, 
        -6.11188064431286, 1.82147869026916, 0.147729965986373, 
        0.386955787953803, 0.354055907735412), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(0.0218241428503349, 
        0.0401722525670395, -0.0114489749801971, 0.0401722525670395, 
        0.149734781830949, -0.0968631171001663, -0.0114489749801971, 
        -0.0968631171001663, 0.125355585802347), .Dim = c(3L, 
        3L), .Dimnames = list(c("shape", "rate", "treatmentB"
        ), c("shape", "rate", "treatmentB"))), coefficients = structure(c(-0.421810756384231, 
        -6.87030005231163, 1.12754186259411), .Names = c("shape", 
        "rate", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -403.01889748077, logliki = structure(c(-5.20471419357728, 
        -6.57985777337958, -6.44066754057857, -6.37583398070567, 
        -5.30475580126526, -6.53415115441085, -7.04883120999425, 
        -6.48932659215073, -6.93882592976526, -6.44573080291722, 
        -5.69113959674568, -6.01245546012585, -5.91011795193974, 
        -6.22957343516533, -6.19731194397237, -6.79454942846803, 
        -6.54272092063546, -5.6601571745151, -6.2372941767741, 
        -6.49857280450052, -6.49396837022925, -6.3641975389443, 
        -7.12169368280698, -5.58902165802987, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -0.688515962902176, -0.688515962902176, 
        -0.688515962902176, -6.15232584780712, -6.11021385035611, 
        -5.77051269548725, -4.90848875328483, -5.25465697861057, 
        -5.67641391257401, -6.02872513047443, -5.06405583564047, 
        -5.61050956075628, -4.77612423565416, -5.57013986062111, 
        -5.59060024465901, -5.83340082769601, -5.55968858776601, 
        -5.86343516082756, -6.03516244669162, -6.58771398835267, 
        -7.22204343925101, -6.12236150782539, -5.00839701869981, 
        -5.44530050319189, -5.59060024465901, -4.97764505921951, 
        -6.26714471951982, -5.32053033378141, -6.00922704071293, 
        -5.36517575970201, -5.92812740483145, -6.65976655950608, 
        -5.39310345845604, -5.62027075546682, -4.97764505921951, 
        -4.86902387639058, -5.54908148884739, -5.25465697861057, 
        -6.01575802221227, -5.19908549674599, -5.30480879168891, 
        -1.66000765732655, -1.66000765732655, -1.66000765732655, 
        -1.66000765732655, -1.66000765732655, -1.66000765732655, 
        -1.66000765732655, -1.66000765732655, -1.66000765732655, 
        -1.66000765732655, -1.66000765732655, -1.66000765732655
        ), .Names = c("1", "2", "3", "4", "5", "6", "7", "8", 
        "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50", "51", "52", "53", 
        "54", "55", "56", "57", "58", "59", "60", "61", "62", 
        "63", "64", "65", "66", "67", "68", "69", "70", "71", 
        "72", "73", "74", "75", "76", "77", "78", "79", "80", 
        "81", "82", "83", "84", "85", "86", "87", "88", "89", 
        "90", "91", "92", "93", "94", "95", "96", "97", "98", 
        "99", "100")), cl = 0.95, opt = structure(list(par = structure(c(-0.421810756384231, 
        -6.87030005231163, 1.12754186259411), .Names = c("shape", 
        "rate", "treatmentB")), value = 403.01889748077, counts = structure(c(32L, 
        12L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(130.292367529705, 
            -54.5011325243649, -30.2135361707201, -54.5011325243649, 
            36.1510308266588, 22.9564515308311, -30.2135361707201, 
            22.9564515308311, 22.9564515592529), .Dim = c(3L, 
            3L), .Dimnames = list(c("shape", "rate", "treatmentB"
            ), c("shape", "rate", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 819.853305519504, m2LL = 806.03779496154), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gompertz", pars = c("shape", 
        "rate"), location = "rate", transforms = list(function (x) 
        x, .Primitive("log")), inv.transforms = list(function (x) 
        x, .Primitive("exp")), inits = function (t, mf, mml, 
            aux) 
        {
            c(0.001, 1/mean(t))
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 1L, 
        ncoveffs = 1L, mx = structure(list(rate = 1L, shape = NULL), .Names = c("rate", 
        "shape")), basepars = 1:2, covpars = 3L, AIC = 794.457893559156, 
        data = structure(list(Y = structure(c(3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
        119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 
        82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
        31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
        22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 3, 116, 85, 73, 4, 105, 278, 95, 
        232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 
        71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 126, 119, 70, 10, 24, 59, 
        106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
        13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
        9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(100L, 6L
        ), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
        "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
        "18", "19", "20", "21", "22", "23", "24", "25", "26", 
        "27", "28", "29", "30", "31", "32", "33", "34", "35", 
        "36", "37", "38", "39", "40", "41", "42", "43", "44", 
        "45", "46", "47", "48", "49", "50", "51", "52", "53", 
        "54", "55", "56", "57", "58", "59", "60", "61", "62", 
        "63", "64", "65", "66", "67", "68", "69", "70", "71", 
        "72", "73", "74", "75", "76", "77", "78", "79", "80", 
        "81", "82", "83", "84", "85", "86", "87", "88", "89", 
        "90", "91", "92", "93", "94", "95", "96", "97", "98", 
        "99", "100"), c("time", "status", "start", "stop", "time1", 
        "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(3, 
        116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
        47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 
        78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
        350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(100L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 100L), covnames = structure("treatment", .Names = "rate"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(100L, 2L), .Dimnames = list(
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                "10", "11", "12", "13", "14", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", 
                "34", "35", "36", "37", "38", "39", "40", "41", 
                "42", "43", "44", "45", "46", "47", "48", "49", 
                "50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99", "100"), c("(Intercept)", "treatmentB"
                )), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                shape = NULL), .Names = c("rate", "shape"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.5, .Names = "treatmentB"), 
        N = 100L, events = 62L, trisk = 18213, concat.formula = quote(survival::Surv(time, 
            status) ~ treatment), all.formulae = structure(list(
            rate = quote(survival::Surv(time, status) ~ treatment)), .Names = "rate"), 
        dfns = structure(list(p = function (q, shape, rate = 1, 
            lower.tail = TRUE, log.p = FALSE) 
        {
            d <- dbase("gompertz", lower.tail = lower.tail, log = log.p, 
                q = q, shape = shape, rate = rate)
            for (i in seq_along(d)) assign(names(d)[i], d[[i]])
            prob <- numeric(length(q))
            prob[shape == 0] <- pexp(q[shape == 0], rate = rate[shape == 
                0])
            sn0 <- shape != 0
            if (any(sn0)) {
                q <- q[sn0]
                shape <- shape[sn0]
                rate <- rate[sn0]
                prob[sn0] <- 1 - exp(-rate/shape * (exp(shape * 
                  q) - 1))
            }
            prob[q == Inf] <- 1
            if (!lower.tail) 
                prob <- 1 - prob
            if (log.p) 
                prob <- log(prob)
            ret[ind] <- prob
            ret
        }, d = function (x, shape, rate = 1, log = FALSE) 
        {
            d <- dbase("gompertz", log = log, x = x, shape = shape, 
                rate = rate)
            for (i in seq_along(d)) assign(names(d)[i], d[[i]])
            logdens <- numeric(length(x))
            logdens[shape == 0] <- dexp(x[shape == 0], rate = rate[shape == 
                0], log = TRUE)
            sn0 <- shape != 0
            if (any(sn0)) {
                x <- x[sn0]
                shape <- shape[sn0]
                rate <- rate[sn0]
                logdens[sn0] <- log(rate) + shape * x - rate/shape * 
                  (exp(shape * x) - 1)
            }
            ret[ind] <- if (log) 
                logdens
            else exp(logdens)
            ret
        }, h = function (x, shape, rate = 1, log = FALSE) 
        {
            h <- dbase("gompertz", log = log, x = x, shape = shape, 
                rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            if (log) 
                ret[ind] <- log(rate) + (shape * x)
            else ret[ind] <- rate * exp(shape * x)
            ret
        }, H = function (x, shape, rate = 1, log = FALSE) 
        {
            h <- dbase("gompertz", log = log, x = x, shape = shape, 
                rate = rate)
            for (i in seq_along(h)) assign(names(h)[i], h[[i]])
            ret[ind] <- ifelse(shape == 0, rate * x, rate/shape * 
                expm1(shape * x))
            if (log) 
                ret[ind] <- log(ret[ind])
            ret
        }, r = function (n, shape = 1, rate = 1) 
        {
            r <- rbase("gompertz", n = n, shape = shape, rate = rate)
            for (i in seq_along(r)) assign(names(r)[i], r[[i]])
            ret[ind] <- qgompertz(p = runif(sum(ind)), shape = shape, 
                rate = rate)
            ret
        }, DLd = function (t, shape, rate) 
        {
            res <- matrix(nrow = length(t), ncol = 2)
            rs <- rate/shape * exp(shape * t)
            res[shape == 0, 1] <- 0
            res[shape == 0, 2] <- 1 - rate[shape == 0] * t[shape == 
                0]
            sn0 <- (shape != 0)
            t <- t[sn0]
            rs <- rs[sn0]
            rate <- rate[sn0]
            shape <- shape[sn0]
            res[shape != 0, 1] <- t + rs * (1/shape - t) - rate/shape^2
            res[shape != 0, 2] <- 1 - rs + rate/shape
            res
        }, DLS = function (t, shape, rate) 
        {
            res <- matrix(nrow = length(t), ncol = 2)
            rs <- rate/shape * exp(shape * t)
            res[shape == 0, 1] <- 0
            res[shape == 0, 2] <- -rate[shape == 0] * t[shape == 
                0]
            sn0 <- (shape != 0)
            t <- t[sn0]
            rs <- rs[sn0]
            rate <- rate[sn0]
            shape <- shape[sn0]
            res[shape != 0, 1] <- rs * (1/shape - t) - rate/shape^2
            res[shape != 0, 2] <- -rs + rate/shape
            res
        }, deriv = TRUE), .Names = c("p", "d", "h", "H", "r", 
        "DLd", "DLS", "deriv")), res = structure(c(-0.00753880278125979, 
        0.00530884276636692, 0.778653649945334, -0.0107658451173598, 
        0.00323389056769971, 0.264212561483069, -0.00431176044515982, 
        0.0087151407655868, 1.2930947384076, 0.00164648042594378, 
        0.00134264171732762, 0.262474766128414), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(-0.00753880278125979, 
        -5.23838140225035, 0.778653649945334, -0.0107658451173598, 
        -5.73406935646415, 0.264212561483069, -0.00431176044515982, 
        -4.74269344803655, 1.2930947384076, 0.00164648042594378, 
        0.252906664675333, 0.262474766128414), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(2.71089779301601e-06, 
        -0.000245845065532298, 4.96833103783105e-05, -0.000245845065532299, 
        0.0639617810372012, -0.046172324099723, 4.96833103783106e-05, 
        -0.046172324099723, 0.0688930028541656), .Dim = c(3L, 
        3L), .Dimnames = list(c("shape", "rate", "treatmentB"
        ), c("shape", "rate", "treatmentB"))), coefficients = structure(c(-0.00753880278125979, 
        -5.23838140225035, 0.778653649945334), .Names = c("shape", 
        "rate", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -394.228946779578, logliki = c(-5.27674558855891, 
        -6.52338318658022, -6.2123577160945, -6.08676206098959, 
        -5.28945500012562, -6.41506222810349, -7.95177278054306, 
        -6.31468820481777, -7.56909183438269, -6.2226830809568, 
        -5.38975650711259, -5.59529597576128, -5.51185817112087, 
        -5.84763801952436, -5.80280546865681, -7.10937064214245, 
        -6.43491494569257, -5.37734971972545, -5.85877752097536, 
        -6.33491482552593, -6.32481121947831, -6.06551422163293, 
        -8.21962642090723, -5.35242535382168, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -0.653878231850485, -0.653878231850485, 
        -0.653878231850485, -6.35037077785006, -6.26544472404999, 
        -5.61651321672962, -4.64651890754993, -4.89456888787478, 
        -5.45533265273555, -6.1030307489673, -4.73684309544554, 
        -5.34927628433621, -4.59135888047875, -5.28738609019828, 
        -5.31845080689022, -5.72979488371093, -5.27176288350531, 
        -5.78526151171635, -6.11575132538507, -7.24214494169497, 
        -8.5228067888518, -6.28988310236142, -4.70095117552007, 
        -5.11208852525538, -5.31845080689022, -4.68288693614848, 
        -6.58419573990725, -4.96275317040559, -6.06463255861729, 
        -5.013148192797, -5.90734633403055, -7.38941921549562, 
        -5.04639889857258, -5.36460042922466, -4.68288693614848, 
        -4.62821390830697, -5.25607849977459, -4.89456888787478, 
        -6.07747166343466, -4.84266857401649, -4.94581433821187, 
        -1.42449806310586, -1.42449806310586, -1.42449806310586, 
        -1.42449806310586, -1.42449806310586, -1.42449806310586, 
        -1.42449806310586, -1.42449806310586, -1.42449806310586, 
        -1.42449806310586, -1.42449806310586, -1.42449806310586
        ), cl = 0.95, opt = structure(list(par = structure(c(-0.00753880278125979, 
        -5.23838140225035, 0.778653649945334), .Names = c("shape", 
        "rate", "treatmentB")), value = 394.228946779578, counts = structure(c(51L, 
        14L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(765232.995321393, 
            4926.20217297287, 2749.69716872389, 4926.20217297287, 
            62.0000108069725, 38.0000065777573, 2749.69716872389, 
            38.0000065777573, 38.0000065777573), .Dim = c(3L, 
            3L), .Dimnames = list(c("shape", "rate", "treatmentB"
            ), c("shape", "rate", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 802.27340411712, m2LL = 788.457893559156), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg"), structure(list(
        call = quote(flexsurv::flexsurvreg(formula = stats::as.formula(this_formula), 
            data = this_data, dist = conditions[i, "dist"])), 
        dlist = structure(list(name = "gengamma", pars = c("mu", 
        "sigma", "Q"), location = "mu", transforms = list(function (x) 
        x, .Primitive("log"), function (x) 
        x), inv.transforms = list(function (x) 
        x, .Primitive("exp"), function (x) 
        x), inits = function (t, mf, mml, aux) 
        {
            lt <- log(t[t > 0])
            c(mean(lt), sd(lt), 0)
        }), .Names = c("name", "pars", "location", "transforms", 
        "inv.transforms", "inits")), aux = NULL, ncovs = 1L, 
        ncoveffs = 1L, mx = structure(list(mu = 1L, sigma = NULL, 
            Q = NULL), .Names = c("mu", "sigma", "Q")), basepars = 1:3, 
        covpars = 4L, AIC = 802.161902463908, data = structure(list(
            Y = structure(c(3, 116, 85, 73, 4, 105, 278, 95, 
            232, 86, 12, 29, 22, 51, 47, 179, 107, 11, 52, 97, 
            96, 71, 311, 9, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
            29, 22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 
            9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 126, 119, 70, 10, 24, 
            59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
            350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
            33, 53, 12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 3, 116, 
            85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 22, 51, 
            47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 126, 119, 70, 10, 24, 59, 106, 
            15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
            13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 
            12, 9, 46, 24, 104, 21, 27, 350, 350, 350, 350, 350, 
            350, 350, 350, 350, 350, 350, 350, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(100L, 
            6L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49", "50", 
            "51", "52", "53", "54", "55", "56", "57", "58", "59", 
            "60", "61", "62", "63", "64", "65", "66", "67", "68", 
            "69", "70", "71", "72", "73", "74", "75", "76", "77", 
            "78", "79", "80", "81", "82", "83", "84", "85", "86", 
            "87", "88", "89", "90", "91", "92", "93", "94", "95", 
            "96", "97", "98", "99", "100"), c("time", "status", 
            "start", "stop", "time1", "time2"))), m = structure(list(
                `survival::Surv(time, status)` = structure(c(3, 
                116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
                22, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 
                9, 350, 350, 350, 350, 350, 350, 350, 350, 350, 
                350, 350, 350, 350, 350, 350, 350, 350, 350, 
                350, 350, 350, 350, 350, 350, 350, 350, 126, 
                119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 
                78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 
                146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 
                24, 104, 21, 27, 350, 350, 350, 350, 350, 350, 
                350, 350, 350, 350, 350, 350, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0), .Dim = c(100L, 2L), .Dimnames = list(
                  NULL, c("time", "status")), type = "right", class = "Surv"), 
                treatment = c("A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "treatment", "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ treatment), row.names = c(NA, 100L), covnames = structure("treatment", .Names = "mu"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(mu = structure(c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(100L, 2L), .Dimnames = list(
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                "10", "11", "12", "13", "14", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", 
                "34", "35", "36", "37", "38", "39", "40", "41", 
                "42", "43", "44", "45", "46", "47", "48", "49", 
                "50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99", "100"), c("(Intercept)", "treatmentB"
                )), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                sigma = NULL, Q = NULL), .Names = c("mu", "sigma", 
            "Q"))), .Names = c("Y", "m", "mml")), datameans = structure(0.5, .Names = "treatmentB"), 
        N = 100L, events = 62L, trisk = 18213, concat.formula = quote(survival::Surv(time, 
            status) ~ treatment), all.formulae = structure(list(
            mu = quote(survival::Surv(time, status) ~ treatment)), .Names = "mu"), 
        dfns = structure(list(p = function (q, mu = 0, sigma = 1, 
            Q, lower.tail = TRUE, log.p = FALSE) 
        {
            d <- dbase("gengamma", lower.tail = lower.tail, log = log.p, 
                q = q, mu = mu, sigma = sigma, Q = Q)
            for (i in seq_along(d)) assign(names(d)[i], d[[i]])
            prob <- numeric(length(q))
            prob[Q == 0] <- plnorm(q[Q == 0], mu[Q == 0], sigma[Q == 
                0])
            qn0 <- Q != 0
            if (any(qn0)) {
                q <- q[qn0]
                mu <- mu[qn0]
                sigma <- sigma[qn0]
                Q <- Q[qn0]
                y <- log(q)
                w <- ((y - mu)/sigma)
                expnu <- exp(Q * w) * Q^-2
                prob[qn0] <- ifelse(Q > 0, pgamma(expnu, Q^-2), 
                  1 - pgamma(expnu, Q^-2))
            }
            if (!lower.tail) 
                prob <- 1 - prob
            if (log.p) 
                prob <- log(prob)
            ret[ind] <- prob
            ret
        }, d = function (x, mu = 0, sigma = 1, Q, log = FALSE) 
        {
            d <- dbase("gengamma", log = log, x = x, mu = mu, 
                sigma = sigma, Q = Q)
            for (i in seq_along(d)) assign(names(d)[i], d[[i]])
            logdens <- numeric(length(x))
            logdens[Q == 0] <- dlnorm(x[Q == 0], mu[Q == 0], 
                sigma[Q == 0], log = TRUE)
            qn0 <- Q != 0
            if (any(qn0)) {
                x <- x[qn0]
                mu <- mu[qn0]
                sigma <- sigma[qn0]
                Q <- Q[qn0]
                y <- log(x)
                w <- ((y - mu)/sigma)
                logdens[qn0] <- -log(sigma * x) + log(abs(Q)) + 
                  (Q^-2) * log(Q^-2) + Q^-2 * (Q * w - exp(Q * 
                  w)) - lgamma(Q^-2)
            }
            ret[ind] <- if (log) 
                logdens
            else exp(logdens)
            ret
        }, h = function (x, mu = 0, sigma = 1, Q) 
        {
            dgengamma(x = x, mu = mu, sigma = sigma, Q = Q)/pgengamma(q = x, 
                mu = mu, sigma = sigma, Q = Q, lower.tail = FALSE)
        }, H = function (x, mu = 0, sigma = 1, Q) 
        {
            -log(pgengamma(q = x, mu = mu, sigma = sigma, Q = Q, 
                lower.tail = FALSE))
        }, r = function (n, mu = 0, sigma = 1, Q) 
        {
            r <- rbase("gengamma", n = n, mu = mu, sigma = sigma, 
                Q = Q)
            for (i in seq_along(r)) assign(names(r)[i], r[[i]])
            ret[ind][Q == 0] <- rlnorm(n, mu, sigma)
            qn0 <- Q != 0
            if (any(qn0)) {
                mu <- mu[qn0]
                sigma <- sigma[qn0]
                Q <- Q[qn0]
                w <- log(Q^2 * rgamma(n, 1/Q^2, 1))/Q
                ret[ind][qn0] <- exp(mu + sigma * w)
            }
            ret
        }, DLd = NULL, DLS = NULL, deriv = FALSE), .Names = c("p", 
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(5.21117193821875, 
        1.92160189736387, -0.487730661951688, -0.874030861538891, 
        3.93462567398273, 1.57001791772887, -1.61769797372356, 
        -1.8179036954446, 6.48771820245477, 2.35191828720905, 
        0.642236649820181, 0.069841972366817, 0.651311082400112, 
        0.198116999792345, 0.576524528350984, 0.481576621484301
        ), .Dim = c(4L, 4L), .Dimnames = list(c("mu", "sigma", 
        "Q", "treatmentB"), c("est", "L95%", "U95%", "se"))), 
        res.t = structure(c(5.21117193821875, 0.653159159730618, 
        -0.487730661951688, -0.874030861538891, 3.93462567398273, 
        0.451087031861253, -1.61769797372356, -1.8179036954446, 
        6.48771820245477, 0.855231287599983, 0.642236649820181, 
        0.069841972366817, 0.651311082400112, 0.103099918908349, 
        0.576524528350984, 0.481576621484301), .Dim = c(4L, 4L
        ), .Dimnames = list(c("mu", "sigma", "Q", "treatmentB"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.424206126057206, 
        -0.0125255638576292, 0.333383485569834, -0.242724524078421, 
        -0.0125255638576292, 0.0106295932789082, -0.0223999223835656, 
        0.00505036297410679, 0.333383485569834, -0.0223999223835655, 
        0.332380531790324, -0.157226134014945, -0.242724524078421, 
        0.00505036297410677, -0.157226134014945, 0.231916042360234
        ), .Dim = c(4L, 4L), .Dimnames = list(c("mu", "sigma", 
        "Q", "treatmentB"), c("mu", "sigma", "Q", "treatmentB"
        ))), coefficients = structure(c(5.21117193821875, 0.653159159730618, 
        -0.487730661951688, -0.874030861538891), .Names = c("mu", 
        "sigma", "Q", "treatmentB")), npars = 4L, fixedpars = NULL, 
        optpars = 1:4, loglik = -397.080951231954, logliki = c(-6.03771197923026, 
        -6.37495652574073, -6.11997440577802, -6.00661965999216, 
        -5.7916495907954, -6.28994075820288, -7.2421831573377, 
        -6.20765589598809, -7.04598806157115, -6.12900503709617, 
        -5.3617728500262, -5.50062279263477, -5.41717439274378, 
        -5.77140478287843, -5.72441902213881, -6.77934688613754, 
        -6.30580622399556, -5.36944480886915, -5.78294489331151, 
        -6.22452342465585, -6.21611627190651, -5.98677530198722, 
        -7.36787855210965, -5.40325870716426, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -0.831022653767944, -0.831022653767944, 
        -0.831022653767944, -6.46052124697622, -6.39649748064032, 
        -5.84145162688254, -4.56532224830215, -4.97108894422627, 
        -5.67875120698601, -6.26935042102461, -4.71440306335802, 
        -5.56396728924497, -4.49566511568266, -5.49367628679983, 
        -5.52928335270901, -5.94864490306233, -5.47551061635069, 
        -5.99925524521468, -6.27954745072088, -7.05416039817559, 
        -7.72630124296078, -6.4150852517859, -4.65373379690519, 
        -5.27881393495389, -5.52928335270901, -4.62357110426624, 
        -6.62910799355069, -5.07324419396929, -6.23830851062642, 
        -5.14526810913079, -6.10677076326629, -7.14096117128968, 
        -5.19126422627693, -5.58097816357532, -4.62357110426624, 
        -4.5384960169327, -5.45709450681205, -4.97108894422627, 
        -6.24873211891568, -4.88956746041349, -5.0483827211409, 
        -1.26420555561377, -1.26420555561377, -1.26420555561377, 
        -1.26420555561377, -1.26420555561377, -1.26420555561377, 
        -1.26420555561377, -1.26420555561377, -1.26420555561377, 
        -1.26420555561377, -1.26420555561377, -1.26420555561377
        ), cl = 0.95, opt = structure(list(par = structure(c(5.21117193821875, 
        0.653159159730618, -0.487730661951688, -0.874030861538891
        ), .Names = c("mu", "sigma", "Q", "treatmentB")), value = 397.080951231954, 
            counts = structure(c(21L, 10L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(25.9608200821049, 
            -20.2138845963873, -21.1105021747926, 13.2991770556146, 
            -20.2138845963873, 127.786360593518, 25.8540586912659, 
            -6.41111029153763, -21.1105021747926, 25.8540586912659, 
            22.3866883004575, -7.48045069087766, 13.2991770556146, 
            -6.41111029153763, -7.48045069087766, 13.2991770698254
            ), .Dim = c(4L, 4L), .Dimnames = list(c("mu", "sigma", 
            "Q", "treatmentB"), c("mu", "sigma", "Q", "treatmentB"
            )))), .Names = c("par", "value", "counts", "convergence", 
        "message", "hessian")), BIC = 812.58258320786, m2LL = 794.161902463908), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg")), .Dim = c(6L, 
3L), .Dimnames = list(c("exp", "weibull", "lnorm", "gamma", "gompertz", 
"gengamma"), c("A", "B", "all")))
