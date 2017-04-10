PFS2.fit <-
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
    basepars = 1L, covpars = NULL, AIC = 494.769764292376, data = structure(list(
        Y = structure(c(24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 
        9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 
        85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 
        179, 107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 
        9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 
        85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 
        179, 107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 24, 19, 
        4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 
        12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 
        232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 
        71, 311, 9, 50, 45, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(49L, 
        6L), .Dimnames = list(c("1", "2", "3", "4", "5", "6", 
        "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", 
        "17", "18", "19", "20", "21", "22", "23", "24", "25", 
        "26", "27", "28", "29", "30", "31", "32", "33", "34", 
        "35", "36", "37", "38", "39", "40", "41", "42", "43", 
        "44", "45", "46", "47", "48", "49"), c("time", "status", 
        "start", "stop", "time1", "time2"))), m = structure(list(
            `survival::Surv(time, status)` = structure(c(24, 
            19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 
            11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 
            105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
            11, 52, 97, 96, 71, 311, 9, 50, 45, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
            `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "(weights)"), terms = quote(survival::Surv(time, status) ~ 
            1), row.names = c(NA, 49L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
        mml = structure(list(rate = structure(c(1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 1L), .Dimnames = list(
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
            "11", "12", "13", "14", "15", "16", "17", "18", "19", 
            "20", "21", "22", "23", "24", "25", "26", "27", "28", 
            "29", "30", "31", "32", "33", "34", "35", "36", "37", 
            "38", "39", "40", "41", "42", "43", "44", "45", "46", 
            "47", "48", "49"), "(Intercept)"), assign = 0L)), .Names = "rate")), .Names = c("Y", 
    "m", "mml")), datameans = numeric(0), N = 49L, events = 49L, 
    trisk = 2752, concat.formula = quote(survival::Surv(time, 
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
    "DLd", "DLS", "deriv")), res = structure(c(0.0178052325581394, 
    0.0134569748311099, 0.0235585122531791, 0.00254360443919587
    ), .Dim = c(1L, 4L), .Dimnames = list("rate", c("est", "L95%", 
    "U95%", "se"))), res.t = structure(c(-4.02826290094262, -4.30825773254402, 
    -3.74826806934121, 0.14285713095239), .Dim = c(1L, 4L), .Dimnames = list(
        "rate", c("est", "L95%", "U95%", "se"))), cov = structure(0.0204081598639482, .Dim = c(1L, 
    1L), .Dimnames = list("rate", "rate")), coefficients = -4.02826290094262, 
    npars = 1L, fixedpars = NULL, optpars = 1L, loglik = -246.384882146188, 
    logliki = structure(c(-4.45558848233796, -4.36656231954727, 
    -4.09948383117517, -5.41707104047749, -4.33095185443099, 
    -4.5624198776868, -4.08167859861704, -4.82949836605889, -4.15289952884959, 
    -4.13509429629145, -4.27753615675657, -4.18850999396587, 
    -4.95413499396586, -4.54461464512866, -4.54461464512866, 
    -4.22412045908215, -4.08167859861704, -4.24192569164029, 
    -4.25973092419843, -4.6514460404775, -4.82949836605889, -4.29534138931471, 
    -4.13509429629145, -4.08167859861704, -6.09366987768678, 
    -5.54170766838446, -5.32804487768679, -4.09948383117517, 
    -5.89781231954725, -8.97811755210536, -5.71975999396586, 
    -8.15907685443095, -5.5595129009426, -4.24192569164029, -4.54461464512866, 
    -4.0638733660589, -4.93632976140773, -4.86510883117517, -7.21539952884956, 
    -5.93342278466353, -4.22412045908215, -4.95413499396586, 
    -5.75537045908214, -5.737565226524, -5.29243441257051, -9.56569022652396, 
    -4.18850999396587, -4.91852452884959, -4.82949836605889), .Names = c("1", 
    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
    "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", 
    "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
    "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
    "43", "44", "45", "46", "47", "48", "49")), cl = 0.95, opt = structure(list(
        par = structure(-4.02826290094262, .Names = "rate"), 
        value = 246.384882146188, counts = structure(c(2L, 1L
        ), .Names = c("function", "gradient")), convergence = 0L, 
        message = NULL, hessian = structure(49.0000081666617, .Dim = c(1L, 
        1L), .Dimnames = list("rate", "rate"))), .Names = c("par", 
    "value", "counts", "convergence", "message", "hessian")), 
    BIC = 496.661584590486, m2LL = 492.769764292376), .Names = c("call", 
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
    "shape")), basepars = 1:2, covpars = NULL, AIC = 495.037508007023, 
    data = structure(list(Y = structure(c(24, 19, 4, 78, 17, 
    30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 
    15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
    2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 
    29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 
    278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
    96, 71, 311, 9, 50, 45, 24, 19, 4, 78, 17, 30, 3, 45, 7, 
    6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 
    85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 
    107, 11, 52, 97, 96, 71, 311, 9, 50, 45, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(49L, 
    6L), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
    "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
    "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
    "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", 
    "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", 
    "48", "49"), c("time", "status", "start", "stop", "time1", 
    "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
    19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 
    12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
    86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 
    9, 50, 45, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 
    2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
        `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
    "(weights)"), terms = quote(survival::Surv(time, status) ~ 
        1), row.names = c(NA, 49L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
        mml = structure(list(scale = structure(c(1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 1L), .Dimnames = list(
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
            "11", "12", "13", "14", "15", "16", "17", "18", "19", 
            "20", "21", "22", "23", "24", "25", "26", "27", "28", 
            "29", "30", "31", "32", "33", "34", "35", "36", "37", 
            "38", "39", "40", "41", "42", "43", "44", "45", "46", 
            "47", "48", "49"), "(Intercept)"), assign = 0L), 
            shape = NULL), .Names = c("scale", "shape"))), .Names = c("Y", 
    "m", "mml")), datameans = numeric(0), N = 49L, events = 49L, 
    trisk = 2752, concat.formula = quote(survival::Surv(time, 
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
    "DLd", "DLS", "deriv")), res = structure(c(0.870929390244855, 
    52.1103089975453, 0.703605447637358, 37.0849006231015, 1.0780445281362, 
    73.2234483089885, 0.0948011400545855, 9.04377479838009), .Dim = c(2L, 
    4L), .Dimnames = list(c("shape", "scale"), c("est", "L95%", 
    "U95%", "se"))), res.t = structure(c(-0.138194372872891, 
    3.9533627986204, -0.3515375236283, 3.61320989547889, 0.0751487778825189, 
    4.2935157017619, 0.108850546458115, 0.173550588594785), .Dim = c(2L, 
    4L), .Dimnames = list(c("shape", "scale"), c("est", "L95%", 
    "U95%", "se"))), cov = structure(c(0.0118484414642303, 0.00617145021935207, 
    0.00617145021935207, 0.0301198068015962), .Dim = c(2L, 2L
    ), .Dimnames = list(c("shape", "scale"), c("shape", "scale"
    ))), coefficients = structure(c(-0.138194372872891, 3.9533627986204
    ), .Names = c("shape", "scale")), npars = 2L, fixedpars = NULL, 
    optpars = 1:2, loglik = -245.518754003511, logliki = structure(c(-4.50052213016329, 
    -4.37665665472143, -3.86713756520077, -5.56451080518962, 
    -4.32395580587992, -4.63851738082711, -3.80631123913219, 
    -4.9526821048565, -4.00651502603274, -3.96475100957844, -4.24024878421131, 
    -4.08154128424745, -5.08943979346972, -4.61615480482391, 
    -4.61615480482391, -4.148816104649, -3.80631123913219, -4.18035993347753, 
    -4.21078672900579, -4.74724305875305, -4.9526821048565, -4.26886787234402, 
    -3.96475100957844, -3.80631123913219, -6.20244617335859, 
    -5.68603849076262, -5.47629680274526, -3.86713756520077, 
    -6.02272986111809, -8.605701884931, -5.85615626541788, -7.95589425033618, 
    -5.70322658719727, -4.18035993347753, -4.61615480482391, 
    -3.72921885670218, -5.0701948299142, -4.99226396849048, -7.18008470196673, 
    -6.05566433686976, -4.148816104649, -5.08943979346972, -5.88973703764063, 
    -5.87296403799842, -5.44065089692232, -9.06126713165153, 
    -4.08154128424745, -5.05085781467629, -4.9526821048565), .Names = c("1", 
    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
    "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", 
    "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
    "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
    "43", "44", "45", "46", "47", "48", "49")), cl = 0.95, opt = structure(list(
        par = structure(c(-0.138194372872891, 3.9533627986204
        ), .Names = c("shape", "scale")), value = 245.518754003511, 
        counts = structure(c(2L, 1L), .Names = c("function", 
        "gradient")), convergence = 0L, message = NULL, hessian = structure(c(94.4828494249173, 
        -19.359227821392, -19.359227821392, 37.167386834815), .Dim = c(2L, 
        2L), .Dimnames = list(c("shape", "scale"), c("shape", 
        "scale")))), .Names = c("par", "value", "counts", "convergence", 
    "message", "hessian")), BIC = 498.821148603244, m2LL = 491.037508007023), .Names = c("call", 
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
    "sdlog")), basepars = 1:2, covpars = NULL, AIC = 493.45245177831, 
    data = structure(list(Y = structure(c(24, 19, 4, 78, 17, 
    30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 
    15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 
    2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 
    29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 
    278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
    96, 71, 311, 9, 50, 45, 24, 19, 4, 78, 17, 30, 3, 45, 7, 
    6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 
    85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 
    107, 11, 52, 97, 96, 71, 311, 9, 50, 45, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
    Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(49L, 
    6L), .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", 
    "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", 
    "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
    "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", 
    "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", 
    "48", "49"), c("time", "status", "start", "stop", "time1", 
    "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
    19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 
    12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
    86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 
    9, 50, 45, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 
    2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
        `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
    "(weights)"), terms = quote(survival::Surv(time, status) ~ 
        1), row.names = c(NA, 49L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
        mml = structure(list(meanlog = structure(c(1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 1L), .Dimnames = list(
            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
            "11", "12", "13", "14", "15", "16", "17", "18", "19", 
            "20", "21", "22", "23", "24", "25", "26", "27", "28", 
            "29", "30", "31", "32", "33", "34", "35", "36", "37", 
            "38", "39", "40", "41", "42", "43", "44", "45", "46", 
            "47", "48", "49"), "(Intercept)"), assign = 0L), 
            sdlog = NULL), .Names = c("meanlog", "sdlog"))), .Names = c("Y", 
    "m", "mml")), datameans = numeric(0), N = 49L, events = 49L, 
    trisk = 2752, concat.formula = quote(survival::Surv(time, 
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
    "DLS", "deriv")), res = structure(c(3.32602900644194, 1.28331329338329, 
    2.96670788705317, 1.05280613962669, 3.68535012583071, 1.56428894835114, 
    0.183330470469381, 0.129634133795963), .Dim = c(2L, 4L), .Dimnames = list(
        c("meanlog", "sdlog"), c("est", "L95%", "U95%", "se"))), 
    res.t = structure(c(3.32602900644194, 0.249445243964171, 
    2.96670788705317, 0.0514591132841958, 3.68535012583071, 0.447431374644146, 
    0.183330470469381, 0.101015188157367), .Dim = c(2L, 4L), .Dimnames = list(
        c("meanlog", "sdlog"), c("est", "L95%", "U95%", "se"))), 
    cov = structure(c(0.0336100614025246, -2.43687281944791e-12, 
    -2.43687281944791e-12, 0.0102040682384682), .Dim = c(2L, 
    2L), .Dimnames = list(c("meanlog", "sdlog"), c("meanlog", 
    "sdlog"))), coefficients = structure(c(3.32602900644194, 
    0.249445243964171), .Names = c("meanlog", "sdlog")), npars = 2L, 
    fixedpars = NULL, optpars = 1:2, loglik = -244.726225889155, 
    logliki = structure(c(-4.35308547493159, -4.15703053064808, 
    -3.69700203964712, -5.84760927193494, -4.07533211061698, 
    -4.57129659650742, -3.77328251176353, -5.0451809217217, -3.69257316177101, 
    -3.67481718088905, -3.95072002113689, -3.75245784388455, 
    -5.23830352210584, -4.53619662638977, -4.53619662638977, 
    -3.82781159119273, -3.77328251176353, -3.8680848352784, -3.90919223079987, 
    -4.73969742619834, -5.0451809217217, -3.99237891269244, -3.67481718088905, 
    -3.77328251176353, -6.54069386229849, -5.98957973647494, 
    -5.74123134502602, -3.69700203964712, -6.35771638405863, 
    -8.40428369199691, -6.17997419990746, -7.9805419581486, -6.00924742760744, 
    -3.8680848352784, -4.53619662638977, -3.96612022712438, -5.21162816946508, 
    -5.10193093805151, -7.40764396579846, -6.39190709113562, 
    -3.82781159119273, -5.23830352210584, -6.21647300758501, 
    -6.19828571002947, -5.69741819230519, -8.67703968193219, 
    -3.75245784388455, -5.18466035735159, -5.0451809217217), .Names = c("1", 
    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
    "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", 
    "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
    "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
    "43", "44", "45", "46", "47", "48", "49")), cl = 0.95, opt = structure(list(
        par = structure(c(3.32602900644194, 0.249445243964171
        ), .Names = c("meanlog", "sdlog")), value = 244.726225889155, 
        counts = structure(c(7L, 1L), .Names = c("function", 
        "gradient")), convergence = 0L, message = NULL, hessian = structure(c(29.7529953314779, 
        7.105427357601e-09, 7.105427357601e-09, 98.0001286379206
        ), .Dim = c(2L, 2L), .Dimnames = list(c("meanlog", "sdlog"
        ), c("meanlog", "sdlog")))), .Names = c("par", "value", 
    "counts", "convergence", "message", "hessian")), BIC = 497.236092374531, 
    m2LL = 489.45245177831), .Names = c("call", "dlist", "aux", 
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
        "shape")), basepars = 1:2, covpars = NULL, AIC = 495.72632360256, 
        data = structure(list(Y = structure(c(24, 19, 4, 78, 
        17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 
        35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 50, 45, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 24, 19, 4, 78, 
        17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 
        35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 50, 45, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 
        9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 
        85, 73, 4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 
        179, 107, 11, 52, 97, 96, 71, 311, 9, 50, 45, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf), .Dim = c(49L, 6L), .Dimnames = list(c("1", 
        "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
        "13", "14", "15", "16", "17", "18", "19", "20", "21", 
        "22", "23", "24", "25", "26", "27", "28", "29", "30", 
        "31", "32", "33", "34", "35", "36", "37", "38", "39", 
        "40", "41", "42", "43", "44", "45", "46", "47", "48", 
        "49"), c("time", "status", "start", "stop", "time1", 
        "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
        19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
        3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 
        95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
        96, 71, 311, 9, 50, 45, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1), .Dim = c(49L, 2L), .Dimnames = list(NULL, c("time", 
        "status")), type = "right", class = "Surv"), `(weights)` = c(1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "(weights)"), terms = quote(survival::Surv(time, status) ~ 
            1), row.names = c(NA, 49L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 
            1L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49"), 
                "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 49L, events = 49L, trisk = 2752, concat.formula = quote(survival::Surv(time, 
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
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(0.839796027285067, 
        0.0149524648898816, 0.595838985359397, 0.00944399598770742, 
        1.18363750068886, 0.0236738989061573, 0.147047801153594, 
        0.0035054788389063), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.174596241292094, -4.20287911715856, 
        -0.517784807209207, -4.66237608460993, 0.168592324625019, 
        -3.7433821497072, 0.175099424593585, 0.234441536209758
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0306598084930045, 
        0.0306604158195601, 0.0306604158195601, 0.0549628339003913
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(-0.174596241292094, 
        -4.20287911715856), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -245.86316180128, 
        logliki = structure(c(-4.51298924065771, -4.40080088896382, 
        -3.92689295774015, -5.50924755758555, -4.35307717057166, 
        -4.63845251340326, -3.86585268196462, -4.9276966078677, 
        -4.06140302483104, -4.02175500863613, -4.27711521106207, 
        -4.13156952442199, -5.05552634933241, -4.61806888525373, 
        -4.61806888525373, -4.19362269682226, -3.86585268196462, 
        -4.22251472997727, -4.25029035462332, -4.7379103891577, 
        -4.9276966078677, -4.30312060805317, -4.02175500863613, 
        -3.86585268196462, -6.14102319467714, -5.62768313049357, 
        -5.42387181838252, -3.92689295774015, -5.96058498456748, 
        -8.70334573291165, -5.79502656800479, -7.98655405390526, 
        -5.64450934741855, -4.22251472997727, -4.61806888525373, 
        -3.78594309595852, -5.0374630299457, -4.96456802533416, 
        -7.15252426356644, -5.99351272049192, -4.19362269682226, 
        -5.05552634933241, -5.82826920127427, -5.8116565767329, 
        -5.38951649207251, -9.2147474420222, -4.13156952442199, 
        -5.01933810549278, -4.9276966078677), .Names = c("1", 
        "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
        "13", "14", "15", "16", "17", "18", "19", "20", "21", 
        "22", "23", "24", "25", "26", "27", "28", "29", "30", 
        "31", "32", "33", "34", "35", "36", "37", "38", "39", 
        "40", "41", "42", "43", "44", "45", "46", "47", "48", 
        "49")), cl = 0.95, opt = structure(list(par = structure(c(-0.174596241292094, 
        -4.20287911715856), .Names = c("shape", "rate")), value = 245.86316180128, 
            counts = structure(c(15L, 6L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(73.7668170884831, 
            -41.1500122012853, -41.1500122012853, 41.1491970950806
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "rate"), c("shape", "rate")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 499.509964198782, m2LL = 491.72632360256), .Names = c("call", 
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
        covpars = NULL, AIC = 494.905612995564, data = structure(list(
            Y = structure(c(24, 19, 4, 78, 17, 30, 3, 45, 7, 
            6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 
            6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
            29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 
            9, 50, 45, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 
            29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
            4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 
            107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 24, 19, 
            4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
            3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 
            278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 
            52, 97, 96, 71, 311, 9, 50, 45, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf), .Dim = c(49L, 6L), .Dimnames = list(
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                "10", "11", "12", "13", "14", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", 
                "34", "35", "36", "37", "38", "39", "40", "41", 
                "42", "43", "44", "45", "46", "47", "48", "49"
                ), c("time", "status", "start", "stop", "time1", 
                "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
            19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 
            11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 
            105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
            11, 52, 97, 96, 71, 311, 9, 50, 45, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = c(NA, 49L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 
            1L), .Dimnames = list(c("1", "2", "3", "4", "5", 
            "6", "7", "8", "9", "10", "11", "12", "13", "14", 
            "15", "16", "17", "18", "19", "20", "21", "22", "23", 
            "24", "25", "26", "27", "28", "29", "30", "31", "32", 
            "33", "34", "35", "36", "37", "38", "39", "40", "41", 
            "42", "43", "44", "45", "46", "47", "48", "49"), 
                "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 49L, events = 49L, trisk = 2752, concat.formula = quote(survival::Surv(time, 
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
        "r", "DLd", "DLS", "deriv")), res = structure(c(-0.003091111248196, 
        0.0215877224713591, -0.0077616835176369, 0.0147043537968429, 
        0.0015794610212449, 0.0316933180430195, 0.00238298882340787, 
        0.00422930113506074), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.003091111248196, -3.83563052999861, 
        -0.0077616835176369, -4.21961165239311, 0.0015794610212449, 
        -3.45164940760412, 0.00238298882340787, 0.195912335850706
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(5.67863573248682e-06, 
        -0.000319476561230048, -0.000319476561230048, 0.0383816433384796
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(-0.003091111248196, 
        -3.83563052999861), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -245.452806497782, 
        logliki = c(-4.40917084542206, -4.29271597539166, -3.93381421789196, 
        -5.57296325615569, -4.24569491574569, -4.54687410725715, 
        -3.90936767199152, -4.88164061709909, -4.00675920802476, 
        -3.98250978280159, -4.1746879793326, -4.0550623548457, 
        -5.03335655712793, -4.52407671603624, -4.52407671603624, 
        -4.10310590401275, -3.90936767199152, -4.12703082984314, 
        -4.15089145546569, -4.65995250510783, -4.88164061709909, 
        -4.1984205992836, -3.98250978280159, -3.90936767199152, 
        -6.29859152685948, -5.71206467494108, -5.47203528295359, 
        -3.93381421789196, -6.09582545297731, -8.72149991393085, 
        -5.90643273558872, -8.12744759108803, -5.73172978507815, 
        -4.12703082984314, -4.52407671603624, -3.88485501193636, 
        -5.01185472930108, -4.92527567446769, -7.35675064881503, 
        -6.13312036407935, -4.10310590401275, -5.03335655712793, 
        -5.94470439289178, -5.92559336222372, -5.43129248785437, 
        -9.11029180332828, -4.0550623548457, -4.99029590385353, 
        -4.88164061709909), cl = 0.95, opt = structure(list(par = structure(c(-0.003091111248196, 
        -3.83563052999861), .Names = c("shape", "rate")), value = 245.452806497782, 
            counts = structure(c(33L, 5L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(331190.01492367, 
            2756.720084871, 2756.720084871, 49.0001805395051), .Dim = c(2L, 
            2L), .Dimnames = list(c("shape", "rate"), c("shape", 
            "rate")))), .Names = c("par", "value", "counts", 
        "convergence", "message", "hessian")), BIC = 498.689253591785, 
        m2LL = 490.905612995564), .Names = c("call", "dlist", 
    "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
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
        covpars = NULL, AIC = 494.921521490201, data = structure(list(
            Y = structure(c(24, 19, 4, 78, 17, 30, 3, 45, 7, 
            6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 
            6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
            29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 
            9, 50, 45, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 
            29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
            4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 
            107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 24, 19, 
            4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
            3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 
            278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 
            52, 97, 96, 71, 311, 9, 50, 45, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf), .Dim = c(49L, 6L), .Dimnames = list(
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                "10", "11", "12", "13", "14", "15", "16", "17", 
                "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", 
                "34", "35", "36", "37", "38", "39", "40", "41", 
                "42", "43", "44", "45", "46", "47", "48", "49"
                ), c("time", "status", "start", "stop", "time1", 
                "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
            19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 
            11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 
            105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
            11, 52, 97, 96, 71, 311, 9, 50, 45, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 2L), .Dimnames = list(
                NULL, c("time", "status")), type = "right", class = "Surv"), 
                `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = c(NA, 49L), covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(mu = structure(c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(49L, 1L
            ), .Dimnames = list(c("1", "2", "3", "4", "5", "6", 
            "7", "8", "9", "10", "11", "12", "13", "14", "15", 
            "16", "17", "18", "19", "20", "21", "22", "23", "24", 
            "25", "26", "27", "28", "29", "30", "31", "32", "33", 
            "34", "35", "36", "37", "38", "39", "40", "41", "42", 
            "43", "44", "45", "46", "47", "48", "49"), "(Intercept)"), assign = 0L), 
                sigma = NULL, Q = NULL), .Names = c("mu", "sigma", 
            "Q"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 49L, events = 49L, trisk = 2752, concat.formula = quote(survival::Surv(time, 
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
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(3.53844525837623, 
        1.25303921883852, 0.332906057933205, 2.87393626800673, 
        1.00787729317341, -0.554525399395045, 4.20295424874573, 
        1.55783575498937, 1.22033751526146, 0.339041429133933, 
        0.139195745614097, 0.45277947162713), .Dim = 3:4, .Dimnames = list(
            c("mu", "sigma", "Q"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(3.53844525837623, 0.225571975375031, 
        0.332906057933205, 2.87393626800673, 0.00784642927623166, 
        -0.554525399395045, 4.20295424874573, 0.443297521473831, 
        1.22033751526146, 0.339041429133933, 0.111086503535877, 
        0.45277947162713), .Dim = 3:4, .Dimnames = list(c("mu", 
        "sigma", "Q"), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.11494909066918, 
        -0.0143933585538802, 0.130023418316939, -0.0143933585538802, 
        0.0123402112678263, -0.0192970295758503, 0.130023418316939, 
        -0.0192970295758502, 0.205009249926943), .Dim = c(3L, 
        3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
        "sigma", "Q"))), coefficients = structure(c(3.53844525837623, 
        0.225571975375031, 0.332906057933205), .Names = c("mu", 
        "sigma", "Q")), npars = 3L, fixedpars = NULL, optpars = 1:3, 
        loglik = -244.460760745101, logliki = c(-4.37186789444736, 
        -4.20485929574508, -3.76988890800989, -5.73999929747029, 
        -4.13589194745474, -4.56086603614497, -3.79704073652259, 
        -4.98386819026499, -3.80448552027127, -3.78271088734614, 
        -4.0310451802012, -3.86142900406352, -5.16128999499139, 
        -4.53022638075989, -4.53022638075989, -3.92703835565902, 
        -3.79704073652259, -3.9613160175449, -3.99606509858758, 
        -4.70918145788905, -4.98386819026499, -4.06608559651723, 
        -3.78271088734614, -3.79704073652259, -6.43251160674032, 
        -5.87891768765277, -5.63691058741013, -3.76988890800989, 
        -6.24620167140035, -8.46848793594573, -6.06760455665894, 
        -7.98370664855367, -5.89828269928536, -3.9613160175449, 
        -4.53022638075989, -3.88166272172877, -5.13660997328539, 
        -5.0357019702287, -7.34859208800456, -6.28082657634409, 
        -3.92703835565902, -5.16128999499139, -6.10408656980508, 
        -6.08589518798955, -5.59470288392584, -8.78724401998193, 
        -3.86142900406352, -5.1117157775586, -4.98386819026499
        ), cl = 0.95, opt = structure(list(par = structure(c(3.53844525837623, 
        0.225571975375031, 0.332906057933205), .Names = c("mu", 
        "sigma", "Q")), value = 244.460760745101, counts = structure(c(32L, 
        6L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(31.2080890267907, 
            6.38929832774693, -19.1917581133794, 6.38929832774693, 
            96.3305513010937, 5.01506682581976, -19.1917581133794, 
            5.01506682581976, 17.5219112676928), .Dim = c(3L, 
            3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
            "sigma", "Q")))), .Names = c("par", "value", "counts", 
        "convergence", "message", "hessian")), BIC = 500.596982384533, 
        m2LL = 488.921521490201), .Names = c("call", "dlist", 
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
        basepars = 1L, covpars = NULL, AIC = 518.262606963851, 
        data = structure(list(Y = structure(c(126, 119, 70, 10, 
        24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
        350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
        33, 53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
        14, 18, 33, 18, 14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 
        78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
        126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
        47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 
        103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 
        37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
            c("50", "51", "52", "53", "54", "55", "56", "57", 
            "58", "59", "60", "61", "62", "63", "64", "65", "66", 
            "67", "68", "69", "70", "71", "72", "73", "74", "75", 
            "76", "77", "78", "79", "80", "81", "82", "83", "84", 
            "85", "86", "87", "88", "89", "90", "91", "92", "93", 
            "94", "95", "96", "97", "98", "99"), c("time", "status", 
            "start", "stop", "time1", "time2"))), m = structure(list(
            `survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
            120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1), .Dim = c(50L, 2L), .Dimnames = list(NULL, c("time", 
            "status")), type = "right", class = "Surv"), `(weights)` = c(1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "(weights)"), terms = quote(survival::Surv(time, status) ~ 
            1), row.names = 50:99, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("50", "51", "52", "53", "54", 
            "55", "56", "57", "58", "59", "60", "61", "62", "63", 
            "64", "65", "66", "67", "68", "69", "70", "71", "72", 
            "73", "74", "75", "76", "77", "78", "79", "80", "81", 
            "82", "83", "84", "85", "86", "87", "88", "89", "90", 
            "91", "92", "93", "94", "95", "96", "97", "98", "99"
            ), "(Intercept)"), assign = 0L)), .Names = "rate")), .Names = c("Y", 
        "m", "mml")), datameans = numeric(0), N = 50L, events = 50L, 
        trisk = 3212, concat.formula = quote(survival::Surv(time, 
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
        "r", "DLd", "DLS", "deriv")), res = structure(c(0.0155666251423606, 
        0.0117982175691362, 0.0205386802627436, 0.00220145305715694
        ), .Dim = c(1L, 4L), .Dimnames = list("rate", c("est", 
        "L95%", "U95%", "se"))), res.t = structure(c(-4.16262607049326, 
        -4.43980681238327, -3.88544532860325, 0.141421344512643
        ), .Dim = c(1L, 4L), .Dimnames = list("rate", c("est", 
        "L95%", "U95%", "se"))), cov = structure(0.0199999966837635, .Dim = c(1L, 
        1L), .Dimnames = list("rate", "rate")), coefficients = -4.16262607049326, 
        npars = 1L, fixedpars = NULL, optpars = 1L, loglik = -258.131303481925, 
        logliki = structure(c(-6.1240208384307, -6.01505446243417, 
        -5.2522898304585, -4.31829232191687, -4.53622507390992, 
        -5.08105695389254, -5.81268833558348, -4.39612544762867, 
        -4.97209057789601, -4.27159244648979, -4.90982407732657, 
        -4.94095732761129, -5.37682283159739, -4.89425745218421, 
        -5.43908933216683, -5.82825496072584, -7.40048410010426, 
        -9.61094487031946, -6.04618771271889, -4.36499219734395, 
        -4.7385912007606, -4.94095732761129, -4.34942557220159, 
        -6.43535334127791, -4.59849157447936, -5.7659884601564, 
        -4.64519144990644, -5.57918895844807, -7.63398347723967, 
        -4.67632470019116, -4.98765720303837, -4.34942557220159, 
        -4.30272569677451, -4.87869082704185, -4.53622507390992, 
        -5.78155508529876, -4.48952519848284, -4.582924949337, 
        -4.7385912007606, -4.94095732761129, -4.42725869791339, 
        -4.27159244648979, -4.44282532305575, -4.38055882248631, 
        -4.44282532305575, -4.67632470019116, -4.44282532305575, 
        -4.38055882248631, -6.09288758814597, -6.03062108757653
        ), .Names = c("50", "51", "52", "53", "54", "55", "56", 
        "57", "58", "59", "60", "61", "62", "63", "64", "65", 
        "66", "67", "68", "69", "70", "71", "72", "73", "74", 
        "75", "76", "77", "78", "79", "80", "81", "82", "83", 
        "84", "85", "86", "87", "88", "89", "90", "91", "92", 
        "93", "94", "95", "96", "97", "98", "99")), cl = 0.95, 
        opt = structure(list(par = structure(-4.16262607049326, .Names = "rate"), 
            value = 258.131303481925, counts = structure(c(2L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(50.0000082905925, .Dim = c(1L, 
            1L), .Dimnames = list("rate", "rate"))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 520.174629969279, m2LL = 516.262606963851), .Names = c("call", 
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
        covpars = NULL, AIC = 519.491770310094, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 
            33, 18, 14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 59, 106, 
            15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
            13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 
            12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 
            18, 33, 18, 14, 124, 120, 126, 119, 70, 10, 24, 59, 
            106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 
            121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 
            53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
            14, 18, 33, 18, 14, 124, 120, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
            120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1), .Dim = c(50L, 2L), .Dimnames = list(NULL, c("time", 
            "status")), type = "right", class = "Surv"), `(weights)` = c(1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 50:99, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(scale = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("50", "51", "52", "53", "54", 
            "55", "56", "57", "58", "59", "60", "61", "62", "63", 
            "64", "65", "66", "67", "68", "69", "70", "71", "72", 
            "73", "74", "75", "76", "77", "78", "79", "80", "81", 
            "82", "83", "84", "85", "86", "87", "88", "89", "90", 
            "91", "92", "93", "94", "95", "96", "97", "98", "99"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("scale", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 50L, trisk = 3212, concat.formula = quote(survival::Surv(time, 
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
        "r", "DLd", "DLS", "deriv")), res = structure(c(1.0993863896788, 
        66.8204441152273, 0.894628602008735, 51.1498992669821, 
        1.35100803964593, 87.2918972616318, 0.115605302956009, 
        9.11122226246083), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "scale"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(0.0947521965959219, 4.20200908334812, 
        -0.111346616653728, 3.93476052303073, 0.300851009845572, 
        4.4692576436655, 0.105154387976172, 0.13635381181767), .Dim = c(2L, 
        4L), .Dimnames = list(c("shape", "scale"), c("est", "L95%", 
        "U95%", "se"))), cov = structure(c(0.0110574453106434, 
        0.00475523872139769, 0.00475523872139769, 0.0185923619972085
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "scale"
        ), c("shape", "scale"))), coefficients = structure(c(0.0947521965959219, 
        4.20200908334812), .Names = c("shape", "scale")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -257.745885155047, 
        logliki = structure(c(-6.05256361957473, -5.93592511172606, 
        -5.15507150399381, -4.41994364333786, -4.53344210333169, 
        -4.99173531014309, -5.72218117932831, -4.4492437493859, 
        -4.89122924631207, -4.41519847308416, -4.83524420635532, 
        -4.86309358966458, -5.27727534410842, -4.82143250837805, 
        -5.33930378987206, -5.73848095793052, -7.47911179334137, 
        -10.1176281069006, -5.96914584136337, -4.43529671192679, 
        -4.68813387425624, -4.86309358966458, -4.42932423188863, 
        -6.39103111854202, -4.57803454963402, -5.67343305911812, 
        -4.61342326594616, -5.48087117951062, -7.74943770482941, 
        -4.63779293493796, -4.90539922674161, -4.42932423188863, 
        -4.41686239206989, -4.80769940995344, -4.53344210333169, 
        -5.68965691843797, -4.50241870393151, -4.56658577971308, 
        -4.68813387425624, -4.86309358966458, -4.46535039303589, 
        -4.41519847308416, -4.47407105470287, -4.44196601100907, 
        -4.47407105470287, -4.63779293493796, -4.47407105470287, 
        -4.44196601100907, -6.01913483555725, -5.95252480393005
        ), .Names = c("50", "51", "52", "53", "54", "55", "56", 
        "57", "58", "59", "60", "61", "62", "63", "64", "65", 
        "66", "67", "68", "69", "70", "71", "72", "73", "74", 
        "75", "76", "77", "78", "79", "80", "81", "82", "83", 
        "84", "85", "86", "87", "88", "89", "90", "91", "92", 
        "93", "94", "95", "96", "97", "98", "99")), cl = 0.95, 
        opt = structure(list(par = structure(c(0.0947521965959219, 
        4.20200908334812), .Names = c("shape", "scale")), value = 257.745885155047, 
            counts = structure(c(2L, 1L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(101.613303475209, 
            -25.9889257409571, -25.9889257409571, 60.4325338641548
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "scale"), c("shape", "scale")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 523.315816320951, m2LL = 515.491770310094), .Names = c("call", 
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
        covpars = NULL, AIC = 513.95097901658, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 
            33, 18, 14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 59, 106, 
            15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
            13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 
            12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 
            18, 33, 18, 14, 124, 120, 126, 119, 70, 10, 24, 59, 
            106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 
            121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 
            53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
            14, 18, 33, 18, 14, 124, 120, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
            120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1), .Dim = c(50L, 2L), .Dimnames = list(NULL, c("time", 
            "status")), type = "right", class = "Surv"), `(weights)` = c(1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 50:99, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(meanlog = structure(c(1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("50", "51", "52", "53", "54", 
            "55", "56", "57", "58", "59", "60", "61", "62", "63", 
            "64", "65", "66", "67", "68", "69", "70", "71", "72", 
            "73", "74", "75", "76", "77", "78", "79", "80", "81", 
            "82", "83", "84", "85", "86", "87", "88", "89", "90", 
            "91", "92", "93", "94", "95", "96", "97", "98", "99"
            ), "(Intercept)"), assign = 0L), sdlog = NULL), .Names = c("meanlog", 
            "sdlog"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 50L, trisk = 3212, concat.formula = quote(survival::Surv(time, 
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
        "DLd", "DLS", "deriv")), res = structure(c(3.72245834704314, 
        0.958978062735419, 3.45664807411655, 0.7882946407991, 
        3.98826861996974, 1.16661826328736, 0.135619978235964, 
        0.0958977433554144), .Dim = c(2L, 4L), .Dimnames = list(
            c("meanlog", "sdlog"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(3.72245834704314, -0.0418870795058872, 
        3.45664807411655, -0.23788334936752, 3.98826861996974, 
        0.154109190355746, 0.135619978235964, 0.0999999343904412
        ), .Dim = c(2L, 4L), .Dimnames = list(c("meanlog", "sdlog"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0183927784967234, 
        0, 0, 0.00999998687809255), .Dim = c(2L, 2L), .Dimnames = list(
            c("meanlog", "sdlog"), c("meanlog", "sdlog"))), coefficients = structure(c(3.72245834704314, 
        -0.0418870795058872), .Names = c("meanlog", "sdlog")), 
        npars = 2L, fixedpars = NULL, optpars = 1:2, loglik = -254.97548950829, 
        logliki = structure(c(-6.3878388046571, -6.26322908555782, 
        -5.27599422871094, -4.2757406823379, -4.21624259011502, 
        -5.02313817364515, -6.0218994695458, -4.14457334460926, 
        -4.85675348609117, -4.53891958073632, -4.76028130519659, 
        -4.80861188775205, -5.45247311434972, -4.73606370071559, 
        -5.53833418612069, -6.04094479129999, -7.63279969854096, 
        -9.21435587564216, -6.29919761418683, -4.17045359019111, 
        -4.4947335800851, -4.80861188775205, -4.19464045417089, 
        -6.72539690267209, -4.29205914514537, -5.96426113860227, 
        -4.35628228470078, -5.72585684935327, -7.82736110560694, 
        -4.40131651657452, -4.88073771096108, -4.19464045417089, 
        -4.33908659390925, -4.71182288649099, -4.21624259011502, 
        -5.98355830274735, -4.17145299328886, -4.27184340042776, 
        -4.4947335800851, -4.80861188775205, -4.1401925955059, 
        -4.53891958073632, -4.1438579585164, -4.15427107337213, 
        -4.1438579585164, -4.40131651657452, -4.1438579585164, 
        -4.15427107337213, -6.35259879084862, -6.28125052771751
        ), .Names = c("50", "51", "52", "53", "54", "55", "56", 
        "57", "58", "59", "60", "61", "62", "63", "64", "65", 
        "66", "67", "68", "69", "70", "71", "72", "73", "74", 
        "75", "76", "77", "78", "79", "80", "81", "82", "83", 
        "84", "85", "86", "87", "88", "89", "90", "91", "92", 
        "93", "94", "95", "96", "97", "98", "99")), cl = 0.95, 
        opt = structure(list(par = structure(c(3.72245834704314, 
        -0.0418870795058872), .Names = c("meanlog", "sdlog")), 
            value = 254.97548950829, counts = structure(c(7L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(54.369164516288, 
            0, 0, 100.000131219247), .Dim = c(2L, 2L), .Dimnames = list(
                c("meanlog", "sdlog"), c("meanlog", "sdlog")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 517.775025027437, m2LL = 509.95097901658), .Names = c("call", 
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
        covpars = NULL, AIC = 518.532773730943, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 
            33, 18, 14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 59, 106, 
            15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
            13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 
            12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 
            18, 33, 18, 14, 124, 120, 126, 119, 70, 10, 24, 59, 
            106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 
            121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 
            53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
            14, 18, 33, 18, 14, 124, 120, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
            120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1), .Dim = c(50L, 2L), .Dimnames = list(NULL, c("time", 
            "status")), type = "right", class = "Surv"), `(weights)` = c(1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 50:99, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("50", "51", "52", "53", "54", 
            "55", "56", "57", "58", "59", "60", "61", "62", "63", 
            "64", "65", "66", "67", "68", "69", "70", "71", "72", 
            "73", "74", "75", "76", "77", "78", "79", "80", "81", 
            "82", "83", "84", "85", "86", "87", "88", "89", "90", 
            "91", "92", "93", "94", "95", "96", "97", "98", "99"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 50L, trisk = 3212, concat.formula = quote(survival::Surv(time, 
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
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(1.27680571112556, 
        0.0198754750223382, 0.897592203409452, 0.0129372963610439, 
        1.81622881501256, 0.030534548822202, 0.229569228162093, 
        0.00435414992372483), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(0.244361420702749, -3.91826871823235, 
        -0.108039430292592, -4.3476409482896, 0.59676227169809, 
        -3.4888964881751, 0.179799656409523, 0.219071489804956
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0323279164449825, 
        0.0323280426163903, 0.0323280426163903, 0.0479923176453631
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(0.244361420702749, 
        -3.91826871823235), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -257.266386865472, 
        logliki = structure(c(-6.06452435212021, -5.94121780235362, 
        -5.11420045663762, -4.46031099789304, -4.49623290179433, 
        -4.94289232627951, -5.7148587293944, -4.44745331541825, 
        -4.83872282557223, -4.49941423432366, -4.78137720410087, 
        -4.8098283929228, -5.24325011856654, -4.76732943298315, 
        -5.30890884862504, -5.73213507066191, -7.55556273259784, 
        -10.2338318570286, -5.97635521708276, -4.44731349616019, 
        -4.63479482696399, -4.8098283929228, -4.44959429975585, 
        -6.42125353013233, -4.53306501333361, -5.66317942560266, 
        -4.56451740732188, -5.45896150530687, -7.83441985436732, 
        -4.58696236549252, -4.85332565144005, -4.44959429975585, 
        -4.46959991533192, -4.75340699839062, -4.49623290179433, 
        -5.68038042850424, -4.47356872882033, -4.52325630991795, 
        -4.63479482696399, -4.8098283929228, -4.45255839267083, 
        -4.49941423432366, -4.45661209230339, -4.44667546125045, 
        -4.45661209230339, -4.58696236549252, -4.45661209230339, 
        -4.44667546125045, -6.02920238794018, -5.95877689807503
        ), .Names = c("50", "51", "52", "53", "54", "55", "56", 
        "57", "58", "59", "60", "61", "62", "63", "64", "65", 
        "66", "67", "68", "69", "70", "71", "72", "73", "74", 
        "75", "76", "77", "78", "79", "80", "81", "82", "83", 
        "84", "85", "86", "87", "88", "89", "90", "91", "92", 
        "93", "94", "95", "96", "97", "98", "99")), cl = 0.95, 
        opt = structure(list(par = structure(c(0.244361420702749, 
        -3.91826871823235), .Names = c("shape", "rate")), value = 257.266386865472, 
            counts = structure(c(15L, 6L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(94.7735627079282, 
            -63.840296206763, -63.840296206763, 63.8400470478473
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "rate"), c("shape", "rate")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 522.3568197418, m2LL = 514.532773730943), .Names = c("call", 
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
        covpars = NULL, AIC = 520.259445229427, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 
            33, 18, 14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 59, 106, 
            15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
            13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 
            12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 
            18, 33, 18, 14, 124, 120, 126, 119, 70, 10, 24, 59, 
            106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 
            121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 
            53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
            14, 18, 33, 18, 14, 124, 120, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
            120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1), .Dim = c(50L, 2L), .Dimnames = list(NULL, c("time", 
            "status")), type = "right", class = "Surv"), `(weights)` = c(1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 50:99, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("50", "51", "52", "53", "54", 
            "55", "56", "57", "58", "59", "60", "61", "62", "63", 
            "64", "65", "66", "67", "68", "69", "70", "71", "72", 
            "73", "74", "75", "76", "77", "78", "79", "80", "81", 
            "82", "83", "84", "85", "86", "87", "88", "89", "90", 
            "91", "92", "93", "94", "95", "96", "97", "98", "99"
            ), "(Intercept)"), assign = 0L), shape = NULL), .Names = c("rate", 
            "shape"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 50L, trisk = 3212, concat.formula = quote(survival::Surv(time, 
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
        "r", "DLd", "DLS", "deriv")), res = structure(c(-0.000119615292780164, 
        0.015687198745234, -0.00427670999440978, 0.0106712127068021, 
        0.00403747940884945, 0.0230609407978165, 0.00212100565848161, 
        0.00308383417892797), .Dim = c(2L, 4L), .Dimnames = list(
            c("shape", "rate"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(-0.000119615292780164, -4.15491026576559, 
        -0.00427670999440978, -4.54020556438152, 0.00403747940884945, 
        -3.76961496714965, 0.00212100565848161, 0.196582846243654
        ), .Dim = c(2L, 4L), .Dimnames = list(c("shape", "rate"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(4.49866500331101e-06, 
        -0.000289614921411798, -0.000289614921411798, 0.0386448154372561
        ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", "rate"
        ), c("shape", "rate"))), coefficients = structure(c(-0.000119615292780164, 
        -4.15491026576559), .Names = c("shape", "rate")), npars = 2L, 
        fixedpars = NULL, optpars = 1:2, loglik = -258.129722614713, 
        logliki = c(-6.13174829166889, -6.02269789685339, -5.25680280191403, 
        -4.31288462209927, -4.53373390792405, -5.0842540388948, 
        -5.81993518937114, -4.39180150428412, -4.9743329156413, 
        -4.2655120043527, -4.91147982465447, -4.94291010062767, 
        -5.3821513749428, -4.8957618882502, -5.44478103826166, 
        -5.8355434312374, -7.40247122653816, -9.57395114292442, 
        -6.05386439873937, -4.3602403721742, -4.7384798623562, 
        -4.94291010062767, -4.34445699596135, -6.44282204950512, 
        -4.59676631921173, -5.77309934616132, -4.64402099026888, 
        -5.58558910272409, -7.63358542034405, -4.67551475651513, 
        -4.99004152584601, -4.34445699596135, -4.29709562400173, 
        -4.88004208593658, -4.53373390792405, -5.78871314705476, 
        -4.48643995421837, -4.58101102218132, -4.7384798623562, 
        -4.94291010062767, -4.4233551441334, -4.2655120043527, 
        -4.43912915502039, -4.37602187487377, -4.43912915502039, 
        -4.67551475651513, -4.43912915502039, -4.37602187487377, 
        -6.10060028112581, -6.03828207264004), cl = 0.95, opt = structure(list(
            par = structure(c(-0.000119615292780164, -4.15491026576559
            ), .Names = c("shape", "rate")), value = 258.129722614713, 
            counts = structure(c(28L, 5L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(429514.375027908, 
            3218.89936752089, 3218.89936752089, 50.0000133392859
            ), .Dim = c(2L, 2L), .Dimnames = list(c("shape", 
            "rate"), c("shape", "rate")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 524.083491240283, m2LL = 516.259445229427), .Names = c("call", 
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
        covpars = NULL, AIC = 515.926644866709, data = structure(list(
            Y = structure(c(126, 119, 70, 10, 24, 59, 106, 15, 
            52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
            37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 
            9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 
            33, 18, 14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 126, 119, 70, 10, 24, 59, 106, 
            15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 
            13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 
            12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 
            18, 33, 18, 14, 124, 120, 126, 119, 70, 10, 24, 59, 
            106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 
            121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 
            53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
            14, 18, 33, 18, 14, 124, 120, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(50L, 6L), .Dimnames = list(
                c("50", "51", "52", "53", "54", "55", "56", "57", 
                "58", "59", "60", "61", "62", "63", "64", "65", 
                "66", "67", "68", "69", "70", "71", "72", "73", 
                "74", "75", "76", "77", "78", "79", "80", "81", 
                "82", "83", "84", "85", "86", "87", "88", "89", 
                "90", "91", "92", "93", "94", "95", "96", "97", 
                "98", "99"), c("time", "status", "start", "stop", 
                "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(126, 
            119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 
            47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
            28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 
            21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
            120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1), .Dim = c(50L, 2L), .Dimnames = list(NULL, c("time", 
            "status")), type = "right", class = "Surv"), `(weights)` = c(1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ 1), row.names = 50:99, covnames = character(0), covnames.orig = character(0), class = "data.frame"), 
            mml = structure(list(mu = structure(c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(50L, 
            1L), .Dimnames = list(c("50", "51", "52", "53", "54", 
            "55", "56", "57", "58", "59", "60", "61", "62", "63", 
            "64", "65", "66", "67", "68", "69", "70", "71", "72", 
            "73", "74", "75", "76", "77", "78", "79", "80", "81", 
            "82", "83", "84", "85", "86", "87", "88", "89", "90", 
            "91", "92", "93", "94", "95", "96", "97", "98", "99"
            ), "(Intercept)"), assign = 0L), sigma = NULL, Q = NULL), .Names = c("mu", 
            "sigma", "Q"))), .Names = c("Y", "m", "mml")), datameans = numeric(0), 
        N = 50L, events = 50L, trisk = 3212, concat.formula = quote(survival::Surv(time, 
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
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(3.68824477445037, 
        0.957935054378837, -0.0713876870211361, 3.18240979417883, 
        0.786652417314019, -0.969640789228241, 4.19407975472191, 
        1.16651210650444, 0.826865415185969, 0.258083813917757, 
        0.0962808338873761, 0.458300820470382), .Dim = 3:4, .Dimnames = list(
            c("mu", "sigma", "Q"), c("est", "L95%", "U95%", "se"
            ))), res.t = structure(c(3.68824477445037, -0.04297529623329, 
        -0.0713876870211361, 3.18240979417883, -0.239968783378518, 
        -0.969640789228241, 4.19407975472191, 0.154018190911938, 
        0.826865415185969, 0.258083813917757, 0.100508728068009, 
        0.458300820470382), .Dim = 3:4, .Dimnames = list(c("mu", 
        "sigma", "Q"), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0666072550063353, 
        0.00239529904535449, 0.100662283071125, 0.00239529904535449, 
        0.010102004417849, 0.00428377688905192, 0.100662283071125, 
        0.00428377688905192, 0.210039642043825), .Dim = c(3L, 
        3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
        "sigma", "Q"))), coefficients = structure(c(3.68824477445037, 
        -0.04297529623329, -0.0713876870211361), .Names = c("mu", 
        "sigma", "Q")), npars = 3L, fixedpars = NULL, optpars = 1:3, 
        loglik = -254.963322433355, logliki = c(-6.41076143184853, 
        -6.2867035734497, -5.29355369049506, -4.26212419219951, 
        -4.19808489058607, -5.03570792088419, -6.04567856167296, 
        -4.1209280882864, -4.8650748611584, -4.55037179704652, 
        -4.76574495612374, -4.81554548191036, -5.47257843560226, 
        -4.74076083535226, -5.55942186675077, -6.06473766541069, 
        -7.63717846686745, -9.16651723863868, -6.32253971541058, 
        -4.14845067849376, -4.49057015678056, -4.81554548191036, 
        -4.17441058427698, -6.74555322113849, -4.2782784484441, 
        -5.98795787469419, -4.34582322610402, -5.74854204431767, 
        -7.82691306126844, -4.39302260282773, -4.88972295967449, 
        -4.17441058427698, -4.33108956101398, -4.71573212812507, 
        -4.19808489058607, -6.00728945491096, -4.15034948298796, 
        -4.25695086206963, -4.49057015678056, -4.81554548191036, 
        -4.11654599301085, -4.55037179704652, -4.12060693263732, 
        -4.13118462327736, -4.12060693263732, -4.39302260282773, 
        -4.12060693263732, -4.13118462327736, -6.37570394337831, 
        -6.30466144034892), cl = 0.95, opt = structure(list(par = structure(c(3.68824477445037, 
        -0.04297529623329, -0.0713876870211361), .Names = c("mu", 
        "sigma", "Q")), value = 254.963322433355, counts = structure(c(31L, 
        5L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(54.4876545234274, 
            -1.86229388532411, -26.075430326955, -1.86229388532411, 
            99.9174997673435, -1.14531485451153, -26.075430326955, 
            -1.14531485451153, 17.2811122070016), .Dim = c(3L, 
            3L), .Dimnames = list(c("mu", "sigma", "Q"), c("mu", 
            "sigma", "Q")))), .Names = c("par", "value", "counts", 
        "convergence", "message", "hessian")), BIC = 521.662713882994, 
        m2LL = 509.926644866709), .Names = c("call", "dlist", 
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
        basepars = 1L, covpars = 2L, AIC = 1013.03237125623, 
        data = structure(list(Y = structure(c(24, 19, 4, 78, 
        17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 
        35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
        120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 
        29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 
        24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
        350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
        33, 53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
        14, 18, 33, 18, 14, 124, 120, 24, 19, 4, 78, 17, 30, 
        3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 
        15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
        29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 
        50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 
        50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf
        ), .Dim = c(99L, 6L), .Dimnames = list(c("1", "2", "3", 
        "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
        "14", "15", "16", "17", "18", "19", "20", "21", "22", 
        "23", "24", "25", "26", "27", "28", "29", "30", "31", 
        "32", "33", "34", "35", "36", "37", "38", "39", "40", 
        "41", "42", "43", "44", "45", "46", "47", "48", "49", 
        "50", "51", "52", "53", "54", "55", "56", "57", "58", 
        "59", "60", "61", "62", "63", "64", "65", "66", "67", 
        "68", "69", "70", "71", "72", "73", "74", "75", "76", 
        "77", "78", "79", "80", "81", "82", "83", "84", "85", 
        "86", "87", "88", "89", "90", "91", "92", "93", "94", 
        "95", "96", "97", "98", "99"), c("time", "status", "start", 
        "stop", "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
        19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
        3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 
        95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
        96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 
        15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
        37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 
        46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 
        14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 99L), covnames = structure("treatment", .Names = "rate"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 2L), .Dimnames = list(
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
                "98", "99"), c("(Intercept)", "treatmentB")), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment"))), .Names = "rate")), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.505050505050505, .Names = "treatmentB"), 
        N = 99L, events = 99L, trisk = 5964, concat.formula = quote(survival::Surv(time, 
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
        "DLd", "DLS", "deriv")), res = structure(c(0.0178052325581392, 
        -0.134363168695908, 0.0134569748311098, -0.528350819939616, 
        0.0235585122531789, 0.2596244825478, 0.00254360443919586, 
        0.201017801526677), .Dim = c(2L, 4L), .Dimnames = list(
            c("rate", "treatmentB"), c("est", "L95%", "U95%", 
            "se"))), res.t = structure(c(-4.02826290094263, -0.134363168695908, 
        -4.30825773254403, -0.528350819939616, -3.74826806934122, 
        0.2596244825478, 0.142857130952391, 0.201017801526677
        ), .Dim = c(2L, 4L), .Dimnames = list(c("rate", "treatmentB"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0204081598639485, 
        -0.0204081598639486, -0.0204081598639486, 0.0404081565306186
        ), .Dim = c(2L, 2L), .Dimnames = list(c("rate", "treatmentB"
        ), c("rate", "treatmentB"))), coefficients = structure(c(-4.02826290094263, 
        -0.134363168695908), .Names = c("rate", "treatmentB")), 
        npars = 2L, fixedpars = NULL, optpars = 1:2, loglik = -504.516185628113, 
        logliki = structure(c(-4.45558848233797, -4.36656231954727, 
        -4.09948383117518, -5.41707104047748, -4.33095185443099, 
        -4.5624198776868, -4.08167859861704, -4.82949836605889, 
        -4.1528995288496, -4.13509429629146, -4.27753615675658, 
        -4.18850999396588, -4.95413499396586, -4.54461464512866, 
        -4.54461464512866, -4.22412045908216, -4.08167859861704, 
        -4.2419256916403, -4.25973092419844, -4.6514460404775, 
        -4.82949836605889, -4.29534138931472, -4.13509429629146, 
        -4.08167859861704, -6.09366987768677, -5.54170766838446, 
        -5.32804487768679, -4.09948383117518, -5.89781231954724, 
        -8.97811755210532, -5.71975999396585, -8.15907685443092, 
        -5.5595129009426, -4.2419256916403, -4.54461464512866, 
        -4.06387336605891, -4.93632976140773, -4.86510883117517, 
        -7.21539952884954, -5.93342278466352, -4.22412045908216, 
        -4.95413499396586, -5.75537045908213, -5.73756522652399, 
        -5.29243441257051, -9.56569022652392, -4.18850999396588, 
        -4.91852452884959, -4.82949836605889, -6.12402083925243, 
        -6.01505446316277, -5.25228983053514, -4.31829232119519, 
        -4.53622507337451, -5.08105695382282, -5.81268833613911, 
        -4.39612544697352, -4.97209057773316, -4.2715924457282, 
        -4.90982407711049, -4.94095732742183, -5.37682283178047, 
        -4.89425745195483, -5.43908933240313, -5.82825496129478, 
        -7.40048410201702, -9.61094487412156, -6.0461877134741, 
        -4.36499219666219, -4.73859120039817, -4.94095732742183, 
        -4.34942557150652, -6.43535334236574, -4.59849157399718, 
        -5.76598846067211, -4.64519144946417, -5.57918895880412, 
        -7.63398347935201, -4.67632469977551, -4.98765720288882, 
        -4.34942557150652, -4.30272569603953, -4.87869082679916, 
        -4.53622507337451, -5.78155508582778, -4.48952519790752, 
        -4.58292494884151, -4.73859120039817, -4.94095732742183, 
        -4.42725869728485, -4.2715924457282, -4.44282532244052, 
        -4.38055882181786, -4.44282532244052, -4.67632469977551, 
        -4.44282532244052, -4.38055882181786, -6.09288758894109, 
        -6.03062108831843), .Names = c("1", "2", "3", "4", "5", 
        "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
        "16", "17", "18", "19", "20", "21", "22", "23", "24", 
        "25", "26", "27", "28", "29", "30", "31", "32", "33", 
        "34", "35", "36", "37", "38", "39", "40", "41", "42", 
        "43", "44", "45", "46", "47", "48", "49", "50", "51", 
        "52", "53", "54", "55", "56", "57", "58", "59", "60", 
        "61", "62", "63", "64", "65", "66", "67", "68", "69", 
        "70", "71", "72", "73", "74", "75", "76", "77", "78", 
        "79", "80", "81", "82", "83", "84", "85", "86", "87", 
        "88", "89", "90", "91", "92", "93", "94", "95", "96", 
        "97", "98", "99")), cl = 0.95, opt = structure(list(par = structure(c(-4.02826290094263, 
        -0.134363168695908), .Names = c("rate", "treatmentB")), 
            value = 504.516185628113, counts = structure(c(2L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(99.0000164999879, 
            50.0000083333267, 50.0000083333267, 50.0000083333266
            ), .Dim = c(2L, 2L), .Dimnames = list(c("rate", "treatmentB"
            ), c("rate", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 1018.2226109565, m2LL = 1009.03237125623), .Names = c("call", 
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
        "shape")), basepars = 1:2, covpars = 3L, AIC = 1014.88878358084, 
        data = structure(list(Y = structure(c(24, 19, 4, 78, 
        17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 
        35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
        120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 
        29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 
        24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
        350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
        33, 53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
        14, 18, 33, 18, 14, 124, 120, 24, 19, 4, 78, 17, 30, 
        3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 
        15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
        29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 
        50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 
        50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf
        ), .Dim = c(99L, 6L), .Dimnames = list(c("1", "2", "3", 
        "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
        "14", "15", "16", "17", "18", "19", "20", "21", "22", 
        "23", "24", "25", "26", "27", "28", "29", "30", "31", 
        "32", "33", "34", "35", "36", "37", "38", "39", "40", 
        "41", "42", "43", "44", "45", "46", "47", "48", "49", 
        "50", "51", "52", "53", "54", "55", "56", "57", "58", 
        "59", "60", "61", "62", "63", "64", "65", "66", "67", 
        "68", "69", "70", "71", "72", "73", "74", "75", "76", 
        "77", "78", "79", "80", "81", "82", "83", "84", "85", 
        "86", "87", "88", "89", "90", "91", "92", "93", "94", 
        "95", "96", "97", "98", "99"), c("time", "status", "start", 
        "stop", "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
        19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
        3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 
        95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
        96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 
        15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
        37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 
        46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 
        14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 99L), covnames = structure("treatment", .Names = "scale"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(scale = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 2L), .Dimnames = list(
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
                "98", "99"), c("(Intercept)", "treatmentB")), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                shape = NULL), .Names = c("scale", "shape"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.505050505050505, .Names = "treatmentB"), 
        N = 99L, events = 99L, trisk = 5964, concat.formula = quote(survival::Surv(time, 
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
        "DLd", "DLS", "deriv")), res = structure(c(0.97183645044183, 
        55.2741284442833, 0.138978513378766, 0.837397441403118, 
        40.9604906837797, -0.267139652708663, 1.12785881555222, 
        74.5896649252053, 0.545096679466196, 0.073825589268911, 
        8.45193612418817, 0.207206953439368), .Dim = 3:4, .Dimnames = list(
            c("shape", "scale", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(-0.0285677495414593, 
        4.01230495898977, 0.138978513378766, -0.17745648080764, 
        3.71260796024396, -0.267139652708663, 0.120320981724721, 
        4.31200195773557, 0.545096679466196, 0.0759650342764437, 
        0.152909441760041, 0.207206953439368), .Dim = 3:4, .Dimnames = list(
            c("shape", "scale", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(0.00577068643262127, 
        0.00319879775458669, -0.000932220485589633, 0.00319879775458669, 
        0.0233812973793674, -0.0221248918610087, -0.000932220485589633, 
        -0.0221248918610087, 0.0429347215536245), .Dim = c(3L, 
        3L), .Dimnames = list(c("shape", "scale", "treatmentB"
        ), c("shape", "scale", "treatmentB"))), coefficients = structure(c(-0.0285677495414593, 
        4.01230495898977, 0.138978513378766), .Names = c("shape", 
        "scale", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -504.444391790418, logliki = structure(c(-4.46189926922201, 
        -4.36503410891071, -4.04483647185753, -5.44809932468143, 
        -4.32560796884463, -4.57583323550542, -4.01772936011109, 
        -4.85393396470796, -4.11690606303921, -4.09388945336952, 
        -4.26546798535044, -4.16111829704926, -4.9815378601373, 
        -4.55698260783195, -4.55698260783195, -4.20367057469033, 
        -4.01772936011109, -4.22449825998005, -4.24508655228464, 
        -4.66941255921145, -4.85393396470796, -4.28566856198568, 
        -4.09388945336952, -4.01772936011109, -6.117021540288, 
        -5.5722571086153, -5.35909110341079, -4.04483647185753, 
        -5.92454657756136, -8.89216380512933, -5.74881591461994, 
        -8.11234889424123, -5.58995395555687, -4.22449825998005, 
        -4.55698260783195, -3.98712225982435, -4.96337379301543, 
        -4.89050534617521, -7.20695270437677, -5.9596032652916, 
        -4.20367057469033, -4.9815378601373, -5.78402445001812, 
        -5.76642424966893, -5.32340529431478, -9.44883771558568, 
        -4.16111829704926, -4.94518916384666, -4.85393396470796, 
        -6.14500789586646, -6.03825529888865, -5.28166957242991, 
        -4.29364178883266, -4.5408025891155, -5.10861315139707, 
        -5.83926073015425, -4.38516456612827, -4.99754102177728, 
        -4.2350117378364, -4.93366931840165, -4.96564507731871, 
        -5.40659967476682, -4.91764986721451, -5.46881502257173, 
        -5.85460484901779, -7.3804526030209, -9.47978740428561, 
        -6.06878284776209, -4.34920052102236, -4.7561010528586, 
        -4.96564507731871, -4.3309285532262, -6.44868857008928, 
        -4.60790806214041, -5.79318893957129, -4.65767976427942, 
        -5.60826415425223, -7.60416433626103, -4.69063885317593, 
        -5.01346057595221, -4.3309285532262, -4.27453240823471, 
        -4.90160841626258, -4.5408025891155, -5.80855286372922, 
        -4.48977710765904, -4.59121799988392, -4.7561010528586, 
        -4.96564507731871, -4.42050336926301, -4.2350117378364, 
        -4.43797970339471, -4.36727064731534, -4.43797970339471, 
        -4.69063885317593, -4.43797970339471, -4.36727064731534, 
        -6.11453366249386, -6.05352181491452), .Names = c("1", 
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
        "94", "95", "96", "97", "98", "99")), cl = 0.95, opt = structure(list(
            par = structure(c(-0.0285677495414593, 4.01230495898977, 
            0.138978513378766), .Names = c("shape", "scale", 
            "treatmentB")), value = 504.444391790418, counts = structure(c(2L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(194.794877890723, 
            -44.2013191406923, -18.5481028320972, -44.2013191406923, 
            93.5021574931592, 47.2233119197427, -18.5481028320972, 
            47.2233119197427, 47.2233119197421), .Dim = c(3L, 
            3L), .Dimnames = list(c("shape", "scale", "treatmentB"
            ), c("shape", "scale", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 1022.67414313124, m2LL = 1008.88878358084), .Names = c("call", 
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
        "sdlog")), basepars = 1:2, covpars = 3L, AIC = 1009.55416605177, 
        data = structure(list(Y = structure(c(24, 19, 4, 78, 
        17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 
        35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
        120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 
        29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 
        24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
        350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
        33, 53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
        14, 18, 33, 18, 14, 124, 120, 24, 19, 4, 78, 17, 30, 
        3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 
        15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
        29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 
        50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 
        50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf
        ), .Dim = c(99L, 6L), .Dimnames = list(c("1", "2", "3", 
        "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
        "14", "15", "16", "17", "18", "19", "20", "21", "22", 
        "23", "24", "25", "26", "27", "28", "29", "30", "31", 
        "32", "33", "34", "35", "36", "37", "38", "39", "40", 
        "41", "42", "43", "44", "45", "46", "47", "48", "49", 
        "50", "51", "52", "53", "54", "55", "56", "57", "58", 
        "59", "60", "61", "62", "63", "64", "65", "66", "67", 
        "68", "69", "70", "71", "72", "73", "74", "75", "76", 
        "77", "78", "79", "80", "81", "82", "83", "84", "85", 
        "86", "87", "88", "89", "90", "91", "92", "93", "94", 
        "95", "96", "97", "98", "99"), c("time", "status", "start", 
        "stop", "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
        19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
        3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 
        95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
        96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 
        15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
        37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 
        46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 
        14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 99L), covnames = structure("treatment", .Names = "meanlog"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(meanlog = structure(c(1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 2L), .Dimnames = list(
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
                "98", "99"), c("(Intercept)", "treatmentB")), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                sdlog = NULL), .Names = c("meanlog", "sdlog"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.505050505050505, .Names = "treatmentB"), 
        N = 99L, events = 99L, trisk = 5964, concat.formula = quote(survival::Surv(time, 
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
        "DLd", "DLS", "deriv")), res = structure(c(3.32602900644194, 
        1.1311909434563, 0.396429340601199, 3.00930136244699, 
        0.984110124635353, -0.0492459592721376, 3.6427566504369, 
        1.30025382172721, 0.842104640474536, 0.161598706146269, 
        0.0803901865693443, 0.227389535414307), .Dim = 3:4, .Dimnames = list(
            c("meanlog", "sdlog", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(3.32602900644194, 
        0.123271009989775, 0.396429340601199, 3.00930136244699, 
        -0.0160174729121012, -0.0492459592721376, 3.6427566504369, 
        0.262559492891651, 0.842104640474536, 0.161598706146269, 
        0.0710668583711568, 0.227389535414307), .Dim = 3:4, .Dimnames = list(
            c("meanlog", "sdlog", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(0.0261141418281481, 
        0, -0.0261141418186508, 0, 0.00505049835874606, 0, -0.0261141418186509, 
        0, 0.0517060008159343), .Dim = c(3L, 3L), .Dimnames = list(
            c("meanlog", "sdlog", "treatmentB"), c("meanlog", 
            "sdlog", "treatmentB"))), coefficients = structure(c(3.32602900644194, 
        0.123271009989775, 0.396429340601199), .Names = c("meanlog", 
        "sdlog", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -501.777083025887, logliki = structure(c(-4.22881947442217, 
        -4.04354589306486, -3.89872554496311, -5.81401165849697, 
        -3.97032309633155, -4.44561476940232, -4.07947944655812, 
        -4.9391384513581, -3.73239077458271, -3.75378613543682, 
        -3.86567320474379, -3.73732660994048, -5.14619458318802, 
        -4.41017079991866, -4.41017079991866, -3.77670882242776, 
        -4.07947944655812, -3.80356614472484, -3.83349737487145, 
        -4.61810602556448, -4.9391384513581, -3.89948602898458, 
        -3.75378613543682, -4.07947944655812, -6.59211974179714, 
        -5.9720646596153, -5.69611505638582, -3.89872554496311, 
        -6.38521778661127, -8.73975696659212, -6.18518409605966, 
        -8.24630419335035, -5.99402056905375, -3.80356614472484, 
        -4.41017079991866, -4.44405666854512, -5.11743607237386, 
        -4.99969608491122, -7.58340443854235, -6.42380665274112, 
        -3.77670882242776, -5.14619458318802, -6.22617937494169, 
        -6.20574609504479, -5.64769955099347, -9.05860775790425, 
        -3.73732660994048, -5.08841153370461, -4.9391384513581, 
        -6.36325612553207, -6.25762069732709, -5.39883089471761, 
        -4.13256079912551, -4.33607218511368, -5.16901310917977, 
        -6.05163552554647, -4.15235006766978, -5.01390615119165, 
        -4.22137261787685, -4.92205563926162, -4.96827402942825, 
        -5.55610650957974, -4.89872813411255, -5.63188266628593, 
        -6.06796469807842, -7.39901024922807, -8.68205805256683, 
        -6.28815965240473, -4.13069533479249, -4.65798887231509, 
        -4.96827402942825, -4.12556205137505, -6.64730089955921, 
        -4.43392440669091, -6.00213480669591, -4.50871318193021, 
        -5.79594956417046, -7.55842904783177, -4.55866632268392, 
        -5.03650185338122, -4.12556205137505, -4.14844899562854, 
        -4.87525657507905, -4.33607218511368, -6.0187215400052, 
        -4.26631920705291, -4.40916507549201, -4.65798887231509, 
        -4.96827402942825, -4.18441047388619, -4.22137261787685, 
        -4.20312362141856, -4.13991185207922, -4.20312362141856, 
        -4.55866632268392, -4.20312362141856, -4.13991185207922, 
        -6.33342829968056, -6.27292666227483), .Names = c("1", 
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
        "94", "95", "96", "97", "98", "99")), cl = 0.95, opt = structure(list(
            par = structure(c(3.32602900644194, 0.123271009989775, 
            0.396429340601199), .Names = c("meanlog", "sdlog", 
            "treatmentB")), value = 501.777083025887, counts = structure(c(8L, 
            1L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(77.3683537147463, 
            0, 39.0749261072187, 0, 198.000262344067, 0, 39.0749261072187, 
            0, 39.0749261214296), .Dim = c(3L, 3L), .Dimnames = list(
                c("meanlog", "sdlog", "treatmentB"), c("meanlog", 
                "sdlog", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 1017.33952560218, m2LL = 1003.55416605177), .Names = c("call", 
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
        "shape")), basepars = 1:2, covpars = 3L, AIC = 1015.02403577152, 
        data = structure(list(Y = structure(c(24, 19, 4, 78, 
        17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 
        35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
        120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 
        29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 
        24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
        350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
        33, 53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
        14, 18, 33, 18, 14, 124, 120, 24, 19, 4, 78, 17, 30, 
        3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 
        15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
        29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 
        50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 
        50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf
        ), .Dim = c(99L, 6L), .Dimnames = list(c("1", "2", "3", 
        "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
        "14", "15", "16", "17", "18", "19", "20", "21", "22", 
        "23", "24", "25", "26", "27", "28", "29", "30", "31", 
        "32", "33", "34", "35", "36", "37", "38", "39", "40", 
        "41", "42", "43", "44", "45", "46", "47", "48", "49", 
        "50", "51", "52", "53", "54", "55", "56", "57", "58", 
        "59", "60", "61", "62", "63", "64", "65", "66", "67", 
        "68", "69", "70", "71", "72", "73", "74", "75", "76", 
        "77", "78", "79", "80", "81", "82", "83", "84", "85", 
        "86", "87", "88", "89", "90", "91", "92", "93", "94", 
        "95", "96", "97", "98", "99"), c("time", "status", "start", 
        "stop", "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
        19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
        3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 
        95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
        96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 
        15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
        37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 
        46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 
        14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 99L), covnames = structure("treatment", .Names = "rate"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 2L), .Dimnames = list(
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
                "98", "99"), c("(Intercept)", "treatmentB")), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                shape = NULL), .Names = c("rate", "shape"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.505050505050505, .Names = "treatmentB"), 
        N = 99L, events = 99L, trisk = 5964, concat.formula = quote(survival::Surv(time, 
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
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(1.0115205952164, 
        0.0180104049956013, -0.134363452269366, 0.791298722486861, 
        0.0124254618650118, -0.526100539637322, 1.29303117200966, 
        0.0261056443317387, 0.25737363509859, 0.126718232830252, 
        0.00341103033269335, 0.1998695335516), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(0.0114547384825394, 
        -4.0168056327844, -0.134363452269366, -0.234079730816844, 
        -4.38800753545597, -0.526100539637322, 0.256989207781923, 
        -3.64560373011284, 0.25737363509859, 0.125274990375399, 
        0.189392205979068, 0.1998695335516), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(0.0156938232135563, 
        0.0156937808945265, 4.46227333159485e-09, 0.0156937808945265, 
        0.0358694076856176, -0.0201756646420765, 4.46227333045604e-09, 
        -0.0201756646420765, 0.0399478304421343), .Dim = c(3L, 
        3L), .Dimnames = list(c("shape", "rate", "treatmentB"
        ), c("shape", "rate", "treatmentB"))), coefficients = structure(c(0.0114547384825394, 
        -4.0168056327844, -0.134363452269366), .Names = c("shape", 
        "rate", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -504.512017885758, logliki = structure(c(-4.45217695791433, 
        -4.36481631507333, -4.11261099357243, -5.41116002056417, 
        -4.33007689060192, -4.5576686413581, -4.09791485728456, 
        -4.82315351690723, -4.16019510158973, -4.14396060417875, 
        -4.27828246846632, -4.19332061978066, -4.94756069006342, 
        -4.54004880241656, -4.54004880241656, -4.22702958391765, 
        -4.09791485728456, -4.24403756605974, -4.26112583142021, 
        -4.64594475875148, -4.82315351690723, -4.2954980345167, 
        -4.14396060417875, -4.09791485728456, -6.09098310084864, 
        -5.53624274758773, -5.32187122794032, -4.11261099357243, 
        -5.89401644096607, -8.99859935367034, -5.71506541202395, 
        -8.17220461224577, -5.55411840724358, -4.24403756605974, 
        -4.54004880241656, -4.08457565167385, -4.92977399297486, 
        -4.85867335252584, -7.22064103188752, -5.92981987478725, 
        -4.22702958391765, -4.94756069006342, -5.75084620093324, 
        -5.73295518141239, -5.28617045506248, -9.5916504326401, 
        -4.19332061978066, -4.91199172603256, -4.82315351690723, 
        -6.12073052385397, -6.01116704163268, -5.24572632673113, 
        -4.3233845312829, -4.53374257267593, -5.07448989191208, 
        -5.80780182973576, -4.39744331844826, -4.96572288962957, 
        -4.2802556470057, -4.9036610400245, -4.93468274096981, 
        -5.37044762030597, -4.88815759051974, -5.43285545973412, 
        -5.82343965164805, -7.40612753381205, -9.63606389082991, 
        -6.04246702188765, -4.36759993072287, -4.73345368583072, 
        -4.93468274096981, -4.35277607304795, -6.43395320165087, 
        -4.59495065433151, -5.7608945953141, -4.64101604904048, 
        -5.57336967775199, -7.64151527188662, -4.67178777193501, 
        -4.98124944039576, -4.35277607304795, -4.30885234982554, 
        -4.87265935749469, -4.53374257267593, -5.77652928165952, 
        -4.48804294186889, -4.57962363392892, -4.73345368583072, 
        -4.93468274096981, -4.42749335916238, -4.2802556470057, 
        -4.44258085752336, -4.38249216008343, -4.44258085752336, 
        -4.67178777193501, -4.44258085752336, -4.38249216008343, 
        -6.08942286268984, -6.02681663172561), .Names = c("1", 
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
        "94", "95", "96", "97", "98", "99")), cl = 0.95, opt = structure(list(
            par = structure(c(0.0114547384825394, -4.0168056327844, 
            -0.134363452269366), .Names = c("shape", "rate", 
            "treatmentB")), value = 504.512017885758, counts = structure(c(34L, 
            7L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(163.859636330699, 
            -100.140555630901, -50.5760382054632, -100.140555630901, 
            100.140811284177, 50.5761602056509, -50.5760382054632, 
            50.5761602056509, 50.5761602198618), .Dim = c(3L, 
            3L), .Dimnames = list(c("shape", "rate", "treatmentB"
            ), c("shape", "rate", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 1022.80939532192, m2LL = 1009.02403577152), .Names = c("call", 
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
        "shape")), basepars = 1:2, covpars = 3L, AIC = 1014.02806918271, 
        data = structure(list(Y = structure(c(24, 19, 4, 78, 
        17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 
        35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 
        86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 
        311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
        7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 
        12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 
        104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 
        120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 
        29, 29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
        4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
        11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 
        24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 
        350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 91, 223, 
        33, 53, 12, 9, 46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 
        14, 18, 33, 18, 14, 124, 120, 24, 19, 4, 78, 17, 30, 
        3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 
        15, 6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
        29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 9, 
        50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 7, 48, 
        50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 50, 12, 146, 
        28, 103, 31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 
        27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
        Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf
        ), .Dim = c(99L, 6L), .Dimnames = list(c("1", "2", "3", 
        "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
        "14", "15", "16", "17", "18", "19", "20", "21", "22", 
        "23", "24", "25", "26", "27", "28", "29", "30", "31", 
        "32", "33", "34", "35", "36", "37", "38", "39", "40", 
        "41", "42", "43", "44", "45", "46", "47", "48", "49", 
        "50", "51", "52", "53", "54", "55", "56", "57", "58", 
        "59", "60", "61", "62", "63", "64", "65", "66", "67", 
        "68", "69", "70", "71", "72", "73", "74", "75", "76", 
        "77", "78", "79", "80", "81", "82", "83", "84", "85", 
        "86", "87", "88", "89", "90", "91", "92", "93", "94", 
        "95", "96", "97", "98", "99"), c("time", "status", "start", 
        "stop", "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
        19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 11, 
        3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 105, 278, 
        95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 11, 52, 97, 
        96, 71, 311, 9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 
        15, 52, 7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 
        37, 50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 
        46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 18, 
        14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 
        2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
            treatment = c("A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
            "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
            "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1)), .Names = c("survival::Surv(time, status)", 
        "treatment", "(weights)"), terms = quote(survival::Surv(time, 
            status) ~ treatment), row.names = c(NA, 99L), covnames = structure("treatment", .Names = "rate"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(rate = structure(c(1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 2L), .Dimnames = list(
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
                "98", "99"), c("(Intercept)", "treatmentB")), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                shape = NULL), .Names = c("rate", "shape"))), .Names = c("Y", 
        "m", "mml")), datameans = structure(0.505050505050505, .Names = "treatmentB"), 
        N = 99L, events = 99L, trisk = 5964, concat.formula = quote(survival::Surv(time, 
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
        "DLd", "DLS", "deriv")), res = structure(c(-0.00155889761968303, 
        0.0197203110378593, -0.140643865177842, -0.00468383483559437, 
        0.0140241873310354, -0.534798204836228, 0.00156603959622831, 
        0.0277299966301296, 0.253510474480544, 0.00159438501960263, 
        0.00342964260027423, 0.20110284819896), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), res.t = structure(c(-0.00155889761968303, 
        -3.92610615722112, -0.140643865177842, -0.00468383483559437, 
        -4.26697177356342, -0.534798204836228, 0.00156603959622831, 
        -3.58524054087882, 0.253510474480544, 0.00159438501960263, 
        0.173914224460758, 0.20110284819896), .Dim = 3:4, .Dimnames = list(
            c("shape", "rate", "treatmentB"), c("est", "L95%", 
            "U95%", "se"))), cov = structure(c(2.54206359073328e-06, 
        -0.000158141790241619, 9.32759238650401e-06, -0.000158141790241619, 
        0.0302461574697869, -0.0209884253863067, 9.32759238650404e-06, 
        -0.0209884253863067, 0.0404423555537338), .Dim = c(3L, 
        3L), .Dimnames = list(c("shape", "rate", "treatmentB"
        ), c("shape", "rate", "treatmentB"))), coefficients = structure(c(-0.00155889761968303, 
        -3.92610615722112, -0.140643865177842), .Names = c("shape", 
        "rate", "treatmentB")), npars = 3L, fixedpars = NULL, 
        optpars = 1:3, loglik = -504.014034591353, logliki = c(-4.42806287534113, 
        -4.3249165817979, -4.0109775666724, -5.49604532678542, 
        -4.28344947599405, -4.55086169924383, -3.98980565984109, 
        -4.85325957787913, -4.07431017232025, -4.05322977536049, 
        -4.22102416569777, -4.11637978867336, -4.99216257309536, 
        -4.53046883936464, -4.53046883936464, -4.15832815034589, 
        -3.98980565984109, -4.17925697867867, -4.20015563479638, 
        -4.65238685134801, -4.85325957787913, -4.24186261830837, 
        -4.05322977536049, -3.98980565984109, -6.19958599207831, 
        -5.62853050933723, -5.40059722883866, -4.0109775666724, 
        -5.99983802852132, -8.80831130451518, -5.81550981001553, 
        -8.12689052568971, -5.64734892007436, -4.17925697867867, 
        -4.53046883936464, -3.96860315449262, -4.97240468733986, 
        -4.89308877617705, -7.28536745524409, -6.03638916222142, 
        -4.15832815034589, -4.99216257309536, -5.85258621884633, 
        -5.83406124892872, -5.36222624158587, -9.27099271132639, 
        -4.11637978867336, -4.95261840910033, -4.85325957787913, 
        -6.22313956039224, -6.11314452217785, -5.31206211945012, 
        -4.2523402821146, -4.50775805625697, -5.1244775595829, 
        -5.90597473572577, -4.34414675630109, -5.00357394571527, 
        -4.19694117915302, -4.9339451706629, -4.96880896989133, 
        -5.44666437560167, -4.91647611587463, -5.51339670299194, 
        -5.92204573301379, -7.4345965251403, -9.23400304301584, 
        -6.14468209020418, -4.30750272576586, -4.74041396779757, 
        -4.96880896989133, -4.28914147176103, -6.53152546546664, 
        -4.57980352151456, -5.85762575939847, -4.63356901800163, 
        -5.66217303312016, -7.64164909573002, -4.66928537332973, 
        -5.02091947102279, -4.28914147176103, -4.23390026461688, 
        -4.89898223943803, -4.50775805625697, -5.87376478426744, 
        -4.45345395547934, -4.56183060675716, -4.74041396779757, 
        -4.96880896989133, -4.38068642167865, -4.19694117915302, 
        -4.39891721899863, -4.32583780702292, -4.39891721899863, 
        -4.66928537332973, -4.39891721899863, -4.32583780702292, 
        -6.19182264887724, -6.12892438206781), cl = 0.95, opt = structure(list(
            par = structure(c(-0.00155889761968303, -3.92610615722112, 
            -0.140643865177842), .Names = c("shape", "rate", 
            "treatmentB")), value = 504.014034591353, counts = structure(c(33L, 
            7L), .Names = c("function", "gradient")), convergence = 0L, 
            message = NULL, hessian = structure(c(754366.351091283, 
            5975.33050243221, 2927.03911418973, 5975.33050243221, 
            99.0000828637747, 50.0000649674446, 2927.03911418973, 
            50.0000649674446, 50.0000649674446), .Dim = c(3L, 
            3L), .Dimnames = list(c("shape", "rate", "treatmentB"
            ), c("shape", "rate", "treatmentB")))), .Names = c("par", 
        "value", "counts", "convergence", "message", "hessian"
        )), BIC = 1021.81342873311, m2LL = 1008.02806918271), .Names = c("call", 
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
        covpars = 4L, AIC = 1011.07359291246, data = structure(list(
            Y = structure(c(24, 19, 4, 78, 17, 30, 3, 45, 7, 
            6, 14, 9, 52, 29, 29, 11, 3, 12, 13, 35, 45, 15, 
            6, 3, 116, 85, 73, 4, 105, 278, 95, 232, 86, 12, 
            29, 2, 51, 47, 179, 107, 11, 52, 97, 96, 71, 311, 
            9, 50, 45, 126, 119, 70, 10, 24, 59, 106, 15, 52, 
            7, 48, 50, 78, 47, 82, 107, 208, 350, 121, 13, 37, 
            50, 12, 146, 28, 103, 31, 91, 223, 33, 53, 12, 9, 
            46, 24, 104, 21, 27, 37, 50, 17, 7, 18, 14, 18, 33, 
            18, 14, 124, 120, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 
            29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
            4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 
            107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 
            70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 
            107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
            31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 
            37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
            24, 19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 
            29, 11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 
            4, 105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 
            107, 11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 
            70, 10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 
            107, 208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 
            31, 91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 
            37, 50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, 
            Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), .Dim = c(99L, 
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
            "96", "97", "98", "99"), c("time", "status", "start", 
            "stop", "time1", "time2"))), m = structure(list(`survival::Surv(time, status)` = structure(c(24, 
            19, 4, 78, 17, 30, 3, 45, 7, 6, 14, 9, 52, 29, 29, 
            11, 3, 12, 13, 35, 45, 15, 6, 3, 116, 85, 73, 4, 
            105, 278, 95, 232, 86, 12, 29, 2, 51, 47, 179, 107, 
            11, 52, 97, 96, 71, 311, 9, 50, 45, 126, 119, 70, 
            10, 24, 59, 106, 15, 52, 7, 48, 50, 78, 47, 82, 107, 
            208, 350, 121, 13, 37, 50, 12, 146, 28, 103, 31, 
            91, 223, 33, 53, 12, 9, 46, 24, 104, 21, 27, 37, 
            50, 17, 7, 18, 14, 18, 33, 18, 14, 124, 120, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 
            2L), .Dimnames = list(NULL, c("time", "status")), type = "right", class = "Surv"), 
                treatment = c("A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "A", "A", "A", 
                "A", "A", "A", "A", "A", "A", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B", "B", "B", "B", "B", "B", "B", "B", 
                "B", "B"), `(weights)` = c(1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1)), .Names = c("survival::Surv(time, status)", 
            "treatment", "(weights)"), terms = quote(survival::Surv(time, 
                status) ~ treatment), row.names = c(NA, 99L), covnames = structure("treatment", .Names = "mu"), covnames.orig = "treatment", class = "data.frame"), 
            mml = structure(list(mu = structure(c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1), .Dim = c(99L, 2L), .Dimnames = list(
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
                "98", "99"), c("(Intercept)", "treatmentB")), assign = 0:1, contrasts = structure(list(
                treatment = "contr.treatment"), .Names = "treatment")), 
                sigma = NULL, Q = NULL), .Names = c("mu", "sigma", 
            "Q"))), .Names = c("Y", "m", "mml")), datameans = structure(0.505050505050505, .Names = "treatmentB"), 
        N = 99L, events = 99L, trisk = 5964, concat.formula = quote(survival::Surv(time, 
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
        "d", "h", "H", "r", "DLd", "DLS", "deriv")), res = structure(c(3.48934038518302, 
        1.1188320212349, 0.226637117128751, 0.326255368304298, 
        2.93933187816279, 0.966707263342592, -0.407038250321332, 
        -0.153040463041778, 4.03934889220325, 1.29489571373682, 
        0.860312484578834, 0.805551199650375, 0.28062174170476, 
        0.0834257921381099, 0.323309699794707, 0.244543183000657
        ), .Dim = c(4L, 4L), .Dimnames = list(c("mu", "sigma", 
        "Q", "treatmentB"), c("est", "L95%", "U95%", "se"))), 
        res.t = structure(c(3.48934038518302, 0.112285302989812, 
        0.226637117128751, 0.326255368304298, 2.93933187816279, 
        -0.0338595559962098, -0.407038250321332, -0.153040463041778, 
        4.03934889220325, 0.258430161975833, 0.860312484578834, 
        0.805551199650375, 0.28062174170476, 0.0745650737150242, 
        0.323309699794707, 0.244543183000657), .Dim = c(4L, 4L
        ), .Dimnames = list(c("mu", "sigma", "Q", "treatmentB"
        ), c("est", "L95%", "U95%", "se"))), cov = structure(c(0.0787485619174131, 
        -0.00562343493820125, 0.0744820892147724, -0.0476932283197646, 
        -0.00562343493820125, 0.00555995021812699, -0.00674750698456497, 
        0.00234074941870914, 0.0744820892147724, -0.00674750698456497, 
        0.104529161981344, -0.0310049683751206, -0.0476932283197645, 
        0.00234074941870914, -0.0310049683751205, 0.0598013683520931
        ), .Dim = c(4L, 4L), .Dimnames = list(c("mu", "sigma", 
        "Q", "treatmentB"), c("mu", "sigma", "Q", "treatmentB"
        ))), coefficients = structure(c(3.48934038518302, 0.112285302989812, 
        0.226637117128751, 0.326255368304298), .Names = c("mu", 
        "sigma", "Q", "treatmentB")), npars = 4L, fixedpars = NULL, 
        optpars = 1:4, loglik = -501.536796456229, logliki = c(-4.25146125853404, 
        -4.09429471585882, -3.96212925910592, -5.7111150691556, 
        -4.03330108943238, -4.43978606383925, -4.08917850988047, 
        -4.88326199300013, -3.84105390263497, -3.85699697811168, 
        -3.94745161204945, -3.84503546567679, -5.07468897154146, 
        -4.40870038235393, -4.40870038235393, -3.87600975136596, 
        -4.08917850988047, -3.89739474345012, -3.92142276818734, 
        -4.59259998934873, -4.88326199300013, -3.97500238821718, 
        -3.85699697811168, -4.08917850988047, -6.48568754611015, 
        -5.86569663297714, -5.59676201576381, -3.96212925910592, 
        -6.27646513554227, -8.78431255917875, -6.07639815604686, 
        -8.23663335102199, -5.88728442492166, -3.89739474345012, 
        -4.40870038235393, -4.33689191860152, -5.04792494822708, 
        -4.93894020564527, -7.51881985259201, -6.31531209338619, 
        -3.87600975136596, -5.07468897154146, -6.11722060731675, 
        -6.09686180479033, -5.55004208504567, -9.14410189443623, 
        -3.84503546567679, -5.02097021839916, -4.88326199300013, 
        -6.31813627442719, -6.21080230140855, -5.36108977282018, 
        -4.16579001776801, -4.36914068591292, -5.14093873856428, 
        -6.00323627938436, -4.19884221318729, -4.9941649782141, 
        -4.21693639781341, -4.9079445004044, -4.95126511295899, 
        -5.51355984731052, -4.88612939255106, -5.58753621618744, 
        -6.01960638066983, -7.40112604387831, -8.81514411491913, 
        -6.24177094725048, -4.17562850498467, -4.66292616779633, 
        -4.95126511295899, -4.16820552798161, -6.60967520859074, 
        -4.45806866117988, -5.95370110267814, -4.52618773394536, 
        -5.74883691559842, -7.57257172908868, -4.57181796796278, 
        -5.01545508059024, -4.16820552798161, -4.1733522134682, 
        -4.86421326013746, -4.36914068591292, -5.97028434142698, 
        -4.30561247880257, -4.4355569683088, -4.66292616779633, 
        -4.95126511295899, -4.2298500656665, -4.21693639781341, 
        -4.24740635295818, -4.1860415871552, -4.24740635295818, 
        -4.57181796796278, -4.24740635295818, -4.1860415871552, 
        -6.28776843578506, -6.22631736856849), cl = 0.95, opt = structure(list(
            par = structure(c(3.48934038518302, 0.112285302989812, 
            0.226637117128751, 0.326255368304298), .Names = c("mu", 
            "sigma", "Q", "treatmentB")), value = 501.536796456229, 
            counts = structure(c(30L, 7L), .Names = c("function", 
            "gradient")), convergence = 0L, message = NULL, hessian = structure(c(79.0870488600603, 
            9.94112646424128, -43.86391761102, 39.9430260529243, 
            9.94112646424128, 196.481492608314, 6.70055685247917, 
            3.71163794454787, -43.86391761102, 6.70055685247917, 
            36.3976135844268, -16.3740284051528, 39.9430260529243, 
            3.71163794454787, -16.3740284051528, 39.9430260529243
            ), .Dim = c(4L, 4L), .Dimnames = list(c("mu", "sigma", 
            "Q", "treatmentB"), c("mu", "sigma", "Q", "treatmentB"
            )))), .Names = c("par", "value", "counts", "convergence", 
        "message", "hessian")), BIC = 1021.454072313, m2LL = 1003.07359291246), .Names = c("call", 
    "dlist", "aux", "ncovs", "ncoveffs", "mx", "basepars", "covpars", 
    "AIC", "data", "datameans", "N", "events", "trisk", "concat.formula", 
    "all.formulae", "dfns", "res", "res.t", "cov", "coefficients", 
    "npars", "fixedpars", "optpars", "loglik", "logliki", "cl", 
    "opt", "BIC", "m2LL"), class = "flexsurvreg")), .Dim = c(6L, 
3L), .Dimnames = list(c("exp", "weibull", "lnorm", "gamma", "gompertz", 
"gengamma"), c("A", "B", "all")))
