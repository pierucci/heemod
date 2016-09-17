context("Get code")

test_that(
  "Translating objects to code", {
    s1 <- define_state(
      cost = 543 + age * 5,
      ly = 1
    )
    mat1 <- define_matrix(
      .4, .6,
      .1, .9
    )
    mod1 <-
      define_model(
        transition_matrix =  mat1,
        s1,
        define_state(
          cost = 432 + age,
          ly = 1 * age / 100
        )
      )
    
    mod2 <-
      define_model(
        transition_matrix = define_matrix(
          .5, .5,
          .1, .9
        ),
        define_state(
          cost = 789 * age / 10,
          ly = 1
        ),
        define_state(
          cost = 456 * age / 10,
          ly = 1 * age / 200
        )
      )
    p1 <- define_parameters(
      age_init = 60,
      age = age_init + markov_cycle
    )
    res <- run_models(
      mod1, mod2,
      parameters = p1,
      init = 1:0,
      cycles = 10,
      cost = cost,
      effect = ly,
      method = "beginning"
    )
    
    expect_identical(
      formatR::tidy_source(
        text = get_code(res),
        width.cutoff = 57,
        output = FALSE)$text.tidy,
      'run_models(I = define_model(transition_matrix = define_matrix(state_names = c("A", 
    "B"), 0.4, 0.6, 0.1, 0.9), A = define_state(cost = 543 + 
    age * 5, ly = 1), B = define_state(cost = 432 + age, ly = 1 * 
    age/100)), II = define_model(transition_matrix = define_matrix(state_names = c("A", 
    "B"), 0.5, 0.5, 0.1, 0.9), A = define_state(cost = 789 * 
    age/10, ly = 1), B = define_state(cost = 456 * age/10, 
    ly = 1 * age/200)), parameters = define_parameters(age_init = 60, 
    age = age_init + markov_cycle), init = c(1, 0), cycles = 10, 
    method = "beginning", base_model = "II", cost = cost, 
    effect = ly)'
    )
    expect_identical(
      formatR::tidy_source(
        text = get_code(res, name = "res"), width.cutoff = 57,
        output = FALSE)$text.tidy,
      'res <- run_models(I = define_model(transition_matrix = define_matrix(state_names = c("A", 
    "B"), 0.4, 0.6, 0.1, 0.9), A = define_state(cost = 543 + 
    age * 5, ly = 1), B = define_state(cost = 432 + age, ly = 1 * 
    age/100)), II = define_model(transition_matrix = define_matrix(state_names = c("A", 
    "B"), 0.5, 0.5, 0.1, 0.9), A = define_state(cost = 789 * 
    age/10, ly = 1), B = define_state(cost = 456 * age/10, 
    ly = 1 * age/200)), parameters = define_parameters(age_init = 60, 
    age = age_init + markov_cycle), init = c(1, 0), cycles = 10, 
    method = "beginning", base_model = "II", cost = cost, 
    effect = ly)'
    )
    expect_identical(
      formatR::tidy_source(
        text = get_code(res, sub = TRUE), width.cutoff = 57,
        output = FALSE)$text.tidy,
      'run_models(I = m_i, II = m_ii, parameters = define_parameters(age_init = 60, 
    age = age_init + markov_cycle), init = c(1, 0), cycles = 10, 
    method = "beginning", base_model = "II", cost = cost, 
    effect = ly)'
    )
    expect_identical(
      formatR::tidy_source(
        text = get_code(mod1, sub = TRUE, name = "m1"),
        width.cutoff = 57,
        output = FALSE)$text.tidy,
      'm1 <- define_model(transition_matrix = define_matrix(state_names = c("A", 
    "B"), 0.4, 0.6, 0.1, 0.9), A = s_a, B = s_b)'
    )
    expect_identical(
      formatR::tidy_source(
        text = get_code(s1, sub = TRUE, name = "m1"),
        width.cutoff = 57,
        output = FALSE)$text.tidy,
      'm1 <- define_state(cost = 543 + age * 5, ly = 1)'
    )
    expect_identical(
      formatR::tidy_source(
        text = get_code(p1, sub = TRUE, name = "m1"),
        width.cutoff = 57,
        output = FALSE)$text.tidy,
      'm1 <- define_parameters(age_init = 60, age = age_init + markov_cycle)'
    )
    expect_identical(
      formatR::tidy_source(
        text = get_code(mat1, sub = TRUE, name = "m1"),
        width.cutoff = 57,
        output = FALSE)$text.tidy,
      'm1 <- define_matrix(state_names = c(\"A\", \"B\"), 0.4, 0.6, 0.1, \n    0.9)'
    )
  })
