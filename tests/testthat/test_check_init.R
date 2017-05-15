context("Init checking")

test_that(
  "check_init() works", {
    init1 <- define_init(a = 1, b = 0, c = 0)
    init2 <- c(a = 1, b = 0, c = 0)
    init2_bis <- c(a = 1, c = 0, b = 0)
    init3 <- c(1, 0, 0)
    init4 <- define_init(a = 1)
    
    ref <- letters[1:3]
    
    expect_identical(
      to_text_dots(check_init(init1, ref)),
      to_text_dots(check_init(init2, ref))
    )
    
    expect_identical(
      to_text_dots(check_init(init2, ref)),
      to_text_dots(check_init(init2_bis, ref))
    )
    
    expect_identical(
      to_text_dots(check_init(init1, ref)),
      to_text_dots(check_init(init3, ref))
    )
    
    expect_identical(
      to_text_dots(check_init(init1, ref)),
      to_text_dots(check_init(init4, ref))
    )
    
    expect_error(
      check_init(c(1, 0), ref)
    )
    expect_error(
      check_init(c(a = 1, b = 0, x = 0), ref)
    )
    expect_error(
      check_init(define_init(1, 0, 0), ref)
    )
    expect_error(
      check_init(define_init(1, 0), ref)
    )
    expect_error(
      check_init(define_init(a = 1, b = 0, x = 0), ref)
    )
    expect_error(
      check_init(define_init(a = 1, x = 0), ref)
    )
    expect_error(
      check_init(define_init(a = 1, a = 0), ref)
    )
  }
)
