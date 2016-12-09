context("Cost Effective Frontier")

test_that(
  "Identify Frontier Scenarios",{
    
    # Frontier:           S1, S5, S8
    # Weakly Dominated:   S2,
    # Strongly Dominated: S3,
    
    test1 <- data.frame(
      .strategy_names=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7","Scenario 8"),
      .cost=c(14807,26041,49706,64266,40022,57392,91744,54651),
      .effect=c(9.635,10.2497,11.1654,11.5152,11.582,11.6248,11.6296,11.6588), stringsAsFactors=FALSE)
    
    result1 <- get_frontier(test1)
    
    expect_identical(
      sort(sort(result1)),
      c("Scenario 1", "Scenario 5", "Scenario 8")
    )  
    
    # Special Case:       Least effective strategy is not the least cost
    # Special Case:       No weakly dominated strategies
    # Frontier:           S2, S5
    # Weakly Dominated:   None
    # Strongly Dominated: S1, S2, S4
    test2 <- data.frame(
      .strategy_names=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5"),
      .cost=c(26041,14807,49706,64266,40022),
      .effect=c(9.13,9.635,11.1654,11.5152,11.582), stringsAsFactors=FALSE)
    
    result2 <- get_frontier(test2)
    expect_identical(
      sort(result2),
      c("Scenario 2", "Scenario 5")
    )
    
    # Special Case:       No strongly dominated strategies
    # Frontier:           S1, S2, S4,S6
    # Weakly Dominated:   S3, S5
    # Strongly Dominated: None  
    test3<-data.frame(.strategy_names=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6"),
                      .cost=c(25000,35000,50000,54651,64266,91744),
                      .effect=c(10.2497,11.1654,11.3,11.6588,11.7,12.1), stringsAsFactors=FALSE)
    
    result3 <- get_frontier(test3)
    
    expect_identical(
      sort(result3),
      c("Scenario 1", "Scenario 2", "Scenario 4", "Scenario 6")
    )
    
    # Special Case:       S5 and S6 have the same QALYs but
    #                     S6 has lower cost
    # Frontier:           S1, S6
    # Weakly Dominated:   S2, S5,
    # Strongly Dominated: S3, S4,     
    test4 <- data.frame(.strategy_names=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6"),
                        .cost=c(26041,49706,64266,57392,91744,54651),
                        .effect=c(10.2497,11.1654,11.5152,11.6248,11.6296,11.6296), stringsAsFactors=FALSE)
    
    result4 <- get_frontier(test4)    
    expect_identical(
      sort(result4),
      c("Scenario 1", "Scenario 6")
    )
    
    # Special Case:       All scenario on the frontier
    # Frontier:           S1, S2, S3, S4, S5
    # Weakly Dominated:   None
    # Strongly Dominated: None 
    test5<-data.frame(.strategy_names=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5"),
                      .cost=c(26041,42000,54651,60000,76000),
                      .effect=c(10.2497,11.1654,11.6296,11.7,11.8), stringsAsFactors=FALSE)
    
    result5 <- get_frontier(test5)
    expect_identical(
      sort(result5),
      c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5")
    )
    
    # Special Case:       two pairs of scenarios with the same 
    #                     QALY (S4,S5 and S1,S2) but different costs
    # Frontier:           S1, S4, S7
    # Weakly Dominated:   S2, S3, S5, S6
    # Strongly Dominated: None 
    test6<-data.frame(.strategy_names=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7"),
                      .cost=c(25000,35000,50000,54651,58500,64266,91744),
                      .effect=c(10.2497,10.2497,11.3,11.6588,11.6588,11.7,12.1),
                      stringsAsFactors=FALSE
    )
    
    result6 <- get_frontier(test6)
    expect_identical(
      sort(result6),
      c("Scenario 1",  "Scenario 4", "Scenario 7")
    )
    
    # Special Case:       two identical scenarios (S4 and S5)
    #                     Two scenario with same cost (S1 and S2)
    # Frontier:           S1, S4, S7
    # Weakly Dominated:   S2, S3, S5, S6
    # Strongly Dominated: None 
    # Note:               The identical scenarios should both 
    #                     be on the frontier.
    test7<-data.frame(.strategy_names=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7"),
                      .cost=c(25000,25000,50000,58500,58500,64266,91744),
                      .effect=c(10.2497,10.25,11.3,11.6588,11.6588,11.7,12.1),
                      stringsAsFactors=FALSE
                      
    )
    result7 <- get_frontier(test7)
    expect_identical(
      sort(result7),
      c("Scenario 2", "Scenario 4","Scenario 5", "Scenario 7")
    )
  }
)
