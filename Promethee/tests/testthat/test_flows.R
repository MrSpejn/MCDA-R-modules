context('flows')

test_that('positive outranking flows are calculated correctly', {
    aggregatedPrefference = matrix(c(
          0, 1,  .6, .1,
         .9, 0,  .8, .3,
          1, 1,   0,  1, 
        .15, 1, .25,  0
    ), nrow=4, byrow=TRUE)
    expectedOutrankingFlows = c(
        0.56666,
        0.66666,
        1,
        0.46666
    )

    result <- calculatePositiveOutrankingFlow(aggregatedPrefference)
    expect_true(all(abs(expectedOutrankingFlows - result) < 0.001))
})

test_that('negative outranking flows are calculated correctly', {
    aggregatedPrefference = matrix(c(
          0, 1,  .6, .1,
         .9, 0,  .8, .3,
          1, 1,   0,  1, 
        .15, 1, .25,  0
    ), nrow=4, byrow=TRUE)
    expectedOutrankingFlows = c(
        0.68333,
        1,
        0.55,
        0.46666
    )


    result <- calculateNegativeOutrankingFlow(aggregatedPrefference)
    expect_true(all(abs(expectedOutrankingFlows - result) < 0.001))
})