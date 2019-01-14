context('rankings')

test_that('getPartialRankingRelation calculates relation correctly', {
    expect_equal(getPartialRankingRelation(.2, .8, .4, .6), '-')
    expect_equal(getPartialRankingRelation(.2, .8, .2, .6), '-')
    expect_equal(getPartialRankingRelation(.2, .8, .4, .8), '-')
    expect_equal(getPartialRankingRelation(1, .5, 1, .5), 'I')
    expect_equal(getPartialRankingRelation(.8, .2, .6, .2), 'P')
    expect_equal(getPartialRankingRelation(1, .4, 1, .5), 'P')
    expect_equal(getPartialRankingRelation(1, .4, .8, .5), 'P')
    expect_equal(getPartialRankingRelation(.8, .4, .7, .3), 'R')
    expect_equal(getPartialRankingRelation(.7, .3, .8, .4), 'R')
})

test_that('getFullRankingRelation calculates relation correctly', {
    expect_equal(getFullRankingRelation(.2, .6), '-')
    expect_equal(getFullRankingRelation(.6, .2), 'P')
    expect_equal(getFullRankingRelation(.8, .8), 'I')
    expect_equal(getFullRankingRelation(-.8, -.8), 'I')
    expect_equal(getFullRankingRelation(-.2, -.6), 'P')
    expect_equal(getFullRankingRelation(-.6, -.2), '-')
})

test_that('createRanking returns a full ranking at default', {
    netFlows = c(
        0.5,
        0.6,
        1,
        0.4
    )
    expectedRanking = matrix(c(
        'I', '-', '-', 'P', 
        'P', 'I', '-', 'P', 
        'P', 'P', 'I', 'P', 
        '-', '-', '-', 'I'
    ), nrow=4, byrow=TRUE)

    expect_equal(
        as.vector(createRanking(netFlows)),
        as.vector(expectedRanking)
    )
})

test_that('createRanking can create partial ranking', {
    posiveFlows = c(
        .8,
        .3,
        1,
        .3
    )
    negativeFlows = c(
        .8,
        .3,
        .2,
        .9
    )

    expectedRanking = matrix(c(
        'I', 'R', '-', 'P', 
        'R', 'I', '-', 'P', 
        'P', 'P', 'I', 'P', 
        '-', '-', '-', 'I'
    ), nrow=4, byrow=TRUE)

    expect_equal(
        createRanking(posiveFlows, negativeFlows, full=FALSE),
        expectedRanking
    )
})