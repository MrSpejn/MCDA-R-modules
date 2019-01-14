context('cycle removal')

test_that('2 node cycle is found', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  1,  1, 
        1,  0,  1,  0,  0, 
        0,  0,  0,  1,  0, 
        0,  1,  0,  0,  0, 
        0,  1,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    cycle = findCycle(adjencencyMatrix)
    expect_equal(cycle, c(1, 2))
})

test_that('3 node cycle is found', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  0, 
        0,  0,  1,  1,  0, 
        0,  0,  0,  1,  0, 
        1,  0,  0,  0,  1, 
        0,  1,  0,  0,  0
    ), nrow=5, byrow=TRUE)

    cycle = findCycle(adjencencyMatrix)
    expect_equal(cycle, c(1, 2, 3, 4))
})

test_that('if no cycle is present 0 length vector returned', {
    adjencencyMatrix = matrix(c(
        0,  0,  0,  0,  0, 
        0,  0,  1,  1,  0, 
        0,  0,  0,  1,  0, 
        1,  0,  0,  0,  0, 
        1,  1,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    cycle = findCycle(adjencencyMatrix)
    expect_equal(cycle, c())
})

test_that('no cycle found on graph without edges', {
    adjencencyMatrix = matrix(c(
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0
    ), nrow=5, byrow=TRUE)

    cycle = findCycle(adjencencyMatrix)
    expect_equal(cycle, c())
})

test_that('finds cicle when first 2 node are edgeless', {
    adjencencyMatrix = matrix(c(
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  1,  0, 
        0,  0,  0,  0,  1, 
        0,  0,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    cycle = findCycle(adjencencyMatrix)
    expect_equal(cycle, c(3, 4, 5))
})

test_that('adjecency matrix is aggregated correctly', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  1, 
        0,  0,  0,  1,  1, 
        1,  1,  0,  1,  0, 
        1,  0,  0,  0,  1, 
        0,  0,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    expected_result = matrix(c(
        0,  0,  1,  1, 
        1,  0,  0,  1,  
        0,  1,  0,  0, 
        1,  0,  1,  0 
    ), nrow=4, byrow=TRUE)
    
    result = aggregateToSingleNode(adjencencyMatrix, c(2, 4))

    expect_equal(as.vector(result), as.vector(expected_result))
})

test_that('can aggregate to single node', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  1, 
        0,  0,  0,  1,  1, 
        1,  1,  0,  1,  0, 
        1,  0,  0,  0,  1, 
        0,  0,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    expected_result = matrix(c(
        0
    ), nrow=1, byrow=TRUE)

    result = aggregateToSingleNode(adjencencyMatrix, c(1, 2, 3, 4, 5))

    expect_equal(as.vector(result), as.vector(expected_result))
})

test_that('can aggregate only one element', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  1, 
        0,  0,  0,  1,  1, 
        1,  1,  0,  1,  0, 
        1,  0,  0,  0,  1, 
        0,  0,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    expected_result = matrix(c(
        0,  1,  0,  1,  0, 
        0,  0,  1,  1,  0, 
        1,  0,  0,  1,  0, 
        0,  0,  0,  0,  1,
        1,  1,  1,  0,  0

    ), nrow=5, byrow=TRUE)

    result = aggregateToSingleNode(adjencencyMatrix, c(3))
    
    expect_equal(as.vector(result), as.vector(expected_result))
})

test_that('can aggregate to 2 elements', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  1, 
        0,  0,  0,  1,  1, 
        1,  1,  0,  1,  0, 
        1,  0,  0,  0,  1, 
        0,  0,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    expected_result = matrix(c(
        0,  1, 
        1,  0
    ), nrow=2, byrow=TRUE)
    
    result = aggregateToSingleNode(adjencencyMatrix, c(1, 2, 3, 4))

    expect_equal(as.vector(result), as.vector(expected_result))
})



test_that('single cycle is remove correctly', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  0, 
        0,  0,  1,  1,  0, 
        0,  0,  0,  1,  0, 
        1,  0,  0,  0,  0, 
        1,  1,  1,  0,  0
    ), nrow=5, byrow=TRUE)

    expected_result = matrix(c(
        0, 1,
        0, 0
    ), nrow=2, byrow=TRUE)

    result = removeCycles(adjencencyMatrix)

    expect_equal(as.vector(result), as.vector(expected_result))

})


test_that('cycles are remove until none is left', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  1, 
        0,  0,  1,  0,  0, 
        0,  1,  0,  0,  0, 
        1,  0,  0,  0,  0, 
        0,  0,  1,  1,  0
    ), nrow=5, byrow=TRUE)

    expected_result = matrix(c(
        0, 0,
        1, 0
    ), nrow=2, byrow=TRUE)

    result = removeCycles(adjencencyMatrix)

    expect_equal(as.vector(result), as.vector(expected_result))
})

test_that('fully connected is reduced to one node', {
    adjencencyMatrix = matrix(c(
        0,  1,  1,  1,  1, 
        1,  0,  1,  1,  1, 
        1,  1,  0,  1,  1, 
        1,  1,  1,  0,  1, 
        1,  1,  1,  1,  0
    ), nrow=5, byrow=TRUE)

    expected_result = matrix(c(
        0
    ), nrow=1, byrow=TRUE)

    result = removeCycles(adjencencyMatrix)

    expect_equal(as.vector(result), as.vector(expected_result))
})