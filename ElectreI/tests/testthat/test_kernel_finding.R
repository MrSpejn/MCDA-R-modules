context('kernel')

test_that('finds kernel for graph', {
    adjencencyMatrix = matrix(c(
        0,  1,  0,  0,  1, 
        0,  0,  1,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  1,  1,  0
    ), nrow=5, byrow=TRUE)

    result = findKernel(adjencencyMatrix)

    expect_equal(sort(result), c(1, 3, 4))
})

test_that('for graph without edges all vertices are in kernel', {
    adjencencyMatrix = matrix(c(
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0
    ), nrow=5, byrow=TRUE)

    result = findKernel(adjencencyMatrix)

    expect_equal(sort(result), c(1, 2, 3, 4, 5))
})

test_that('finds kernel for long narrow graph', {
    adjencencyMatrix = matrix(c(
        0,  0,  0,  1,  0, 
        0,  0,  0,  0,  1, 
        1,  0,  0,  0,  0, 
        0,  1,  0,  0,  0, 
        0,  0,  0,  0,  0
    ), nrow=5, byrow=TRUE)

    result = findKernel(adjencencyMatrix)

    expect_equal(sort(result), c(3, 4, 5))
})

test_that('all not connected are in the kernel', {
     adjencencyMatrix = matrix(c(
        0,  0,  0,  0,  0, 
        0,  0,  0,  0,  0, 
        0,  0,  0,  1,  0, 
        0,  0,  0,  0,  1, 
        0,  0,  0,  0,  0
    ), nrow=5, byrow=TRUE)

    result = findKernel(adjencencyMatrix)

    expect_equal(sort(result), c(1, 2, 3, 5))
})