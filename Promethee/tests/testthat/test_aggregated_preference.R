context('aggregated_preference')

test_that('aggregate preference is an average from preference array on criteria dimension', {
    preference_array = array(c(
       1, 1, 1, 1,
       0, 0, 0, 0,
       0.5, 0.5, 0.5, 0.5,
       .33, .66, .33, .33,

       1, 1, 1, 1,
       0, 0, 0.1, 0,
       0.5, 1.5, 1.5, 0.5,
       .33, .66, .33, .33,

       1, 1, 1, 1,
       0, 0, 0, 0,
       0.5, 0.5, 1.5, 0.5,
       .33, .66, .33, .33
    ), dim = c(4, 4, 3))

    
    result = calculateAggregatedPreference(preference_array, c(1, 1, 1))

    expected_result = array(c(
        3, 3, 3, 3,
        0, 0, 0.1, 0,
        1.5, 2.5, 3.5, 1.5,
        0.99, 1.98, 0.99, 0.99
    ), dim = c(4, 4))

    distance = abs(3*result - expected_result)
    expect_true(all(distance < 0.001))
})

test_that('aggregate preference is an average from preference array on criteria dimension with non equal weights', {
    preference_array = array(c(
       1, 1, 1, 1,
       0, 0, 0, 0,
       0.5, 0.5, 0.5, 0.5,
       .33, .66, .33, .33,

       1, 1, 1, 1,
       0, 0, 0.1, 0,
       0.5, 0.5, 1.5, 0.5,
       .33, .66, .33, .33,

       1, 1, 1, 1,
       0, 0, 0, 0,
       0.5, 1.5, 1.5, 0.5,
       .33, .66, .33, .33
    ), dim = c(4, 4, 3))

    
    result = calculateAggregatedPreference(preference_array, c(1, 2, 3))

    expected_result = array(c(
        1, 1, 1, 1,
        0, 0, .033, 0,
        .5, 1, 1.3333, .5,
        .33, .66, .33, .33
    ), dim = c(4, 4))

    distance = abs(result - expected_result)
    expect_true(all(distance < 0.001))
})
