ElectreI <- function(performanceMatrix, criteriaWeights, directions, indifferenceThresholds, preferenceTreshholds, vetoThresholds) {
    concorndanceCoefficients <- calculateConcordanceCoefficients(
        calculatePartialConcordanceCoefficients(
            performanceMatrix,
            directions,
            indifferenceThresholds,
            preferenceTreshholds
        ),
        criteriaWeights,
    )
    vetoMatrix <- calculateVetoMatrix(
        performanceMatrix,
        directions,
        vetoThresholds
    )
    
    preferenceGraph <- concorndanceCoefficients > lambda & !vetoMatrix

    acyclicPreferenceGraph <- remove_cycles(preferenceGraph)

    kernel <- find_kernel(acyclicPreferenceGraph)
    
    return(list(
        kernel=kernel,
        graph=acyclicPreferenceGraph,
    ))
}