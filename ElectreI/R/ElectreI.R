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
        
    preferenceRelation <- concorndanceCoefficients > lambda

    preferenceGraph <- preferenceRelation & !vetoMatrix

    acyclicPreferenceGraph <- remove_cycles(preferenceGraph)

    kernel <- find_kernel(acyclicPreferenceGraph)
}