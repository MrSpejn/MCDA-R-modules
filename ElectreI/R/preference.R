concordanceTest <- function(alternative, comparedAlternative, indifference, preference, gain = TRUE) {
    criterionDiff <- alternative - comparedAlternative
    
    if (criterion$gain == TRUE) {
        criterionDiff <- -1 * criterionDiff
    }
    
    if (indifference > criterionDiff) {
        return(1)
    }
    
    if (preference < criterionDiff) {
        return(0)
    }
    (preference - criterionDiff) / (preference - indifference)
    
}

discordanceTest <- function(alternative, comparedAlternative, veto, gain = TRUE) {
    criterionDiff <- alternative - comparedAlternative
    
    if (gain == TRUE) {
        criterionDiff <- -1 * criterionDiff
    }
    
    if (difference > veto) {
        return(TRUE)
    }
    return(FALSE)
}

calculatePartialConcordanceCoefficients <- function(
    performanceTable,
    directions,
    indifferenceThresholds,
    preferenceTreshholds
) {
    partialConcordanceCoefficientsCube <- array(
        0, 
        dim=c(
            nrow(performanceTable),
            nrow(performanceTable),
            ncol(performanceTable)
        ),
        dimnames = list(
            rownames(performanceTable),
            rownames(performanceTable),
            colnames(performanceTable)
        )
    )
    
    for (c in seq(ncol(performanceTable))) {
        criterion <- criteria[[c]]

        for (i in seq(nrow(performanceTable))) {
            for (j in seq(nrow(performanceTable))) {
                if (i == j) next
                partialConcordanceCoefficientsCube[i, j, c] <- 
                    concordanceTest(
                        performanceTable[i, c],
                        performanceTable[j, c],
                        indifferenceThresholds[c],
                        preferenceTreshholds[c],
                        gain = directions[c] == 'max'
                    )                    
            }
        }
    }
    return(partialConcordanceCoefficientsCube)
}

calculateConcordanceCoefficients <- function(partialConcordanceCoefficientsCube, criteriaWeights) {
    criteriaWeightsSum <- sum(criteriaWeights)

    apply(partialConcordanceCoefficientsCube, c(1, 2), function(partialConcordanceCoefficient) {
        sum(partialConcordanceCoefficient*criteriaWeights) / criteriaWeightsSum
    })
}

calculateVetoMatrix <- function(performanceTable, directions, vetoThresholds) {
    vetoMatrix <- array(
        FALSE, 
        dim=c(
            nrow(performanceTable),
            nrow(performanceTable)
        ),
        dimnames = list(
            rownames(performanceTable),
            rownames(performanceTable)
        )
    )

    for (i in seq(nrow(performanceTable))) {
        for (j in seq(nrow(performanceTable))) {
            if (i == j) next
            vetoThresholdReached <- sapply(
                seq(length(criteria)),
                function(idx) discordanceTest(
                    performanceTable[i, idx],
                    performanceTable[j, idx],
                    vetoThresholds[idx],
                    gain = directions[idx] == 'max',
                )
            )
            vetoMatrix[i, j] <- any(vetoThresholdReached)
        }
    }

    return(vetoMatrix)
}
