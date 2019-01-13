calculatePreference <- function(
    performanceTable,
    criteriaPreferenceFunctions
) {
    preferenceCube <- array(
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
        criterionPreferenceFunction <- criteriaPreferenceFunctions[[c]]

        for (i in seq(nrow(performanceTable))) {
            for (j in seq(nrow(performanceTable))) {
                if (i == j) next
                preferenceCube[i, j, c] <- 
                    criterionPreferenceFunction(performanceTable[i, c], performanceTable[j, c])
            }
        }
    }
    return(preferenceCube)
}

calculateCriteriaPositiveFlows <- function(preferenceCube) {
    alternativesLength <- dim(preferenceCube)[1]

    apply(preferenceCube, c(1, 3), function(alernativeAgainsOther) {
        sum(alernativeAgainsOther)/(alternativesLength - 1)
    })
}

calculateCriteriaNegativeFlows <- function(preferenceCube) {
    alternativesLength <- dim(preferenceCube)[1]

    apply(preferenceCube, c(2, 3), function(othersAgainstAlternative) {
        sum(othersAgainstAlternative)/(alternativesLength - 1)
    })
}


calculateAggregatedPreference <- function(preferenceCube, criteriaWeights) {
    criteriaWeightsSum <- sum(criteriaWeights)
    
    apply(preferenceCube, c(1, 2), function(criteria) {
        sum(criteria*criteriaWeights)/criteriaWeightsSum
    })
}

mapPositiveOutrankingFlow <- function(aggregatedPreferenceTable) {
    numberOfAlternatives <- dim(aggregatedPreferenceTable)[1]

    apply(aggregatedPreferenceTable, 1, function(row) {
        sum(row) / (numberOfAlternatives - 1)
    })
}

mapNegativeOutrankingFlow <- function(aggregatedPreferenceTable) {
    numberOfAlternatives <- dim(aggregatedPreferenceTable)[1]
    
    apply(aggregatedPreferenceTable, 2, function(row) {
        sum(row) / (numberOfAlternatives - 1)
    })
}

calculateCriteriaNetFlows <- function(preferenceCube) {
    calculateCriteriaPositiveFlows(preferenceCube) - calculateCriteriaNegativeFlows(preferenceCube)
}

PrometheeI <- function(
    performanceTable,
    criteriaPreferenceFunction,
    criteriaWeights
) {
    if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
        stop("wrong performanceTable, should be a matrix or a data frame")

    
    numberOfAlternatives <- dim(performanceTable)[1]
    preferenceCube <- calculatePreference(performanceTable, criteriaPreferenceFunction)
    print(preferenceCube)
    aggregatedPreferenceTable <- calculateAggregatedPreference(preferenceCube, criteriaWeights)
    print(aggregatedPreferenceTable)
    positiveFlow <- mapPositiveOutrankingFlow(aggregatedPreferenceTable)
    negativeFlow <- mapNegativeOutrankingFlow(aggregatedPreferenceTable)

    
    flow <- cbind(positiveFlow, negativeFlow)
    
    ranking <- array('_', dim=c(numberOfAlternatives, numberOfAlternatives))

    print(aggregatedPreferenceTable)
    print(flow)

    for (i in seq(numberOfAlternatives)) {
        for (k in seq(numberOfAlternatives)) {
            positiveI <- positiveFlow[i]
            positiveK <- positiveFlow[k]
            negativeI <- negativeFlow[i]
            negativeK <- negativeFlow[k]

            if (positiveI > positiveK) {
                if (negativeI <= negativeK) {
                    ranking[i, k] = 'P'
                } else {
                    ranking[i, k] = 'R'
                }
            } else if (negativeI < negativeK) {
                if (positiveI >= positiveK) {
                    ranking[i, k] = 'P'
                } else {
                    ranking[i, k] = 'R'
                }
            } else if (positiveI == positiveK && negativeI == negativeK) {
                ranking[i, k] = 'I'
            }
        }
    }

    ranking
}

PrometheeII <- function(
    performanceTable,
    criteriaPreferenceFunction,
    criteriaWeights
) {
    if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
        stop("wrong performanceTable, should be a matrix or a data frame")
    
    numberOfAlternatives <- dim(performanceTable)[1]
    preferenceCube <- calculatePreference(performanceTable, criteriaPreferenceFunction)
    aggregatedPreferenceTable <- calculateAggregatedPreference(preferenceCube, criteriaWeights)
    positiveFlow <- mapPositiveOutrankingFlow(aggregatedPreferenceTable)
    negativeFlow <- mapNegativeOutrankingFlow(aggregatedPreferenceTable)

    flow <- positiveFlow + negativeFlow

    ranking <- array('_', dim=c(numberOfAlternatives, numberOfAlternatives))

    for (i in seq(numberOfAlternatives)) {
        for (k in seq(numberOfAlternatives)) {
            if (flow[i] > flow[k]) {
                ranking[i, k] = 'P'
            } else if (flow[i] == flow[k]) {
                ranking[i, k] = 'I'
            }
        }
    }

    ranking
}

get_criteria_difference <- function (a_criterion_value, b_criterion_value, criterion_gain) {
    criteria_difference <- a_criterion_value - b_criterion_value
    if (!criterion_gain) {
        criteria_difference <- -1 * criteria_difference 
    }
    criteria_difference
}

criteriaFunctions = list()
criteriaFunctions[["Usual"]] = function (criterion_gain=TRUE) { 
    function(a_criterion_value, b_criterion_value) {
        criteria_difference <- get_criteria_difference(a_criterion_value, b_criterion_value, criterion_gain)
        
        if (criteria_difference > 0) return(1)
        return(0)
    }
}
criteriaFunctions[["U-shape"]] = function (q, criterion_gain=TRUE) { 
    function(a_criterion_value, b_criterion_value) {
        criteria_difference <- get_criteria_difference(a_criterion_value, b_criterion_value, criterion_gain)
        
        if (criteria_difference > q) return(1)
        return(0)
    }
}
criteriaFunctions[["V-shape"]] = function (p, criterion_gain=TRUE) { 
    function(a_criterion_value, b_criterion_value) {
        criteria_difference <- get_criteria_difference(a_criterion_value, b_criterion_value, criterion_gain)

        if (criteria_difference > p) return(1)
        if (criteria_difference <= 0) return(0)
        return(criteria_difference/p)
    }
}
criteriaFunctions[["V-shape-with-incriteria"]] = function (q, p, criterion_gain=TRUE) { 
    function(a_criterion_value, b_criterion_value) {
        criteria_difference <- get_criteria_difference(a_criterion_value, b_criterion_value, criterion_gain)
        
        if (criteria_difference < q) return(0)
        if (criteria_difference > p) return(1)
        return((criteria_difference - q) / (p - q))
    }
}
criteriaFunctions[["Level"]] = function (q, p, criterion_gain=TRUE) { 
    function(a_criterion_value, b_criterion_value) {
        criteria_difference <- get_criteria_difference(a_criterion_value, b_criterion_value, criterion_gain)
        
        if (criteria_difference <= q) return(0)
        if (criteria_difference > p) return(1)
        return(0.5)
    }
}
criteriaFunctions[["Gaussian"]] = function (s, criterion_gain=TRUE) { 
    function(a_criterion_value, b_criterion_value) {
        criteria_difference <- get_criteria_difference(a_criterion_value, b_criterion_value, criterion_gain)
        
        if (criteria_difference <= 0) return(0)
        squared <- criteria_difference * criteria_difference
        return (1 - exp(-1*squared/(2*s*s)))
    }
}