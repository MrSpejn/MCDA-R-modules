Criterion <- function(weight, type="max", b_params, a_params=c(0, 0, 0)) {
    q <- function (value) a_params[0]*value + b_params[0]
    p <- function (value) a_params[1]*value + b_params[1]
    v <- function (value) a_params[2]*value + b_params[2]
    
    return(list(
        weight = weight,
        gain = type=="max",
        q = q,
        p = p,
        v = v
    ))
}

concordance_test <- function(alternative, comparedAlternative, criterion) {
    indifference_treshhold <- criterion$q(alternative)
    preference_treshhold <- criterion$p(alternative)
    difference <- alternative - comparedAlternative
    
    if (criterion$gain == TRUE) {
        difference <- -1 * difference
    }
    
    if (indifference_treshhold > difference) {
        return(1)
    }
    
    if (preference_treshhold < difference) {
        return(0)
    }
    (preference_treshhold - difference) / (preference_treshhold - indifference_treshhold)
    
}

discordance_test <- function(alternative, comparedAlternative, criterion) {
    veto_treshhold <- criterion$v(alternative)
    difference <- alternative - comparedAlternative
    
    if (criterion$gain == TRUE) {
        difference <- -1 * difference
    }
    
    if (difference > veto_treshhold) {
        return(TRUE)
    }
    return(FALSE)
}

calculatePartialConcordanceCoefficients <- function(
    performanceTable,
    criteria
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
                    concordance_test(performanceTable[i, c], performanceTable[j, c], criterion)                    
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

calculateVetoMatrix <- function(performanceTable, criteria) {
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
            veto_threshold_reached <- sapply(
                seq(length(criteria)),
                function(idx) discordance_test(performanceTable[i, idx], performanceTable[j, idx], criteria[[idx]]))
            vetoMatrix[i, j] <- any(veto_threshold_reached)
        }
    }

    return(vetoMatrix)
}

traverse_deep <- function(node, nodes_dependants, path, cycles) {
    current_path <- append(path, node)
    if (length(nodes_dependants[[node]]) == 0) {
        return(list(nodes_dependants, cycles))
    }
    
    updated_nodes_dependants <- nodes_dependants
    updated_cycles <- cycles
    
    
    for (successor_idx in seq(1:length(updated_nodes_dependants[[node]]))) {
        
        updated_nodes_dependants[[node]][[successor_idx]]$traversed <- TRUE
        successor <- updated_nodes_dependants[[node]][[successor_idx]]

        if (successor$idx %in% current_path) {
            updated_cycles[[length(updated_cycles)+1]] <- 
                unlist(current_path[match(successor$idx, current_path):length(current_path)])
            next  
        }
        new_data <- traverse_deep(successor$idx, updated_nodes_dependants, current_path, updated_cycles)
        updated_nodes_dependants <- new_data[[1]]
        updated_cycles <- new_data[[2]]
    }
    return(list(updated_nodes_dependants, updated_cycles))
}

find_cicles <- function(adjacency_matrix) {
    nodes_dependants <- list()

    for (row_idx in seq(1, nrow(adjacency_matrix))) {
        row <- adjacency_matrix[row_idx, ]
        dependants <- Filter(function (idx) { row[idx] == 1 }, seq(1, length(row)))
        nodes_dependants[[row_idx]] <- sapply(dependants, function (idx) { 
            a <- list(list(idx=idx, traversed=FALSE))
            return(a)
        })
       
    }
    
    start_node <- 0
    cycles <- list()
    while (any(unlist(nodes_dependants) == FALSE) && start_node < length(nodes_dependants)) {
        start_node <- start_node + 1
        if (all(unlist(nodes_dependants[start_node])) == TRUE) {
            next
        }
        current_path <- list(start_node)
        for (node_idx in seq(1:length(nodes_dependants[[start_node]]))) {
            nodes_dependants[[start_node]][[node_idx]]$traversed <- TRUE
            node <- nodes_dependants[[start_node]][[node_idx]]
            
            result <- traverse_deep(node$idx, nodes_dependants, current_path, cycles)
            cycles <- result[[2]]
            nodes_dependants <- result[[1]]
        }
    }

    cycles <- Map(sort, cycles)
    cycles <- Map(as.integer, cycles)

    return(cycles[!duplicated(cycles)])
}

aggregate <- function(adjacency_matrix, vertices_to_squash, name="Agg1") {
    rows_to_squash <- adjacency_matrix[vertices_to_squash, -vertices_to_squash, drop = FALSE]
    cols_to_squash <- adjacency_matrix[-vertices_to_squash, vertices_to_squash, drop = FALSE]
    reminder_matrix <- adjacency_matrix[-vertices_to_squash, -vertices_to_squash, drop = FALSE]
    
    squashed_rows <- append(apply(rows_to_squash, 2, function(col) { min(sum(col), 1)}), 0)
    squashed_cols <- apply(cols_to_squash, 1, function(row) { min(sum(row), 1)})
    
    reminder_matrix <- cbind(reminder_matrix, squashed_cols)
    reminder_matrix <- rbind(reminder_matrix, squashed_rows)
    
    rn <- rownames(reminder_matrix)
    rn[nrow(reminder_matrix)] <- paste(rownames(adjacency_matrix)[vertices_to_squash], collapse='_')
    rownames(reminder_matrix) <- rn
    colnames(reminder_matrix) <- rn
    
    reminder_matrix
}

remove_cycles <- function(adjacency_matrix) {
    processed_matrix <- adjacency_matrix
    cycles <- find_cicles(processed_matrix)

    while (length(cycles) > 0) {
        shortest <- cycles[which.min(Map(length, cycles))][[1]]
        processed_matrix <- aggregate(processed_matrix, shortest)

        cycles <- find_cicles(processed_matrix)
    }
    processed_matrix
}

get_predecessors <- function(adjacency_matrix) {
    predecessors <- list()
    for (col_idx in seq(1, ncol(adjacency_matrix))) {
        vertex_pred <- rownames(adjacency_matrix)[adjacency_matrix[,col_idx] == 1]
        predecessors[[col_idx]] <- vertex_pred        
    }
    
    return(predecessors)
}

all_marked <- function(data_frame) {
    all(data_frame$status == 'Y' | data_frame$status == 'N')
}

remove_predecessors <- function (predecessors, to_remove) {
    if (length(predecessors)) {
        should_be_removed <- as.logical(!(predecessors %in% to_remove))

        if (any(should_be_removed)) {
            return(predecessors[should_be_removed])
        }
    }
    return(list())
}


find_kernel <- function(adjacency_matrix) {
    predecessors <- get_predecessors(adjacency_matrix)
    data_frame <- data.frame(
        vertex_name=rownames(adjacency_matrix),
        predecessors=I(predecessors),
        status='',
        stringsAsFactors = FALSE
    )

    iterations <- 0
    while(!all_marked(data_frame) && iterations <= nrow(data_frame)) {
        to_remove <- list()

        for (row_idx in seq(1, nrow(data_frame))) {
            row <- data_frame[row_idx,]
            predecessors <- row$predecessors[[1]]
            
            if (row$status != '') next
            if (length(predecessors) == 0) {
                data_frame[row_idx,]$status <- 'Y'
            }
            else {
                in_kernel <- data_frame[data_frame$status == 'Y',]$vertex_name
                if (any(in_kernel %in% predecessors)) {
                    data_frame[row_idx,]$status <- 'N'
                    to_remove <- append(to_remove, row$vertex_name)
                }
            }
        }
        for (row_idx in seq(1, nrow(data_frame))) {
            predecessors <- data_frame[row_idx,]$predecessors[[1]]
            
            data_frame[row_idx,]$predecessors[[1]] <-
                remove_predecessors(predecessors, to_remove)
        }
        iterations <- iterations + 1
        print(data_frame)
    }
    if (iterations > nrow(data_frame)) stop("Maximum number iteration exeded finding kernel")
    return(data_frame[data_frame$status == 'Y',]$vertex_name)
}