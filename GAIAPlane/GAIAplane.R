source("../Promethee/module.R")

GAIAPlain <- function(flows, criteria_weights) {
    pca <- prcomp(flows)
    
    rotation <- pca[['rotation']]

    points <- flows %*% rotation
    points <- as.data.frame(points[,1:2])

    criteria_points <- (diag(dim(flows)[2]) %*% rotation)[,1:2]
    criteria_points <- as.data.frame(criteria_points, row.names=colnames(flows))

    if (missing(criteria_weights) || len(criteria_weights) != dim(flows)[2]) {
        weights <- rep(1, dim(flows)[2])
    } else {
        weights <- criteria_weights
    }

    weights <- matrix(weights / sqrt(sum(weights)), nrow=1, ncol=dim(flows)[2])
    decision_stick <- as.matrix((weights %*% rotation)[,1:2])
    decision_stick <- as.data.frame(t(decision_stick), col.names=c("PC1", "PC2"))

    x_upper_bound <- max(points$PC1, decision_stick$PC1, criteria_points$PC1)*1.05
    y_upper_bound <- max(points$PC2, decision_stick$PC2, criteria_points$PC2)*1.05
    x_lower_bound <- min(points$PC1, decision_stick$PC1, criteria_points$PC1)*1.05
    y_lower_bound <- min(points$PC2, decision_stick$PC2, criteria_points$PC2)*1.05

    ggplot(points, aes(x=PC1, y=PC2)) + 
        geom_hline(yintercept=0, color="grey68") +
        geom_vline(xintercept=0, color="grey68") +
        geom_point(size=2, shape=15) +
        geom_text(
            data=points,
            aes(label=row.names(points), x=PC1, y=PC2),
            nudge_x = 0.05,
            nudge_y=0.05
        ) +
        geom_segment(
            data=criteria_points,
            inherit.aes=FALSE,
            aes(xend=PC1, yend=PC2),
            x=0,
            y=0,
            color="darkslategray4",
            arrow=arrow(length=unit(0.30,"cm"), type = "closed")
        ) +
        geom_text(
            data=criteria_points,
            aes(label=row.names(criteria_points), x=PC1/2, y=PC2/2),
            colour="darkslategray",
            nudge_x = 0.03,
            nudge_y=0.03
        ) +
        geom_segment(
            data=decision_stick,
            inherit.aes=FALSE,
            aes(xend=PC1, yend=PC2),
            x=0,
            y=0,
            color="chocolate1",
            arrow=arrow(length=unit(0.30,"cm"), type = "closed")
        ) +
        geom_segment(
            data=decision_stick,
            inherit.aes=FALSE,
            aes(
                x=PC1*-100,
                y=PC2*-100,
                xend=PC1*100,
                yend=PC2*100,
            ),
            color="chocolate1",
            linetype="dashed",
            arrow=arrow(length=unit(0.30,"cm"), type = "closed")
        ) +
        geom_text(
            data=decision_stick,
            label="D",
            aes(x=PC1/2, y=PC2/2),
            colour="chocolate3",
            nudge_x = 0.05,
            nudge_y=0.05
        ) +
        coord_cartesian(ylim=c(y_lower_bound, y_upper_bound), xlim=c(x_lower_bound, x_upper_bound))
}