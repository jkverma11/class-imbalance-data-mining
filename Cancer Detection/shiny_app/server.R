library(shiny)
library(ggplot2)
library(ggraph)
library(igraph)
library(dplyr)

given <- readRDS("data/given.RData")
sampled <- readRDS("data/sampled.RData")
under_sampled <- readRDS("data/under_sampled.RData")
over_sampled <- readRDS("data/over_sampled.RData")
smote_sampled <- readRDS("data/smote_sampled.RData")
mdl <- readRDS("data/rf_mdl.RData")
mdl_under <- readRDS("data/rf_mdl_under.RData")
mdl_over <- readRDS("data/rf_mdl_over.RData")
mdl_both <- readRDS("data/rf_mdl_both.RData")
mdl_smote <- readRDS("data/rf_mdl_smote.RData")

tree_func <- function(final_model, tree_num)
{
	# get tree by index
	tree <- randomForest::getTree(final_model, k = tree_num, labelVar = TRUE) %>% tibble::rownames_to_column() %>% mutate(`split point` = ifelse(is.na(prediction), `split point`, NA));
	# make leaf split points to NA, so the 0s won't get plotted

	# prepare data frame for graph
	graph_frame <- data.frame(from = rep(tree$rowname, 2), to = c(tree$`left daughter`, tree$`right daughter`));

	# convert to graph and delete the last node that we don't want to plot
	graph <- graph_from_data_frame(graph_frame) %>% delete_vertices("0")

	# set node labels
	V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`));
	V(graph)$leaf_label <- as.character(tree$prediction);
	V(graph)$split <- as.character(round(tree$`split point`, digits = 2));

	# plot
	plot_tree <- ggraph(graph, 'dendrogram') + theme_bw() +
		geom_edge_link() +
		geom_node_point() +
		geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
		geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
		geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
		theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
			panel.background = element_blank(),
			plot.background = element_rect(fill = "white"),
			panel.border = element_blank(),
			axis.line = element_blank(),
			axis.text.x = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks = element_blank(),
			axis.title.x = element_blank(),
			axis.title.y = element_blank(),
			plot.title = element_text(size = 18))

	return(plot_tree)
}

shinyServer(function(input, output) {
	url_for_data <- a("Web Link to the Dataset", href = "http://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+%28diagnostic%29", target = "_blank");
	output$datasrc <- renderUI({
		tagList(url_for_data);
	})
	output$tree_plot <- renderPlot({
		choice <- as.integer(input$model_select)
		if (choice == 1) {
			suppressWarnings(tree_func(mdl$finalModel, input$tree_num))
		} else if (choice == 2) {
			suppressWarnings(tree_func(mdl_under$finalModel, input$tree_num))
		} else if (choice == 3) {
			suppressWarnings(tree_func(mdl_over$finalModel, input$tree_num))
		} else if (choice == 4) {
			suppressWarnings(tree_func(mdl_both$finalModel, input$tree_num))
		} else {
			suppressWarnings(tree_func(mdl_smote$finalModel, input$tree_num))
		}
	})
	output$roc_curves <- renderImage({
		f1 <- input$uc
		f2 <- input$oc
		f3 <- input$bc
		f4 <- input$sc
		if (f1 == TRUE && f2 == TRUE && f3 == TRUE && f4 == TRUE) {
			list(src = "www/all_plot.jpeg", width = 602, height = 325, alt = "All the Plots") #
		} else if (f1 == TRUE && f2 == TRUE && f3 == TRUE) {
			list(src = "www/under_over_both_plot.jpeg", width = 602, height = 325, alt = "Under-Over-Both") #
		} else if (f1 == TRUE && f2 == TRUE && f4 == TRUE) {
			list(src = "www/under_over_smote_plot.jpeg", width = 602, height = 325, alt = "Under-Over-SMOTE") # 
		} else if (f1 == TRUE && f3 == TRUE && f4 == TRUE) {
			list(src = "www/under_both_smote_plot.jpeg", width = 602, height = 325, alt = "Under-Both-SMOTE") #
		} else if (f2 == TRUE && f3 == TRUE && f4 == TRUE) {
			list(src = "www/over_both_smote_plot.jpeg", width = 602, height = 325, alt = "Over-Both-SMOTE") #
		} else if (f1 == TRUE && f2 == TRUE) {
			list(src = "www/over_under_plot.jpeg", width = 602, height = 325, alt = "Under-Over") #
		} else if (f1 == TRUE && f3 == TRUE) {
			list(src = "www/under_both_plot.jpeg", width = 602, height = 325, alt = "Under-Both") #
		} else if (f1 == TRUE && f4 == TRUE) {
			list(src = "www/under_smote_plot.jpeg", width = 602, height = 325, alt = "Under-SMOTE") #
		} else if (f2 == TRUE && f3 == TRUE) {
			list(src = "www/over_both_plot.jpeg", width = 602, height = 325, alt = "Over-Both") # 
		} else if (f2 == TRUE && f4 == TRUE) {
			list(src = "www/over_smote_plot.jpeg", width = 602, height = 325, alt = "Over-SMOTE") # 
		} else if (f3 == TRUE && f4 == TRUE) {
			list(src = "www/both_smote_plot.jpeg", width = 602, height = 325, alt = "Both-SMOTE") #
		} else if (f1 == TRUE) {
			list(src = "www/under_plot.jpeg", width = 602, height = 325, alt = "Under Plot") #
		} else if (f2 == TRUE) {
			list(src = "www/over_plot.jpeg", width = 602, height = 325, alt = "Over Plot") #
		} else if (f3 == TRUE) {
			list(src = "www/both_plot.jpeg", width = 602, height = 325, alt = "Both Plot") #
		} else if (f4 == TRUE) {
			list(src = "www/smote_plot.jpeg", width = 602, height = 325, alt = "SMOTE Plot") #
		} else {
			list(src = "www/base_plot.jpeg", width = 602, height = 325, alt = "Base Plot") #
		}
	}, deleteFile = FALSE)
	output$class_plots <- renderPlot({
		choice <- as.integer(input$select)
		if (choice == 1) {
			plot_fin <- barplot(table(given$Diagnosis)/nrow(given), col = c("darkblue", "red"), main = "Original Distribution showing Class Imbalance")
		} else if (choice == 2) {
			plot_fin <- barplot(table(under_sampled$Diagnosis)/nrow(under_sampled), col = c("darkblue", "red"), main = "Class Distribution of Under-Sampled")
		} else if (choice == 3) {
			plot_fin <- barplot(table(over_sampled$Diagnosis)/nrow(over_sampled), col = c("darkblue", "red"), main = "Class Distribution of Over-Sampled")
		} else if (choice == 4) {
			plot_fin <- barplot(table(sampled$Diagnosis)/nrow(sampled), col = c("darkblue", "red"), main = "Class Distribution of Modified-Sampled")
		} else {
			plot_fin <- barplot(table(smote_sampled$Diagnosis)/nrow(smote_sampled), col = c("darkblue", "red"), main = "Class Distribution after SMOTE")
		}
		axis(2, at = 0:5, labels = 0:5)
		legend("topright", legend = c("Benign", "Malignant"), fill = c("darkblue", "red"))
		plot_fin
	})
	output$data_size <- renderText({
		choice <- as.integer(input$select)
		if (choice == 1) {
			paste("Size =", nrow(given), "*", ncol(given), sep = " ")
		} else if (choice == 2) {
			paste("Size =", nrow(under_sampled), "*", ncol(under_sampled), sep = " ")
		} else if (choice == 3) {
			paste("Size =", nrow(over_sampled), "*", ncol(over_sampled), sep = " ")
		} else if (choice == 4) {
			paste("Size =", nrow(sampled), "*", ncol(sampled), sep = " ")
		} else {
			paste("Size =", nrow(smote_sampled), "*", ncol(smote_sampled), sep = " ")
		}
	})
	output$data_stats <- renderText({
		choice <- as.integer(input$select)
		if (choice == 1) {
			paste("Benign =", table(given$Diagnosis)[[1]], "Malignant =", table(given$Diagnosis)[[2]], sep = " ")
		} else if (choice == 2) {
			paste("Benign =", table(under_sampled$Diagnosis)[[1]], "Malignant =", table(under_sampled$Diagnosis)[[2]], sep = " ")
		} else if (choice == 3) {
			paste("Benign =", table(over_sampled$Diagnosis)[[1]], "Malignant =", table(over_sampled$Diagnosis)[[2]], sep = " ")
		} else if (choice == 4) {
			paste("Benign =", table(sampled$Diagnosis)[[1]], "Malignant =", table(sampled$Diagnosis)[[2]], sep = " ")
		} else {
			paste("Benign =", table(smote_sampled$Diagnosis)[[1]], "Malignant =", table(smote_sampled$Diagnosis)[[2]], sep = " ")
		}
	})
})