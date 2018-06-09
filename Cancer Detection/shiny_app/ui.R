library(shiny)
shinyUI(navbarPage(
	"Mini Project Sixth Semester",
	tabPanel("Class Distribution",
		sidebarPanel(
			selectInput("select", label = h3("Data (before/after) modifications"), choices = list("Original" = 1, "Under Sampled" = 2, "Over Sampled" = 3, "Over-Under Sampled" = 4, "SMOTE" = 5), selected = 1),
			p("Information on Class Distribution:"), br(),
			textOutput("data_size"), br(),
			textOutput("data_stats"), hr()
		),
		mainPanel(
			plotOutput("class_plots")
		)
	),
	tabPanel("ROC Curves",
		sidebarPanel(
			p("Original (black) :: AUC = 0.9110206"),
			checkboxInput("uc", "Under (green) :: AUC = 0.9348301", value = FALSE),
			checkboxInput("oc", "Over (red) :: AUC = 0.9394109", value = FALSE),
			checkboxInput("bc", "Both (cyan) :: AUC = 0.9648983", value = TRUE),
			checkboxInput("sc", "SMOTE (dark blue) :: AUC = 0.9507031", value = FALSE),
			hr(),
			p(strong("true positives (TP): "), "These are cases in which we predicted yes (they have the disease), and they do have the disease.", br(),
				strong("true negatives (TN): "), "We predicted no, and they don\'t have the disease.", br(),
				strong("false positives (FP): "), "We predicted yes, but they don\'t actually have the disease. (Also known as a \"Type I error.\")", br(),
				strong("false negatives (FN): "), "We predicted no, but they actually do have the disease. (Also known as a \"Type II error.\")"),
			hr()
		),
		mainPanel(
			imageOutput("roc_curves"), hr(),
			p(strong("True Positive Rate: "), "TP/(FN + TP)", br(), "also known as \"Sensitivity\" or \"Recall\"", br(), br(),
				strong("False Positive Rate: "), "FP/(TN + FP)", br(), br(),
				strong("Specificity: "), "TN/TN + FP = 1 - False_positive_rate"
			)
		)
	),
	tabPanel("Decision Trees",
		sidebarPanel(fluidRow = TRUE,
			selectInput("model_select", label = h3("Random Forest Models"), choices = list("Original" = 1, "Under Sampled" = 2, "Over Sampled" = 3, "Over-Under Sampled" = 4, "SMOTE" = 5), selected = 1),
			sliderInput("tree_num", "Input the index for tree from random_forest", min = 1, max = 500, step = 1, value = 45, animate = TRUE)
		),
		mainPanel(
			h4("Plot of the tree"),
			plotOutput("tree_plot")
		)
	),
	tabPanel("Information on Dataset",
		mainPanel(fluidRow = TRUE,
			p("This breast cancer databases was obtained from the ", strong("University of Wisconsin Hospital and Clinics,"), "Wisconsin from Dr. William H. Wolberg.", br(),
				"Features are computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. They describe characteristics of the cell nuclei present in the image.", uiOutput("datasrc"), hr(),
				strong("Attribute Information:"), br(), br(),
				"1) ID number", br(),
				"2) Diagnosis (M = malignant, B = benign)", br(),
				"3-32 (variations from mean, standard-error, worst)", br(), 
				"Ten real-valued features are computed for each cell nucleus:", br(),
				"a) radius (mean of distances from center to points on the perimeter)", br(),
				"b) texture (standard deviation of gray-scale values)", br(),
				"c) perimeter", br(),
				"d) area", br(),
				"e) smoothness (local variation in radius lengths)", br(),
				"f) compactness (perimeter^2 / area - 1.0)", br(),
				"g) concavity (severity of concave portions of the contour)", br(),
				"h) concave points (number of concave portions of the contour)", br(),
				"i) symmetry", br(),
				"j) fractal dimension (\"coastline approximation\" - 1)", br(), br(),
				"The mean, standard error and \"worst\" or largest (mean of the three largest values) of these features were computed for each image, resulting in 30 features.", br(),
				"For instance, field 3 is Mean Radius, field 13 is Radius SE, field 23 is Worst Radius.", br(),
				"All feature values are recoded with four significant digits."
			)
		)
	),
	tabPanel("Credits",
		mainPanel(fluidRow = TRUE,
			h3("Students involved"), hr(),
			p(h4("Chinmoy Das"), strong("IIT2015028")), br(),
			p(h4("Boris Hembrom"), strong("IIT2015058")), br(),
			p(h4("Jitendra Kumar"), strong("IIT2015078")), br(),
			p(h4("Vinay Pandey"), strong("IIT2015095")),
			hr()
		)
	)
))