# Configure constants

config <- list(
    min_year = 3, # Initial min value on time range slider
    max_year = 16,  # Initial max value on time range slider

    # Grid layout
    layout    = c("area1 area0"),
    row_sizes = c("1fr"),
    # The following minimum widths are chose so that all tabs fit.
    col_sizes = c("minmax(200px, 0.2fr)", "minmax(800px, 0.8fr)"),
    gap_size  = "10px"
)
