#' Create a Shiny Box with a Fixed Aspect Ratio
#'
#' This function creates a `shinydashboard::box()` with a specified aspect ratio. 
#' You can choose from common aspect ratios such as "4x3", "16x9", "1x1", and "3x4".
#'
#' @param title Character. The title of the box.
#' @param content Any UI content (e.g., `plotlyOutput`, `textOutput`) to be placed inside the box.
#' @param aspect_ratio Character. The aspect ratio of the box. Choose from "4x3", "16x9", "1x1", or "3x4".
#' @param width Numeric. The width of the box in terms of grid columns (default is 12, full width).
#' @param status Character. The status of the box (e.g., "primary", "success", "info"). Used to style the box header.
#' @param solidHeader Logical. Whether the header should have a solid background color (default is TRUE).
#'
#' @return A Shiny box with a fixed aspect ratio.
#' @examples
#' aspect_box(
#'   title = "4x3 Aspect Ratio Box",
#'   content = plotlyOutput("plot1"),
#'   aspect_ratio = "4x3"
#' )
#'
#' @importFrom shinydashboard box
#' @export
aspect_box <- function(title, content, aspect_ratio = "16x9", status = "primary", solidHeader = TRUE, max_width = 900, min_width = 400) {
  
  # Check for valid aspect_ratio
  valid_ratios <- c("4x3", "16x9", "1x1", "3x4")
  if (!aspect_ratio %in% valid_ratios) {
    stop("Invalid aspect ratio. Choose from '4x3', '16x9', '1x1', or '3x4'.")
  }
  
  # Create dynamic CSS for max-width and min-width, with !important to enforce them
  custom_style <- paste0(
    "max-width:", max_width, "px !important; ",
    "min-width:", min_width, "px !important; ",
    "width: 100% !important; margin: 0 auto !important;"
  )
  
  # Create CSS class for box with aspect ratio
  css_class <- paste0("box-", aspect_ratio)
  
  # Wrap the box in a custom container that controls the max-width and min-width
  tags$div(style = custom_style,  # Apply max-width and min-width to the outer container
           box(
             title = title, status = status, solidHeader = solidHeader, width = NULL,
             div(class = css_class, 
                 div(class = "box-content", content)
             )
           )
  )
}
