# Define the custom CSS to apply the color palette
customCSS <- function(){
c <- paste0(
"@import url('https://fonts.googleapis.com/css?family=Noto+Sans&display=swap');",
" /* Remove borders from selectInput and actionButton */
.selectize-control, .btn, .box {
border: none !important; /* Removes border */
box-shadow: none !important; /* Removes shadow if any */
}
",
"/* Remove borders from plotly graphs */
.plot-container .plotly {
border: none !important;
}",
"/* Custom CSS to set the background color to white */
    .content-wrapper, .main-footer {
      background-color: #FFFFFF !important;
    }
  ",
"/* Header background color */
.skin-blue .main-header .navbar {
  background-color: rgb(42, 114, 165);
}",
"/* Header background color */
.skin-blue .main-header .navbar {
  background-color: rgb(42, 114, 165);
}",
"
/* Sidebar background color */
.skin-blue .main-sidebar {
  background-color: rgb(28, 94, 134);
}",
"/* Box header background color */
.box.box-solid>.box-header {
  background-color: rgb(49, 159, 190);
}",
"/* Accent color for box backgrounds or elements */
.accent-bg {
  background-color: rgb(255, 191, 23);
}",
"/* Apply Montserrat font to selectize inputs, dropdowns, labels, and action buttons */
.selectize-input, .selectize-dropdown, .shiny-input-container label, .btn {
  font-family: 'Noto Sans' !important;
}",
"/* Apply Montserrat font to the main dashboard title */
.skin-blue .main-header .logo, .skin-blue .main-header .navbar-brand {
  font-family: 'Noto Sans' !important;
  font-size: 13px;
}",
"/* Apply Montserrat font to sidebar menu items */
.skin-blue .sidebar-menu li a {font-family: 'Noto Sans' !important;}"
)}

