# Define the custom CSS to apply the color palette
customCSS <- function(){
c <- paste0(
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
  font-family: 'Montserrat' !important;
}",
"/* Apply Montserrat font to the main dashboard title */
.skin-blue .main-header .logo, .skin-blue .main-header .navbar-brand {
  font-family: 'Montserrat' !important;
  font-size: 15px;
}",
"/* Apply Montserrat font to sidebar menu items */
.skin-blue .sidebar-menu li a {font-family: 'Montserrat' !important;}"
)}

