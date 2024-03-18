# Define the custom CSS to apply the color palette
customCSS <- function(){
c <- paste0(
#first import the font, need to do this first
"@import url('https://fonts.googleapis.com/css?family=Noto+Sans&display=swap');",

#colors, borders:
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
"/* Custom border for specific sections */
.my-custom-border {
  border: 1px solid #ddd !important;
  padding: 20px;
  border-radius: 5px;
}",
"/* Custom styling for the fast facts box */
.fast-facts-box {
  border: 4px solid var(--yellow-a); /* Thick yellow border */
  background-color: #FFFFFF; /* White background */
  padding: 15px; /* Padding inside the box */
  border-radius: 10px; /* Rounded corners */
}",
"/* Custom styling for the Fast Facts title */
.fast-facts-title {
  color: var(--yellow-a); /* Yellow text color */
  font-size: 24px; /* Larger font size */
  font-weight: bold; /* Bold font */
}",
".custom-yellow-bg {
  background-color: var(--yellow-a) !important;
}",
"#totalVerifiedBox .small-box {
  background-color: var(--yellow-a) !important;
}",
"#maleVerifiedBox .small-box {
  background-color: var(--yellow-a) !important;
}",
"#femaleVerifiedBox .small-box {
  background-color: var(--yellow-a) !important;
}",
"#commonIncomeBox .small-box {
  background-color: var(--yellow-a) !important;
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
.accent-bg .small-box.bg-yellow {
  background-color: rgb(255, 191, 23);
}",

##FONTS##
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
.skin-blue .sidebar-menu li a {font-family: 'Noto Sans' !important;}",

#spacing
"/* Increase bottom margin for main title */
h3 {
    margin-bottom: 30px; /* Adjust this value as needed */
}",
"/* Add more space around plot containers */
.plot-container, .pie-plot-container {
    margin-bottom: 40px; /* Space between plots vertically */
    padding: 10px; /* Optional: Adds space inside the container, around the plot */
}",
##COLORS##
":root {
  --blue-a: #2973A5;
  --blue-b: #648EB4;
  --blue-c: #8BAAC7;
  --blue-d: #B1C7D9;
  --blue-e: #D8E3EC;
  
  --darkblue-a: #164C71;
  --darkblue-b: #51708A;
  --darkblue-c: #7C94A8;
  --darkblue-d: #A8B7C5;
  --darkblue-e: #D3DBE2;

  --yellow-a: #FDBE19;
  --yellow-b: #F6CC6C;
  --yellow-c: #F8D991;
  --yellow-d: #FBE5B5;
  --yellow-e: #FDF2DA;

  --skyblue-a: #309EBD;
  --skyblue-b: #74B0C7;
  --skyblue-c: #97C4D5;
  --skyblue-d: #B9D7E3;
  --skyblue-e: #DDECF1;

  --turq-a: #3C989E;
  --turq-b: #77ACB0;
  --turq-c: #99C0C4;
  --turq-d: #BBD5D7;
  --turq-e: #DDEAEC;

  --grey-a: #565C65;
  --grey-b: #797D83;
  --grey-c: #9A9DA3;
  --grey-d: #BBBEC1;
  --grey-e: #DDDEE0;

  --brown-a: #CC7D15;
  --brown-b: #CD995B;
  --brown-c: #DAB384;
  --brown-d: #E7CCAD;
  --brown-e: #F3E6D6;
}")}

