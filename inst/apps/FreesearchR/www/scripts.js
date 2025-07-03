// Automatically close drop-downs on navigation
// Thanks to claude.ai
$(document).on('shown.bs.tab', '#main_panel', function(e) {
  // Close dropdown in this specific navset only
  $('#main_panel .dropdown-menu').removeClass('show');
  $('#main_panel .dropdown-toggle').removeClass('show').attr('aria-expanded', 'false');
});

$(document).on('shiny:sessioninitialized', function() {
// Function to collapse navbar on mobile
function collapseNavbar() {
  var navbar = $('.navbar-collapse');
  if (navbar.hasClass('show')) {
    navbar.removeClass('show');
    $('.navbar-toggler').addClass('collapsed');
    $('.navbar-toggler').attr('aria-expanded', 'false');
  }
  }

  // Add click event to navigation tabs
  $(document).on('click', '.nav-link[data-bs-toggle=\"tab\"]', function() {
    setTimeout(collapseNavbar, 10);
  });

  // Also handle direct clicks on nav items
  $(document).on('click', '.navbar-nav .nav-link', function() {
    setTimeout(collapseNavbar, 10);
  });
});
