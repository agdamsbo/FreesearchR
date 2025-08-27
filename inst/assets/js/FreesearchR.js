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

  // Main approach: Handle clicks on nav elements
  $(document).on('click', '.navbar-nav .nav-link, .dropdown-item', function(event) {
    var $target = $(event.currentTarget);

    // Don't collapse if this is a dropdown toggle
    if ($target.hasClass('dropdown-toggle')) {
      return;
    }

    // Don't collapse if this is inside a dropdown and the dropdown should stay open
    if ($target.hasClass('nav-link') && $target.closest('.dropdown').length &&
        !$target.attr('data-bs-toggle')) {
      return;
    }

    // Collapse the navbar after a short delay
    setTimeout(collapseNavbar, 10);
  });

  // Handle tab toggles specifically
  $(document).on('click', '.nav-link[data-bs-toggle="tab"]', function() {
    if (!$(this).hasClass('dropdown-toggle')) {
      setTimeout(collapseNavbar, 10);
    }
  });

  // Optional: Handle clicks outside the navbar to close it
  $(document).on('click', function(event) {
    var navbar = $('.navbar-collapse');

    // Check if click is outside navbar and navbar is open
    if (navbar.hasClass('show') &&
        !$(event.target).closest('.navbar').length) {
      collapseNavbar();
    }
  });
});
