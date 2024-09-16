console.log("ciao");

// Trigger window resize to fit chart to the content area
function triggerUpdatePlotSize() {
  window.dispatchEvent(new Event('resize'));
}

// Create new observer when exit button triggered
$(document).on('click', '#exit-button', function() {
  Shiny.setInputValue('app-filters-exit_button_clicked', Math.random());
});

// Scroll to top of the page when filter button is clicked
$(document).on('click', '#scroll-to-filter-button', function() {
  globalThis.scrollTo({ top: 0, left: 0, behavior: "smooth" });
});

// Scroll to the bottom of the page when filter button is clicked
$(document).on('click', '#app-filters-apply_filter', function() {
  window.scrollTo({
    top: window.innerHeight,
    left: 0,
    behavior: "smooth"
  });
});

