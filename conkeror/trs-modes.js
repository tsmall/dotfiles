// XKCD ------------------------------------------------------------------------

// Show the XKCD title text in the page.
xkcd_add_title = true;


// Google Search ---------------------------------------------------------------

// Bind the keys 1 through 9 to Google search results.
require("google-search-results.js");
google_search_bind_number_shortcuts();

// Bind the "n" key to the next page link.
define_key(google_search_results_keymap, "n", "follow",
           $browser_object = browser_object_relationship_next);
