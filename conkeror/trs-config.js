// Make all new buffers blank.
homepage = "about:blank";

// Load URLs from the command line in new buffers instead of new windows.
url_remoting_fn = load_url_in_new_buffer;

// Load download buffers in the background in the current window, instead of
// in new windows.
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// Session Management ---------------------------------------------------------

// Automatically save the browsing session. This is done automatically
// by just requiring the "session.js" script.
require("session.js");

// Automatically restore the last auto-saved session.
session_auto_save_auto_load = true;
