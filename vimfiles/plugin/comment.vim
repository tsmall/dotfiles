function! Comment()
   :s/\([A-Za-z0-9\/{}]\)/\/\/ \1/
endfunction

function! Uncomment()
   :s/\/\/ //
endfunction
