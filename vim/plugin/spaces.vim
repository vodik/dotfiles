" Function to strip trailing spaces from a file
function spaces#StripTrailing()
  if ! &bin
    normal mZ
    %s/\s\+$//e
    normal `Z
  endif
endfunc
