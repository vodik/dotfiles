" Function to strip trailing spaces from a file
function spaces#StripTrailing()
  normal mZ
  %s/\s\+$//e
  normal `Z
endfunc
