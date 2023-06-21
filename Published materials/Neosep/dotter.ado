*! version 0.2   19jun2006
*! version 0.3   13nov2006:   enable -dotter reset-
* now copes with being called more than once per loop
prog def dotter
version 8
args i imax
if "`i'"=="reset" {
   global DOTTER
   exit
}
local header 0%.........20%.........40%.........60%.........80%.........100%
if `i'==1 & "$DOTTER"!="0" {
    di as text "`header'"
    global DOTTER 0
}
cap confirm integer number $DOTTER
if _rc==0 {
   local newdots = int((`i'/`imax') * length("`header'")) - $DOTTER
   if `newdots'>0 {
       di as result _dup(`newdots') "." _c
       global DOTTER=$DOTTER+`newdots'
   }
   if `i'>=`imax' {
       di
       global DOTTER
   }
}
end
