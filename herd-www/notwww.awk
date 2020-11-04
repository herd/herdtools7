BEGIN { out=1 ; }
/\(\* END NOTWWW/   { out=1; }
{ if (out)  { print $0; } else { printf("(* %s *)\n",$0); } }
/\(\* START NOTWWW/ { out=0; }
