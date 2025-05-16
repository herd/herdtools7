BEGIN {
    out=1
}
/^L.a/ { next ; }
/^ARM DDI/ {  next; next;}
// { out=0; N=0 }
{ if (!out && N < 4) { N++; } else { out=1; print $0; } }
