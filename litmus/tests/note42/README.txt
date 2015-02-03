Examples from
http://www.cl.cam.ac.uk/~pes20/cpp/notes42.html
By M. Batty & P. Sewell

The sixth example cannot be coded yet.

// in one compilation unit
void f(int ra, int*rb) {
  if (ra==42) 
    *rb=42;
  else    
    *rb=42;
} 

// in another compilation unit

r1 = x; // reads 42
f(r1,&r2);
y = r2;
-------------------
r3 = y; // reads 42
f(r3,&r4);
x = r4;

