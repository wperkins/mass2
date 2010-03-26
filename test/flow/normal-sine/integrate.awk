BEGIN {
  q = 0.0;
  a = 0.0;
  xlast = 0.0;
  dlast = 0.0;
  ulast = 0.0;
  first = 1;
}

NF == 8 {
  x = $3;
  d = $4;
  u = $8;
  if (!first) {
    ai = (x - xlast)*0.5*(d+dlast);
    qi = ai*0.5*(u+ulast);
    a += ai;
    q += qi;
  }
  first = 0;
  xlast = x;
  dlast = d;
  ulast = u;
}

END {
  print a, q, q/a;
}
  
