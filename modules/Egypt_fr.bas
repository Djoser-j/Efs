' ***********************************************
'Subject: Egyptian fraction expansions
'Refs.  : Fibonacci-Sylvester, Golomb-Brown,
'         Fibonacci-Hultsch and the Eye of Horus.
'Author : Djoser.j.Spacher
'Code   : FreeBasic 1.07.1

' ***********************************************
width 64, 30

'list of practical numbers
const prn = $".\modules\practical.num"
const slx = 31             ' short list
const llx = 127            ' longer list
const tmx = 21             ' switch subsetSum method
'set to be verbose
dim shared as integer verb = 0
#define verbose(a) verb = (a) <> 0

'fractions
type frc
declare function parse (byref g as string) as integer
declare sub getfr (byref x as longint, byref y as longint)
declare sub prunit (byref g as string)

declare sub pairs ()
declare function dupf () as integer
declare function tail () as integer

   as ulongint a(3)        ' arguments
   as longint u(slx)       ' unit fractions
   as integer fl, t        ' input flag, length
   as longint cs(13)       ' checksums
   as integer tc           ' count
end type

'------------------------------------------------
#define res(a, b) a -((a)\(b))*(b)

Sub reduce (byref a as longint, byref b as longint)
dim as longint r, x = a, y = b
do
   r = res(x, y)
   x = y: y = r
loop until r = 0
x = abs(x)
if x > 1 then
   a \= x: b \= x
end if
End Sub

'**************************************
'Example: 5439/1276 -4 *10 ro 'Matryoshka
'Parsed as: fraction 5439/1276, subtract 1/4,
'times 10/10, expand ro fraction and skip comment.

Function frc.parse (byref g as string) as integer
dim as const string tok = "'0123456789/-*vrt"
dim as const integer amx = (1 shl 16) - 1
dim as integer i, j, t
dim q as ulongint
parse = 0

   tc = -1: fl = 0
   for i = 0 to 3
      a(i) = 0: next

   'parse input string
   for i = 1 to len(g)
      t = instr(tok, mid(g, i, 1))

      select case t
      case 0
         continue for
      case 1
         'comment
         if fl then exit for
         return -1

      case is < 12
         'build arguments
         j = fl and 3
         a(j) *= 10: a(j) += t - 2

      case is < 15
         'select argument
         fl shr= 2: fl shl= 2
         fl or= t - 11

      case else
         'verbose mode, ro expansion or -2/3
         fl or= 1 shl (t - 13)
      end select
   next i

   if a(1) = 0 then return 1
   if a(0) > amx or a(1) > amx then
      print "overflow"
      return -1
   end if

   verbose(fl and 4)
   fl shr= 3: fl shl= 2
   if fl and 8 then fl -= 7
   '1 for "t", 4 for "r",
   'unit a(2), multiplier a(3)

   q = a(0) \ a(1)
   print "n/d: ";
   if q then
      'proper fraction
      a(0) -= q * a(1)
      print q;" + ";
   end if
   print a(0);"/";a(1)
End Function

'input conversion
Sub frc.getfr (byref x as longint, byref y as longint)
dim as const integer amx = 1 shl 30
dim as longint u, v

  'switches 1, 4 and 8
   fl and= 13
   x = a(0): y = a(1)

   if fl and 1 then
      u = x * 3: v = y shl 1
      if u > v then
         'subtract 2/3
         x = u - v: y *= 3
         fl or= 16
      end if
   end if

   if a(2) then
      u = x * a(2)
      if u > y then
         'subtract unit a(2)
         x = u - y: y *= a(2)
         fl or= 32
      end if
   end if

   if fl and 48 then reduce(x, y)
   fl or= iif(x = 1, 2, 0)

   if a(3) then
      u = x * a(3): v = y * a(3)
      if u < amx and v < amx then
         'multiply by (m/m)
         x = u: y = v
      end if
   end if
End Sub

'print units vector
Sub frc.prunit (byref g as string)
dim i as integer

   print lcase(g); " [";
   if fl and 16 then print " 3";chr(34);
   for i = 0 to t
      print u(i);
      if i < t then print " ";
   next i
   print iif(g="F",",..."," ]");

   if fl and 16 then i += 1
   print iif(verb," "+str(i),"")
End Sub

'------------------------------------------------
'convert equal unit pairs
Sub frc.pairs ()
dim as integer i, j, sw
dim as longint d

do
   sw = -1
   for i = 0 to t - 1
      d = u(i)
      for j = i + 1 to t
         if u(j) = d then
            if d and 1 then
              'odd case
               u(i) = (d + 1) shr 1
               u(j) = d * u(i)
            else
              'even case
               u(i) = d shr 1
               u(j) = u(t): t -= 1
            end if
            sw = 0: exit for,for
         end if
      next j
   next i
loop until sw
End Sub

'filter duplicates
Function frc.dupf () as integer
dim i as integer, c as longint
dupf = 0

c = iif(fl and 16, 1, 0) ' 2/3
for i = 0 to t
   c xor= u(i): next

for i = 0 to tc
   if cs(i) = c then
      if verb then print "duplicate"
      return -1
   end if
next i
tc += 1: cs(tc) = c
End Function

'sort ascending
Sub bubble (byval lp as longint ptr, byval t as integer)
dim as integer i, j, sw

for i = 0 to t - 1
   sw = -1
   for j = 1 to t - i
      if lp[j - 1] > lp[j] then
         swap lp[j - 1], lp[j]
         sw = 0
      end if
   next j
   if sw then exit for
next i
End Sub

'checks & output
Function frc.tail () as integer
dim as longint x, y
dim lp as longint ptr
dim i as integer
tail = -1

   if fl and 32 then
      t += 1: u(t) = a(2)
   end if

   pairs
   if dupf then return 0
   lp = @u(0)
   bubble(lp, t)

   if fl and 16 then
      x = 2: y = 3
   else
      x = 0: y = 1
   end if
   'sum unit fractions
   for i = 0 to t
      x = x * u(i) + y
      y *= u(i)
      reduce(x, y)
   next i

   'valid expansion?
   if a(0) * y - x * a(1) then
      print "e []"
      return 0
   end if
End Function

'**************************************
type fstep
declare sub pinter ()
   'numerator, quotient,
   'residue (d = q*r), unit
   as longint n, q, r, u
end type

'print intermediate fraction
Sub fstep.pinter ()
dim as string st = str(n) + "/"
   if q > 1 then st += str(q)
   if q > 1 and r > 1 then st += "*"
   if r > 1 then st += str(r)
   print st;" -> 1/";str(u)
End Sub

'------------------------------------------------
'Fibonacci's ‘rule of the seventh category...
'which is of much utility’ (Liber Abaci VII,6:7),
'rediscovered by Sylvester in 1880.
'Ref. AJM, ‘On a Point in the Theory of
'Vulgar Fractions’

Sub Fibosyl (byref ep as frc)
dim as longint n, d, q, r
dim as integer i, sw
dim f as fstep

with ep
  .t = -1
   n = .a(0)
   d = .a(1)

   for i = 0 to slx
      reduce(n, d)

      q = d \ n
      r = d - q * n
     .t += 1: .u(.t) = q

      if r = 0 then
        .dupf
        '(skip sum check)
        .prunit("f")
         exit sub
      end if
     'unit fraction
     .u(.t) += 1

      if verb then
         f.n = n: f.q = d: f.r = 1
         f.u = q + 1: f.pinter
      end if

      'iterate
      n -= r: r = d
      d *= q + 1

      sw = (d \ r = q + 1)
      if not sw then
        'partial expansion w/deficiency < 1e-19
        .prunit("F")
         exit for
      end if
   next i
end with
End Sub

'**************************************
'Bézout's identity ax - by = c
Sub Bezout (byref a as longint, byref b as longint,_
 byval c as longint)
dim as longint q, r, x = a, y = b
dim as longint rs(1) = {1,0}
dim as integer t = 1

'Euclidean division & inversion
do
   q = x \ y
   r = x - q * y
   rs(t) -= q * rs(1 - t)
   t = 1 - t
   x = y: y = r
loop until r = 0

if x > 1 then
   print "impossible inverse mod Q, gcd ="; x
   b = 0: exit sub
end if

y = rs(t) * c ' (mod a)
y -= (y \ a) * a
x = (c - b * y) \ a

'adjust signs
while x < 0 or y > 0
   x += b: y -= a
wend
a = x: b = abs(y)
End Sub

'------------------------------------------------
'Golomb-Brown method
'Refs. AMM, ‘An Algebraic Algorithm for the
'Representation Problems of the Ahmes Papyrus’,
'kmath150.htm, ‘Reverse Greed for Unit Fractions’

Sub Golobro (byref ep as frc)
dim as longint n, d, q, r, x, y
dim i as integer
dim f as fstep

with ep
  .t = -1
  .getfr(n, d)

   for i = 0 to slx
      reduce(n, d)

      if n = 0 then
         if .tail then .prunit("g")
         exit sub
      end if

      r = int(sqr(d))
      'tiebreak squares
      if r * r = d then r -= 1
      'split denominator
      while res(d, r)
         r -= 1
      wend
      q = d \ r

      x = n: y = q
      Bezout(x, y, r)
     'unit fraction
     .t += 1: .u(.t) = q * x

      if verb then
         f.n = n: f.q = q: f.r = r
         f.u = q * x: f.pinter
      end if

      'iterate
      n = y: d = r * x
   next i
end with
End Sub

'**************************************
type triple
   as ulongint a(2)
end type

'primefactor
type primf
declare sub set (byval p as long, byval v as integer,_
 byval pv as long, byval s as long)

   as long p               ' prime
   as integer v            ' exponent
   as long pv              ' power
   as long s               ' divisor sum term
end type

'divisors
type div
declare sub trialdiv (byval m as long)
declare sub divisors (byval k as integer, byval d as long)
declare sub practical (byref m as longint, byval d as long)

declare function subset (byval s as long) as triple
declare sub shortsets (byref k as triple, byval s as long)
declare function singleset (byval k as integer, byval s as long) as integer
declare sub units (byref f as frc, byval k as ulongint, byval m as longint)

   as primf pf(11)         ' prime factors
   as integer tp           ' length
   as longint dv(llx)      ' divisors
   as integer td, tm       ' length, minimal segment
   as integer id(llx)      ' indices
end type

'------------------------------------------------
'select practical multipliers
Function multipr (byval n as longint, byval d as longint) as triple
dim as longint m, r, rm = 1 shl 30
dim st as string, p as triple
static as integer sw = 0

if Open(prn for input as #1) then
   if sw = 0 then
      print "warning: ";prn;" file not found"
      sw = -1
   end if
   return p
end if

do
   do
      line input #1, st
      m = vallng(st)
      if eof(1) or (m > d) then exit do,do
      r = res(m * n, d)
   loop until r < (m shl 1)

   if r < rm then
      rm = r
      'max. 3 small remainder m's
      with p
        .a(2) =.a(1)
        .a(1) =.a(0)
        .a(0) = m
      end with
   end if
loop

Close #1
return p
End Function

'------------------------------------------------
Sub primf.set (byval f as long, byval k as integer,_
 byval fk as long, byval z as long)
   p = f: v = k: pv = fk: s = z
End Sub

'store factorization_a in pf(0..tp)
Sub div.trialdiv (byval a as long)
dim as long fk, q, f = 2, df = 1
dim as integer k, fl = 0

tp = -1
if a = 0 then exit sub

do
  'trial division
   k = 0: fk = 1
   do
      q = a \ f
      if a - q * f then exit do
      a = q: fk *= f: k += 1
   loop

   if k then '               factor found
      tp += 1
      q = (f * fk - 1) \ (f - 1)
      pf(tp).set(f, k, fk, q)

   elseif f > q then '       f > sqrt(a)
      if a > 1 then
         tp += 1 '            coprime a
         pf(tp).set(a, 1, a, a + 1)
      end if

      exit do
   end if

  'sieve multiples of 2 and 3
   if fl then
      df = 6 - df: f += df
   else
      f += df: df shl= 1: fl = (f = 5)
   end if
loop
End Sub

'------------------------------------------------
'make m practical
Sub div.practical (byref m as longint, byval d as long)
dim as integer h, i, j, k = -1, sw
dim as long p, prim(8) = {3,5,7,11,13,17,19,23,29}
dim sum as longint

do
   sum = 1: sw = -1
   for i = 1 to tp
      sum *= pf(i - 1).s

      if pf(i).p > sum + 1 then
        'condition failed: make practical
         do
            k += 1: p = prim(k)

            if res(m, p) then
               h = iif(p < pf(i-1).p, i - 1, i)
               for j = tp to h step -1
                  pf(j + 1) =  pf(j): next

              'add prime to factor list
               pf(h).set(p, 1, p, p + 1)
               tp += 1: m *= p
               sw = 0: exit for
            end if
         loop
      end if
   next i
loop until sw

p = 1
'number of divisors
for i = 0 to tp
   p *= pf(i).v + 1
next i
'd divides practical_m
if p < tmx then exit sub

if sum + 1 > d then
  'exclude largest prime factor
  'for faster subset searching
   m \= pf(tp).p
   pf(tp).v -= 1
   if pf(tp).v = 0 then tp -= 1
end if
End Sub

'compute all divisors
Sub div.divisors (byval k as integer, byval dp as long)
dim as integer i, j
dim as long d

for i = k to tp
   d = pf(i).p * dp
   for j = 1 to pf(i).v
      td += 1: dv(td) = d

      divisors(i + 1, d)
      d *= pf(i).p
   next j
next i
End Sub

'------------------------------------------------
'select short subsets
Sub div.shortsets (byref ks as triple, byval sum as long)
dim as integer tl = (1 shl (tm + 1)) - 1
dim as integer i, j, jm = llx
dim as long h, k, s

'binary expansion
for k = 0 to tl
   s = sum: j = 0
   h = k: i = 0
   while h
      if h and 1 then
         s -= dv(i): j += 1
      end if
      h shr= 1: i += 1
   wend

   if (s = 0) and (j < jm) then
      jm = j
      'max. 3 divisor sets
      with ks
        .a(2) =.a(1)
        .a(1) =.a(0)
        .a(0) = k
      end with
   end if
next k
End Sub

'first subset
Function div.singleset (byval i as integer,_
 byval sum as long) as integer
dim as integer sw

if sum = 0 then return -1
if (sum < 0) or (i > tm) then return 0
sw = singleset(i + 1, sum)
if sw then
   id(i) = 0: return -1
end if
sw = singleset(i + 1, sum - dv(i))
if sw then
   id(i) = -1: return -1
end if
return 0
End Function

'subsetSum wrapper
Function div.subset (byval sum as long) as triple
dim i as integer, k as ulongint
dim ks as triple
if sum < 1 then return ks

'select minimal divisor segment
for i = 0 to td
   if dv(i) > sum then exit for
next i
tm = i - 1

'select search algorithm
if tm < tmx then
   shortsets(ks, sum)

elseif tm < 64 then
   for i = 0 to tm
      id(i) = 0: next

   singleset(0, sum)

   'store divisor set
   k = 0
   for i = 0 to tm
      k shl= 1
      if id(tm - i) then k or= 1
   next i
   ks.a(0) = k

else
   print "overflow: subset"
end if
return ks
End Function

'divisor set to unit fractions
Sub div.units (byref ep as frc,_
 byval k as ulongint, byval m as longint)
dim as integer i = 0, j = -1
dim as longint x, y
dim as long dss(slx)
if k = 0 then exit sub

while k
   if k and 1 then
      x = dv(i)
      if verb then j += 1: dss(j) = x
      y = m
      reduce(x, y)
      with ep
        'unit fraction
        .t += 1: .u(.t) = y
      end with
   end if
   k shr= 1: i += 1
wend

if verb then
   'divisor subset
   if j then print "(";
   for i = 0 to j
      print str(dss(i)); iif(i=j,""," + ");
   next i
   print iif(j,")",""); " /"; m
end if
End Sub

'------------------------------------------------
'Fibonacci's ‘other universal rule’ from the
'aforementioned chapter of the Liber Abaci
'is, according to Hultsch (1895), akin to
'a method used by the Egyptian scribes.
'Ref. ‘Die Elemente der Ägyptischen
'Theilungsrechnung’

Sub Fibohul (byref ep as frc)
dim as integer i, k
dim as longint m, n, d, q, r
dim lp as longint ptr
dim as ulongint s
dim as triple p, qs, rs
dim dl as div

ep.getfr(n, d)
p = multipr(n, d)

with dl
for k = 0 to 3
   'multiplier m
   if k < 3 then
      m = p.a(k)

   else
      'custom-built
      m = d
      if m and 1 then m shl= 1
   end if

  'factorize m
  .trialdiv(m)

   if k = 3 then
     'make practical
     .practical(m, d)
      'check identic m
      for i = 0 to 2
         if p.a(i) = m then m = 0
      next i
   end if

   if m = 0 then continue for

  .td = 0
  .dv(0) = 1
  .divisors(0, 1)
   lp = @.dv(0)
   bubble(lp,.td)

  'n * m = d * q + r
   q = m * n \ d
   r = m * n - q * d

   'divisor subsets
   qs =.subset(q)
   rs =.subset(r)

   for i = 0 to (ep.fl and 2)
      s = qs.a(i)
      if s then
         if verb then
            ? " ----------------"
            ? "q";q
         end if

         ep.t = -1
         'to units set
        .units(ep, s, m)

         s = rs.a(0)
         if s then
            if verb then ? "r";r
           .units(ep, s, m * d)
         end if

         with ep
            if .tail then
              .prunit(iif(.fl and 8,"r","a"))
            end if
         end with
      end if
   next i
next k
end with
End Sub

'**************************************
'Eye of Horus expansion:
'unit of volume hekat (ca. 4.8 l)
'denominators [2,4,...,64], with remainder
'1/64 scaled to ro units [0,1,...,5) / 320
'(a trader's practical partition).
'For "r" input, the exact ro fraction
'is also given (a learned exercise).
'Ref. AO, ‘The Wooden Tablets from Cairo’

Sub Horus (byref ep as frc)
dim as longint n, d, k, p = 1, r
dim as integer fl, sw
dim as string fr = "1/2"
dim ro as frc

   n = ep.a(0)
   d = ep.a(1)
   'power of 2 denominator:
   'expansion already found
   if (d and (d - 1)) = 0 then exit sub

   print "h [";
   do
      while n < d and p < 64
         n shl= 1: p shl= 1
      wend
      if n < d then exit do
      n -= d
      print p;" ";
   loop
   print "]";

   'ro remainder
   k = 5 * n \ d
   r = 5 * n - k * d

   sw = (r shl 1) < d
   fl = (ep.fl and 4) = 0
   if sw and fl then r = 0

   sw = iif(k = 0, 0, 1)
   sw or= iif(r = 0, 0, 2)

   if (sw < 2) or fl then
      if verb then
         reduce(r, d)
         fr = str(r)+"/"+str(d)
      end if

      select case sw
         case 0 : print
         case 1 : print " r";k
         case 2 : print " r ";fr
         case 3 : print " r";k;" ";fr
      end select

   else
      if sw = 3 then print " r";k;
      print " +"

      reduce(r, d)
      ro.a(0) = r
      ro.a(1) = d
      ro.fl = ep.fl or 8
      Fibohul(ro)
   end if
End Sub

'main
'**************************************
dim as double tim = timer
dim ep as frc, q as string
dim t as integer

do
   input "a / b "; q
   t = ep.parse(q)

   if t = 1 then exit do
   if t then continue do

   if verb then ? " ----------------"
   Fibosyl(ep)

   if verb then ? " ----------------"
   Golobro(ep)

   Fibohul(ep)

   if verb then ? " ----------------"
   Horus(ep)

   print
loop

print "timer:"; csng(timer - tim); "s"
system
