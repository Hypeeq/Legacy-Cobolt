    
       identification division.
       program-id. statmeasure.

       environment division.
       input-output section.
       file-control.
           select input-file assign to dynamic-file
               organization is line sequential.

       data division.
       file section.
       fd  input-file.
       01  file-record.
           02 file-x         pic 9(6)v9(2).

       working-storage section.
       77  dynamic-file        pic x(50).
       77  sum-of-x            pic 9(10)v9(2) value zeros.
       77  sum-of-x-sqr        pic 9(14)v9(2) value zeros.
       77  sum-of-log-x        pic 9(14)v9(6) value zeros.
       77  sum-of-reciprocal   pic 9(14)v9(6) value zeros.
       77  product-of-x        pic 9(14)v9(6) value 1.
       77  n                   pic 9(4) value zeros.
       77  mean                pic 9(10)v9(2) value zeros.
       77  std-deviation       pic 9(10)v9(2) value zeros.
       77  geometric-mean      pic 9(10)v9(2) value zeros.
       77  harmonic-mean       pic 9(10)v9(2) value zeros.
       77  root-mean-square    pic 9(10)v9(2) value zeros.
       77  temp-mean           pic 9(10)v9(2) value zeros.
       77  sum-of-x-squares    pic 9(14)v9(2) value zeros.
       77  temp-in-x           pic 9(6)v9(2) value zeros.
       77  in-x                pic 9(6)v9(2) value zeros.
       77  i                   pic 9(4) value zeros.
       77  ws-end-of-file      pic x value 'n'.

       *> output formatting variables
       77  fmt-sum             pic zzzz9.99.
       77  fmt-mean            pic zzzz9.99.
       77  fmt-geometric-mean  pic zzzz9.99.
       77  fmt-harmonic-mean   pic zzzz9.99.
       77  fmt-std-deviation   pic zzzz9.99.
       77  fmt-root-mean-sq    pic zzzz9.99.

       01  x                   pic 9(6)v9(2) occurs 1000 times.

       procedure division.
       main-logic.
           display "******************************************"
           display "*        statistical measure tool        *"
           display "******************************************"
           display "enter the input file name: "
           accept dynamic-file

           open input input-file

           display "******************************************"
           display "*          mean and standard dev         *"
           display "******************************************"
           display " data values"

           move zero to sum-of-x
           move zero to sum-of-x-sqr
           move zero to sum-of-log-x
           move zero to sum-of-reciprocal
           move zero to sum-of-x-squares
           move 1 to product-of-x
           move zero to n

           perform input-loop until ws-end-of-file = 'y'

           perform compute-mean
           move mean to temp-mean
           perform compute-std-deviation
           perform compute-geometric-mean
           perform compute-harmonic-mean
           perform compute-root-mean-square

           *> format values for output
           move sum-of-x to fmt-sum
           move mean to fmt-mean
           move geometric-mean to fmt-geometric-mean
           move harmonic-mean to fmt-harmonic-mean
           move std-deviation to fmt-std-deviation
           move root-mean-square to fmt-root-mean-sq

           display "******************************************"
           display "*              results                   *"
           display "******************************************"
           display " sum          = ", fmt-sum
           display " mean         = ", fmt-mean
           display " geometric mean = ", fmt-geometric-mean
           display " harmonic mean  = ", fmt-harmonic-mean
           display " standard dev   = ", fmt-std-deviation
           display " root mean sq   = ", fmt-root-mean-sq
           display "******************************************"

           close input-file
           stop run.

       input-loop.
           read input-file
               at end 
                   move 'y' to ws-end-of-file
               not at end
                   move file-x to in-x
                   perform process-data
           end-read.

       process-data.
           compute temp-in-x = in-x
           add 1 to n
           move temp-in-x to x(n)
           add x(n) to sum-of-x
           compute sum-of-x-squares rounded = sum-of-x-squares + (x(n) ** 2)
           compute sum-of-reciprocal rounded = sum-of-reciprocal + (1 / x(n))
           compute sum-of-log-x rounded = sum-of-log-x + function log10(x(n))
           move x(n) to fmt-sum
           display fmt-sum.

       compute-mean.
           compute mean rounded = sum-of-x / n.

       compute-std-deviation.
           move zero to sum-of-x-sqr
           perform calculate-std-dev varying i from 1 by 1 until i > n
           compute std-deviation rounded = (sum-of-x-sqr / n) ** 0.5.

       calculate-std-dev.
           compute sum-of-x-sqr rounded = sum-of-x-sqr + (x(i) - temp-mean) ** 2.

       compute-geometric-mean.
           compute geometric-mean rounded = function exp10(sum-of-log-x / n).

       compute-harmonic-mean.
           compute harmonic-mean rounded = n / sum-of-reciprocal.

       compute-root-mean-square.
           compute root-mean-square rounded = (sum-of-x-squares / n) ** 0.5.
