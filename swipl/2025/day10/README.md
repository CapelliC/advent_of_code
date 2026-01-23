# This day part 2 turned out as interesting.

## searching...

Since part 1 was rather easily solved manually crafting a breadth first search,
I expected the same strategy to work for part 2.
Actually, it worked well for the small (i.e. example) data set.
But my naive search couldn't solve in reasonable time even the first equation
of the input data set. You can see the attempt in `day10_saved_p2.pl`.

## first small optimization

A first small optimization was to replace the `memberchk/2` lookup
of already seen patters with a better data structure, `library(nb_set)`.
You can see the attempt in say10_failed_with_nbset.pl.

## change strategy

Apparently, a radical change in soving strategy was required.
I encoded a compact solution with the help of `library(clpfd)`.
Again, my naive model turned out to be faster than the breadth first search,
but too slow even to solve the *second* equation.
The code is saved in `day10_fd_too_slow.pl`.

## change again

I first thought to switch to `library(clpBNR)`, but then recalled that
Markus Triska long ago contributed `library(simplex)`, that I never tried before.
Indeed, now the problem get quickly solved.

Overall, I think this day is very instructive. At least, it has been for me.
