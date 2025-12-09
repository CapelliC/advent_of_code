sizes([2],"162,817,812"-0,"425,690,689"-0)
sizes([3],"162,817,812"-1,"431,825,988"-0)
sizes([3,2],"906,360,560"-0,"805,96,715"-0)
sizes([3,2],"431,825,988"-1,"425,690,689"-1)
sizes([3,2,2],"862,61,35"-0,"984,92,344"-0)
sizes([3,2,2,2],"52,470,668"-0,"117,168,530"-0)
sizes([3,2,2,2,2],"819,987,18"-0,"941,993,340"-0)
sizes([3,3,2,2,2],"906,360,560"-2,"739,650,466"-0)
sizes([4,3,2,2,2],"346,949,466"-0,"425,690,689"-1)
what_about("906,360,560"-2,"984,92,344"-3)
sizes([4,4,3,2,2],"906,360,560"-2,"984,92,344"-3)

https://www.reddit.com/r/adventofcode/comments/1phnc54/2025day_8_part_1_en_example_problem/

Hey. I think I'm not spoiling anything by publishing the result for the example problem here that I took from another user. I appear to be too stupid to come to the same solution. So apparently the folowing nodes are connected:

[[0, 19, 7, 14, 3], [2, 13, 18, 8], [9, 12], [11, 16], ...] This then yields the desired output.

I agree with the first 5 nodes. However, I just can't wrap my head around how the second block of 4 nodes came to be. Because node 18, is actually closest to node 17, thus it has no right to be in the same list as 2, 13 and 8. OR I messed up big time calculating the euclidean distances.

Did I miss anything in the puzzle description maybe? What am I missing? My solution yields
[[0, 19, 7, 14, 3], [2, 13, 8], [17, 18], ...]

Any pointer is appreciated.
