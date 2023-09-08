# HPC_project

The code requires two input files, the first one with information on initial modules, containing 3 collumns separated by spaces as such:
module name    x position     y position

The second input file contains the tracks, the first line in the file contains the total number of tracks. Then there are 6 collumns separated by spaces as such:
track's module name      track's number in module      charge      gradient     y-intercept     transverse momentum

The user can introduce shifts in the position of the modules (translations in x, y) and can set the maximum accepted distance betweeen pair origin and the central collision point.

3 output files are produced,
final_modules.txt contains the updated modules coordinates after shifts were applied,
global_tracks.txt contains tracks information with the y-intercepts in global coordinates, 
pairs.txt contains the global track names of the tracks in a pair and the invariant mass of the pair.
