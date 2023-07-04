program tabnumlab
version 9
syntax varlist(min=1 max=2) [if] [in] ///
[fweight aweight iweight] [, KEEPlabels *]
tokenize "`varlist'"
args row col
local rlbl : value label `row'
if "`rlbl'" != "" {
quietly numlabel `rlbl', add
}
if "`col'" != "" {
local clbl : value label `col'
if "`clbl'" != "" {
quietly numlabel `clbl', add
}
}
tab `row' `col' `if' `in' [`weight' `exp'] , `options'
if "`keeplabels'" == "" {
capture numlabel `rlbl', remove
capture numlabel `clbl', remove
}
end
