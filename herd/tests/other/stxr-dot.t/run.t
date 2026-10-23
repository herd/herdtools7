Check STXR relations in dot output.

  $ stxr_relations () {
  >   herd7 -set-libdir ../libdir "$@" LxSx.litmus \
  >     -show all -showevents all -showinitwrites false -o - |
  >   awk '
  >     function kind(label) {
  >       if (label !~ /STXR/) return ""
  >       if (label ~ /R\[tag\(x\)\]/) return "stxr-tag-read"
  >       if (label ~ /R\[PTE\(x\)\]/) return "stxr-pte-read"
  >       if (label ~ /R0:X0q=x \(addr\)/) return "stxr-address-read"
  >       if (label ~ /R0:Resq=/) return "stxr-reservation-read"
  >       if (label ~ /R0:X2q=0 \(data\)/) return "stxr-data-read"
  >       if (label ~ /STXR/ && label ~ /Branching\(pred\)\(valid:/) return "stxr-translation-branch"
  >       if (label ~ /STXR/ && label ~ /Branching\(pred\)\(color\)/) return "stxr-color-branch"
  >       if (label ~ /STXR/ && label ~ /Branching\(pred\)\\l/) return "stxr-branch"
  >       if (label ~ /STXR/ && label ~ /Empty/) return "stxr-empty"
  >       if (label ~ /STXR/ && label ~ /W\[(PA\()?x/) return "stxr-write"
  >       if (label ~ /STXR/ && label ~ /W0:Resq=0/) return "stxr-clear-reservation"
  >       if (label ~ /STXR/ && label ~ /W0:X3q=/) return "stxr-result"
  >       return ""
  >     }
  >     /^eiid[0-9]+ \[label=/ {
  >       id = $1
  >       label = $0
  >       sub(/^eiid[0-9]+ \[label="/, "", label)
  >       node[id] = kind(label)
  >     }
  >     /^eiid[0-9]+ -> eiid[0-9]+ \[label=".*"/ {
  >       src = $1
  >       dst = $3
  >       rel = $0
  >       sub(/^.*label="/, "", rel)
  >       sub(/".*$/, "", rel)
  >       if (node[src] != "" && node[dst] != "")
  >         print node[src] " -" rel "-> " node[dst]
  >     }
  >   ' | sort -u
  > }

No variant.

  $ stxr_relations
  stxr-address-read -iico_data-> stxr-branch
  stxr-address-read -iico_data-> stxr-write
  stxr-branch -iico_ctrl-> stxr-clear-reservation
  stxr-branch -iico_ctrl-> stxr-empty
  stxr-branch -iico_ctrl-> stxr-result
  stxr-branch -iico_ctrl-> stxr-write
  stxr-data-read -iico_data-> stxr-write
  stxr-reservation-read -iico_data-> stxr-branch

MTE, synchronous by default.

  $ stxr_relations -variant=mte,sync
  stxr-address-read -iico_data-> stxr-branch
  stxr-address-read -iico_data-> stxr-color-branch
  stxr-address-read -iico_data-> stxr-tag-read
  stxr-address-read -iico_data-> stxr-write
  stxr-branch -iico_ctrl-> stxr-clear-reservation
  stxr-branch -iico_ctrl-> stxr-empty
  stxr-branch -iico_ctrl-> stxr-result
  stxr-branch -iico_ctrl-> stxr-write
  stxr-color-branch -iico_ctrl-> stxr-branch
  stxr-color-branch -iico_ctrl-> stxr-empty
  stxr-color-branch -iico_ctrl-> stxr-write
  stxr-data-read -iico_data-> stxr-write
  stxr-reservation-read -iico_data-> stxr-branch
  stxr-tag-read -iico_data-> stxr-color-branch

MTE, asynchronous.

  $ stxr_relations -variant=mte,async
  stxr-address-read -iico_data-> stxr-branch
  stxr-address-read -iico_data-> stxr-color-branch
  stxr-address-read -iico_data-> stxr-tag-read
  stxr-address-read -iico_data-> stxr-write
  stxr-branch -iico_ctrl-> stxr-clear-reservation
  stxr-branch -iico_ctrl-> stxr-empty
  stxr-branch -iico_ctrl-> stxr-result
  stxr-branch -iico_ctrl-> stxr-write
  stxr-color-branch -iico_ctrl-> stxr-empty
  stxr-data-read -iico_data-> stxr-write
  stxr-reservation-read -iico_data-> stxr-branch
  stxr-tag-read -iico_data-> stxr-color-branch

VMSA.

  $ stxr_relations -variant=vmsa
  stxr-address-read -iico_data-> stxr-pte-read
  stxr-branch -iico_ctrl-> stxr-clear-reservation
  stxr-branch -iico_ctrl-> stxr-empty
  stxr-branch -iico_ctrl-> stxr-result
  stxr-branch -iico_ctrl-> stxr-write
  stxr-data-read -iico_data-> stxr-write
  stxr-pte-read -iico_data-> stxr-branch
  stxr-pte-read -iico_data-> stxr-translation-branch
  stxr-pte-read -iico_data-> stxr-write
  stxr-reservation-read -iico_data-> stxr-branch
  stxr-translation-branch -iico_ctrl-> stxr-branch
  stxr-translation-branch -iico_ctrl-> stxr-empty
  stxr-translation-branch -iico_ctrl-> stxr-write
