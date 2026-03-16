# Skip ARM manual header blocks starting with "ARM DDI" and
# ending with the line that starts "B2.3".
BEGIN {
    skip=0
}
/^ARM DDI/ { skip=1; next }
skip && /^B2\.3/ { skip=0; next }
!skip
