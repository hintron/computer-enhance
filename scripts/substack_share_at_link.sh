#!/bin/bash

# This script takes a substack post link and a min:sec timestamp and converts it
# into a link that automatically navigates to that timestamp in the video.
# This just scriptifies the "Share from" -> "Copy link" action when hovering
# over the video timeline.

# $1: The substack article URL. There should be no parameters
# $2: The hour:min:sec timestamp

# E.g.:
# $ ./scripts/substack_share_at_link.sh https://www.computerenhance.com/p/q-and-a-48-2024-03-25 4:27
# https://www.computerenhance.com/p/q-and-a-48-2024-03-25?utm_campaign=post&utm_medium=web&timestamp=267

URL=$1
# Remove any [, ] from input
HR_MIN_SEC="$(echo "$2" | tr -d [])"

# In awk, NF is a predefined variable that prints the number of fields found
# (i.e. the # of delimiters found + 1)
FIELD_COUNT=$(awk -F":" '{print NF}' <<< "${HR_MIN_SEC}")
# echo "FIELD_COUNT: $FIELD_COUNT"

HOUR=0
MIN=0
SEC=0

# See https://stackoverflow.com/questions/16679369/count-occurrences-of-a-char-in-a-string-using-bash
case "$FIELD_COUNT" in 
    1)
        # echo "Found only seconds"
        SEC=$HR_MIN_SEC
        ;;
    2)
        # echo "Found minutes:seconds"
        MIN=$(awk -F: '{ print $1 }' <<< "$HR_MIN_SEC")
        SEC=$(awk -F: '{ print $2 }' <<< "$HR_MIN_SEC")
        ;;
    3)
        # echo "Found hours:minutes:seconds"
        HOUR=$(awk -F: '{ print $1 }' <<< "$HR_MIN_SEC")
        MIN=$(awk -F: '{ print $2 }' <<< "$HR_MIN_SEC")
        SEC=$(awk -F: '{ print $3 }' <<< "$HR_MIN_SEC")
        ;;
    0)
        echo "ERROR: No hrs:min:sec parameter specified"
        exit 1
        ;;
    *)
        echo "ERROR: Unhandled case: $FIELD_COUNT"
        exit 1
        ;;
esac

# Use 10# to force base 10, so e.g. "08" doesn't fail as an invalid octal number
# See https://stackoverflow.com/questions/24777597/value-too-great-for-base-error-token-is-08
# See https://stackoverflow.com/questions/12821715/convert-string-into-integer-in-bash-script-leading-zero-number-error/12821845#12821845
TOTAL_SECONDS=$(( (10#$HOUR * 60 * 60) + (10#$MIN * 60) + 10#$SEC ))
# echo "TOTAL_SECONDS: $TOTAL_SECONDS"
echo "${URL}?utm_campaign=post&utm_medium=web&timestamp=$TOTAL_SECONDS"
