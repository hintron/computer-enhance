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

# Explanation of how I use this script:
# I have a Google Doc that lists all the Q+A questions from the course, and each
# question has a [HH:MM:SS] timestamp. I want to convert each of those text
# timestamps into a link, so that it's immediatly accessible from the doc.

# So, the workflow is that I have a VSCode terminal open in one window, and the
# Google Doc open in another window. I use the VSCode terminal instead of
# Windows Terminal because it copy and pastes easier. I copy the link to the
# Q+A article and paste it into the terminal as the first parameter to this
# script. Then, I click to the beginning or end of the [HH:MM:SS] timestamp in
# the Google Doc and use shift+ctrl+left/right arrow to select the timestamp.
# I do ctrl+c to copy, then ctrl+k to create a link pop-up. I use my mouse to
# focus on the VSCode terminal. (I had to disable Window's 'helpful' clipboard
# popup, since it added an extra click to this process).
# Then, I ctrl+v paste into the terminal and hit enter.
# I double-click the output URL, do ctrl+shift+c to copy, use alt+tab to shift
# focus to the link creation popup in Google docs, and then
# do ctrl+shift+l/r-arrow to select the timestamp text. Finally, I do ctrl+v to
# paste the URL into the link. To quickly get to the next bullet point, I do
# ctrl+down. Whew!

# For a Q+A of 5 questions (5 links), it took me 58 seconds with this method, or
# about 11.6 seconds per link. That is much faster than it took to manually get
# the share at link from the substack video player!

# Starting at Q+A 23
# 23, 26: 3:56 (236) / 19 q's = 12.42 sec/q

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

