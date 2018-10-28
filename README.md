# rerec

One day, this will be a terminal-based tool for recording sentences one fragment at a time. Its main feature will be to use binary search to let the user find just the right amount of silence between fragments in a minimal number of keystrokes.

But so far, all we have is:

## interplay

Best understood by example. This:

    $ interplay part1.wav 1 part2.mp3 -0.5 part3.sox

will play `part1.wav`, pause for 1 second, then play `part2.mp3`, but half a second before it ends, it will start to play `part3.sox`.

Requires [`sox`](https://github.com/chirlu/sox) to be installed and on the PATH.
