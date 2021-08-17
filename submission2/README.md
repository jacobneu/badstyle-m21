# Submission 2
## Usage
```
smlnj readwrite.sml memo.sml rev.sml
```

## Description
Introducing...`memorev`! It's memoized `rev`! Every time you call `rev` of a list, it looks up in its memo table (stored in `memo.sml`, compiled into a super-duper-efficient-optimizied exception-handling style) to see if it's solved this problem before! If it has, it can reverse your list super quick! And if not, then it'll calculate it and store the computation in `memo.sml` for future use -- and you'll never have to wait to reverse a list of that length ever again! And, since the lookup table is written to the file `memo.sml`, it persists even if you exit SMLNJ and start a new process. The future is now.

Memo is initialized with some values, just for demonstration. YMMV on longer inputs. Enter `memo.sml` at your own risk.

## Testing
Appears to work as specified on small-to-medium inputs, but gets rather slow on lists of several hundred elements (unless that particular length is already memoized). -JN
