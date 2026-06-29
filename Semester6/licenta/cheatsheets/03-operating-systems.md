# Operating Systems (UNIX) — Cheatsheet

Exam style: **Problem 1** — trace a C program using `fork`/`exec`/`wait` (count processes, predict output). **Problem 2** — trace a Bash script (variable values, output), often involving `sed -E`/`grep -E` and `while read` loops.

---

## 1. UNIX file system structure

### On-disk layout of a (classic) file system
| Block | Contents |
|-------|----------|
| **Boot block** | bootstrap code (first block) |
| **Superblock** | metadata about the FS: size, # inodes, # free blocks, free lists |
| **Inode list** | array of inodes (one per file) |
| **Data blocks** | actual file contents + directory entries |

### Inode (index node)
Stores **everything about a file except its name**:
- file type + permissions (mode), owner UID/GID
- size, timestamps (atime/mtime/ctime)
- link count (# hard links)
- **block pointers**: direct blocks + single/double/triple **indirect** blocks (allows large files).

The file **name** lives in the **directory**, not the inode.

### Directories
A directory is a special file: a table of `(name → inode number)` entries. Always contains `.` (itself) and `..` (parent).

### File types
`-` regular · `d` directory · `l` symbolic link · `c` char device · `b` block device · `p` FIFO (named pipe) · `s` socket. (Shown as first char of `ls -l`.)

### Links
- **Hard link**: another directory entry pointing to the **same inode** (same file, share data; can't cross file systems, not for directories). Increments link count.
- **Symbolic (soft) link**: a small file containing a **path** to another file (can cross FS, can dangle).

### Permissions
9 bits: `rwx` for **owner / group / others**.
```
-rwxr-x---   owner: rwx (7)  group: r-x (5)  others: --- (0)   → 750
```
`r=4 w=2 x=1`. On a **directory**: `r`=list names, `w`=create/delete entries, `x`=enter/traverse.

### Path
Absolute path starts at root `/`; relative path starts at the current directory. Every process has a **current working directory** (`pwd`).

---

## 2. UNIX processes

A process = a running program with its own address space, PID, and parent PID (PPID). PID 1 = `init`.

### fork() — create a child process
```c
pid_t pid = fork();
```
- Creates an almost-exact **copy** of the parent (same code, copied data, open file descriptors, same current line).
- Returns **twice**:
  - in the **parent**: child's PID (`> 0`)
  - in the **child**: `0`
  - on failure: `-1` (no child created)
- After `fork`, **both** processes continue from the line *after* `fork`.

```c
if (fork() == 0) {
    // child only
} else {
    // parent only
}
```

**Counting processes:** `n` successive `fork()` calls (all reached by everyone) create `2ⁿ − 1` new processes ⇒ `2ⁿ` total.
```c
fork(); fork(); fork();   // → 2^3 = 8 total processes
```

### exec family — replace the program image
`execl`, `execlp`, `execv`, `execvp`, `execle`, … **replace** the current process's code/data with a new program. The **PID stays the same**; no new process is created.
- On **success it never returns** — the old code (everything after the exec call) is gone.
- Returns `-1` **only on failure**.
- `l` = args as a list ending in `NULL`; `v` = args as a `char* argv[]` array.
- `p` = search the program in **`PATH`**; without `p` you must give a full/relative path.

```c
execl("/bin/ls", "ls", "-l", NULL);   // absolute path
execlp("ls", "ls", "-l", NULL);       // searched in PATH
// code here runs ONLY if exec failed
```
> Exam trap: a `printf` *after* a successful `exec` never runs. Two execs in a row → only the first that succeeds executes; the second is unreachable.
> Exam trap: `execlp("a", …)` with `a` in the current dir runs only if `.` is in `PATH`; otherwise it fails.

### exit() — terminate the process
```c
exit(0);          // 0 = success, non-zero = error; returns status to parent
```
`return` from `main` is equivalent. The exit status is collected by the parent via `wait`.

### wait() — parent waits for a child to finish
```c
int status;
pid_t child = wait(&status);   // blocks until ANY child terminates; returns its PID
waitpid(pid, &status, 0);      // wait for a specific child
```
- Returns the PID of the terminated child, or `-1` if there are **no children** (then `wait` returns immediately with error).
- Prevents **zombies** (a finished child whose status hasn't been collected). A child whose parent died is reparented to `init` (**orphan**).
- `wait(NULL)` if you don't need the status.

```c
while ((pid = wait(NULL)) > 0) { /* runs once per child */ }
```

### Tracing checklist (Problem 1)
1. Track how many processes exist after each `fork`.
2. Remember both branches continue; identify which lines each runs.
3. After a successful `exec`, the rest of that process's original code is replaced — it won't run.
4. `wait` makes the parent pause until a child ends; output ordering of parent vs child is otherwise nondeterministic.
5. Count `printf`s **per process**.

### Pipe — communication between **related** processes
Anonymous, unidirectional byte stream; one end write, one end read. Created **before** fork so parent & child share it.
```c
int fd[2];
pipe(fd);            // fd[0] = read end, fd[1] = write end
fork();
// parent: close(fd[0]); write(fd[1], ...)
// child:  close(fd[1]); read(fd[0], ...)
```
Shell pipe `|` connects stdout of one command to stdin of the next: `ls | wc -l`.

### FIFO — named pipe, for **unrelated** processes
A pipe with a name in the file system; any process can open it by path.
```bash
mkfifo mypipe
echo hi > mypipe &     # writer blocks until a reader opens
cat < mypipe           # reader
```
```c
mkfifo("mypipe", 0666);
```
Reads/writes block until both ends are open. Persists in the FS until removed.

---

## 3. Shell programming (Bash)

### Script basics
```bash
#!/bin/bash          # shebang: interpreter
VAR=value            # NO spaces around =
echo "$VAR"          # use the value
```
- Variables are **strings**; reference with `$VAR` or `${VAR}`.
- Quoting: `"$x"` expands variables; `'$x'` is literal; backticks/`$(...)` run a command.

### Predefined variables
| Var | Meaning |
|-----|---------|
| `$0` | script name |
| `$1 … $9` | positional arguments (`${10}`+ need braces) |
| `$#` | number of arguments |
| `$@` | all args, as separate words ("$1" "$2" …) |
| `$*` | all args, as one word when quoted ("$1 $2 …") |
| `$?` | exit status of the last command (0 = success) |
| `$$` | PID of the current shell |
| `$!` | PID of the last background command |

`shift` drops `$1`, shifting the rest left (`shift 2` drops two). Useful to loop over args.

### Control structures
```bash
if [ cond ]; then ...; elif [ cond ]; then ...; else ...; fi

for x in a b c; do echo "$x"; done
for i in $(seq 1 5); do ...; done
for f in *.txt; do ...; done

while [ cond ]; do ...; done
while read LINE; do echo "$LINE"; done < file.txt   # read file line by line

case "$x" in
    a) ...;;
    b|c) ...;;
    *) ...;;        # default
esac
```
- `break` exits the loop; `continue` skips to the next iteration.
- `read VAR` reads one line from stdin into VAR; `read A B C` splits the line into words.

### test / [ ] operators
```bash
[ "$a" = "$b" ]      # string equal      ( != not equal, -z empty, -n non-empty )
[ "$a" -eq "$b" ]    # numeric: -eq -ne -lt -le -gt -ge
[ -f file ]          # regular file exists
[ -d dir ]           # directory exists
[ -e path ]          # exists  ( -r readable, -w writable, -x executable )
[ cond1 ] && [ cond2 ]      # AND        [ c1 ] || [ c2 ]   # OR
```

### I/O redirection
| Syntax | Effect |
|--------|--------|
| `cmd > file` | stdout → file (overwrite) |
| `cmd >> file` | stdout → file (append) |
| `cmd < file` | stdin ← file |
| `cmd 2> file` | stderr → file |
| `cmd 2>> file` | stderr → file (append) |
| `cmd > f 2>&1` | stderr → wherever stdout goes (merge) |
| `cmd1 \| cmd2` | stdout of cmd1 → stdin of cmd2 (pipe) |
| `cmd > /dev/null` | discard output ("black hole" file) |
| `cmd > /dev/null 2>&1` | discard both stdout and stderr |

File descriptors: `0` stdin, `1` stdout, `2` stderr.

### Command substitution (back-quotes)
```bash
today=`date`           # back-quotes: capture command output
today=$(date)          # preferred, nestable
count=$(ls | wc -l)
for f in `find . -name '*.c'`; do ...; done
```

### Arithmetic
```bash
x=$((3 + 4 * 2))       # arithmetic expansion → 11
x=`expr 3 + 4`         # external expr (spaces required; escape \* )
```

---

## 4. Extended regular expressions (POSIX ERE)

Used by `grep -E` and `sed -E`.

| Pattern | Matches |
|---------|---------|
| `.` | any single character |
| `*` | 0+ of the preceding |
| `+` | 1+ of the preceding |
| `?` | 0 or 1 of the preceding |
| `{n}` `{n,}` `{n,m}` | exactly n / n+ / between n and m |
| `[abc]` | one of a, b, c |
| `[^abc]` | any char **except** a, b, c |
| `[a-z]` `[0-9]` | ranges |
| `^` | start of line |
| `$` | end of line |
| `(...)` | group |
| `a\|b` | a OR b (alternation) |
| `\1`, `\2` | back-reference to captured group n (in `sed` replacement) |

> In **ERE** (`-E`), `+ ? { } ( ) |` are special **without** backslash. In basic RE (BRE) you'd write `\+ \( \)` etc.

### grep -E (search lines)
```bash
grep -E 'pattern' file
grep -E -q 'pat' file    # -q quiet: no output, sets exit status ($? = 0 if matched)
grep -i 'pat'            # case-insensitive
grep -v 'pat'            # invert: lines NOT matching
grep -c 'pat'            # count matching lines
```
Common in `if`: `if echo "$X" | grep -E -q '^[+-]?[0-9]+$'; then ...` (is X an integer?).

### sed -E (stream editor)
```bash
sed -E 's/regex/replacement/'      # substitute first match per line
sed -E 's/regex/replacement/g'     # global: every match on the line
sed -E 's/(.)/\1 /g'               # put a space after every char (back-ref \1)
sed -E 's/[^0-9+-]//g'             # delete every char that isn't a digit, + or -
sed -n '2p'                        # print only line 2 (-n suppress auto-print, p print)
sed '3d'                           # delete line 3
sed -E 's/([+-])/ \1 /g'           # surround + and - with spaces
```
Exam-allowed sed commands: **s** (substitute), **d** (delete), **y** (transliterate, like `tr`).
```bash
sed 'y/abc/xyz/'    # a→x, b→y, c→z, position by position
```

### Worked example (from exam)
`echo "abc def" | sed -E 's/(.)/\1 /g'` → `a b c   d e f ` (space after each char, including the space). Looping that over reversed words produces `cba fed`-type outputs.

---

## 5. Basic commands reference

The flags listed are the exam-relevant ones.

| Command | Purpose / key flags |
|---------|---------------------|
| `cat` | concatenate / print files |
| `chmod -R` | change permissions; `-R` recursive. `chmod 755 f`, `chmod u+x f` |
| `cp -r` | copy; `-r` recursive (directories) |
| `cut -d -f` | cut columns; `-d` delimiter, `-f` field list. `cut -d: -f1` |
| `echo` | print arguments + newline |
| `expr` | evaluate expression. `expr 3 + 4`, `expr length "$s"` |
| `file` | report a file's type by content |
| `find -name -type` | search files; `-name '*.c'`, `-type f` (file) / `d` (dir). `find . -name '*.txt'` |
| `grep -E -i -q -v` | search lines (see §4); `-c` count |
| `head -n` | first n lines (`head -n 5`) |
| `ls -l` | list; `-l` long (perms, owner, size, date) |
| `mkdir -p` | make directory; `-p` create parents, no error if exists |
| `mv` | move / rename |
| `ps -e -f` | processes; `-e` all, `-f` full format |
| `pwd` | print working directory |
| `read -p` | read a line from stdin into variable(s) |
| `rm -f -r` | remove; `-f` force (no prompt), `-r` recursive |
| `sed -E` | stream edit (commands d, s, y) — see §4 |
| `sleep` | pause N seconds (`sleep 2`) |
| `sort -n -r` | sort lines; `-n` numeric, `-r` reverse |
| `tail -n` | last n lines (`tail -n 5`) |
| `test` | evaluate condition (= `[ ]`); numeric/string/file operators |
| `true` | does nothing, exit status 0 (loop/placeholder) |
| `uniq -c` | collapse **adjacent** duplicate lines; `-c` prefix counts. Usually `sort \| uniq` |
| `wc -c -l -w` | count: `-c` bytes, `-l` lines, `-w` words |
| `who` | who is logged in |

### Common idioms
```bash
sort file | uniq -c | sort -nr        # frequency count, most common first
cat file | wc -l                       # number of lines
find . -type f | wc -l                 # count files
ls -l | grep '^d'                      # only directories
cut -d: -f1 /etc/passwd | sort         # usernames sorted
while read L; do echo "$L"; done < f   # iterate lines of f
```
