`wgetqueue.sh` — bash script for queuing downloads for `wget`

To print help use `-h` flag:

    ./wgetqueue.sh -h


TODO
----

* make use of bash builtin `getopts` instead of manual flag processing
* use `wget`'s option `--directory-prefix=PREFIX` to choose directory instead
  of `name="$DPATH/$1"`
  outline:
    
      echo "$DPATH" > "$r$SUFDIR" # inside `while (( "$#" ));
      ...
      # inside daemon function
      if -f $req$SUFDIR
      then
          dir=...
      else
          echo "ERROR"
          mv <request files> into [new] special ERRORDIR
      fi
      ...
  
      ... wget ... --directory-prefix="$dir" ...
  
