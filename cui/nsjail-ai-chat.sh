#!/bin/bash
dir="$(dirname $0)"
nsjail \
  --mode=o \
  --chroot=. \
  --disable_clone_newnet \
  --no_pivotroot \
  --disable_proc \
  --user 65534 \
  --group 65534 \
  --time_limit 600 \
  -- /bin/python3 $dir/ai-chat.py \
  --model gemini-1.5-flash-latest \
  --api-key AIzaSyBIjhW6tjhrY8fIS18shxDdVQpkCenNtf8

#  -- /bin/ls /home
