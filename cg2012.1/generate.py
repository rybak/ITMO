import os

tasks = []

for directory, subdirs, files in os.walk('.'):
  if directory.endswith('statement'):
    tasks.append(directory.split('/')[1])

print tasks
