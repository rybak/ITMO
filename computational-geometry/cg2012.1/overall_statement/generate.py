import os
import shutil

tasks = []

for directory, subdirs, files in os.walk('..'):
  if directory.endswith('statement'):
    candidates = filter(lambda name: name.endswith('tex') and not name.startswith('statement'), files)
    if len(candidates) == 1:
      tasks.append((directory, candidates[0]))
      for fn in filter(lambda x: x != 'statement.tex', files):
        filename = directory + '/' + fn
        shutil.copyfile(filename, '.' + '/' + fn)

tasks.sort()

f = open('tasks.tex', 'w')
f.writelines(map(lambda t: '\\input %s\n' % t[1], tasks))
f.close()
