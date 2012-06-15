import os

tasks = []

for directory, subdirs, files in os.walk('.'):
  if directory.endswith('statement'):
    candidates = filter(lambda name: name.endswith('tex') and not name.startswith('statement'), files)
    if len(candidates) == 1:
      tasks.append((directory.split('/')[1], candidates[0]))

tasks.sort()

f = open('tasks.tex', 'w')
f.writelines(map(lambda (dirname, filename): '\\input %s/statement/%s\n' % (dirname, filename), tasks))
f.close()
