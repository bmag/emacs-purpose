#! /usr/bin/python

import sys
import re
import os


def run():
    lines = sys.stdin.readlines()
    current_todo = ''

    todo = []
    lines = list(filter(None, [line.strip() for line in lines]))
    for line in lines:
        matches = re.search(r'^(\d+):\s*(.*)$', line)
        if not matches:
            current_file = line
            continue
        line_in_file = matches.group(1)
        description = matches.group(2)
        importance = re.search(r'\@tod(o+)', description)
        importance = len(importance.group(1))
        description = re.sub(r'.*\@todo+\s*', '', description)
        if '[[file:{file}::{line}]]'.format(file=current_file, line=line_in_file) in current_todo:
            continue
        todo.append({
            'importance': importance,
            'description': description,
            'file': current_file,
            'line': line_in_file
        })
    todo.sort(key=lambda x: x['importance'], reverse=True)
    todo_buffer = []
    for i, _todo in enumerate(todo):
        todo_item = '''{description}
[[file:{file}::{line}]]
'''.format(i=i+1, **_todo)
        todo_buffer.append(todo_item)
    print '{}{}'.format(current_todo, ''.join(todo_buffer))

run()
