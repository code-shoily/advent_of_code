#!/usr/bin/env python3
import re
from pathlib import Path

FILES = list(Path("lib").glob("**/day_*.ex"))

MAPPING = {
    'path-finding': 'pathfinding',
    'shortest-path': 'pathfinding',
    'performance': 'slow',
    'concurrent': 'parallel',
    'concurrency': 'parallel',
    'combination': 'combinatorics',
    'permutation': 'combinatorics',
    'power-set': 'combinatorics',
    'grid2d': 'grid',
    'infinite-grid': 'grid',
    'graph-route': 'graph',
    'dag': 'graph',
    'recursive': 'recursion',
    'algebra': 'math',
    'arithmetic': 'math',
    'modular-arithmetic': 'math',
    'number-theory': 'math',
    'lcm': 'math',
    'chinese-remainder': 'math',
    'list': 'sequence',
    'ordered-list': 'sequence',
    'circular-list': 'sequence',
    'circular-buffer': 'sequence',
    'circular-linked-list': 'sequence',
    'linked-list': 'sequence',
    'cellular-automata': 'simulation',
    'parse-heavy': 'parsing',
    'manual-parse': 'parsing',
    'parser': 'parsing',
    'regex': 'parsing',
    'bitwise': 'bitwise',
    'bitmask': 'bitwise',
    'geometry2d': 'geometry',
    'geometry3d': 'geometry',
    'polygon': 'geometry',
    'surface': 'geometry',
    'trigonometry': 'geometry',
    'coordinate-geometry': 'geometry',
    'memoization': 'dynamic-programming',
    'dp': 'dynamic-programming',
    'disjoint-set': 'union-find',
    'word-search': 'grid',
    'walk3d': 'walk',
    'subsequence-optimization': 'optimization',
    'quadratic-interpolation': 'math',
    'operator-precedence': 'parsing',
    'number-system': 'math',
    'mask': 'bitwise',
    'infinite-sequence': 'sequence',
    'double-parse': 'parsing',
    'char-sequence': 'sequence',
    'assembled-optimization': 'optimization',
    'hard-description': 'annoying',
    'range': 'ranges',
}

GARBAGE = [
    "", "|>", "tags", "string.trim()", "defp", "do", "tags)", "trim", 
    "string.split(", "lib/helpers/meta.ex:", "format_tags(\"tags:\"", "<>", "true)"
]

def clean_tag(t):
    # Strip common punctuation that might get caught
    t = t.lower().strip(':-_.,()[]{}|"\'').strip()
    
    if t in GARBAGE:
        return None
    
    # Replace internal underscores/spaces with dashes
    t = t.replace('_', '-').replace(' ', '-')
    
    if t in MAPPING:
        return MAPPING[t]
    
    return t

def process_file(path):
    content = path.read_text()
    
    pattern = re.compile(r'(^\s*Tags:)(.*)', re.MULTILINE)
    
    def replacer(match):
        prefix = match.group(1)
        raw_tags = match.group(2).strip().split()
        
        cleaned = set()
        for t in raw_tags:
            c = clean_tag(t)
            if c:
                cleaned.add(c)
        
        if not cleaned:
            return f"{prefix}"
            
        sorted_tags = sorted(list(cleaned))
        return f"{prefix} {' '.join(sorted_tags)}"

    new_content, count = pattern.subn(replacer, content)
    if count > 0 and new_content != content:
        path.write_text(new_content)
        return True
    return False

if __name__ == "__main__":
    updated_count = 0
    for f in FILES:
        if process_file(f):
            updated_count += 1
    print(f"Updated tags in {updated_count} files.")
