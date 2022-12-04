import argparse

print("Day 4")

parser = argparse.ArgumentParser()
parser.add_argument('input_filepath')
args = parser.parse_args()

print(f'Reading input from {args.input_filepath}')
puzzle_input = open(args.input_filepath, 'r').read()

lines = [line for line in puzzle_input.split("\n") if len(line) > 0]

if len(lines) <= 10:
    print(puzzle_input)

def parse_range(line):
    pair = line.split(",")
    r1 = pair[0].split("-")
    r2 = pair[1].split("-")
    range1 = range(int(r1[0]), int(r1[1]) + 1)
    range2 = range(int(r2[0]), int(r2[1]) + 1)
    return ({x for x in range1}, {x for x in range2})

ranges = [parse_range(line) for line in lines]

print(ranges)

def is_contained(a: set, b: set) -> bool:
    u = a.union(b)
    return a == u or b == u

contained_ranges = [pair for pair in ranges if is_contained(pair[0], pair[1])]

print(contained_ranges)

print(f"contained_ranges = {len(contained_ranges)}")

def is_overlapping(a: set, b: set) -> bool:
    return len(a.intersection(b)) > 0

overlapping_ranges = [pair for pair in ranges if is_overlapping(pair[0], pair[1])]

print(f"overlapping ranges = {len(overlapping_ranges)}")
