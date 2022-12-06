import argparse

print("Day 3")

parser = argparse.ArgumentParser()
parser.add_argument('input_filepath')
args = parser.parse_args()

print(f'Reading input from {args.input_filepath}')
puzzle_input = open(args.input_filepath, 'r').read()

priorities = {chr(v):k for (k, v) in zip(range(1, 27), range(ord('a'), ord('z')+1))} | { chr(v):k for (k, v) in zip(range(27, 53), range(ord('A'), ord('Z')+1))}

print(f"priorities = {priorities}")

lines = [line for line in puzzle_input.split("\n") if len(line) > 0]

dupes = [set(line[0:int(len(line)/2)]).intersection(set(line[int(len(line)/2):])).pop()
        for line in lines]

print(dupes)

dps = [priorities[d] for d in dupes]

# print(dps)

print(f"part one = {sum(dps)}")

groups = [lines[x*3:(x*3)+3] for x in range(0, int(len(lines)/3))]

# print(groups)

gps = [set(group[0]).intersection(set(group[1])).intersection(set(group[2])).pop()
        for group in groups]

# print(gps)

print(f"part two = {sum([priorities[p] for p in gps])}")
