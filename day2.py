import argparse

print("Day 2")

parser = argparse.ArgumentParser()
parser.add_argument('input_filename')
args = parser.parse_args()

print(f'reading input from {args.input_filename}')
test_input = open(args.input_filename, 'r').read()

guide = [(line[0], line[2]) for line in test_input.split("\n") if line != '']

if len(guide) < 10:
    print(guide)

mapping = {
        'A': 'ROCK',
        'B': 'PAPER',
        'C': 'SCISSORS',
        'X': 'ROCK',
        'Y': 'PAPER',
        'Z': 'SCISSORS',
}

def result_score(result):
    return {'WIN': 6, 'LOSE': 0, 'DRAW': 3}[result]

def move_score(move):
    return {'ROCK': 1, 'PAPER': 2, 'SCISSORS': 3}[move]

def result(them, you):
    if them == 'ROCK' and you == 'ROCK':
        return 'DRAW'
    elif them == 'ROCK' and you == 'PAPER':
        return 'WIN'
    elif them == 'ROCK' and you == 'SCISSORS':
        return 'LOSE'

    elif them == 'PAPER' and you == 'ROCK':
        return 'LOSE'
    elif them == 'PAPER' and you == 'PAPER':
        return 'DRAW'
    elif them == 'PAPER' and you == 'SCISSORS':
        return 'WIN'

    elif them == 'SCISSORS' and you == 'ROCK':
        return 'WIN'
    elif them == 'SCISSORS' and you == 'PAPER':
        return 'LOSE'
    elif them == 'SCISSORS' and you == 'SCISSORS':
        return 'DRAW'

def move_for_result(them, target_result):
    if target_result == 'DRAW':
        return them

    if them == 'ROCK' and target_result == 'WIN':
        return 'PAPER'
    elif them == 'ROCK' and target_result == 'LOSE':
        return 'SCISSORS'

    elif them == 'PAPER' and target_result == 'WIN':
        return 'SCISSORS'
    elif them == 'PAPER' and target_result == 'LOSE':
        return 'ROCK'

    elif them == 'SCISSORS' and target_result == 'WIN':
        return 'ROCK'
    elif them == 'SCISSORS' and target_result == 'LOSE':
        return 'PAPER'

def map_target_result(t):
    return {'X': 'LOSE', 'Y': 'DRAW', 'Z': 'WIN'}[t]

def score_round_p2(them, target_result):
    you = move_for_result(them, map_target_result(target_result))
    return result_score(result(them, you)) + move_score(you)

def score_round_p1(them, you):
    return result_score(result(them, mapping[you])) + move_score(mapping[you])

scores_p1 = [score_round_p1(mapping[a], b) for (a, b) in guide]

print(f"part one")
# print(f"scores = {scores_p1}")
print(f"total score = {sum(scores_p1)}")

scores_p2 = [score_round_p2(mapping[a], b) for (a, b) in guide]

print("part two")
# print(f"scores = {scores_p2}")
print(f"total score = {sum(scores_p2)}")

