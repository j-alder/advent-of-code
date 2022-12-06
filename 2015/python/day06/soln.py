def fmt_inst(inst):
    if inst[0] == "turn":
        return { "op": inst[1], "start": inst[2].split(','), "end": inst[4].strip('\n').split(',') }
    elif inst[0] == "toggle":
        return { "op": "toggle", "start": inst[1].split(','), "end": inst[3].strip('\n').split(',') }
    else:
        return

def initial_grid():
    return 

def soln():
    inst = [fmt_inst(x.split(' ')) for x in open("../../../2022/input/six.txt").readlines()]

soln()
