import hashlib

def get_hash(input):
    h = hashlib.md5('yzbqklnj{i}'.format(i = input).encode()).hexdigest()
    print(h)
    return h

def check_hash(input):
    return str(get_hash(input)).startswith('000000')

def find_hash():
    x = 1
    while(not check_hash(x)):
        x = x + 1
    return x

if __name__ == '__main__':
    print(find_hash())
