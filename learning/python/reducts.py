import blocks 

def scheme_cdf(dictionary, compress, dimension, tries):
    reducts, word_count = parse_dictionary(dictionary, compress)
    block_list = get_block_list(reducts)
    covered = 0
    blocks = block_list.power(dimension)
    for trial in range(tries):
        try:
            block = next(blocks)
        except StopIteration:
            break

        covered += block.size * block.count

    return covered/(word_count**dimension)

def parse_dictionary(dictionary, compress):
    reducts = {}
    word_count = 0
    with open(dictionary, "r") as f:
        for word in f:
            reduct = compress(word)
            reducts[reduct] = reducts.get(reduct, 0) + 1
            word_count += 1
    
    return reducts, word_count

def get_block_list(reducts):
    frequency_dict = {}
    for reduct, frequency in reducts.items():
        frequency_dict[frequency] = frequency_dict.get(frequency, 0) + 1


    return blocks.BlockList(
            blocks.Block(size=frequency, count=occurrences)
            for frequency, occurrences in frequency_dict.items()
    )
