import reducts

reduction = {}
for i in "AEIOUTNaeioutn":
    reduction[ord(i)] = None

print(reducts.scheme_cdf("/usr/share/dict/cracklib-small",
                         lambda string: string.translate(reduction),
                         3, 10**12 * 3600 * 365))
