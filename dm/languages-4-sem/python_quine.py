a = 5
def getSrc():
    src = getOtherSrc()
    return src + '\ngetOtherSrc(): return "' + src + '"'

def getOtherSrc():
    return 'a = 5\ndef getSrc():\nsrc = getOtherSrc()\nreturn src + \'getOtherSrc(): return "\' + src \'"\'\n print(getSrc())' 

print(getSrc())

