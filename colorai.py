from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter
from pygments.filters import VisibleWhitespaceFilter

from pygments.formatter import Formatter
from pygments.util import OptionError, get_choice_opt
from pygments.token import Token
from pygments.console import colorize


TOKEN_DICT = {}
TOKEND_DICT = {}
TOKENS_DICT = {}
## Taken from source code of pygments, change RawTokenFormatter() to AITokenFormatter()
class AITokenFormatter(Formatter):
    r"""
    Format tokens as a raw representation for storing token streams.

    The format is ``tokentype<TAB>repr(tokenstring)\n``. The output can later
    be converted to a token stream with the `RawTokenLexer`, described in the
    :doc:`lexer list <lexers>`.

    Only two options are accepted:

    `compress`
        If set to ``'gz'`` or ``'bz2'``, compress the output with the given
        compression algorithm after encoding (default: ``''``).
    `error_color`
        If set to a color name, highlight error tokens using that color.  If
        set but with no value, defaults to ``'red'``.

        .. versionadded:: 0.11

    """
    name = 'Raw tokens'
    aliases = ['raw', 'tokens']
    filenames = ['*.raw']

    unicodeoutput = False

    def __init__(self, **options):
        Formatter.__init__(self, **options)
        if self.encoding:
            raise OptionError('the raw formatter does not support the '
                              'encoding option')
        self.encoding = 'ascii'  # let pygments.format() do the right thing
        self.compress = get_choice_opt(options, 'compress',
                                       ['', 'none', 'gz', 'bz2'], '')
        self.error_color = options.get('error_color', None)
        if self.error_color is True:
            self.error_color = 'red'
        if self.error_color is not None:
            try:
                colorize(self.error_color, '')
            except KeyError:
                raise ValueError("Invalid color %r specified" %
                                 self.error_color)

    def format(self, tokensource, outfile):
        try:
            outfile.write(b'')
        except TypeError:
            raise TypeError('The raw tokens formatter needs a binary '
                            'output file')
        if self.compress == 'gz':
            import gzip
            outfile = gzip.GzipFile('', 'wb', 9, outfile)
            def write(text):
                outfile.write(text.encode())
            flush = outfile.flush
        elif self.compress == 'bz2':
            import bz2
            compressor = bz2.BZ2Compressor(9)
            def write(text):
                outfile.write(compressor.compress(text.encode()))
            def flush():
                outfile.write(compressor.flush())
                outfile.flush()
        else:
            def write(text):
                outfile.write(text.encode())
            flush = outfile.flush

        if self.error_color:
            for ttype, value in tokensource:
                line = "%s\t%r\n" % (ttype, value)
                if ttype is Token.Error:
                    write(colorize(self.error_color, line))
                else:
                    write(line)
        else:
            pttype = None
            pvalue = None
            pdict = {}
            from pygments.token import Token

            TOKEN_DICT[Token] = (0,0)
            for ttype, value in tokensource:
                pdict[ttype] = 1 # check the presence of ttype of current line

                if ttype in TOKEN_DICT:
                    TOKEN_DICT[ttype] = (TOKEN_DICT[ttype][0] + len(value), TOKEN_DICT[ttype][1])
                else: 
                    TOKEN_DICT[ttype] = (len(value), 0)

                
                if ttype == Token.Text.Whitespace:
                    if value == '\n':
                        if pttype is not None:
                            if pttype in TOKENS_DICT:
                                TOKENS_DICT[pttype].append(len(pvalue))
                            else:
                                TOKENS_DICT[pttype] = [len(pvalue)]                        
                            TOKEN_DICT[Token] = (0, TOKEN_DICT[Token][1] + 1) # count total non-empty lines
                        pttype = None
                        pvalue = None
                        for ttype in pdict:
                            TOKEN_DICT[ttype] = (TOKEN_DICT[ttype][0], TOKEN_DICT[ttype][1] + 1)
                        pdict = {}
                    continue
                #else:
                #    print (ttype, value)


                if pttype is None:
                    pttype = ttype
                    pvalue = value
                    continue
                    
                if pttype == ttype:
                    pvalue = pvalue + value
                else:
                    if (pttype, ttype) in TOKEND_DICT:
                        TOKEND_DICT[(pttype, ttype)].append((len(pvalue), len(value)))
                    else:
                        TOKEND_DICT[(pttype, ttype)] = [(len(pvalue), len(value))]

                    if pttype in TOKENS_DICT:
                        TOKENS_DICT[pttype].append(len(pvalue))
                    else:
                        TOKENS_DICT[pttype] = [len(pvalue)]
                    pttype = ttype
                    pvalue = value
        flush()


from pygments.style import Style
from pygments.token import STANDARD_TYPES, string_to_tokentype

def toHex(colorDict):
    hexcd = colorDict.copy()
    for name, val in colorDict.items():
        hexcd[name] = '#%02x%02x%02x' % colorDict[name]
    return hexcd


class RandomStyleBlack(Style):
    background_color = "#000"
    default_style = ""


    def generate_random_styles(st):
        import random
        dict = st.copy()
        for name in dict.keys():
            dict[name] = (random.randint(100,255), random.randint(100,255), random.randint(100,255))
        return toHex(dict)

    styles = generate_random_styles(STANDARD_TYPES)

class RandomStyleWhite(Style):
    background_color = "#fff"
    default_style = ""


    def generate_random_styles(st):
        import random
        dict = st.copy()
        for name in dict.keys():
            dict[name] = (random.randint(0,155), random.randint(0,155), random.randint(0,155))
        return toHex(dict)

    styles = generate_random_styles(STANDARD_TYPES)

def make_MRFStyle():
#    background_color_tuple = (0,0,0) 

    def load_json_styles():
        import json
        themeFile = open("theme.json",'r')
        jColorTheme = json.load(themeFile)        
        themeFile.close()
        jColorThemeTuple = {string_to_tokentype(k):tuple(v) for k, v in jColorTheme.items()}
        background_color_tuple = jColorThemeTuple[Token]
        ## output
        return toHex(jColorThemeTuple)

    class MRFStyle(Style):
        default_style = ""
        styles = load_json_styles()
        styles[Token.Comment] = styles[Token.Comment] + " italic"
#        styles[Token.Keyword] = styles[Token.Keyword] + " bold"

        background_color = styles[Token] #'#%02x%02x%02x' % background_color_tuple


    return MRFStyle

    
import os 
from pygments.lexers import guess_lexer, guess_lexer_for_filename

def main():
    filename = __file__
    codeSampleFile = open(filename,'r')
    codeSample = codeSampleFile.read()
    codeSampleFile.close()

    lexer = guess_lexer_for_filename(filename, codeSample)
    lexer2 = guess_lexer_for_filename(filename, codeSample)

    lexer.add_filter(VisibleWhitespaceFilter())
    highlight(codeSample, lexer, AITokenFormatter())
    
    DataUnary = {}
    DataPair = {}
    for key, val in TOKENS_DICT.items():
        import numpy, math
        arrayOfTokens = numpy.log2(TOKENS_DICT[key])
        DataUnary[str(key)] = (TOKEN_DICT[key][0], TOKEN_DICT[key][1], numpy.mean(arrayOfTokens), numpy.median(arrayOfTokens), numpy.std(arrayOfTokens))
    DataUnary[str(Token)] = (0, TOKEN_DICT[Token][1], 0, 0, 0)
    for key, val in TOKEND_DICT.items():
        DataPair[str(key)] = val
        
    Data = {'unary': DataUnary, 'pair': DataPair}

    import json
    dataString = json.dumps(Data)
    fileoutput = open('out.json','w+')
    fileoutput.write(dataString)
    fileoutput.close()

    #os.system("cd color; java -jar target/color-*-SNAPSHOT.jar; cd ..")
    #import time
    #time.sleep(1) # wait theme.json to be sync

    renderHtmlFile = open('out.html','w')
    highlight(codeSample, lexer2, HtmlFormatter(full="True", style=make_MRFStyle()), renderHtmlFile)
    #highlight(codeSample, lexer2, HtmlFormatter(full="True", style="vim"), renderHtmlFile)

    renderHtmlFile.close()
    os.system("open out.html")

if __name__ == "__main__":
    main()







