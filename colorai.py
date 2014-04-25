from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import HtmlFormatter
from pygments.filters import VisibleWhitespaceFilter

from pygments.formatter import Formatter
from pygments.util import OptionError, get_choice_opt
from pygments.token import Token
from pygments.console import colorize


TOKEN_DICT = {}

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
            for ttype, value in tokensource:
                #write("%s\t%r\n" % (ttype, value)) 
                if ttype in TOKEN_DICT:
                    TOKEN_DICT[ttype] = (TOKEN_DICT[ttype][0] + len(value), TOKEN_DICT[ttype][1] + 1)
                else: 
                    TOKEN_DICT[ttype] = (len(value), 1)
                
        flush()


from pygments.style import Style
from pygments.token import STANDARD_TYPES

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

def make_MRFStyle(TD):
    background_color_tuple = (0,0,0) 

    def generate_mcmc_styles(st, bgColor):
        ## initialization
        import random
        dict = st.copy()
        for name in dict.keys():
            dict[name] = (random.randint(100,255), random.randint(100,255), random.randint(100,255))
        ## mcmc sampling
            
        ## output
        return toHex(dict)

    class MRFStyle(Style):
        background_color = '#%02x%02x%02x' % background_color_tuple
        default_style = ""
        styles = generate_mcmc_styles(TD, background_color_tuple)


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
    
    import csv
    w = csv.writer(open("out.csv", "w"))
    for key, val in TOKEN_DICT.items():
        w.writerow([key, val])

    renderHtmlFile = open('out.html','w')
    highlight(codeSample, lexer2, HtmlFormatter(full="True", style=make_MRFStyle(TOKEN_DICT)), renderHtmlFile)
    renderHtmlFile.close()
    os.system("open out.html")

if __name__ == "__main__":
    main()


