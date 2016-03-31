#!/usr/bin/env python
# -*- coding: utf-8 -*-

# translate utility that utilize google translator, support python2 & python3
# Note that the order or arguments in the URL matters.

try:
    from urllib import urlencode
except:
    from urllib.parse import urlencode

try:
    import urllib2
except:
    import urllib.request as urllib2

import json
import sys
reload(sys)
sys.setdefaultencoding('utf-8')

def translate(text, from_lang="auto", to_lang="zh-CN"):
    """translate text, return the result as json"""
    url = 'https://translate.googleapis.com/translate_a/single?'

    params = []
    params.append('client=gtx')
    params.append('sl=' + from_lang)
    params.append('tl=' + to_lang)
    params.append('hl=en-US')
    params.append('dt=t')
    params.append('dt=bd')
    params.append('dj=1')
    params.append('source=input')
    params.append(urlencode({'q': text}))
    url += '&'.join(params)

    request = urllib2.Request(url)
    browser = "Mozilla/5.0 (X11; Linux x86_64; rv:45.0) Gecko/20100101 Firefox/45.0"
    request.add_header('User-Agent', browser)
    response = urllib2.urlopen(request)
    return json.loads(response.read().decode('utf8'))

def voice(text, lang='en'):
    """return the sound of an word, in mp3 bytes"""
    url = 'https://translate.googleapis.com/translate_tts?'

    params = []
    params.append('client=gtx')
    params.append('ie=UTF-8')
    params.append('tl=' + lang)
    params.append(urlencode({'q': text}))
    url += '&'.join(params)
    request = urllib2.Request(url)
    browser = "Mozilla/5.0 (X11; Linux x86_64; rv:45.0) Gecko/20100101 Firefox/45.0"
    request.add_header('User-Agent', browser)
    response = urllib2.urlopen(request)
    return response.read()

def format_json(result, max_entries=5):
    ret = []
    for sentence in result['sentences']:
        ret.append(sentence['orig'] + ': ' + sentence['trans'])
        ret.append('')

    # format pos
    if 'dict' in result:
        for pos in result['dict']:
            ret.append(pos['pos'])
            for entry in pos['entry'][:max_entries]:
                ret.append(entry['word'] + ': ' + ', '.join(entry['reverse_translation']))
            ret.append('')

    return '\n'.join(ret)


import argparse
def main():
    parser = argparse.ArgumentParser(description="translator")
    parser.add_argument('words', type=str, help="word or sentence", nargs="+")
    parser.add_argument('-f', '--from_lang', help="language of the input", default='auto')
    parser.add_argument('-t', '--to_lang', help="target language", default='zh-CN')
    parser.add_argument('-v', '--voice', help="output voice stream data instead of translation", action='store_true', default=False)

    args = parser.parse_args()

    sentence = ' '.join(args.words)
    result = translate(sentence, from_lang=args.from_lang, to_lang=args.to_lang)
    if args.voice:
        sys.stdout.write(voice(args.words, result['src']))
    else:
        sys.stdout.write(format_json(result))

if __name__ == '__main__':
    main()
