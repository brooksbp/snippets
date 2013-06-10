import requests
import json
import math
import signal
import sys

class HistData():

    def __init__(self, symbol):
        self.symbol = symbol
        self.data = None
    
    def setRange(self, startDate, endDate):
        self.startDate = startDate
        self.endDate = endDate
        self.data = None

    def get(self):
        r = 'http://query.yahooapis.com/v1/public/yql?q=select * from yahoo.finance.historicaldata where symbol="{}" and startDate="{}" and endDate="{}"&env=store://datatables.org/alltableswithkeys&format=json&diagnostics=true'.format(self.symbol, self.startDate, self.endDate)
        while self.data is None:
            request = requests.get(r)
            self.data = json.loads(request.text)
            if self.data['query']['count'] < 1:
                self.data = None

    def get_close(self):
        if self.data is None:
            self.get()
        return [ float(quote['Close']) for quote in self.data['query']['results']['quote'] ]

def corr(a, b):
    n = float(len(a))
    a_avg    = math.fsum(a) / n
    a_sq_avg = math.fsum([x ** 2 for x in a]) / n
    b_avg    = math.fsum(b) / n
    b_sq_avg = math.fsum([x ** 2 for x in b]) / n
    m_avg    = math.fsum([i*j for i,j in zip(a, b)]) / n
    a_var    = a_sq_avg - (a_avg ** 2)
    b_var    = b_sq_avg - (b_avg ** 2)
    covar    = m_avg - (a_avg * b_avg)
    return covar / math.sqrt(a_var * b_var)

def main():
    symbols = [
        'VYM', 'MGV', 'VTV', 'VIG', 'VV',
        'MGC', 'VOO', 'VTI', 'VUG', 'MGK',
        'VOE', 'VXF', 'VO', 'VBR', 'VB',
        'VBK',
        'VCR', 'VDC', 'VDE', 'VFH', 'VHT',
        'VIS', 'VGT', 'VAW', 'VNQ', 'VOX',
        'VPU',
        'VEU', 'VSS', 'VEA', 'VWO', 'VGK',
        'VPL', 'VNQI', 'VXUS', 'VT'
    ]

    ETFs = [HistData(x) for x in symbols]

    start = '2012-06-07'
    end   = '2013-06-07'
    for e in ETFs:
        e.setRange(start, end)

    c = []
    for a in range(0, len(ETFs) - 1):
        for b in range(a + 1, len(ETFs)):
            an = ETFs[a].symbol
            bn = ETFs[b].symbol
            cor = corr(ETFs[a].get_close(), ETFs[b].get_close())
            c.append((cor, an, bn))

    cs = sorted(c, key=lambda e: e[0])
    for e in cs:
        print('corr({},{})={}'.format(e[1], e[2], e[0]))

def SIGINT(signo, frame):
    sys.exit(0)

if __name__ == '__main__':
    signal.signal(signal.SIGINT, SIGINT)
    main()
