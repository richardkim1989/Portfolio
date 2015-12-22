import time
import urllib2
from urllib2 import urlopen

watchlist = ['ibm', 'aapl', 'goog', 'yhoo', 'adbe', 'ge', 'xom', 'cvx', 'pg', 'mmm', 'jnj', 'mcd', 'wmt', 'utx', 'ko', 'ba', 'cat', 'jpm', 'hpq', 'vz', 't', 'dd', 'mrk', 'dis', 'hd', 'msft', 'axp', 'bac', 'pfe', 'ge', 'intc', 'aa', 'c', 'gm', 'wfc']

def yahooKeyStats(stock):
    try:
        sourceCode = urllib2.urlopen('http://finance.yahoo.com/q/ks?s='+stock).read()

        #Price/Book Ratio
        PBR = sourceCode.split('Price/Book (mrq):</td><td class="yfnc_tabledata1">')[1].split('</td>')[0]
        if float(PBR) < 2:

            #PEG Ratio (5 years expected)
            PEG5e = sourceCode.split('PEG Ratio (5 yr expected)<font size="-1"><sup>1</sup></font>:</td><td class="yfnc_tabledata1">')[1].split('</td>')[0]
            if 0 < float(PEG5e) < 2:

                #Debt to Equity Ratio
                DE = sourceCode.split('Total Debt/Equity (mrq):</td><td class="yfnc_tabledata1">')[1].split('</td>')[0]
                #if 0 < float(DE) < 1:
            
                #Trailing PE Ratio (12 months)
                PE12t = sourceCode.split('Trailing P/E (ttm, intraday):</td><td class="yfnc_tabledata1">')[1].split('</td>')[0]
                if float(PE12t) < 15:
                    print '____________________________________________'
                    print ''
                    print stock, 'meets requirements'
                    print 'Price to Book: ', PBR
                    print 'PEG Ratio Forward 5 Years: ', PEG5e
                    print 'Trailing P/E 12 Months: ', PE12t
                    print 'Debt to Equity: ', DE
                    print '____________________________________________'

    except Exception, e:
        print stock
        print 'failed in the main loop', str(e)
        pass

for eachStock in watchlist:
    yahooKeyStats(eachStock)
    #time.sleep(0.2)
