#!/usr/bin/env python

def print_summary(grammar, concrete, results):

    successes = [(o,l,r,t) for (o,l,r,t) in results if r == "s"]
    failures = [(o,l,r,t) for (o,l,r,t) in results if r == "f"]

    summary =  "\n"
    summary += "\n"
    summary += u"  Abstract: "+grammar+"\n"
    summary += u"  Concrete: "+concrete+"\n"
    summary += "\n"
    summary += u"  Successes: "+str(len(successes))+"\n"
    summary += u"  Failures : "+str(len(failures))+"\n"
    summary += "\n"
    summary += u"----------------------------------------"+'\n\n'
        
    return summary

def print_result(obj,lin,res,time):
    result  = u"Tree: "+ obj["Abs"]+'\n'
    result += u"Gold: " + obj["Afr"]+'\n'
    if res == "s":
        result += u"Succ: " + unicode(lin,'utf8')+'\n'
    else:
        result += u"Fail: " + unicode(lin,'utf8')+'\n'
    result += "Time: " + str(time)+'\n\n'
    return result
    

if __name__ == "__main__":
    
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('treebank', help='input file: json file with treebank')
    parser.add_argument('grammar', help='input file: PGF grammar file for parsing treebank')
    parser.add_argument('concrete', help='input: grammar module name for linearising treebank')
    parser.add_argument('result', help="output file: text file with results")
    args = parser.parse_args()
    
    import codecs
    import json
    treebank_str = codecs.open(args.treebank, 'r', 'utf8').read()
    treebank = json.loads(treebank_str)
    
    import pgf
    gr = pgf.readPGF(args.grammar)
    conc = gr.languages[args.concrete]
    
    results = []
    times = []
    
    import time
    for o in treebank:
        tree = o["Abs"]
        gold = o["Afr"]
        
        try:
            e = pgf.readExpr(tree)
            start = time.clock()
            l = conc.linearize(e)
            end = time.clock()
            if unicode(l,'utf8') == gold:
                results.append((o,l,"s",end - start))
            else:
                results.append((o,l,"f",end - start))
        except pgf.PGFError:
            pass
    
    summary_str = print_summary(args.grammar, args.concrete, results)
    result_file = codecs.open(args.result, 'w', 'utf8')
    result_file.write(summary_str)
    
    for (o,l,r,t) in results:
        result_file.write(print_result(o,l,r,t))
    result_file.close()
    
    
