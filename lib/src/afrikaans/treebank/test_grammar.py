#!/usr/bin/env python

def print_summary(grammar, concrete, results):

    successes = [(o,l,r) for (o,l,r) in results if r == "s"]
    failures = [(o,l,r) for (o,l,r) in results if r == "f"]

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

def print_result(obj,lin,res):
    result  = u"Tree: "+ obj["Abs"]+'\n'
    result += u"Gold: " + obj["Afr"]+'\n'
    if res == "s":
        result += u"Succ: " + unicode(lin,'utf8')+'\n\n'
    else:
        result += u"Fail: " + unicode(lin,'utf8')+'\n\n'
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
    
    for o in treebank:
        tree = o["Abs"]
        gold = o["Afr"]
        
        try:
            e = pgf.readExpr(tree)
            l = conc.linearize(e)
            if unicode(l,'utf8') == gold:
                results.append((o,l,"s"))
            else:
                results.append((o,l,"f"))
        except pgf.PGFError:
            pass
    
    summary_str = print_summary(args.grammar, args.concrete, results)
    result_file = codecs.open(args.result, 'w', 'utf8')
    result_file.write(summary_str)
    
    for (o,l,r) in results:
        result_file.write(print_result(o,l,r))
    result_file.close()
    
    
