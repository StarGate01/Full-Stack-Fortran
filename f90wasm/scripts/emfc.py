#!/usr/bin/python3

import sys, os, re
from shlex import quote as cmd_quote


re_load = re.compile(r'(.*=\s*load(?:\s|atomic|volatile)*)(.*)(\*.*)')
re_getelptr = re.compile(r'(.*=\s*getelementptr(?:\s|inbounds)*)(.*)(\*.*)')
re_metadata = re.compile(r'^\!.*=\s*metadata')
re_metadata_rec = re.compile(r'\!(.*)\s*=\s*(?:distinct|)\s*\!{\s*\!\1\s*}')

def compile(args):
    os.system("gfortran-4.6 -emit-llvm -S -flto -m32 -fverbose-asm -nostdlib -fplugin=/app/bin/dragonegg.so " + args)

def upgrade(source, target):
    rec_meta = 0
    with open(source, 'r') as sfile:
        with open(target, 'w') as tfile:
            for line in sfile:
                if(line.startswith("target datalayout")):
                    line = 'target datalayout = "e-m:e-p:32:32-i64:64-n32:64-S128"\n'
                elif(line.startswith("target triple")):
                    line = 'target triple = "wasm32-unknown-emscripten"\n'
                else:
                    m_load = re_load.match(line)
                    m_getelptr = re_getelptr.match(line)
                    if(m_load != None):
                        line = m_load.group(1) + m_load.group(2) + ", " + m_load.group(2) + m_load.group(3) + "\n"
                    if(m_getelptr != None):
                        line = m_getelptr.group(1) + m_getelptr.group(2) + ", " + m_getelptr.group(2) + m_getelptr.group(3) + "\n"
                    elif(re_metadata.match(line)):
                        line = line.replace("metadata", "")

                    m_metarec = re_metadata_rec.match(line)
                    if(m_metarec != None):
                        line = "!" + m_metarec.group(1) + ' = !{ !"rec_meta_' + str(rec_meta) + '" }'
                        rec_meta = rec_meta + 1
                    
                tfile.write(line)

def assemble(source, target):
    os.system("llvm-as-5.0 " + source + " -o " + target)


if __name__ == "__main__":
    outfile = ""
    for i, arg in enumerate(sys.argv):
        if(arg == "-o" and len(sys.argv) > i+1):
            outfile = sys.argv[i+1]
            break
    if(outfile == ""):
        print("Cannot detect outfile")
        print(sys.argv)
        sys.exit(1)
    else:
        args = " ".join(map(cmd_quote, sys.argv[1:]))
        compile(args)
        os.rename(outfile, outfile + ".ll_pre")
        upgrade(outfile + ".ll_pre", outfile + ".ll")
        assemble(outfile + ".ll", outfile)
