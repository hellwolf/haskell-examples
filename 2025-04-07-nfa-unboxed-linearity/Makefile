bench: aa.dat aaz.dat ab.dat abz.dat
	for i in aa aaz ab abz;do ./nfa-perf.hs linear2 $$i.dat +RTS -s;done
	for i in aa aaz ab abz;do ./nfa-perf.hs st $$i.dat +RTS -s;done

aa.dat:
	runghc <<< 'main = writeFile "aa.dat" (concat (replicate 10_000_000 "aa"))'
aaz.dat: aa.dat
	cp $< $@
	echo -n z >> $@
ab.dat:
	runghc <<< 'main = writeFile "ab.dat" (concat (replicate 10_000_000 "ab"))'
abz.dat: ab.dat
	cp $< $@
	echo -n z >> $@

clean:
	rm -f *.dat

.PHONY: bench clean
