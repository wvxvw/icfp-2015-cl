all:
	sbcl --non-interactive --eval "(asdf:operate 'asdf:program-op :icfp-2015-cl)"
	mv ./icfp-2015-cl ./play_icfp2015

clean:
	rm -f ./play_icfp2015
