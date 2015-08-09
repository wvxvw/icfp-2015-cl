all:
	/usr/local/bin/sbcl --non-interactive --eval "(asdf:operate 'asdf:program-op :icfp-2015-cl)"

clean:
	rm -f ./icfp-2015-cl
