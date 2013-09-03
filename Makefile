compile:
	@ghc -O -isrc -odir bin -hidir bin -o bin/buildindicator src/Main.hs

clean:
	@rm -rf bin/*