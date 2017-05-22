.PHONY: clean docs

docs:
	./docs/doc-parser.ros docs/README.org README.org

clean:
	rm README.org
