.ONESHELL:
.PHONY: testdata bench

testdata:
	@for file in testdata/*.html; do
		echo $$file
		cargo run $(CARGO_FLAGS) -q --manifest-path htmlminify_cli/Cargo.toml $$file $$file.minified
	done