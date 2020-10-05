.ONESHELL:
.PHONY: testdata bench

testdata:
	@for file in testdata/*.html; do
		cargo run -q --manifest-path htmlminify_cli/Cargo.toml $$file $$file.minified
	done