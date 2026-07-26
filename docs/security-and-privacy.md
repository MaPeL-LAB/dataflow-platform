# Security and Privacy

## Python pickle

Pickle deserialization can execute code embedded in a malicious file. The pipeline therefore rejects `.pkl` and `.pickle` files unless `input.allow_unsafe_pickle` or `--allow-unsafe-pickle true` is explicitly set. Enable it only for a file whose source and integrity are trusted. Prefer JSON, CSV, Parquet, or Feather/Arrow for exchange between Python and R.

## Representative examples

Variable examples make a dictionary easier to review but can expose personal or confidential values. Defaults therefore mask examples when a column name or sampled values suggest an identifier, name, email, phone number, address, government identifier, patient/customer/employee field, or similar sensitive content.

These rules are heuristics. A field not flagged is not guaranteed to be nonsensitive, and a flagged field is not necessarily regulated data. For stricter environments, set:

```yaml
privacy:
  include_examples: false
```

## Source files

The pipeline reads data without intentionally modifying the source files. Outputs can still contain labels, counts, ranges, and unmasked examples for fields that were not recognized as sensitive. Store output bundles according to the same access controls applied to the source data.

## Hashes

File MD5 calculation is off by default because it can add substantial I/O for large files and MD5 is not a modern authenticity mechanism. It is included only as a reproducibility fingerprint when `input.compute_md5: true` is enabled.
